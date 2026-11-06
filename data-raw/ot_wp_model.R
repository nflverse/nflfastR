# Builds the overtime win probability model that lives in R/sysdata.rda
# (objects `ot_wp_models` and `ot_wp_dp`). See R/helper_add_ot_wp.R for the idea.
#
# Needs nflverse play-by-play 2001+ (loaded from nflreadr unless a path to a
# saved data frame is supplied in the environment variable OT_PBP).
#
# Run from the package root.

library(dplyr)
library(xgboost)
source("R/helper_add_ot_wp.R")
set.seed(1)

# ---- helpers only needed to build the tables ------------------------------------
# (possession number, differential) combinations the recursion can reach
ot_reachable <- list(
  c(1, 0),
  c(2, 0),
  c(2, -3),
  c(2, -6),
  c(2, -7),
  c(2, -8),
  c(3, 0)
)

# seasons whose drive starts are used for each kickoff regime, built from the
# regime boundaries in R/helper_add_ot_wp.R (ot_kick_first) so both agree
ot_kick_seasons <- Map(
  c,
  c(2001L, ot_kick_first),
  c(ot_kick_first - 1L, 2100L)
)
# season fed to the drive model for each regime
ot_kick_season_c <- c(2008, 2013, 2020, 2024, 2025)

# thin the full solution to the stored grid
ot_thin_q <- function(Q) {
  Q[,,, match(ot_s_store, ot_s_grid), seq(1L, ot_n + 1L, by = ot_t_stride)]
}


# Build the dynamic programming table for one rule / period length / kickoff
# regime. Called from data-raw/ot_wp_model.R
ot_build_dp <- function(rule, len, kick, models, starts) {
  S <- length(ot_s_grid)
  N <- ot_n
  out_mod <- xgboost::xgb.load.raw(models$out_raw)
  dur_mod <- xgboost::xgb.load.raw(models$dur_raw)
  win <- ot_kick_seasons[[kick]]
  season_c <- ot_kick_season_c[kick]

  # start of drive outcome probabilities and durations
  P <- array(0, c(2, S, 5))
  Pm <- array(0, c(2, S, 5))
  PMF <- vector("list", 2)
  for (ty in 1:2) {
    smp <- starts[
      starts$type == c("kick", "stop")[ty] &
        starts$season >= win[1] &
        starts$season <= win[2],
    ]
    PMF[[ty]] <- vector("list", S)
    for (si in seq_len(S)) {
      feat <- cbind(
        yardline_100 = pmin(pmax(smp$yardline_100, 1), 99),
        down = smp$down,
        ydstogo = pmin(smp$ydstogo, 30),
        posteam_spread = ot_s_grid[si],
        season_c = season_c
      )
      cp <- ot_collapse_preds(ot_predict_matrix(out_mod, feat))
      pr <- cp$P
      P[ty, si, ] <- colMeans(pr)
      # teams that have to score do not punt. A conversion continues the drive
      # from around midfield
      cfeat <- feat
      cfeat[, "yardline_100"] <- 55
      cfeat[, "down"] <- 1
      cfeat[, "ydstogo"] <- 10
      r <- ot_collapse_preds(ot_predict_matrix(out_mod, cfeat))$P
      Pm[ty, si, ] <- colMeans(ot_apply_must_score(
        pr,
        cp$pun,
        ot_go_avg,
        r[, 1],
        r[, 2]
      ))
      PMF[[ty]][[si]] <- vapply(
        1:5,
        function(o) {
          oh <- matrix(0, nrow(feat), 5)
          oh[, o] <- 1
          mu <- pmax(stats::predict(dur_mod, cbind(feat, oh)) - 5, 3)
          sum(pr[, o] * mu) / sum(pr[, o])
        },
        numeric(1)
      )
    }
  }

  Q <- array(NA_real_, c(2, 3, length(ot_d_set), S, N + 1))
  for (r in ot_reachable) {
    di <- ot_map_d(r[2])
    Q[, r[1], di, , 1] <- ot_end_value(r[2])
  }
  for (t in seq_len(N)) {
    jj <- seq_len(t)
    h <- ot_tempo(t * ot_step)
    pm_t <- lapply(1:2, function(ty) {
      lapply(seq_len(S), function(si) {
        lapply(1:5, function(o) {
          if (!o %in% c(1L, 2L, 5L)) {
            return(NULL)
          }
          pm <- ot_pmf(PMF[[ty]][[si]][o] * h, models$shape[[ot_outcomes[o]]])
          list(pm = pm, cum = cumsum(pm))
        })
      })
    })
    for (ty in 1:2) {
      for (r in ot_reachable) {
        p <- r[1]
        d <- r[2]
        di <- ot_map_d(d)
        for (si in seq_len(S)) {
          val <- 0
          for (o in c(1L, 2L, 5L)) {
            pmo <- pm_t[[ty]][[si]][[o]]
            tail <- 1 - pmo$cum[t]
            comps <- ot_trans(rule, p, d, ot_outcomes[o])
            v <- 0
            for (cp in comps) {
              if (is.na(cp$val)) {
                qn <- Q[cp$ty, cp$p, cp$di, S + 1L - si, t - jj + 1L]
                v <- v + cp$w * sum(pmo$pm[jj] * (1 - qn))
              } else {
                v <- v + cp$w * cp$val * sum(pmo$pm[jj])
              }
            }
            pw <- if (p == 2 && d < 0) Pm[ty, si, o] else P[ty, si, o]
            val <- val + pw * (v + tail * ot_end_value(d))
          }
          Q[ty, p, di, si, t + 1L] <- val
        }
      }
    }
  }
  list(Q = Q)
}

# ---- build -------------------------------------------------------------------

pbp_path <- Sys.getenv("OT_PBP")
if (nzchar(pbp_path)) {
  pbp <- readRDS(pbp_path)
} else {
  pbp <- nflreadr::load_pbp(2001:nflreadr::most_recent_season())
}
pbp <- pbp |> filter(season >= 2001)

# ---- drive level data -------------------------------------------------------
d <- pbp |>
  filter(!is.na(posteam), !is.na(fixed_drive), qtr <= 4) |>
  group_by(game_id) |>
  arrange(play_id, .by_group = TRUE) |>
  ungroup()

# a drive ends where the next drive starts
ends <- d |>
  group_by(game_id, fixed_drive) |>
  summarise(start_clock = first(game_seconds_remaining), .groups = "drop") |>
  group_by(game_id) |>
  arrange(fixed_drive, .by_group = TRUE) |>
  mutate(end_clock = lead(start_clock)) |>
  ungroup()
d <- d |>
  left_join(
    ends |> select(game_id, fixed_drive, end_clock),
    by = c("game_id", "fixed_drive")
  )

lab <- c(
  "Touchdown" = "TD",
  "Field goal" = "FG",
  "Safety" = "SAF",
  "Opp touchdown" = "OPPTD",
  "Punt" = "PUNT",
  "Turnover" = "STOP",
  "Turnover on downs" = "STOP",
  "Missed field goal" = "STOP"
)
d <- d |>
  mutate(
    outcome = unname(lab[fixed_drive_result]),
    posteam_spread = ifelse(posteam == home_team, spread_line, -spread_line),
    rem = game_seconds_remaining - end_clock
  )

# plays that are not distorted by the end of a half or by garbage time
tr <- d |>
  filter(
    !is.na(outcome),
    !is.na(down),
    !is.na(yardline_100),
    !is.na(posteam_spread),
    !is.na(rem),
    rem >= 0,
    rem <= 600,
    half_seconds_remaining >= 180,
    abs(score_differential) <= 16,
    play_type %in% c("run", "pass", "punt", "field_goal"),
    two_point_attempt %in% c(0, NA)
  ) |>
  mutate(season_c = pmin(pmax(season, 2006), 2026))

classes <- c("TD", "FG", "SAF", "OPPTD", "STOP", "PUNT")
# durations do not distinguish punts from other stops
classes5 <- c("TD", "FG", "SAF", "OPPTD", "STOP")
tr$outcome5 <- ifelse(tr$outcome == "PUNT", "STOP", tr$outcome)
X <- function(df) {
  as.matrix(
    df |>
      transmute(
        yardline_100,
        down,
        ydstogo = pmin(ydstogo, 30),
        posteam_spread,
        season_c
      )
  )
}
y <- match(tr$outcome, classes) - 1

# early stopping on held out games
test_g <- sample(unique(tr$game_id), length(unique(tr$game_id)) * 0.15)
is_te <- tr$game_id %in% test_g
par <- list(
  objective = "multi:softprob",
  num_class = 6,
  eta = 0.05,
  max_depth = 4,
  min_child_weight = 200,
  subsample = 0.8,
  eval_metric = "mlogloss"
)
m0 <- xgb.train(
  par,
  xgb.DMatrix(X(tr[!is_te, ]), label = y[!is_te]),
  nrounds = 1500,
  evals = list(te = xgb.DMatrix(X(tr[is_te, ]), label = y[is_te])),
  early_stopping_rounds = 30,
  verbose = 0
)
nr <- as.integer(xgb.attr(m0, "best_iteration")) + 1L
out_model <- xgb.train(
  par,
  xgb.DMatrix(X(tr), label = y),
  nrounds = nr,
  verbose = 0
)

# time the rest of the drive takes (gamma regression, outcome as feature)
Dm <- function(df) {
  cbind(X(df), sapply(classes5, function(k) as.numeric(df$outcome5 == k)))
}
dur <- xgb.train(
  list(
    objective = "reg:gamma",
    eta = 0.05,
    max_depth = 4,
    min_child_weight = 200,
    subsample = 0.8
  ),
  xgb.DMatrix(Dm(tr), label = tr$rem + 5),
  nrounds = 300,
  verbose = 0
)
ratio <- (tr$rem + 5) / predict(dur, xgb.DMatrix(Dm(tr)))
shape <- as.list(tapply(ratio, tr$outcome5, function(r) 1 / var(r)))

# The models are stored as raw UBJSON, which is meant to be stable across
# xgboost versions (unlike the legacy binary format). Say so explicitly instead
# of relying on the default of whatever xgboost version is installed.
ot_wp_models <- list(
  out_raw = xgb.save.raw(out_model, raw_format = "ubj"),
  dur_raw = xgb.save.raw(dur, raw_format = "ubj"),
  shape = shape,
  xgboost_version = as.character(utils::packageVersion("xgboost"))
)

# ---- where drives start -----------------------------------------------------
starts <- d |>
  filter(
    !is.na(down),
    !is.na(yardline_100),
    play_type %in% c("run", "pass", "punt", "field_goal")
  ) |>
  group_by(game_id, fixed_drive) |>
  arrange(play_id, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  group_by(game_id) |>
  arrange(fixed_drive, .by_group = TRUE) |>
  mutate(prev_res = lag(fixed_drive_result)) |>
  ungroup() |>
  filter(!is.na(prev_res)) |>
  mutate(
    type = ifelse(
      prev_res %in% c("Touchdown", "Field goal", "Safety"),
      "kick",
      "stop"
    )
  ) |>
  select(season, type, yardline_100, down, ydstogo) |>
  as.data.frame()

# ---- dynamic programming ----------------------------------------------------
keys <- unique(c(
  ot_rule_key(2001:2035, "REG"),
  ot_rule_key(2001:2035, "POST")
))
ot_wp_dp <- lapply(keys, function(k) {
  parts <- strsplit(k, "|", fixed = TRUE)[[1]]
  ot_build_dp(
    parts[1],
    as.integer(parts[2]),
    as.integer(parts[3]),
    ot_wp_models,
    starts
  )
})
names(ot_wp_dp) <- keys

# ---- store on a coarser grid ---------------------------------------------------
# check that interpolating the coarse grid reproduces the full solution. Test
# points are between the stored nodes where the interpolation error is largest
# linear interpolation on the full grid (what the stored grid is compared to)
ot_q_interp_full <- function(Qm, s, tn) {
  S <- length(ot_s_grid)
  pos <- (pmin(pmax(s, ot_s_grid[1]), ot_s_grid[S]) - ot_s_grid[1]) / 2 + 1
  i0 <- min(floor(pos), S - 1L)
  row <- Qm[i0, ] * (1 - (pos - i0)) + Qm[i0 + 1L, ] * (pos - i0)
  k0 <- min(floor(tn), ot_n - 1L)
  row[k0 + 1L] * (1 - (tn - k0)) + row[k0 + 2L] * (tn - k0)
}
max_err <- 0
for (dp in ot_wp_dp) {
  thin <- ot_thin_q(dp$Q)
  for (ty in 1:2) {
    for (r in ot_reachable) {
      di <- ot_map_d(r[2])
      for (s in c(-11, -6, -2, 1, 5, 9)) {
        for (tn in c(7, 33, 61, 101, 143, 177)) {
          full <- ot_q_interp_full(dp$Q[ty, r[1], di, , ], s, tn)
          err <- abs(ot_q_interp(thin[ty, r[1], di, , ], s, tn) - full)
          max_err <- max(max_err, err)
        }
      }
    }
  }
}
cat("max interpolation error of the stored grid:", max_err, "\n")
# the error is largest with less than a minute left
stopifnot(max_err < 0.006)
ot_wp_dp <- lapply(ot_wp_dp, function(x) list(Q = round(ot_thin_q(x$Q), 4)))

# ---- save, keeping the other internal data ----------------------------------
# R/sysdata.rda holds other objects. Load them and write them back untouched so
# that only the overtime objects change. Do NOT rebuild or re-serialize anything
# else here (in particular no xgboost models) because a different xgboost
# version on the machine running this script would change those bytes too.
e <- new.env()
load("R/sysdata.rda", envir = e)
untouched <- setdiff(ls(e), c("ot_wp_models", "ot_wp_dp"))
before <- mget(untouched, envir = e)
e$ot_wp_models <- ot_wp_models
e$ot_wp_dp <- ot_wp_dp
save(
  list = ls(e),
  file = "R/sysdata.rda",
  envir = e,
  compress = "bzip2",
  version = 2
)
# make sure we did not change anything else
e2 <- new.env()
load("R/sysdata.rda", envir = e2)
stopifnot(all(vapply(
  untouched,
  function(n) identical(before[[n]], e2[[n]]),
  TRUE
)))
