# Overtime win probability
#
# Regular win probability models know nothing about the overtime rules, so
# overtime is handled by a separate rule-aware model:
#
# 1. A drive model (xgboost) predicts how the CURRENT drive ends from any play
#    state (down, distance, yardline, spread): TD / FG / safety against /
#    defensive TD / stop (punt, turnover, downs, missed FG). A second model
#    predicts the time the rest of the drive will take.
# 2. A dynamic program (built in data-raw/ot_wp_model.R and stored in
#    sysdata.rda) values the game at the START of each possession as a
#    function of the overtime rules, the possession number, the score
#    differential, the clock and the spread.
# 3. The WP of any play is the probability-weighted value of the possible
#    drive outcomes, using the rules in force for that game.
#
# Rules that are encoded (see ot_rule_key()):
#   SD   - sudden death. First score wins. Regular season through 2011,
#          postseason through 2009.
#   MSD  - modified sudden death. A first possession TD wins, a first
#          possession FG lets the other team answer. Regular season
#          2012-2024, postseason 2010-2021.
#   BOTH - both teams are guaranteed a possession. Postseason 2022+, regular
#          season 2025+.
# Regular season overtime lasts 15 minutes through 2016 and 10 minutes since
# 2017 and the game can end in a tie (counted as 0.5 wins). Postseason
# periods are 15 minutes and are repeated until there is a winner, so a tie
# is never a final result.

ot_s_grid <- seq(-14, 14, by = 2) # spread grid (posteam perspective)
ot_step <- 5 # seconds per time node
ot_n <- 180L # nodes: 0 ... 900 seconds
# The dynamic program is solved on the grid above but only every 2nd time node
# is stored (values are smooth in time and are interpolated when used) to keep
# R/sysdata.rda small. Thinning the spread grid costs too much accuracy.
ot_s_store <- ot_s_grid
ot_t_stride <- 2L
ot_d_set <- c(0, -3, -6, -7, -8) # score differentials of the receiving team
ot_2pt_prob <- 0.4735 # same value as the PAT fix in add_wp_variables()
ot_outcomes <- c("TD", "FG", "SAF", "OPPTD", "STOP")
# the drive model has a sixth class, PUNT, that is part of STOP for durations.
# It is only used to model teams that must score (see ot_apply_must_score())
ot_go_avg <- 0.45 # average 4th down conversion rate when going for it

# first season of kickoff regimes 2 to 5 (regime 1 is everything before). Where
# drives start changed with the kickoff rules. data-raw/ot_wp_model.R builds the
# training windows from the same seasons.
ot_kick_first <- c(2011L, 2016L, 2024L, 2025L)

# rule, period length and kickoff regime for each game
ot_rule_key <- function(season, season_type) {
  post <- season_type %in% "POST"
  rule <- dplyr::case_when(
    post & season >= 2022 ~ "BOTH",
    !post & season >= 2025 ~ "BOTH",
    post & season >= 2010 ~ "MSD",
    !post & season >= 2012 ~ "MSD",
    TRUE ~ "SD"
  )
  len <- dplyr::if_else(!post & season >= 2017, 600L, 900L)
  # kickoff regime determines where drives start
  kick <- findInterval(season, ot_kick_first) + 1L
  paste(rule, len, kick, sep = "|")
}

# season_type is not available when win probability is added, so derive it
# from the week in the game id
ot_season_type <- function(game_id, season) {
  wk <- suppressWarnings(as.integer(substr(game_id, 6, 7)))
  dplyr::if_else(wk > dplyr::if_else(season >= 2021, 18L, 17L), "POST", "REG")
}

# Score differentials the rules can produce (receiving team, start of a
# possession) are exactly ot_d_set. Anything else, such as a lead or a deficit
# of 1 or 2 points, can't occur in the supported rules and snaps to the nearest
# value (a lead of 3 or 7 therefore counts as tied).
ot_map_d <- function(x) {
  vapply(x, function(z) which.min(abs(z - ot_d_set) - 1e-6 * ot_d_set), 1L)
}

ot_end_value <- function(d) dplyr::case_when(d > 0 ~ 1, d < 0 ~ 0, TRUE ~ 0.5)

# Possible results of a completed possession for the team that had the ball
# (R). Returns a list of components. Each has a weight w and either a terminal
# value (for R) or the state in which the opponent starts a possession.
ot_trans <- function(rule, p, d, kind) {
  term <- function(v, w = 1) list(w = w, val = v)
  cont <- function(ty, p2, d2, w = 1) {
    list(w = w, val = NA_real_, ty = ty, p = p2, di = ot_map_d(d2))
  }
  if (kind == "STOP") {
    if (rule == "SD") {
      return(list(cont(2L, 3, 0)))
    }
    if (p == 2 && d < 0) {
      return(list(term(0)))
    }
    return(list(cont(2L, min(p + 1, 3), -d)))
  }
  pts <- if (kind == "TD") 7 else 3
  d2 <- d + pts
  if (rule == "SD") {
    return(list(term(1)))
  }
  if (p == 1) {
    if (rule == "MSD" && pts == 7) {
      return(list(term(1)))
    }
    return(list(cont(1L, 2, -d2)))
  }
  if (p == 2) {
    if (d2 > 0) {
      return(list(term(1)))
    }
    if (d2 == 0) {
      return(list(cont(1L, 3, 0)))
    }
    if (d2 == -1) {
      # TD but still trailing by one after a team scored TD + 2: go for two
      return(list(
        cont(1L, 3, 0, w = ot_2pt_prob),
        term(0, w = 1 - ot_2pt_prob)
      ))
    }
    return(list(term(0)))
  }
  list(term(1))
}

# interpolate a stored [spread x time] matrix. `tn` is time in 5 second nodes
ot_q_interp <- function(Qm, s, tn) {
  S <- length(ot_s_store)
  sc <- pmin(pmax(s, ot_s_store[1]), ot_s_store[S])
  pos <- (sc - ot_s_store[1]) / (ot_s_store[2] - ot_s_store[1]) + 1
  i0 <- min(floor(pos), S - 1L)
  fs <- pos - i0
  row <- Qm[i0, ] * (1 - fs) + Qm[i0 + 1L, ] * fs
  nt <- ncol(Qm)
  tp <- pmin(pmax(tn / ot_t_stride, 0), nt - 1) + 1
  k0 <- pmin(floor(tp), nt - 1L)
  ft <- tp - k0
  row[k0] * (1 - ft) + row[k0 + 1L] * ft
}

# Offenses hurry when the clock is short, so a possession takes a fraction of
# its normal time. A normal play takes about 38 seconds of game clock. The
# ratios above two minutes come from regulation two minute drills (tied or
# trailing by up to 8, drives that scored). Below that the drives we can
# observe are only the ones that finished in time, which makes the ratio look
# smaller than it is, so the curve levels off at a floor of about 6.5 seconds
# per play (snap to whistle plus spikes and timeouts) instead of following the
# data down.
#
# The chance that a possession finishes before the clock runs out must never
# fall when there is more time. A possession needs `ratio * normal time`, so
# that requires ratio / secs to never increase. All knots are chosen so that
# this holds (it is checked in tests/testthat/test-ot_wp.R). Without it a team
# with 30 seconds left looked more likely to score than the same team with 60
# seconds left. Note that win probability itself is not monotone in the clock:
# more time also gives the other team a chance to answer.
ot_tempo <- function(secs) {
  stats::approx(
    x = c(0, 45, 75, 105, 150, 210, 270, 330),
    y = c(0.17, 0.17, 0.28, 0.39, 0.48, 0.65, 0.82, 1),
    xout = secs,
    rule = 2
  )$y
}

ot_pmf <- function(mu, shape) {
  j <- seq_len(ot_n)
  up <- (j + 0.5) * ot_step
  lo <- c(0, (j[-1] - 0.5) * ot_step)
  scale <- max(mu, 3) / shape
  stats::pgamma(up, shape, scale = scale) -
    stats::pgamma(lo, shape, scale = scale)
}

ot_cache <- new.env(parent = emptyenv())

ot_load_boosters <- function() {
  if (is.null(ot_cache$out)) {
    tryCatch(
      {
        ot_cache$out <- xgboost::xgb.load.raw(ot_wp_models$out_raw)
        ot_cache$dur <- xgboost::xgb.load.raw(ot_wp_models$dur_raw)
      },
      error = function(e) {
        cli::cli_abort(
          c(
            "Can't load the overtime win probability models.",
            "i" = "They were saved with xgboost {.val {ot_wp_models$xgboost_version}} and you have {.val {as.character(utils::packageVersion('xgboost'))}}.",
            "x" = conditionMessage(e)
          )
        )
      }
    )
  }
  ot_cache
}

ot_predict_matrix <- function(model, x) {
  p <- stats::predict(model, x)
  if (is.vector(p)) {
    p <- matrix(p, nrow = nrow(x), byrow = TRUE)
  }
  p
}

# 4th down conversion probability for a team that has to go for it
ot_go_prob <- function(ydstogo) {
  stats::plogis(0.75 - 0.17 * pmin(ydstogo, 30))
}

# The drive model learned from teams that punt on 4th down. A team that is
# behind on its last possession cannot. Instead of punting, it converts with
# probability `c` and then has a fresh drive with scoring probabilities `r`.
# P has columns TD, FG, SAF, OPPTD, STOP (STOP includes punts), pun is the
# probability of a punt and c, r1, r2 the conversion probability and the
# TD / FG probability of the continued drive
ot_apply_must_score <- function(P, pun, c, r1, r2) {
  gain1 <- pun * c * r1
  gain2 <- pun * c * r2
  P[, 1] <- P[, 1] + gain1
  P[, 2] <- P[, 2] + gain2
  P[, 5] <- P[, 5] - gain1 - gain2
  P
}

# collapse the 6 class model output to the 5 outcomes used in the recursion
ot_collapse_preds <- function(p6) {
  list(
    P = cbind(p6[, 1:4, drop = FALSE], p6[, 5] + p6[, 6]),
    pun = p6[, 6]
  )
}

# drive outcome probabilities and expected remaining drive time (per outcome).
# `cont_feat` are the states of a drive that continues after a conversion
ot_drive_preds <- function(feat, cont_feat) {
  b <- ot_load_boosters()
  cp <- ot_collapse_preds(ot_predict_matrix(b$out, feat))
  mu <- vapply(
    seq_along(ot_outcomes),
    function(o) {
      oh <- matrix(0, nrow(feat), length(ot_outcomes))
      oh[, o] <- 1
      pmax(stats::predict(b$dur, cbind(feat, oh)) - 5, 3)
    },
    numeric(nrow(feat))
  )
  if (is.vector(mu)) {
    mu <- matrix(mu, nrow = nrow(feat))
  }
  r <- ot_collapse_preds(ot_predict_matrix(b$out, cont_feat))$P
  list(P = cp$P, pun = cp$pun, mu = mu, r1 = r[, 1], r2 = r[, 2])
}

# value to the team with the ball in the middle of a possession
ot_row_scrimmage <- function(dp, rule, p, di, secs, s, P, mu) {
  shape <- ot_wp_models$shape
  d <- ot_d_set[di]
  j <- seq_len(ot_n)
  # the last snap of a possession has to happen before the clock expires, so a
  # possession can still finish up to one play after time has run out
  tleft <- pmax(secs / ot_step - j, 0)
  ok <- secs / ot_step - j > -1
  val <- 0
  for (o in c(1L, 2L, 5L)) {
    kind <- ot_outcomes[o]
    pm <- ot_pmf(mu[o] * ot_tempo(secs), shape[[kind]])
    comps <- ot_trans(rule, p, d, kind)
    v <- 0
    for (cp in comps) {
      if (is.na(cp$val)) {
        qv <- ot_q_interp(dp$Q[cp$ty, cp$p, cp$di, , ], -s, tleft[ok])
        v <- v + cp$w * sum(pm[ok] * (1 - qv))
      } else {
        v <- v + cp$w * cp$val * sum(pm[ok])
      }
    }
    val <- val + P[o] * (v + (1 - sum(pm[ok])) * ot_end_value(d))
  }
  # SAF / OPPTD: the other team scored and wins
  val
}

# value to the scoring team after the point(s) after touchdown are resolved
ot_row_post_score <- function(dp, rule, p, d2, secs, s) {
  if (d2 > 0) {
    if (p == 1 && rule == "BOTH") {
      return(
        1 -
          ot_q_interp(
            dp$Q[1, 2, ot_map_d(-d2), , ],
            -s,
            secs / ot_step
          )
      )
    }
    return(1)
  }
  if (d2 == 0 && p == 2) {
    return(1 - ot_q_interp(dp$Q[1, 3, 1, , ], -s, secs / ot_step))
  }
  if (d2 < 0) {
    return(0)
  }
  0.5
}

# possession number of each row in overtime (NA outside of overtime)
# A new possession starts when the drive changes. The drive id can also stay the
# same when the ball changes hands, e.g. a fumble or interception inside a
# drive that is recovered right away, so a change of posteam (ignoring kickoff
# rows, which carry the kicking team) starts a new possession as well.
ot_possession_number <- function(pbp) {
  dr <- if ("fixed_drive" %in% names(pbp)) pbp$fixed_drive else pbp$drive
  ot <- pbp$qtr > 4
  is_kick <- if ("kickoff_attempt" %in% names(pbp)) {
    pbp$kickoff_attempt %in% 1
  } else {
    rep(FALSE, nrow(pbp))
  }
  team <- match(pbp$posteam, unique(pbp$posteam))
  team[!ot | is_kick] <- NA
  dr[!ot] <- NA
  # counts changes of posteam within a drive
  segment <- stats::ave(
    team,
    pbp$game_id,
    dr,
    FUN = function(x) {
      k <- !is.na(x)
      out <- numeric(length(x))
      if (any(k)) {
        out[k] <- cumsum(c(0, diff(x[k]) != 0))
        last <- cummax(seq_along(x) * k)
        out <- ifelse(last > 0, out[pmax(last, 1)], 0)
      }
      out
    }
  )
  id <- dr + segment / 1000
  out <- stats::ave(
    id,
    pbp$game_id,
    FUN = \(x) match(x, sort(unique(x[!is.na(x)])))
  )
  as.numeric(out)
}

# Overtime win probability of the team with the ball.
# `st` has one row per state to value. Required columns: season, season_type,
# kind ("scrimmage", "kickoff", "pat", "twopt"), yardline_100, down, ydstogo,
# secs (seconds left in the period), d (score differential of the team with the
# ball), spread (spread of the team with the ball, only if use_spread), phase
# (possession number), p_make (PAT make prob). Optional: p_fg (probability that
# a field goal from here is good). If use_spread is FALSE the spread is ignored.
ot_wp_states <- function(st, use_spread = TRUE) {
  n <- nrow(st)
  out <- rep(NA_real_, n)
  if (n == 0) {
    return(out)
  }
  key <- ot_rule_key(st$season, st$season_type)
  rule <- sub("\\|.*$", "", key)
  s <- if (use_spread) st$spread else rep(0, n)
  s[is.na(s)] <- 0
  s <- pmin(pmax(s, ot_s_grid[1]), ot_s_grid[length(ot_s_grid)])
  secs <- pmin(pmax(st$secs, 0), ot_n * ot_step)
  # Postseason games don't end when the period does. The next period starts and
  # play goes on, so there is no clock pressure and the end of the period has no
  # value of its own. Value those states as if there were lots of time left.
  secs[st$season_type %in% "POST"] <- ot_n * ot_step

  # sanitize the state
  di <- ot_map_d(st$d)
  p <- pmin(pmax(st$phase, 1), 3)
  p[rule == "SD"] <- 3
  di[rule == "SD"] <- 1L
  p[p == 1 & di != 1L] <- 2
  p[p == 3 & di != 1L] <- 2

  scrim <- which(st$kind == "scrimmage")
  if (length(scrim) > 0) {
    feat <- cbind(
      yardline_100 = pmin(pmax(st$yardline_100[scrim], 1), 99),
      down = st$down[scrim],
      ydstogo = pmin(st$ydstogo[scrim], 30),
      posteam_spread = s[scrim],
      season_c = pmin(pmax(st$season[scrim], 2006), 2026)
    )
    # state of a fresh drive after a conversion
    y_new <- pmax(
      feat[, "yardline_100"] -
        ifelse(feat[, "down"] == 4, feat[, "ydstogo"], 8),
      1
    )
    cont_feat <- feat
    cont_feat[, "yardline_100"] <- y_new
    cont_feat[, "down"] <- 1
    cont_feat[, "ydstogo"] <- pmin(y_new, 10)
    dpred <- ot_drive_preds(feat, cont_feat)
    # teams that have to score do not punt
    must <- p[scrim] == 2 & di[scrim] > 1L
    cc <- ifelse(feat[, "down"] == 4, ot_go_prob(feat[, "ydstogo"]), ot_go_avg)

    # 4th down with a short clock: nobody punts. In range the offense kicks a
    # field goal (if that helps), otherwise it goes for it.
    sc <- secs[scrim]
    dd <- di[scrim]
    pf <- if ("p_fg" %in% names(st)) {
      st$p_fg[scrim]
    } else {
      rep(NA_real_, length(scrim))
    }
    fourth <- feat[, "down"] == 4
    # a field goal helps when it wins or ties the game
    kick <- fourth &
      !is.na(pf) &
      dd %in% c(1L, 2L) &
      ((sc <= 45 & pf >= 0.1) | (p[scrim] == 2 & dd == 2L & pf >= 0.55))
    late_go <- fourth & sc <= 45 & !kick
    must <- (must | late_go) & !kick
    if (any(must)) {
      dpred$P[must, ] <- ot_apply_must_score(
        dpred$P[must, , drop = FALSE],
        dpred$pun[must],
        cc[must],
        dpred$r1[must],
        dpred$r2[must]
      )
    }
    if (any(kick)) {
      dpred$P[kick, ] <- cbind(0, pf[kick], 0, 0, 1 - pf[kick])
      dpred$mu[kick, ] <- 4
    }
  }

  for (i in seq_len(n)) {
    dp <- ot_wp_dp[[key[i]]]
    if (is.null(dp) || is.na(secs[i]) || is.na(di[i])) {
      next
    }
    k <- st$kind[i]
    if (k == "scrimmage") {
      ii <- match(i, scrim)
      out[i] <- ot_row_scrimmage(
        dp,
        rule[i],
        p[i],
        di[i],
        secs[i],
        s[i],
        dpred$P[ii, ],
        dpred$mu[ii, ]
      )
      # The offense can always kick a field goal instead. On 4th down that is
      # the usual choice, and with a play or two left it is the choice on any
      # down. Value that option with the rules (a miss gives the other team the
      # ball, or ends the game) and never go below it.
      pfi <- if ("p_fg" %in% names(st)) st$p_fg[i] else NA_real_
      if (!is.na(pfi) && (st$down[i] == 4 || secs[i] <= 12)) {
        kick_val <- ot_row_scrimmage(
          dp,
          rule[i],
          p[i],
          di[i],
          secs[i],
          s[i],
          c(0, pfi, 0, 0, 1 - pfi),
          rep(4, 5)
        )
        out[i] <- max(out[i], kick_val)
      }
    } else if (k == "kickoff") {
      out[i] <- ot_q_interp(dp$Q[1, p[i], di[i], , ], s[i], secs[i] / ot_step)
    } else if (k == "pat") {
      pm <- st$p_make[i]
      out[i] <- pm *
        ot_row_post_score(dp, rule[i], p[i], st$d[i] + 1, secs[i], s[i]) +
        (1 - pm) *
          ot_row_post_score(dp, rule[i], p[i], st$d[i], secs[i], s[i])
    } else if (k == "twopt") {
      out[i] <- ot_2pt_prob *
        ot_row_post_score(dp, rule[i], p[i], st$d[i] + 2, secs[i], s[i]) +
        (1 - ot_2pt_prob) *
          ot_row_post_score(dp, rule[i], p[i], st$d[i], secs[i], s[i])
    }
  }
  out
}

# Overtime WP for all overtime rows of a pbp data frame (already passed through
# prepare_wp_data()). Returns list(wp, vegas_wp) of length nrow(pbp), NA outside
# of overtime and for rows that have no state (timeouts, end of quarter ...)
add_ot_wp_vectors <- function(
  pbp,
  kickoff_i,
  pat_i,
  two_pt_i,
  make_pat_prob,
  fg_prob = rep(NA_real_, nrow(pbp))
) {
  n <- nrow(pbp)
  res <- list(wp = rep(NA_real_, n), vegas_wp = rep(NA_real_, n))
  ot_i <- which(pbp$qtr > 4)
  if (length(ot_i) == 0) {
    return(res)
  }
  kind <- rep(NA_character_, n)
  kind[!is.na(pbp$down) & !is.na(pbp$posteam)] <- "scrimmage"
  kind[kickoff_i] <- "kickoff"
  kind[pat_i] <- "pat"
  kind[two_pt_i] <- "twopt"
  # rows without a team with the ball cannot be valued
  kind[is.na(pbp$posteam)] <- NA_character_
  st <- data.frame(
    season = pbp$season,
    season_type = ot_season_type(pbp$game_id, pbp$season),
    kind = kind,
    yardline_100 = pbp$yardline_100,
    down = pbp$down,
    ydstogo = pbp$ydstogo,
    secs = pbp$game_seconds_remaining,
    d = pbp$score_differential,
    spread = pbp$posteam_spread,
    phase = ot_possession_number(pbp),
    p_make = make_pat_prob,
    p_fg = fg_prob,
    stringsAsFactors = FALSE
  )
  keep <- ot_i[!is.na(kind[ot_i]) & !is.na(st$phase[ot_i])]
  sub <- st[keep, , drop = FALSE]
  res$wp[keep] <- ot_wp_states(sub, use_spread = FALSE)
  res$vegas_wp[keep] <- ot_wp_states(sub, use_spread = TRUE)
  res
}

# airWP for overtime passes: value of the state reached after the air yards
# (including a turnover on downs). `pass_pbp` are the pass rows, `phase` the
# possession number of every row in `pass_pbp`.
add_ot_air_wp <- function(pass_pbp, phase) {
  air_wp <- rep(NA_real_, nrow(pass_pbp))
  i <- which(pass_pbp$qtr > 4 & !is.na(phase) & !is.na(pass_pbp$down))
  if (length(i) == 0) {
    return(air_wp)
  }
  x <- pass_pbp[i, ]
  ph <- phase[i]
  season_type <- ot_season_type(x$game_id, x$season)
  to <- x$down == 4 & x$air_yards < x$ydstogo
  new_y <- x$yardline_100 - x$air_yards
  first <- x$air_yards >= x$ydstogo
  rule <- sub("\\|.*$", "", ot_rule_key(x$season, season_type))
  st <- data.frame(
    season = x$season,
    season_type = season_type,
    kind = "scrimmage",
    yardline_100 = ifelse(to, 100 - new_y, new_y),
    down = ifelse(to | first, 1, x$down + 1),
    ydstogo = ifelse(to | first, 10, x$ydstogo - x$air_yards),
    secs = x$game_seconds_remaining, # already reduced by the average pass time
    d = ifelse(to, -x$score_differential, x$score_differential),
    phase = ifelse(
      to,
      ifelse(rule == "SD", 3, pmin(ph + 1, 3)),
      ph
    ),
    stringsAsFactors = FALSE
  )
  st$yardline_100 <- pmin(pmax(st$yardline_100, 1), 99)
  # wp (and with it airWPA) ignores the spread, so airWP must too
  w <- ot_wp_states(st, use_spread = FALSE)
  # a turnover in the second possession while trailing ends the game
  lost <- to & ph == 2 & x$score_differential < 0 & rule != "SD"
  w <- ifelse(to, 1 - w, w)
  w[lost] <- 0
  # the game is over when the clock runs out in the regular season
  over <- st$secs <= 0 & season_type == "REG"
  w[over] <- ot_end_value(x$score_differential[over])
  air_wp[i] <- w
  air_wp
}
