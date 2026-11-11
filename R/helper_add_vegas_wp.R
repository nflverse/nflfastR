# Regulation win probability with the Vegas line (vegas_wp)
#
# P(win) = plogis(fade(t) * offset + trees), where t is the game seconds left:
#
# 1. offset: a probit model of the final margin in the spirit of Stern (1994).
#    Every term is divided by the remaining-time scale of the margin,
#    sigma * ((t + eps) / 3600) ^ alpha (alpha is fit to the data: the spread of
#    the remaining margin shrinks more slowly than Brownian motion would say).
#    The score differential, spread and home terms are weighted by `shrink` so
#    the monotone trees can correct their slope in the direction they are
#    allowed to. Possession, field position, down and distance stay at full
#    strength.
#    At the end of a half a possession is worth the field goals still in play:
#    the possession terms fade, by the time needed to get into field goal
#    range, toward the value of a field goal from the current spot, minus the
#    other team's chance of a field goal if it gets the ball back in time.
# 2. fade(t): the whole offset fades over the last minutes so the trees can
#    learn the end game staircase in score differential (a field goal ties, one
#    score games) that a smooth offset can't represent.
# 3. trees: xgboost trained on top of the offset with monotone constraints. The
#    number of trees used depends on the clock: few early in the game, where
#    labels are noisy and more trees would fit noise, and many in the last
#    minutes. Margins are interpolated between knots so WP is continuous in time.
# 4. A team can't be worse off after scoring a touchdown on this snap: with more
#    than five minutes left, WP is capped at the value of a TD + PAT. In the last
#    minutes not scoring (kneeling, running clock) can be the better play.
# 5. A team can't be worse off than if it kicked a field goal on this snap: WP
#    is at least the value of the attempt (made: 3 points and a kickoff, missed:
#    the other team takes over at the spot of the kick, 8 yards behind the line
#    of scrimmage).
#
# Field goal make probabilities (here and in the end of half terms) come from
# the package's field goal model, see R/helper_fg_model.R.
#
# Built by data-raw/wp_vegas_model.R and stored in R/sysdata.rda as
# `wp_vegas_model` (list with the xgboost model as raw UBJSON, the probit
# coefficients, the constants and the kickoff start spots).

wp_vegas_features <- c(
  "r2h_first_half",
  "home",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "score_differential",
  "posteam_spread",
  "stern_z",
  "spread_z",
  "down",
  "ydstogo",
  "yardline_100",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# terms of the probit offset weighted by `shrink`
wp_vegas_score_terms <- c("sd", "spread_t", "home_t")

# remaining-time scale of the final margin
wp_vegas_tscale <- function(g, k) {
  k$sigma * ((g + k$eps) / 3600)^k$alpha
}

# feature matrix for the trees. `x` needs game_seconds_remaining,
# half_seconds_remaining, score_differential, posteam_spread, home,
# receive_2h_ko, down, ydstogo, yardline_100 and both timeouts columns
wp_vegas_tree_matrix <- function(x, k) {
  g <- x$game_seconds_remaining
  s <- wp_vegas_tscale(g, k)
  spread_rem <- x$posteam_spread * g / 3600
  X <- cbind(
    r2h_first_half = x$receive_2h_ko * as.numeric(g > 1800),
    home = x$home,
    half_seconds_remaining = x$half_seconds_remaining,
    game_seconds_remaining = g,
    score_differential = x$score_differential,
    posteam_spread = x$posteam_spread,
    stern_z = (x$score_differential + spread_rem) / s,
    spread_z = spread_rem / s,
    down = x$down,
    ydstogo = x$ydstogo,
    yardline_100 = x$yardline_100,
    posteam_timeouts_remaining = x$posteam_timeouts_remaining,
    defteam_timeouts_remaining = x$defteam_timeouts_remaining
  )
  storage.mode(X) <- "double"
  X
}

# columns of the probit offset (all divided by the time scale)
wp_vegas_design <- function(x, k) {
  g <- x$game_seconds_remaining
  h <- x$half_seconds_remaining
  s <- wp_vegas_tscale(g, k)
  tau <- g / 3600
  yl <- x$yardline_100
  dn <- x$down
  ytg <- pmin(x$ydstogo, 20)
  # The possession terms fade when the half runs out, by the time needed to get
  # into field goal range (scoring before halftime from the opponent's 35 or
  # closer only needs the time to kick)...
  left <- pmin(h / wp_fg_range_time(yl), 1)
  # ... toward the value of a field goal from here
  fg_eoh <- (1 - left) * 3 * fg_make_prob(x[["season"]], x[["roof"]], yl)
  # The other team's chance of a field goal before halftime: it takes over
  # after a punt and needs the time to get into range. This possession ends
  # after the downs that are left. The defense stops the clock after each play
  # while it has timeouts; without timeouts a play takes about 40 seconds.
  opp_start <- pmin(140 - yl, 80)
  plays <- 4 - dn
  dto <- x$defteam_timeouts_remaining
  t_end <- 5 + 6 * pmin(plays, dto) + 40 * pmax(plays - dto, 0)
  opp_reach <- (1 - left) *
    pmin(pmax(h - t_end, 0) / wp_fg_range_time(opp_start), 1)
  # timeouts and the clock matter at the end of either half. In the 2nd half
  # half seconds and game seconds are the same
  late <- exp(-g / 300)
  late1 <- exp(-h / 300) * as.numeric(g > 1800)
  to_diff <- x$posteam_timeouts_remaining - x$defteam_timeouts_remaining
  Z <- cbind(
    sd = x$score_differential,
    spread_t = x$posteam_spread * tau,
    home_t = x$home * tau,
    const = left,
    field = (50 - yl) / 50 * left,
    field2 = ((50 - yl) / 50)^2 * left,
    d2 = (dn == 2) * left,
    d3 = (dn == 3) * left,
    d4 = (dn == 4) * left,
    ytg = (ytg - 10) / 10 * left,
    r2h = x$receive_2h_ko * as.numeric(g > 1800),
    to_diff_late = to_diff * late,
    const_late = late,
    to_diff_late1 = to_diff * late1,
    const_late1 = late1,
    fg_eoh = fg_eoh,
    opp_eoh4 = (dn == 4) * opp_reach,
    opp_eoh13 = (dn < 4) * opp_reach
  )
  Z / s
}

# seconds needed to get from yardline_100 into field goal range (the opponent's
# 35) and kick
wp_fg_range_time <- function(yl) {
  10 + 2 * pmax(yl - 35, 0)
}

# weight of the whole offset
wp_vegas_fade <- function(g, k) {
  ifelse(g >= k$fade_t, 1, k$fade_end + (1 - k$fade_end) * g / k$fade_t)
}

# offset on the logit scale
wp_vegas_offset <- function(x, beta, k) {
  Z <- wp_vegas_design(x, k)
  s <- colnames(Z) %in% wp_vegas_score_terms
  eta <- k$shrink *
    drop(Z[, s, drop = FALSE] %*% beta[s]) +
    drop(Z[, !s, drop = FALSE] %*% beta[!s])
  p <- stats::pnorm(eta)
  wp_vegas_fade(x$game_seconds_remaining, k) *
    stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
}

wp_vegas_cache <- new.env(parent = emptyenv())

wp_vegas_booster <- function(model = wp_vegas_model) {
  if (is.null(wp_vegas_cache$booster)) {
    tryCatch(
      wp_vegas_cache$booster <- xgboost::xgb.load.raw(model$trees_raw),
      error = function(e) {
        cli::cli_abort(
          c(
            "Can't load the win probability model.",
            "i" = "It was saved with xgboost {.val {model$xgboost_version}} and you have {.val {as.character(utils::packageVersion('xgboost'))}}.",
            "x" = conditionMessage(e)
          )
        )
      }
    )
  }
  wp_vegas_cache$booster
}

# xgboost changed `iterationrange` from a half open [start, end) range (1.x) to
# an inclusive one (3.x). Find out which one the installed version uses by
# comparing the first n_trees rounds with the full model (they are the same
# model only if the range is inclusive).
wp_vegas_inclusive_range <- function(booster, model = wp_vegas_model) {
  if (is.null(wp_vegas_cache$inclusive)) {
    k <- model$constants
    x <- data.frame(
      game_seconds_remaining = c(3000, 1500, 400, 90),
      half_seconds_remaining = c(1200, 1500, 400, 90),
      score_differential = c(0, 3, -4, 7),
      posteam_spread = c(-3, 6.5, 2.5, -1),
      home = c(1, 0, 1, 0),
      receive_2h_ko = c(1, 0, 0, 0),
      down = c(1, 2, 3, 4),
      ydstogo = c(10, 7, 3, 1),
      yardline_100 = c(75, 42, 18, 3),
      posteam_timeouts_remaining = c(3, 2, 1, 0),
      defteam_timeouts_remaining = c(3, 3, 2, 1)
    )
    dm <- xgboost::xgb.DMatrix(wp_vegas_tree_matrix(x, k))
    full <- stats::predict(booster, dm, outputmargin = TRUE)
    upto <- stats::predict(
      booster,
      dm,
      outputmargin = TRUE,
      iterationrange = c(1L, k$n_trees)
    )
    wp_vegas_cache$inclusive <- isTRUE(all.equal(
      as.numeric(full),
      as.numeric(upto)
    ))
  }
  wp_vegas_cache$inclusive
}

# margin of the first n trees. The model is stored with base_score 0.5, so the
# margin has no intercept of its own.
wp_vegas_tree_margin <- function(booster, X, n, inclusive) {
  if (n <= 0) {
    return(rep(0, nrow(X)))
  }
  as.numeric(stats::predict(
    booster,
    xgboost::xgb.DMatrix(X),
    outputmargin = TRUE,
    iterationrange = c(1L, if (inclusive) n else n + 1L)
  ))
}

# vegas_wp of the team with the ball, without the touchdown cap
wp_vegas_logit <- function(x, model = wp_vegas_model) {
  k <- model$constants
  n <- nrow(x)
  if (n == 0) {
    return(numeric(0))
  }
  booster <- wp_vegas_booster(model)
  inclusive <- wp_vegas_inclusive_range(booster, model)
  X <- wp_vegas_tree_matrix(x, k)
  sch <- k$schedule[order(k$schedule$t), ]
  g <- pmin(pmax(x$game_seconds_remaining, min(sch$t)), max(sch$t))
  j <- pmin(findInterval(g, sch$t), nrow(sch) - 1L)
  j[is.na(j)] <- 1L
  w <- (g - sch$t[j]) / (sch$t[j + 1L] - sch$t[j])
  # only the tree counts these rows need
  ns <- unique(c(sch$n[unique(j)], sch$n[unique(j) + 1L]))
  F <- lapply(ns, function(nt) wp_vegas_tree_margin(booster, X, nt, inclusive))
  names(F) <- as.character(ns)
  tm <- numeric(n)
  for (jj in unique(j)) {
    i <- which(j == jj)
    a <- F[[as.character(sch$n[jj])]][i]
    b <- F[[as.character(sch$n[jj + 1L])]][i]
    tm[i] <- (1 - w[i]) * a + w[i] * b
  }
  wp_vegas_offset(x, model$beta, k) + tm
}

# In the last minutes of the game the trees (up to 1,200, with few plays per
# state) put steps of several points between consecutive seconds. The clock at
# the snap and the length of a play vary by more than that, so there the logit
# is averaged over +-wp_end_smooth seconds around the clock. The window
# narrows to nothing at 0:00 and at wp_end_window seconds left. In season
# grouped CV this lowers log loss in the last 4 minutes (16 seconds was about
# the best width; 32 and more blur real structure).
wp_end_smooth <- 16
wp_end_window <- 240

wp_vegas_raw <- function(x, model = wp_vegas_model) {
  l <- wp_vegas_logit(x, model)
  t <- x$game_seconds_remaining
  i <- which(!is.na(l) & !is.na(t) & t > 0 & t < wp_end_window)
  if (length(i) > 0) {
    xi <- x[i, , drop = FALSE]
    r <- pmin(wp_end_smooth, t[i], wp_end_window - t[i])
    u <- seq(-1, 1, length.out = 11)
    acc <- 0
    for (uu in u) {
      xi$game_seconds_remaining <- xi$half_seconds_remaining <- t[i] + uu * r
      acc <- acc + wp_vegas_logit(xi, model)
    }
    l[i] <- acc / length(u)
  }
  p <- stats::plogis(l)
  # A team ahead with the ball wins if it can kneel out the clock, and nearly
  # always if it is a few seconds short
  kn <- which(
    !is.na(p) & !is.na(x$down) & x$score_differential > 0 & t > 0 & t < 300
  )
  if (length(kn) > 0) {
    over <- t[kn] -
      wp_kneel_time(t[kn], x$down[kn], x$defteam_timeouts_remaining[kn])
    w <- pmin(pmax((40 - over) / 30, 0), 1)
    p[kn] <- pmax(p[kn], (1 - w) * p[kn] + w * wp_kneel_value)
  }
  p
}

# Seconds a team can run off by kneeling: each kneel takes about 3 seconds and
# 40 more run off between kneels unless the other team stops the clock (its
# timeouts, and the 2 minute warning once). Since 1999 teams ahead with the
# ball and at most 10 seconds more than this left won all but 3 of 7,093 snaps
# (the trees gave them 93-98% when up 1-3); 10-40 seconds more they won
# 95-100%, past that the trees are right. So WP is the kneel value up to 10
# seconds beyond, moving to the model's value at 40.
wp_kneel_time <- function(t, down, dto) {
  n <- 5 - down
  stops <- dto + as.numeric(t > 120)
  3 * n + 40 * pmax(n - 1 - stops, 0)
}
wp_kneel_value <- 0.9998

# where the receiving team's first snap typically is after a kickoff, by
# kickoff rule regime (the regimes of the overtime model, see ot_kick_first)
wp_kickoff_start <- function(season, model = wp_vegas_model) {
  st <- model$kick_start
  if (is.null(season)) {
    season <- NA_real_
  }
  regime <- findInterval(season, ot_kick_first) + 1L
  # without a season use the current rules
  regime[is.na(regime)] <- length(st)
  unname(st[regime])
}

# The state of the next possession after the offense scores `pts` on this snap:
# the other team has 1st and 10 at the typical start after a kickoff, a few
# seconds later
wp_post_td_state <- function(x, pts, dt = 6) {
  y <- x
  y$score_differential <- -(x$score_differential + pts)
  y$posteam_spread <- -x$posteam_spread
  y$home <- 1 - x$home
  y$down <- 1
  y$ydstogo <- 10
  y$yardline_100 <- wp_kickoff_start(x[["season"]])
  y$posteam_timeouts_remaining <- x$defteam_timeouts_remaining
  y$defteam_timeouts_remaining <- x$posteam_timeouts_remaining
  first <- x$game_seconds_remaining > 1800
  y$receive_2h_ko <- ifelse(first, 1 - x$receive_2h_ko, 0)
  y$game_seconds_remaining <- pmax(x$game_seconds_remaining - dt, 0)
  y$half_seconds_remaining <- pmax(x$half_seconds_remaining - dt, 0)
  y
}

# vegas_wp of kicking a field goal on this snap: made, the other team receives
# the kickoff; missed, it takes over at the spot of the kick, 8 yards behind the
# line of scrimmage (or its 20)
wp_fg_value <- function(x, model, dt = 5) {
  p_fg <- fg_make_prob(x[["season"]], x[["roof"]], x$yardline_100)
  made <- wp_post_td_state(x, 3, dt)
  miss <- made
  miss$score_differential <- -x$score_differential
  miss$yardline_100 <- pmin(100 - (x$yardline_100 + 8), 80)
  # a kick at the end of the 1st half ends the half: value it just before
  # halftime, where the 2nd half kickoff still counts
  first <- x$game_seconds_remaining > 1800
  h <- ifelse(
    first,
    pmax(x$half_seconds_remaining - dt, 1),
    made$half_seconds_remaining
  )
  g <- ifelse(first, 1800 + h, made$game_seconds_remaining)
  made$half_seconds_remaining <- miss$half_seconds_remaining <- h
  made$game_seconds_remaining <- miss$game_seconds_remaining <- g
  v_made <- 1 - wp_vegas_raw(made, model)
  v_miss <- 1 - wp_vegas_raw(miss, model)
  # A kick at the end of the game decides it: made, the team wins if it is
  # ahead; missed, the score stands (a tie goes to overtime, a coin flip). The
  # model doesn't know that the game ends, so with up to wp_fg_end_t seconds
  # left after the kick its value is blended toward the final result.
  w <- ifelse(
    first,
    1,
    pmin(pmax(x$game_seconds_remaining - dt, 0) / wp_fg_end_t, 1)
  )
  v_made <- w * v_made + (1 - w) * wp_end_value(x$score_differential + 3)
  v_miss <- w * v_miss + (1 - w) * wp_end_value(x$score_differential)
  p_fg * v_made + (1 - p_fg) * v_miss
}

# seconds left after a kick at the end of the game below which the field goal
# value moves toward the final result (the model gets the last seconds of a
# game wrong: it gives a team down 1-3 in its own half 7% with 5 seconds left,
# which win 3% of the time, and doesn't know a kick at 0:04 ends the game)
wp_fg_end_t <- 15

# value of the final score of regulation for the team with the ball
wp_end_value <- function(sd) ifelse(sd > 0, 1, ifelse(sd < 0, 0, 0.5))

# vegas_wp of the team with the ball (regulation). WP is at least the value of
# kicking a field goal on this snap. It is capped at the value of scoring a
# touchdown on this snap, except in the last five minutes (where not scoring
# can be the better play) and in the last seconds of a half (where the next
# possession is not a kickoff return).
wp_vegas_predict <- function(x, model = wp_vegas_model, pat = 0.94, dt = 6) {
  x <- as.data.frame(x)
  w <- wp_vegas_raw(x, model)
  # a field goal of more than 71 yards is never good
  fl <- which(x$yardline_100 <= 54 & !is.na(w))
  if (length(fl) > 0) {
    xf <- x[fl, , drop = FALSE]
    w[fl] <- pmax(w[fl], wp_fg_value(xf, model), na.rm = TRUE)
  }
  cap <- which(
    x$game_seconds_remaining > 300 &
      x$half_seconds_remaining > dt &
      !is.na(w)
  )
  if (length(cap) > 0) {
    xc <- x[cap, , drop = FALSE]
    post <- pat *
      (1 - wp_vegas_raw(wp_post_td_state(xc, 7, dt), model)) +
      (1 - pat) * (1 - wp_vegas_raw(wp_post_td_state(xc, 6, dt), model))
    w[cap] <- pmin(w[cap], post)
  }
  w
}
