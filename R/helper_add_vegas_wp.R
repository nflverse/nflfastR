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

# Tree margin of each row of the tree matrix X, interpolated between the tree
# counts of the two schedule knots around its clock. A row is only predicted at
# its own two counts, and rows that repeat (the same state valued more than
# once) only once. Trees value each row on its own, so neither changes a row's
# result.
wp_vegas_trees <- function(X, model = wp_vegas_model) {
  if (nrow(X) == 0) {
    return(numeric(0))
  }
  k <- model$constants
  booster <- wp_vegas_booster(model)
  inclusive <- wp_vegas_inclusive_range(booster, model)
  id <- vctrs::vec_group_id(as.data.frame(X))
  u <- which(!duplicated(id))
  X <- X[u, , drop = FALSE]
  sch <- k$schedule[order(k$schedule$t), ]
  g <- pmin(
    pmax(X[, "game_seconds_remaining"], min(sch$t)),
    max(sch$t)
  )
  j <- pmin(findInterval(g, sch$t), nrow(sch) - 1L)
  j[is.na(j)] <- 1L
  w <- (g - sch$t[j]) / (sch$t[j + 1L] - sch$t[j])
  n_lo <- sch$n[j]
  n_hi <- sch$n[j + 1L]
  a <- b <- numeric(length(u))
  for (nt in unique(c(n_lo, n_hi))) {
    i <- which(n_lo == nt | n_hi == nt)
    f <- wp_vegas_tree_margin(booster, X[i, , drop = FALSE], nt, inclusive)
    lo <- n_lo[i] == nt
    hi <- n_hi[i] == nt
    a[i[lo]] <- f[lo]
    b[i[hi]] <- f[hi]
  }
  tm <- (1 - w) * a + w * b
  # vec_group_id numbers the groups in order of first appearance
  tm[id]
}

# In the last minutes of the game the trees (up to 1,200, with few plays per
# state) put steps of several points between consecutive seconds, and between
# adjacent yard lines (8 points between the opponent's 41 and 42 with 8 seconds
# left). The clock at the snap and the length of a play vary by more than a
# second, and the spot by more than a yard, so there the logit is averaged over
# 11 points within +-wp_end_smooth seconds and +-wp_end_yards yards. Each
# point has its own clock and yard offset (each yard offset is used 2 or 3
# times, symmetric around the state and uncorrelated with the clock offset),
# which smooths about as well as all 55 combinations at the cost of 11. The
# clock window narrows to nothing at 0:00 and at wp_end_window seconds left,
# the yard window at either goal line. In season grouped CV this lowers log
# loss in the last 4 minutes (16 seconds was about the best width, 32 and
# more blur real structure; 2 yards is level with 1, both a little better
# than none).
wp_end_smooth <- 16
wp_end_yards <- 2
wp_end_window <- 240
wp_end_u <- seq(-1, 1, length.out = 11)
wp_end_v <- c(1, -2, 0, 2, -1, 0, 1, -2, 0, 2, -1) / 2

# vegas_wp of the team with the ball, without the field goal and touchdown
# rules of wp_vegas_predict()
wp_vegas_raw <- function(x, model = wp_vegas_model) {
  wp_vegas_raw_run(list(wp_vegas_raw_prep(x, model)), model)[[1]]
}

# wp_vegas_raw() of several sets of states, prepared by wp_vegas_raw_prep(). The
# trees of all of them are predicted in one go.
wp_vegas_raw_run <- function(preps, model = wp_vegas_model) {
  rows <- vapply(preps, function(p) nrow(p$X), 1L)
  tm <- wp_vegas_trees(do.call(rbind, lapply(preps, function(p) p$X)), model)
  start <- cumsum(c(0L, rows))
  lapply(seq_along(preps), function(q) {
    wp_vegas_raw_finish(preps[[q]], tm[start[q] + seq_len(rows[q])])
  })
}

# The part of wp_vegas_raw() before the trees: the offsets, and the tree
# matrix of the states and (in the last minutes) of their shifted copies. Each
# offset is computed on the same rows as when the states are valued on their
# own (a matrix product over other rows could round differently).
wp_vegas_raw_prep <- function(x, model = wp_vegas_model) {
  k <- model$constants
  n <- nrow(x)
  t <- x$game_seconds_remaining
  off <- wp_vegas_offset(x, model$beta, k)
  # the trees give every row with a clock a value, so the logit is missing
  # where the offset is
  i <- which(!is.na(off) & !is.na(t) & t > 0 & t < wp_end_window)
  m <- length(i)
  K <- length(wp_end_u)
  X <- list(wp_vegas_tree_matrix(x, k))
  off_i <- NULL
  if (m > 0) {
    xi <- x[i, , drop = FALSE]
    r <- pmin(wp_end_smooth, t[i], wp_end_window - t[i])
    yl <- xi$yardline_100
    ytg <- xi$ydstogo
    ry <- pmin(wp_end_yards, yl - 1, 99 - yl)
    ry[is.na(ry)] <- 0
    off_i <- vector("list", K)
    for (kk in seq_len(K)) {
      xi$game_seconds_remaining <- xi$half_seconds_remaining <- t[i] +
        wp_end_u[kk] * r
      xi$yardline_100 <- yl + round(wp_end_v[kk] * ry)
      xi$ydstogo <- pmin(ytg, xi$yardline_100)
      off_i[[kk]] <- wp_vegas_offset(xi, model$beta, k)
      X[[kk + 1L]] <- wp_vegas_tree_matrix(xi, k)
    }
  }
  list(x = x, off = off, i = i, off_i = off_i, X = do.call(rbind, X))
}

# The part of wp_vegas_raw() after the trees. `tm` are the tree margins of the
# rows of prep$X.
wp_vegas_raw_finish <- function(prep, tm) {
  x <- prep$x
  n <- nrow(x)
  if (n == 0) {
    return(numeric(0))
  }
  t <- x$game_seconds_remaining
  i <- prep$i
  m <- length(i)
  K <- length(wp_end_u)
  off_i <- prep$off_i
  l <- prep$off + tm[seq_len(n)]
  if (m > 0) {
    acc <- 0
    for (kk in seq_len(K)) {
      acc <- acc + (off_i[[kk]] + tm[n + (kk - 1L) * m + seq_len(m)])
    }
    l[i] <- acc / K
  }
  p <- stats::plogis(l)
  # A team ahead with the ball wins if it can kneel out the clock, and nearly
  # always if it is a few seconds short
  kn <- which(
    !is.na(p) & !is.na(x$down) & x$score_differential > 0 & t >= 0 & t < 300
  )
  if (length(kn) > 0) {
    over <- t[kn] -
      wp_kneel_time(t[kn], x$down[kn], x$defteam_timeouts_remaining[kn])
    w <- pmin(pmax((40 - over) / 30, 0), 1)
    p[kn] <- pmax(p[kn], (1 - w) * p[kn] + w * wp_kneel_value)
  }
  # A team behind deep in its own half with seconds left rarely wins: down 1-3
  # inside its own 40 with 10-20 seconds left the trees gave 8-11% and those
  # teams won 3-6%, with 5 seconds or less none of 108 won. WP moves to 0 over
  # the last wp_behind_t seconds. From the own 40 on the trees are right, so
  # the rule fades in from the own 45 to the own 35 (no cliff along the field).
  bh <- which(
    !is.na(p) &
      !is.na(x$down) &
      x$score_differential < 0 &
      x$yardline_100 > 55 &
      t >= 0 &
      t < wp_behind_t
  )
  if (length(bh) > 0) {
    wy <- pmin((x$yardline_100[bh] - 55) / 10, 1)
    p[bh] <- p[bh] * (1 - wy * (1 - t[bh] / wp_behind_t))
  }
  p
}
wp_behind_t <- 20

# Seconds a team can run off by kneeling: each kneel takes about 3 seconds and
# 40 more run off between kneels unless the other team stops the clock (its
# timeouts, and the 2 minute warning once). Since 1999 teams ahead with the
# ball and at most 10 seconds more than this left lost 1 of 3,877 games (the
# 2022 MIN-BUF fumble at the 1); the trees gave them 95-97% when up 1-3. 10-40 seconds more they won 99% (96% when up 1-3,
# trees 94%), past that the trees are right. So WP is the kneel value up to 10
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
wp_fg_value <- function(x, model = wp_vegas_model, dt = 5) {
  s <- wp_fg_states(x, dt)
  r <- wp_vegas_raw_run(
    list(wp_vegas_raw_prep(s$made, model), wp_vegas_raw_prep(s$miss, model)),
    model
  )
  wp_fg_combine(x, r[[1]], r[[2]], dt)
}

# the states after the kick of wp_fg_value()
wp_fg_states <- function(x, dt = 5) {
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
  list(made = made, miss = miss)
}

# the value of the kick from wp_vegas_raw() of the states of wp_fg_states()
wp_fg_combine <- function(x, raw_made, raw_miss, dt = 5) {
  p_fg <- fg_make_prob(x[["season"]], x[["roof"]], x$yardline_100)
  first <- x$game_seconds_remaining > 1800
  v_made <- 1 - raw_made
  v_miss <- 1 - raw_miss
  # A kick at the end of the game decides it: made, the team wins if it is
  # ahead; missed, the score stands (a tie goes to overtime). The model doesn't
  # know that the game ends, so with up to wp_fg_end_t seconds left after the
  # kick its value is blended toward the final result.
  w <- ifelse(
    first,
    1,
    pmin(pmax(x$game_seconds_remaining - dt, 0) / wp_fg_end_t, 1)
  )
  tie <- rep(0.5, nrow(x))
  end <- which(w < 1)
  if (length(end) > 0) {
    tie[end] <- wp_ot_start(x[end, , drop = FALSE])
  }
  v_made <- w * v_made + (1 - w) * wp_end_value(x$score_differential + 3, tie)
  v_miss <- w * v_miss + (1 - w) * wp_end_value(x$score_differential, tie)
  p_fg * v_made + (1 - p_fg) * v_miss
}

# WP of the team with the ball at the start of overtime, before the coin toss:
# the average of receiving and kicking the overtime kickoff (overtime model)
wp_ot_start <- function(x) {
  n <- nrow(x)
  season <- if (is.null(x[["season"]])) rep(NA_real_, n) else x[["season"]]
  if (anyNA(season)) {
    season[is.na(season)] <- nflreadr::most_recent_season()
  }
  type <- if (is.null(x[["season_type"]])) {
    rep("REG", n)
  } else {
    dplyr::coalesce(x[["season_type"]], "REG")
  }
  st <- data.frame(
    season = season,
    season_type = type,
    kind = "kickoff",
    yardline_100 = NA_real_,
    down = NA_real_,
    ydstogo = NA_real_,
    secs = ifelse(type == "POST" | season < 2017, 900, 600),
    d = 0,
    spread = x$posteam_spread,
    phase = 1,
    p_make = NA_real_
  )
  kick <- st
  kick$spread <- -x$posteam_spread
  w <- ot_wp_states(rbind(st, kick))
  0.5 * w[seq_len(n)] + 0.5 * (1 - w[n + seq_len(n)])
}

# seconds before the end of regulation over which a tied game's WP moves to its
# value at 0:00. In season grouped CV (tied, last 2:00 of Q4, out of fold) this
# lowers log loss by 0.0006 (se 0.0005), 0.004 in the last 5 seconds; 20
# seconds was level with 10, 30 worse (the team can still score).
wp_tie_t <- 10

# seconds left after a kick at the end of the game below which the field goal
# value moves toward the final result (the model gets the last seconds of a
# game wrong: it gives a team down 1-3 in its own half 7% with 5 seconds left,
# which win 3% of the time, and doesn't know a kick at 0:04 ends the game)
wp_fg_end_t <- 15

# value of the final score of regulation for the team with the ball (a tie
# goes to overtime)
wp_end_value <- function(sd, tie = 0.5) {
  ifelse(sd > 0, 1, ifelse(sd < 0, 0, tie))
}

# vegas_wp of the team with the ball (regulation). WP is at least the value of
# kicking a field goal on this snap. It is capped at the value of scoring a
# touchdown on this snap, except in the last five minutes (where not scoring
# can be the better play) and in the last seconds of a half (where the next
# possession is not a kickoff return).
wp_vegas_predict <- function(x, model = wp_vegas_model, dt = 6) {
  x <- as.data.frame(x)
  g <- x$game_seconds_remaining
  px <- wp_vegas_raw_prep(x, model)
  # Raw WP is missing exactly where the offset is, and the floor never makes it
  # missing (the last kick rows, under 30 seconds, are never capped). So the
  # rows each rule below applies to are known before the trees run, and the
  # trees of all the states are predicted in one go.
  ok <- !is.na(px$off)
  p_fg <- fg_make_prob(x[["season"]], x[["roof"]], x$yardline_100)
  # WP is at least the value of kicking a field goal
  fl <- which(p_fg > 0 & ok)
  # Down 1 to 3 on 4th down in field goal range in the last 30 seconds, the
  # team kicks: WP moves to the value of the kick over the last 30 to 10
  # seconds. The trees valued these states above the kick (a tying kick at
  # 0:05 at 0.6, when a make only buys overtime). The rule fades out as the
  # kick gets long (make probability from 0.3 to 0.1), where the team may
  # rather throw. Down 3 at 4th and goal it applies too: going for the win is
  # an option there, but 7 of 8 such teams since 1999 kicked.
  lc <- which(
    ok &
      x$down %in% 4 &
      g < 30 &
      x$score_differential %in% -3:-1 &
      p_fg > 0.1
  )
  # capped at the value of a touchdown
  cap <- which(
    x$game_seconds_remaining > 300 &
      x$half_seconds_remaining > dt &
      ok
  )
  xf <- x[fl, , drop = FALSE]
  xl <- x[lc, , drop = FALSE]
  xc <- x[cap, , drop = FALSE]
  # states to value (none for an empty set of rows)
  none <- x[0, , drop = FALSE]
  sf <- if (length(fl) > 0) wp_fg_states(xf) else list(made = none, miss = none)
  sl <- if (length(lc) > 0) wp_fg_states(xl) else list(made = none, miss = none)
  td <- if (length(cap) > 0) {
    list(wp_post_td_state(xc, 7, dt), wp_post_td_state(xc, 6, dt))
  } else {
    list(none, none)
  }
  r <- wp_vegas_raw_run(
    c(
      list(px),
      lapply(
        list(sf$made, sf$miss, sl$made, sl$miss, td[[1]], td[[2]]),
        wp_vegas_raw_prep,
        model = model
      )
    ),
    model
  )
  w <- r[[1]]
  fgv <- if (length(fl) > 0) wp_fg_combine(xf, r[[2]], r[[3]]) else numeric(0)
  # Tied at the end of regulation, WP moves over the last wp_tie_t seconds to
  # the value of the best option left at 0:00: a field goal in range (a miss
  # goes to overtime), otherwise the overtime kickoff before the coin toss.
  # There the line counts again, which the trees don't see (its weight fades
  # to 0 at 0:00).
  tie <- which(ok & x$score_differential %in% 0 & g >= 0 & g < wp_tie_t)
  if (length(tie) > 0) {
    end <- wp_ot_start(x[tie, , drop = FALSE])
    k <- match(tie, fl)
    kick <- which(!is.na(k))
    end[kick] <- pmax(end[kick], fgv[k[kick]], na.rm = TRUE)
    a <- g[tie] / wp_tie_t
    w[tie] <- a * w[tie] + (1 - a) * end
  }
  if (length(fl) > 0) {
    w[fl] <- pmax(w[fl], fgv, na.rm = TRUE)
  }
  if (length(lc) > 0) {
    wt <- pmin(pmax((30 - g[lc]) / 20, 0), 1) *
      pmin((p_fg[lc] - 0.1) / 0.2, 1)
    fv <- wp_fg_combine(xl, r[[4]], r[[5]])
    w[lc] <- (1 - wt) * w[lc] + wt * fv
  }
  if (length(cap) > 0) {
    # the PAT is a field goal from the 15 since 2015 (the 2 before)
    season <- if (is.null(xc[["season"]])) NA_real_ else xc[["season"]]
    if (anyNA(season)) {
      season_now <- dplyr::coalesce(season, nflreadr::most_recent_season())
    } else {
      season_now <- season
    }
    pat_yl <- ifelse(season_now >= 2015, 15, 2)
    pat <- fg_make_prob(season, xc[["roof"]], pat_yl)
    post <- pat *
      (1 - r[[6]]) +
      (1 - pat) * (1 - r[[7]])
    w[cap] <- pmin(w[cap], post)
  }
  w
}
