################################################################################
# Author: Sebastian Carl
# Purpose: Functions to add ep(a) and wp(a) variables
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_ep <- function(pbp) {
  out <- pbp |> add_ep_variables()
  user_message("added ep variables", "done")
  return(out)
}

add_air_yac_ep <- function(pbp) {
  if (!any(!is.na(pbp$air_yards))) {
    cols <- c(
      "air_epa",
      "yac_epa",
      "comp_air_epa",
      "comp_yac_epa",
      "home_team_comp_air_epa",
      "away_team_comp_air_epa",
      "home_team_comp_yac_epa",
      "away_team_comp_yac_epa",
      "total_home_comp_air_epa",
      "total_away_comp_air_epa",
      "total_home_comp_yac_epa",
      "total_away_comp_yac_epa",
      "home_team_raw_air_epa",
      "away_team_raw_air_epa",
      "home_team_raw_yac_epa",
      "away_team_raw_yac_epa",
      "total_home_raw_air_epa",
      "total_away_raw_air_epa",
      "total_home_raw_yac_epa",
      "total_away_raw_yac_epa"
    )
    data.table::set(pbp, j = cols, value = NA_real_)
    user_message(
      "No non-NA air_yards detected. air_yac_ep variables set to NA",
      "info"
    )
  } else {
    pbp <- add_air_yac_ep_variables(pbp)
    user_message("added air_yac_ep variables", "done")
  }
  pbp
}

add_wp <- function(pbp) {
  out <- pbp |> add_wp_variables()
  user_message("added wp variables", "done")
  return(out)
}

add_air_yac_wp <- function(pbp) {
  if (!any(!is.na(pbp$air_yards))) {
    cols <- c(
      "air_wpa",
      "yac_wpa",
      "comp_air_wpa",
      "comp_yac_wpa",
      "home_team_comp_air_wpa",
      "away_team_comp_air_wpa",
      "home_team_comp_yac_wpa",
      "away_team_comp_yac_wpa",
      "total_home_comp_air_wpa",
      "total_away_comp_air_wpa",
      "total_home_comp_yac_wpa",
      "total_away_comp_yac_wpa",
      "home_team_raw_air_wpa",
      "away_team_raw_air_wpa",
      "home_team_raw_yac_wpa",
      "away_team_raw_yac_wpa",
      "total_home_raw_air_wpa",
      "total_away_raw_air_wpa",
      "total_home_raw_yac_wpa",
      "total_away_raw_yac_wpa"
    )
    data.table::set(pbp, j = cols, value = NA_real_)
    user_message(
      "No non-NA air_yards detected. air_yac_wp variables set to NA",
      "info"
    )
  } else {
    pbp <- add_air_yac_wp_variables(pbp)
    user_message("added air_yac_wp variables", "done")
  }
  pbp
}

# Run the prediction function fn() only on the rows i of data (a data.table or
# a list of columns). The result has one element for every row of data and is
# NA outside of i. Model predictions of a row don't depend on other rows, so
# the values in i are the same as if fn() ran on all rows.
predict_rows <- function(fn, data, i) {
  out <- rep(NA_real_, length(data[[1L]]))
  if (length(i) > 0) {
    out[i] <- fn(lapply(data, `[`, i))
  }
  out
}

# The columns cols of pbp in the rows i, as list of columns. Steps that work on
# a part of the plays use it to copy only the columns they need.
subset_columns <- function(pbp, i, cols) {
  cols <- intersect(cols, names(pbp))
  lapply(stats::setNames(nm = cols), function(col) pbp[[col]][i])
}

# Expand a data frame of predictions for the rows i to n rows with NA elsewhere
expand_rows <- function(preds, n, i) {
  out <- preds[rep(NA_integer_, n), , drop = FALSE]
  rownames(out) <- NULL
  if (length(i) > 0) {
    out[i, ] <- preds
  }
  out
}

# The features of the EP model, in the order the model expects
ep_model_features <- c(
  "half_seconds_remaining",
  "yardline_100",
  "home",
  "retractable",
  "dome",
  "outdoors",
  "ydstogo",
  "era0",
  "era1",
  "era2",
  "era3",
  "era4",
  "down1",
  "down2",
  "down3",
  "down4",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# EP predictions for several data frames with one call of the model. Returns a
# list with a data frame of probabilities for each data frame.
get_preds_stacked <- function(data_list) {
  matrices <- lapply(data_list, function(pbp) {
    m <- model_matrix(pbp, ep_model_features)
    if ("location" %in% names(pbp)) {
      m[, "home"] <- data.table::fifelse(pbp$location == "Neutral", 0, pbp$home)
    }
    m
  })
  preds <- stats::predict(load_model("ep"), do.call(rbind, matrices))

  # xgboost v3 returns a matrix of predictions instead of a vector as returned
  # by xgboost v1.
  if (is.vector(preds)) {
    preds <- matrix(preds, ncol = 7, byrow = TRUE)
  }
  preds <- as.data.frame(preds)
  colnames(preds) <- c(
    "Touchdown",
    "Opp_Touchdown",
    "Field_Goal",
    "Opp_Field_Goal",
    "Safety",
    "Opp_Safety",
    "No_Score"
  )
  part <- rep(seq_along(matrices), vapply(matrices, nrow, integer(1)))
  lapply(seq_along(matrices), function(k) {
    out <- preds[part == k, , drop = FALSE]
    rownames(out) <- NULL
    out
  })
}

#get predictions for a set of pbp data
#for predict stage of EP
get_preds <- function(pbp) {
  get_preds_stacked(list(pbp))[[1]]
}

# The expected net points of the conversion after a touchdown, before the team
# chooses between a kick and a two point try, used in the value of a touchdown.
# This is not the probability of making a kicked extra point: it is the average
# over all conversions after touchdowns 2004-2025 (kicks and two point tries,
# including botched ones and defensive returns), which was 0.991 through 2014
# (before the move back to the 15 yard line), 0.941 from 2015 through 2022, and
# 0.956 since (the kick success rates were 0.990, 0.940 and 0.959). The
# differences between the kick or two point try that was chosen and this are in
# the conversion's EPA.
conversion_value <- function(season) {
  data.table::fcase(
    season <= 2014,
    0.99,
    season <= 2022,
    0.94,
    season > 2022,
    0.96
  )
}

# Probability of making a two point conversion: 0.49 for tries from the 2 yard
# line since 2004, leaving out kneel downs and botched one point tries
two_point_make_prob <- 0.49

# Where the scoring team's drive starts after a safety free kick, as
# yardline_100 for the receiving (scoring) team. This is a data-informed guess:
# the first scrimmage play after the free kick was on average at yardline_100
# 61.4 (median 63, n = 256 safeties, 2009-2025, and about the same before and
# after 2016), so own 39 is used
safety_free_kick_yardline <- 61

# Expected points per yard of field position between a kickoff start and
# the safety free kick spot. The EP model gives about 0.05 points per yard
# between those two states, so the value of a free kick is the kickoff value
# plus this slope times the yards. This saves model predictions on the rows
# where a safety is unlikely, which it can get wrong by up to 0.24 points
# (older seasons, where the yards are the most), so the rows where a safety is
# likely or is the play get the value from the model (see get_kickoff_values()).
safety_free_kick_ep_per_yard <- 0.05

# The rows where the free kick value W has to come from the model: those where
# a safety is likely (more than 1% of the next score), since the shortcut for W
# can be off by up to 0.24 points and that is worth about 0.003 points of EP
# at that probability. This only uses the probabilities of the state, never what
# happens next, so that EP does not depend on the outcome of the play, and it is
# the same rule everywhere (play-by-play, calculator, xYAC and air EP).
ep_exact_w <- function(safety, opp_safety) {
  ((safety + opp_safety) > 0.01) %in% TRUE
}

# The value of a scoring event is not only its points: the other team gets the
# ball afterwards, so a touchdown is worth 6 + xp - V where V is the expected
# points of the opponent's kickoff possession at the current clock. A safety
# gives the scoring team the ball on a free kick, so it is worth 2 + W.
#
# V is the EP of 1st and 10 at the kickoff start spot and W is the EP of
# 1st and 10 at the safety free kick spot. These come from the same EP model,
# whose scoring values depend on V and W themselves, which is a pair of linear
# equations that is solved exactly. It is solved on probabilities averaged over
# the two teams' versions of the state (home field and timeouts flipped), so V
# and W are the same for both teams. Using each receiving team's own state
# does not work: the model's home field effect would be applied twice (the
# home team's scores would subtract the road team's small kickoff value and
# the road team's scores the home team's large one), which doubles the
# home / road difference in EP at the same state (to about 1.2 from 0.7), and
# solving the teams' values together blows up. The cost of one value for both
# teams is that the `ep` of a kickoff row, which has the receiving team's home
# field effect, can differ from the V subtracted on the score before it
# (see the NEWS). No play's EPA includes that difference. Score differential is
# ignored and the clock is the current clock ("as if they scored now"). Valuing
# the possession at the clock of the next score is worse, since the home field
# effect then compounds over each alternating possession. All values are 0
# when no kickoff or free kick can follow: end of half or game.
#
# Model predictions are the expensive part, so this needs as few as possible:
# `kickoff_preds` are the predictions for the possession team at the kickoff
# start spot for all rows of `pbp_data` if the caller already has them, so the
# only new prediction is the other team's kickoff state. The free kick (safety)
# value is the kickoff value plus the EP of the extra field position (see
# safety_free_kick_ep_per_yard), except for the rows flagged in `exact_w` (where
# a safety is likely or is the play), where it comes from the model (two more
# predictions on those rows only). Only rows that can have a kickoff after a
# score are predicted, with one call of the model.
#
# pbp_data is a data frame or a list of columns with the EP model features and
# season (and location for neutral site games). Returns V and W as columns `v`
# and `w` of a data frame.
get_kickoff_values <- function(
  pbp_data,
  kickoff_preds = NULL,
  exact_w = FALSE
) {
  n <- length(pbp_data$half_seconds_remaining)
  exact_w <- rep_len(exact_w, n)

  # no kickoff possible at the end of the half (or overtime period) or game.
  # Whether a score in overtime ends the game is handled by
  # ep_continue_factors(), not here
  can_kick <- which(
    !is.na(pbp_data$half_seconds_remaining) &
      pbp_data$half_seconds_remaining > 0
  )

  out <- data.frame(v = numeric(n), w = numeric(n))
  if (length(can_kick) == 0L) {
    return(out)
  }

  base <- subset_columns(pbp_data, can_kick, ep_model_features)
  m <- length(can_kick)
  neutral <- if ("location" %chin% names(pbp_data)) {
    pbp_data$location[can_kick] == "Neutral"
  } else {
    rep(FALSE, m)
  }

  # model input for 1st and 10 at yardline_100 for the possession team
  # (flip = FALSE) or the other team (flip = TRUE), in the rows `rows` of base
  state <- function(yardline_100, flip, rows = seq_len(m)) {
    d <- lapply(base, `[`, rows)
    if (flip) {
      d$home <- 1 - d$home
      d$posteam_timeouts_remaining <- base$defteam_timeouts_remaining[rows]
      d$defteam_timeouts_remaining <- base$posteam_timeouts_remaining[rows]
    }
    d$home <- data.table::fifelse(neutral[rows], 0, d$home)
    d$yardline_100 <- yardline_100
    d$down1 <- rep(1, length(rows))
    d$down2 <- rep(0, length(rows))
    d$down3 <- rep(0, length(rows))
    d$down4 <- rep(0, length(rows))
    d$ydstogo <- rep(10, length(rows))
    d
  }

  ko_yardline <- wp_kickoff_start(pbp_data$season[can_kick])
  late <- which(exact_w[can_kick])
  fk_yardline <- rep(safety_free_kick_yardline, length(late))
  states <- list(ko_opp = state(ko_yardline, flip = TRUE))
  if (is.null(kickoff_preds)) {
    states$ko_own <- state(ko_yardline, flip = FALSE)
  }
  if (length(late) > 0) {
    states$fk_own <- state(fk_yardline, flip = FALSE, rows = late)
    states$fk_opp <- state(fk_yardline, flip = TRUE, rows = late)
  }
  preds <- stats::setNames(get_preds_stacked(states), names(states))
  k_own <- if (is.null(kickoff_preds)) {
    preds$ko_own
  } else {
    kickoff_preds[can_kick, ]
  }

  # EP of a state = a + b V + d W on the average of the two teams' states
  xp <- conversion_value(pbp_data$season[can_kick])
  coefs <- function(p_own, p_opp, xp) {
    p <- (p_own + p_opp) / 2
    td <- p$Touchdown - p$Opp_Touchdown
    fg <- p$Field_Goal - p$Opp_Field_Goal
    sf <- p$Safety - p$Opp_Safety
    list(a = td * (6 + xp) + 3 * fg + 2 * sf, b = -(td + fg), d = sf)
  }
  k <- coefs(k_own, preds$ko_opp, xp)

  # W = V + g, so that V = (a + d g) / (1 - b - d)
  g <- safety_free_kick_ep_per_yard * (ko_yardline - safety_free_kick_yardline)
  v <- (k$a + k$d * g) / (1 - k$b - k$d)
  w <- v + g

  # The difference between W and V depends on the season and the clock, and the
  # shortcut can be off by 0.24 points in the early 2000s, so for the rows where
  # W matters (`exact_w`: a safety is likely or is the play) W comes from the
  # model: V and W are the solution of the two linear equations for the
  # kickoff state and the free kick state
  if (length(late) > 0) {
    f <- coefs(preds$fk_own, preds$fk_opp, xp[late])
    kl <- lapply(k, `[`, late)
    denom <- (1 - kl$b) * (1 - f$d) - kl$d * f$b
    v[late] <- (kl$a * (1 - f$d) + kl$d * f$a) / denom
    w[late] <- ((1 - kl$b) * f$a + f$b * kl$a) / denom
  }

  out$v[can_kick] <- v
  out$w[can_kick] <- w

  out
}

# Overtime rules, by season and regular season / postseason (the same rules
# as in the overtime win probability model):
#   SD   - sudden death: the first score ends the game. Regular season through
#          2011, postseason through 2009.
#   MSD  - modified sudden death: a touchdown on the first possession ends the
#          game, a field goal on the first possession lets the other team
#          answer. Regular season 2012-2024, postseason 2010-2021.
#   BOTH - both teams are guaranteed a possession. Regular season 2025+,
#          postseason 2022+.
# season_type does not exist when EP is added, so it is derived from the week
# in the game id.
ep_ot_rule <- function(game_id, season) {
  week <- suppressWarnings(as.integer(substr(game_id, 6, 7)))
  post <- (week > data.table::fifelse(season >= 2021, 18L, 17L)) %in% TRUE
  data.table::fcase(
    post & season >= 2022,
    "BOTH",
    !post & season >= 2025,
    "BOTH",
    post & season >= 2010,
    "MSD",
    !post & season >= 2012,
    "MSD",
    default = "SD"
  )
}

# Does a kickoff (or free kick) follow a score? In regulation always. In
# overtime it depends on the rule, the number of the possession in which the
# score happens (1, 2, 3 for the third and later ones) and the score
# differential (for the scoring team, before the score).
# pts is the value of the score: 7 touchdown, 3 field goal, 2 safety
ep_ot_continues <- function(rule, possession, diff, pts) {
  # fcase() needs conditions of the same length
  n <- max(length(rule), length(possession), length(diff), length(pts))
  rule <- rep_len(rule, n)
  possession <- rep_len(possession, n)
  diff <- rep_len(diff, n)
  pts <- rep_len(pts, n)
  data.table::fcase(
    is.na(possession) | is.na(diff),
    1,
    rule == "SD",
    0,
    possession >= 3,
    0,
    # a safety ends the game on the first possession, a touchdown does only
    # if the other team does not get a possession
    possession == 1 & pts == 2,
    0,
    possession == 1 & pts == 7 & rule == "MSD",
    0,
    possession == 1,
    1,
    # second possession: the game continues only if the score ties it. A score
    # that wins it ends the game, and so does one that still trails because
    # both teams have had their possession
    diff + pts == 0,
    1,
    default = 0
  )
}

# The touchdown cases in overtime: does a kickoff follow (cont), is there a
# conversion (conv) and is it a two point try that is certain to be attempted
# (two). A touchdown counts as 7 points to see whether it ends the game. A
# conversion follows unless the touchdown ends the game. In the era where both
# teams get a possession, a team that scores a touchdown on the second
# possession and is down 1 after it (the other team scored a touchdown and made
# the extra point) always goes for two, since a successful try wins the game and
# a kick only ties it. The game ends after the try either way.
ep_ot_td <- function(rule, possession, diff) {
  cont <- ep_ot_continues(rule, possession, diff, 7)
  two <- rule == "BOTH" & possession %in% 2 & diff %in% -7
  cont[two] <- 0
  list(cont = cont, conv = as.numeric(cont == 1 | two), two = two)
}

# Factors between 0 and 1 for the value of the possession that follows each
# kind of score (1 everywhere in regulation, and if the data does not have what
# is needed to tell overtime possessions apart), whether a conversion follows a
# touchdown (k_td_*) and whether it is certain to be a two point try (two_td_*).
# Scores by the opponent are assumed to happen in the next possession.
# two_try is true for a conversion that is that two point try.
ep_continue_factors <- function(pbp_data) {
  n <- length(pbp_data[[1L]])
  out <- data.frame(
    c_td_own = rep(1, n),
    c_fg_own = rep(1, n),
    c_saf_own = rep(1, n),
    c_td_opp = rep(1, n),
    c_fg_opp = rep(1, n),
    c_saf_opp = rep(1, n),
    k_td_own = rep(1, n),
    k_td_opp = rep(1, n),
    two_td_own = rep(FALSE, n),
    two_td_opp = rep(FALSE, n),
    two_try = rep(FALSE, n)
  )

  needed <- c("qtr", "game_id", "drive", "score_differential", "season")
  if (!all(needed %chin% names(pbp_data))) {
    return(out)
  }
  ot <- which(pbp_data$qtr > 4)
  if (length(ot) == 0L) {
    return(out)
  }

  game <- pbp_data$game_id[ot]
  drive <- pbp_data$drive[ot]
  first_drive <- suppressWarnings(tapply(drive, game, min, na.rm = TRUE))
  possession <- pmin(drive - as.vector(first_drive[game]) + 1, 3)
  rule <- ep_ot_rule(game, pbp_data$season[ot])
  diff <- pbp_data$score_differential[ot]
  next_possession <- pmin(possession + 1, 3)

  own_td <- ep_ot_td(rule, possession, diff)
  opp_td <- ep_ot_td(rule, next_possession, -diff)
  out$c_td_own[ot] <- own_td$cont
  out$k_td_own[ot] <- own_td$conv
  out$two_td_own[ot] <- own_td$two
  out$c_td_opp[ot] <- opp_td$cont
  out$k_td_opp[ot] <- opp_td$conv
  out$two_td_opp[ot] <- opp_td$two
  out$c_fg_own[ot] <- ep_ot_continues(rule, possession, diff, 3)
  out$c_saf_own[ot] <- ep_ot_continues(rule, possession, diff, 2)
  out$c_fg_opp[ot] <- ep_ot_continues(rule, next_possession, -diff, 3)
  out$c_saf_opp[ot] <- ep_ot_continues(rule, next_possession, -diff, 2)
  # the conversion after that touchdown: the team is down 1 on the second
  # possession
  out$two_try[ot] <- rule == "BOTH" & possession %in% 2 & diff %in% -1

  out
}

# Value of each kind of score for the possession team, net of the possession
# the other team gets afterwards. Input is get_kickoff_values() and
# ep_continue_factors() together, and the season (for the conversion). A
# touchdown includes the value of the conversion that follows it (none if the
# touchdown ends the game).
score_values <- function(kv, season) {
  conv <- conversion_value(season)
  two <- 2 * two_point_make_prob
  conv_own <- data.table::fifelse(kv$two_td_own, two, conv)
  conv_opp <- data.table::fifelse(kv$two_td_opp, two, conv)
  data.frame(
    td_own = 6 + kv$k_td_own * conv_own - kv$c_td_own * kv$v,
    fg_own = 3 - kv$c_fg_own * kv$v,
    saf_own = 2 + kv$c_saf_own * kv$w,
    td_opp = -6 - (kv$k_td_opp * conv_opp - kv$c_td_opp * kv$v),
    fg_opp = -3 + kv$c_fg_opp * kv$v,
    saf_opp = -2 - kv$c_saf_opp * kv$w
  )
}

# The same values from the view of the other team
swap_score_values <- function(sv) {
  data.frame(
    td_own = -sv$td_opp,
    fg_own = -sv$fg_opp,
    saf_own = -sv$saf_opp,
    td_opp = -sv$td_own,
    fg_opp = -sv$fg_own,
    saf_opp = -sv$saf_own
  )
}

# EP from the probabilities of the next scoring event
ep_from_probs <- function(td, opp_td, fg, opp_fg, saf, opp_saf, sv) {
  td *
    sv$td_own +
    opp_td * sv$td_opp +
    fg * sv$fg_own +
    opp_fg * sv$fg_opp +
    saf * sv$saf_own +
    opp_saf * sv$saf_opp
}

#get predictions for a set of pbp data
#for predict stage
get_preds_wp <- function(pbp) {
  stats::predict(load_model("wp"), model_matrix(pbp, wp_model_features))
}

#get predictions for a set of pbp data
#for predict stage. See R/helper_add_vegas_wp.R for the model
get_preds_wp_spread <- function(pbp) {
  # only the columns the model uses (the scenario data have all columns)
  cols <- intersect(wp_input_columns, names(pbp))
  wp_vegas_predict(lapply(stats::setNames(nm = cols), function(col) pbp[[col]]))
}


#get the columns needed for wp predictions
#making sure they're in the right order
wp_model_features <- c(
  "receive_2h_ko",
  "home",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "Diff_Time_Ratio",
  "score_differential",
  "down",
  "ydstogo",
  "yardline_100",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# the columns that get_preds_wp() and get_preds_wp_spread() need (season,
# roof and season_type are optional for the vegas_wp model)
wp_input_columns <- c(
  wp_model_features,
  "posteam_spread",
  "season",
  "roof",
  "season_type"
)
prepare_wp_data <- function(pbp) {
  if (anyNA(pbp$spread_line)) {
    broken_games <- sort(unique(pbp$game_id[is.na(pbp$spread_line)]))
    cli::cli_alert_danger(
      "The following game{?s} {?is/are} missing valid spread lines: {.val {broken_games}}."
    )
    cli::cli_alert_warning(
      "nflfastR will manually set the spread for the home team to {.val 1.5} points!"
    )
    cli::cli_alert_warning(
      "If you see this, please reach out to the package maintainers {.url https://github.com/nflverse/nflfastR/issues}"
    )
    pbp[is.na(spread_line), spread_line := 1.5]
  }

  pbp[,
    receive_2h_ko := as.numeric(
      qtr <= 2 & posteam == defteam[!is.na(defteam)][1L]
    ),
    by = "game_id"
  ]
  data.table::set(
    pbp,
    j = "posteam_spread",
    value = data.table::fifelse(
      pbp$home == 1,
      pbp$spread_line,
      -1 * pbp$spread_line
    )
  )
  data.table::set(
    pbp,
    j = "elapsed_share",
    value = (3600 - pbp$game_seconds_remaining) / 3600
  )
  data.table::set(
    pbp,
    j = "spread_time",
    value = pbp$posteam_spread * exp(-4 * pbp$elapsed_share)
  )
  data.table::set(
    pbp,
    j = "Diff_Time_Ratio",
    value = pbp$score_differential /
      (exp(-4 * pbp$elapsed_share))
  )

  pbp
}


#add ep variables
#All of these are heavily borrowed from nflscrapR (Maksim Horowitz, Ronald Yurko, and Samuel Ventura)
add_ep_variables <- function(pbp_data) {
  #testing
  #pbp_data <- g

  # ----------------------------------------------------------------------------
  # ---- special case: deal with FG attempts
  # Now make another dataset that to get the EP probabilities from a missed FG:
  # the scenario data are lists of columns (modifying them doesn't touch
  # pbp_data)
  missed_fg_data <- as.list(pbp_data)
  # Subtract 5.065401 from TimeSecs:
  missed_fg_data$half_seconds_remaining <- missed_fg_data$half_seconds_remaining -
    5.065401

  # Correct the yrdline100: the other team takes over at the spot of the kick or
  # at its own 20, whichever is farther from its end zone
  missed_fg_data$yardline_100 <- pmin(
    100 - (missed_fg_data$yardline_100 + 8),
    80
  )
  # The other team has the ball after a miss, so home field and the timeouts
  # are from its view
  missed_fg_data$home <- 1 - missed_fg_data$home
  missed_fg_data$posteam_timeouts_remaining <- pbp_data$defteam_timeouts_remaining
  missed_fg_data$defteam_timeouts_remaining <- pbp_data$posteam_timeouts_remaining
  # Now first down:
  missed_fg_data$down1 <- rep(1, nrow(pbp_data))
  missed_fg_data$down2 <- rep(0, nrow(pbp_data))
  missed_fg_data$down3 <- rep(0, nrow(pbp_data))
  missed_fg_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  missed_fg_data$ydstogo <- rep(10, nrow(pbp_data))

  # Find the FG attempts and extra points. The predictions of the special
  # cases below are only needed for those plays.
  fg_attempt_i <- which(pbp_data$play_type == "field_goal")
  extrapoint_i <- which(
    (pbp_data$play_type == "extra_point" |
      pbp_data$play_type_nfl == "XP_KICK") &
      (is.na(pbp_data$play_type_nfl) | pbp_data$play_type_nfl != "PAT2")
  )

  # ---- special case: deal with kickoffs (probabilities are updated below)
  # Calculate the EP for receiving a kickoff (from the point of view for
  # recieving team). The predictions are needed for all rows, because they are
  # also the possession team's kickoff state in get_kickoff_values()
  kickoff_data <- as.list(pbp_data)

  # Change the yard line to the receiving team's typical first snap after a
  # kickoff, which depends on the kickoff rules in the season (see
  # wp_kickoff_start())
  kickoff_data$yardline_100 <- wp_kickoff_start(kickoff_data$season)
  # Now first down:
  kickoff_data$down1 <- rep(1, nrow(pbp_data))
  kickoff_data$down2 <- rep(0, nrow(pbp_data))
  kickoff_data$down3 <- rep(0, nrow(pbp_data))
  kickoff_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10, nrow(pbp_data))

  # Find the kickoffs:
  kickoff_i <- which(
    pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1
  )

  # The free kick after a safety starts the scoring team's drive further up the
  # field than a kickoff start does. The safety was valued with this spot
  # (see safety_free_kick_yardline), so the free kick has to start from it too.
  # A free kick is a kickoff within 3 plays after a safety of the same game.
  is_safety <- pbp_data$safety %in% 1
  after_safety <- Reduce(
    `|`,
    lapply(1:3, function(back) {
      data.table::shift(is_safety, back, fill = FALSE) &
        data.table::shift(pbp_data$game_id, back) == pbp_data$game_id
    })
  )
  free_kick_i <- intersect(kickoff_i, which(after_safety))
  free_kick_data <- lapply(kickoff_data, `[`, free_kick_i)
  free_kick_data$yardline_100 <- rep(
    safety_free_kick_yardline,
    length(free_kick_i)
  )

  # A kickoff from midfield (after a penalty) that is a touchback is spotted at
  # the receiving team's 20 since 2026, not at the usual start spot. This is
  # only known on the actual kickoff, and not for a kickoff in the future
  midfield_i <- intersect(
    kickoff_i,
    which(pbp_data$season >= 2026 & pbp_data$yardline_100 == 50)
  )
  midfield_data <- lapply(kickoff_data, `[`, midfield_i)
  midfield_data$yardline_100 <- rep(80, length(midfield_i))

  # Get the predicted probabilites of all plays and of the special cases with
  # one call of the EP model
  preds <- get_preds_stacked(list(
    pbp_data,
    lapply(missed_fg_data, `[`, fg_attempt_i),
    kickoff_data,
    free_kick_data,
    midfield_data
  ))
  base_ep_preds <- preds[[1]]
  missed_fg_ep_preds <- expand_rows(preds[[2]], nrow(pbp_data), fg_attempt_i)
  kickoff_preds <- preds[[3]]

  # Find the rows where TimeSecs_Remaining became 0 or negative and make all the probs equal to 0:
  end_game_i <- which(missed_fg_data$half_seconds_remaining <= 0)
  missed_fg_ep_preds[end_game_i, ] <- rep(0, ncol(missed_fg_ep_preds))

  # if the half ends, no one scored
  missed_fg_ep_preds[end_game_i, "No_Score"] <- 1

  # Get the probability of making the field goal (see R/helper_fg_model.R):
  make_fg_prob <- fg_make_prob(
    pbp_data$season,
    pbp_data$roof,
    pbp_data$yardline_100
  )

  # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
  missed_fg_ep_preds <- missed_fg_ep_preds * (1 - make_fg_prob)
  # Now update the probabilities for the FG attempts (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
  base_ep_preds[fg_attempt_i, "Field_Goal"] <- make_fg_prob[fg_attempt_i] +
    missed_fg_ep_preds[fg_attempt_i, "Opp_Field_Goal"]
  # Update the other columns based on the opposite possession:
  base_ep_preds[
    fg_attempt_i,
    c(
      "Touchdown",
      "Opp_Field_Goal",
      "Opp_Touchdown",
      "Safety",
      "Opp_Safety",
      "No_Score"
    )
  ] <- missed_fg_ep_preds[
    fg_attempt_i,
    c(
      "Opp_Touchdown",
      "Field_Goal",
      "Touchdown",
      "Opp_Safety",
      "Safety",
      "No_Score"
    )
  ]

  # ----------------------------------------------------------------------------------
  # ---- special case: deal with kickoffs
  # update the columns for Kickoff plays with the probabilities of the kickoff
  # start (the free kick spot after a safety, the 20 after a touchback on a
  # kickoff from midfield)

  # Now update the probabilities:
  base_ep_preds[kickoff_i, ] <- kickoff_preds[kickoff_i, ]
  base_ep_preds[free_kick_i, ] <- preds[[4]]
  base_ep_preds[midfield_i, ] <- preds[[5]]

  # ----------------------------------------------------------------------------------
  # Insert probabilities of 0 for everything but No_Score for QB Kneels that
  # occur on the possession team's side of the field:
  # Find these QB Kneels:
  qb_kneels_i <- which(
    pbp_data$play_type == "qb_kneel" & pbp_data$yardline_100 > 50
  )

  # Now update the probabilities:
  base_ep_preds[qb_kneels_i, ] <- 0
  base_ep_preds[qb_kneels_i, "No_Score"] <- 1

  # ----------------------------------------------------------------------------------
  # Create two new columns, ExPoint_Prob and TwoPoint_Prob, for the PAT events:
  base_ep_preds$ExPoint_Prob <- 0
  base_ep_preds$TwoPoint_Prob <- 0

  # Find the indices for these types of plays:
  twopoint_i <- which(pbp_data$two_point_attempt == 1)

  #new: special case for PAT or kickoff with penalty
  #for inserting NAs
  lag <- function(x) data.table::shift(x)
  lead <- function(x) data.table::shift(x, type = "lead")
  st_penalty_i_1 <- which(
    # pat: prior play was TD or PAT or Timeout and next play is PAT and this play isn't a td and it's not a regular down
    (pbp_data$touchdown == 0 &
      is.na(pbp_data$down) &
      (lag(pbp_data$touchdown) == 1 |
        lag(pbp_data$play_type_nfl) == "XP_KICK" |
        lag(pbp_data$timeout) == 1) &
      (lead(pbp_data$two_point_attempt) == 1 |
        lead(pbp_data$extra_point_attempt) == 1 |
        lead(pbp_data$play_type_nfl) == "XP_KICK")) |
      #kickoff: prior play was PAT and next play is kickoff, unless it is a
      #kickoff that ended in a safety (the next kickoff is the free kick after it)
      ((lag(pbp_data$two_point_attempt) == 1 |
        lag(pbp_data$extra_point_attempt) == 1) &
        lead(pbp_data$kickoff_attempt == 1) &
        !(pbp_data$safety %in% 1))
  )

  # down is NA and play type no play
  no_play_without_down <- is.na(pbp_data$down) & pbp_data$play_type == "no_play"
  # has a key term in desc
  st_penalty_term <- no_play_without_down &
    stringi::stri_detect_regex(
      pbp_data$desc,
      "Kick formation|Pass formation|kicks onside|Offside on Free Kick"
    )
  # two point conversion and next play isn't a kickoff
  st_penalty_two_point <- no_play_without_down &
    stringi::stri_detect_fixed(pbp_data$desc, "TWO-POINT CONVERSION") &
    lead(pbp_data$kickoff_attempt) == 0
  st_penalty_i_2 <- which(
    is.na(lead(pbp_data$down)) & (st_penalty_term | st_penalty_two_point)
  )

  # Assign the make_fg_probs of the extra-point PATs:
  base_ep_preds$ExPoint_Prob[extrapoint_i] <- make_fg_prob[extrapoint_i]

  # Assign the TwoPoint_Prob with the historical success rate:
  base_ep_preds$TwoPoint_Prob[twopoint_i] <- two_point_make_prob

  # ----------------------------------------------------------------------------------
  # Insert NAs for timeouts and end of play rows:
  missing_i <- which(
    (pbp_data$timeout == 1 &
      pbp_data$play_type == "no_play" &
      !stringi::stri_detect_regex(
        pbp_data$desc,
        " (pass|sacked|scramble|punts|up the middle|left end|left guard|left tackle|right end|right guard|right tackle) "
      )) |
      is.na(pbp_data$play_type)
  )

  # Now update the probabilities for missing and PATs:
  ep_outcomes <- names(kickoff_preds)
  base_ep_preds[
    c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2),
    ep_outcomes
  ] <- 0

  # Rename the events to all have _Prob at the end of them:
  names(base_ep_preds)[match(ep_outcomes, names(base_ep_preds))] <- paste0(
    ep_outcomes,
    "_Prob"
  )

  # Join them together:
  data.table::set(
    pbp_data,
    j = names(base_ep_preds),
    value = as.list(base_ep_preds)
  )

  # Value of the kickoff (or safety free kick) possession that follows a score
  # (the columns are kept for add_air_yac_ep_variables() and removed by
  # select_variables() at the end of the pipeline)
  # The free kick value (W) is exact where a safety is likely (see
  # ep_exact_w()), using only what is known before the play
  kick_values <- cbind(
    get_kickoff_values(
      pbp_data,
      kickoff_preds = kickoff_preds,
      exact_w = ep_exact_w(pbp_data$Safety_Prob, pbp_data$Opp_Safety_Prob)
    ),
    ep_continue_factors(pbp_data)
  )
  score_vals <- score_values(kick_values, pbp_data$season)

  # The endpoint of an actual safety always uses the exact W. It is separate from
  # the EP before the play, which cannot depend on what happens next
  end_saf <- score_vals[, c("saf_own", "saf_opp")]
  safety_i <- which(is_safety)
  if (length(safety_i) > 0) {
    safety_kv <- kick_values[safety_i, ]
    safety_kv[, c("v", "w")] <- get_kickoff_values(
      subset_columns(
        pbp_data,
        safety_i,
        c(ep_model_features, "season", "location")
      ),
      exact_w = TRUE
    )
    safety_sv <- score_values(safety_kv, pbp_data$season[safety_i])
    end_saf$saf_own[safety_i] <- safety_sv$saf_own
    end_saf$saf_opp[safety_i] <- safety_sv$saf_opp
  }
  names(end_saf) <- paste0("ep_end_", names(end_saf))
  names(kick_values) <- paste0("ep_", names(kick_values))
  names(score_vals) <- paste0("ep_sv_", names(score_vals))
  data.table::set(
    pbp_data,
    j = c(names(kick_values), names(score_vals), names(end_saf)),
    value = c(as.list(kick_values), as.list(score_vals), as.list(end_saf))
  )

  # Calculate the ExpPts. The value of a score is its points plus or minus the
  # value of the possession the other team gets afterwards (see
  # get_kickoff_values()). The extra point and two point rows are not adjusted
  # because the touchdown value already accounts for the kickoff.
  data.table::set(
    pbp_data,
    j = "ExpPts",
    value = (0 * pbp_data$No_Score_Prob) +
      (pbp_data$Touchdown_Prob * pbp_data$ep_sv_td_own) +
      (pbp_data$Opp_Touchdown_Prob * pbp_data$ep_sv_td_opp) +
      (pbp_data$Field_Goal_Prob * pbp_data$ep_sv_fg_own) +
      (pbp_data$Opp_Field_Goal_Prob * pbp_data$ep_sv_fg_opp) +
      (pbp_data$Safety_Prob * pbp_data$ep_sv_saf_own) +
      (pbp_data$Opp_Safety_Prob * pbp_data$ep_sv_saf_opp) +
      (1 * pbp_data$ExPoint_Prob) +
      (2 * pbp_data$TwoPoint_Prob)
  )

  #just going to set these to NA bc we have no way of calculating EPA for them
  data.table::set(
    pbp_data,
    i = c(st_penalty_i_1, st_penalty_i_2, missing_i),
    j = "ExpPts",
    value = NA_real_
  )

  #################################################################
  # Calculate EPA:

  # fill missing values with the next value of the same game
  pbp_data[,
    c("ep", "tmp_posteam") := list(
      data.table::nafill(ExpPts, type = "nocb"),
      # nafill() only works for numbers, so we fill the row index instead
      posteam[data.table::nafill(
        data.table::fifelse(is.na(posteam), NA_integer_, seq_len(.N)),
        type = "nocb"
      )]
    ),
    by = "game_id"
  ]

  # helper for end of game
  data.table::set(
    pbp_data,
    j = "end_game",
    value = as.numeric(stringi::stri_detect_regex(
      tolower(pbp_data$desc),
      "(end of game)|(end game)"
    ))
  )
  # the conversion value in the touchdown's value, which is what the
  # conversion that follows is measured against
  data.table::set(
    pbp_data,
    j = "ep_conversion",
    value = data.table::fifelse(
      pbp_data$ep_two_try,
      2 * two_point_make_prob,
      conversion_value(pbp_data$season)
    )
  )
  # a score on the last play of the game has no extra point and no kickoff
  # after it, so it is worth just its points. That is the case if the next play
  # of the same game is the end of the game or if there is no next play.
  next_in_game <- data.table::shift(pbp_data$game_id, type = "lead") ==
    pbp_data$game_id
  data.table::set(
    pbp_data,
    j = "ep_game_over",
    value = data.table::fifelse(
      next_in_game %in% TRUE,
      data.table::shift(pbp_data$end_game, type = "lead") == 1,
      TRUE
    ) %in%
      TRUE
  )

  # get epa for non-scoring plays
  data.table::set(
    pbp_data,
    j = "home_ep",
    value = data.table::fifelse(
      pbp_data$tmp_posteam == pbp_data$home_team,
      pbp_data$ep,
      -pbp_data$ep
    )
  )
  pbp_data[,
    home_epa := data.table::shift(home_ep, type = "lead") - home_ep,
    by = "game_id"
  ]
  data.table::set(
    pbp_data,
    j = "epa",
    value = data.table::fifelse(
      pbp_data$tmp_posteam == pbp_data$home_team,
      pbp_data$home_epa,
      -pbp_data$home_epa
    )
  )
  # scoring plays (later updates win)
  # td
  pbp_data[
    !is.na(td_team),
    epa := data.table::fifelse(
      td_team == posteam,
      data.table::fifelse(ep_game_over, 6, ep_sv_td_own) - ep,
      data.table::fifelse(ep_game_over, -6, ep_sv_td_opp) - ep
    )
  ]
  # Offense field goal:
  pbp_data[
    is.na(td_team) & field_goal_made == 1,
    epa := data.table::fifelse(ep_game_over, 3, ep_sv_fg_own) - ep
  ]
  # Conversions (extra point and two point tries): the net points minus the
  # conversion value in the touchdown's value, so a touchdown and its
  # conversion add up to 6 + the conversion's points - V whichever action was
  # taken (this includes the choice between a kick and a two point try, the
  # difference between that action's expected points and the average)
  # Offense extra-point:
  pbp_data[
    is.na(td_team) & field_goal_made == 0 & extra_point_good == 1,
    epa := 1 - ep_conversion
  ]
  # Offense two-point conversion:
  pbp_data[
    is.na(td_team) &
      field_goal_made == 0 &
      extra_point_good == 0 &
      (two_point_rush_good == 1 |
        two_point_pass_good == 1 |
        two_point_pass_reception_good == 1),
    epa := 2 - ep_conversion
  ]
  # Failed PAT (both 1 and 2):
  pbp_data[
    is.na(td_team) &
      field_goal_made == 0 &
      extra_point_good == 0 &
      ((extra_point_failed == 1 |
        extra_point_blocked == 1 |
        extra_point_aborted == 1) |
        (two_point_rush_failed == 1 |
          two_point_pass_failed == 1 |
          two_point_pass_reception_failed == 1)),
    epa := 0 - ep_conversion
  ]
  # Opponent scores defensive 2 point:
  pbp_data[defensive_two_point_conv == 1, epa := -2 - ep_conversion]
  # Safety:
  pbp_data[
    safety_team == defteam,
    epa := data.table::fifelse(ep_game_over, -2, ep_end_saf_opp) - ep
  ]
  pbp_data[
    safety_team == posteam,
    epa := data.table::fifelse(ep_game_over, 2, ep_end_saf_own) - ep
  ]

  # Now rename each of the expected points columns to match the style of
  # the updated code:
  data.table::setnames(
    pbp_data,
    c(
      "No_Score_Prob",
      "Opp_Field_Goal_Prob",
      "Opp_Safety_Prob",
      "Opp_Touchdown_Prob",
      "Field_Goal_Prob",
      "Safety_Prob",
      "Touchdown_Prob",
      "ExPoint_Prob",
      "TwoPoint_Prob"
    ),
    c(
      "no_score_prob",
      "opp_fg_prob",
      "opp_safety_prob",
      "opp_td_prob",
      "fg_prob",
      "safety_prob",
      "td_prob",
      "extra_point_prob",
      "two_point_conversion_prob"
    )
  )

  # Change epa for plays occurring at end of half with no scoring plays to be
  # just the difference between 0 and starting ep (the next play of the same
  # game decides that):
  pbp_data[,
    c("tmp_next_qtr", "tmp_next_desc", "tmp_next_end_game") := list(
      data.table::shift(qtr, type = "lead"),
      data.table::shift(desc, type = "lead"),
      data.table::shift(end_game, type = "lead")
    ),
    by = "game_id"
  ]
  # (fifelse() because a missing condition makes epa missing)
  pbp_data[,
    epa := data.table::fifelse(
      ((qtr == 2 & (tmp_next_qtr == 3 | tmp_next_desc == "END QUARTER 2")) |
        (qtr == 4 &
          (tmp_next_qtr == 5 |
            tmp_next_desc == "END QUARTER 4" |
            tmp_next_end_game == 1))) &
        sp == 0 &
        !is.na(play_type),
      0 - ep,
      epa
    )
  ]
  # last play of OT
  pbp_data[,
    epa := data.table::fifelse(
      qtr > 4 & tmp_next_end_game == 1 & sp == 0,
      0 - ep,
      epa
    )
  ]
  pbp_data[
    desc == "END QUARTER 2" | end_game == 1,
    c("epa", "ep") := NA_real_
  ]
  data.table::set(
    pbp_data,
    j = c("tmp_next_qtr", "tmp_next_desc", "tmp_next_end_game"),
    value = NULL
  )

  # Create columns with cumulative epa totals for both teams:
  add_team_totals(pbp_data, pbp_data$epa, "epa")
}

# The value x from the point of view of the home and away team (0 if missing),
# also only for run and pass plays, and their cumulative totals within each
# game. type is "epa" or "wpa".
add_team_totals <- function(pbp, x, type) {
  team_value <- function(team) {
    data.table::nafill(
      data.table::fifelse(pbp$posteam == team, x, -x),
      fill = 0
    )
  }
  play_value <- function(is_type, value) {
    data.table::nafill(data.table::fifelse(is_type, value, 0), fill = 0)
  }
  home <- team_value(pbp$home_team)
  away <- team_value(pbp$away_team)
  is_run <- pbp$play_type == "run"
  is_pass <- pbp$play_type == "pass"
  team_cols <- list(
    home_team = home,
    away_team = away,
    home_team_rush = play_value(is_run, home),
    away_team_rush = play_value(is_run, away),
    home_team_pass = play_value(is_pass, home),
    away_team_pass = play_value(is_pass, away)
  )
  names(team_cols) <- paste0(names(team_cols), "_", type)
  data.table::set(pbp, j = names(team_cols), value = team_cols)
  total_cols <- sub("_team_", "_", paste0("total_", names(team_cols)))
  pbp[,
    (total_cols) := lapply(.SD, cumsum),
    by = "game_id",
    .SDcols = names(team_cols)
  ]
  pbp
}


#################################################################
# Calculate WP and WPA:
add_wp_variables <- function(pbp_data) {
  #testing only
  # pbp_data <- g

  # Initialize the df to store predicted win probability
  OffWinProb <- rep(NA_real_, nrow(pbp_data))
  OffWinProb_spread <- rep(NA_real_, nrow(pbp_data))

  pbp_data <- pbp_data |>
    prepare_wp_data()

  #regulation plays
  regular_i <- which(pbp_data$qtr <= 4)

  # the model inputs of just the regulation plays
  regular_df <- subset_columns(pbp_data, regular_i, wp_input_columns)

  # do predictions for the regular df. Rows without a down are set to NA below
  # anyway, so the vegas_wp model only needs the rows with a down
  OffWinProb[regular_i] <- get_preds_wp(regular_df)
  OffWinProb_spread[regular_i] <- predict_rows(
    get_preds_wp_spread,
    regular_df,
    which(!is.na(regular_df$down))
  )

  ## set to NA WP for plays down is missing
  # for kickoffs and PATs, these will get overwritten by the fixes after this

  down_na <- which(is.na(pbp_data$down))
  OffWinProb[down_na] <- NA_real_
  OffWinProb_spread[down_na] <- NA_real_

  ## start PAT fix

  # PAT attempts in a play without down that isn't an onside kick
  pat_formation <- pbp_data$kickoff_attempt == 0 &
    !stringi::stri_detect_fixed(pbp_data$desc, "Onside Kick") &
    is.na(pbp_data$down)
  # plays with 1 point PAT attempts
  pat_i <- which(
    (pat_formation &
      stringi::stri_detect_fixed(pbp_data$desc, "Kick formation")) |
      # or has PAT indicators
      stringi::stri_detect_fixed(pbp_data$desc, "extra point") |
      !is.na(pbp_data$extra_point_result)
  )

  # plays with 2 point PAT attempts
  two_pt_i <- which(
    (pat_formation &
      stringi::stri_detect_fixed(pbp_data$desc, "Pass formation")) |
      # or has PAT indicators
      stringi::stri_detect_fixed(
        pbp_data$desc,
        "TWO-POINT CONVERSION ATTEMPT"
      ) |
      !is.na(pbp_data$two_point_conv_result)
  )

  # some rare 2 point PAT attempts have duplicated matches in 1 point PAT attempts
  # so we remove them in the next line
  pat_i <- pat_i[!pat_i %in% two_pt_i]

  # PAT make probability (also in overtime): the PAT is a field goal from the
  # 15 since 2015 (the 2 before)
  make_pat_prob <- fg_make_prob(
    pbp_data$season,
    pbp_data$roof,
    data.table::fifelse(pbp_data$season >= 2015, 15, 2)
  )

  # overtime has its own win probability model (see add_ot_wp_vectors()), the
  # regulation formulas below only apply to regulation
  pat_i_ot <- pat_i[pbp_data$qtr[pat_i] > 4]
  two_pt_i_ot <- two_pt_i[pbp_data$qtr[two_pt_i] > 4]
  pat_i <- pat_i[!pat_i %in% pat_i_ot]
  two_pt_i <- two_pt_i[!two_pt_i %in% two_pt_i_ot]

  # make df of post-PAT plays
  # the scenario data are lists of columns (modifying them doesn't touch
  # pbp_data)
  pat_data <- as.list(pbp_data)
  # swap timeouts
  pat_data$posteam_timeouts_remaining <- pbp_data$defteam_timeouts_remaining
  pat_data$defteam_timeouts_remaining <- pbp_data$posteam_timeouts_remaining
  # swap score
  pat_data$score_differential <- -pbp_data$score_differential
  # 1st and 10
  pat_data$down <- rep(1, nrow(pbp_data))
  pat_data$ydstogo <- rep(10, nrow(pbp_data))
  # flip receive_2h_ko var
  pat_data$receive_2h_ko <- data.table::fcase(
    pbp_data$qtr <= 2 & pbp_data$receive_2h_ko == 0,
    1,
    pbp_data$qtr <= 2 & pbp_data$receive_2h_ko == 1,
    0,
    default = pbp_data$receive_2h_ko
  )
  # switch posteam
  pat_data$posteam <- data.table::fifelse(
    pbp_data$home_team == pbp_data$posteam,
    pbp_data$away_team,
    pbp_data$home_team
  )
  # the other team's first snap after the kickoff
  pat_data$yardline_100 <- wp_kickoff_start(pbp_data$season)
  pat_data$home <- data.table::fcase(
    pbp_data$home == 0,
    1,
    pbp_data$home == 1,
    0
  )
  pat_data$posteam_spread <- data.table::fifelse(
    pat_data$home == 1,
    pat_data$spread_line,
    -1 * pat_data$spread_line
  )
  pat_data$elapsed_share <- (3600 - pat_data$game_seconds_remaining) / 3600
  pat_data$spread_time <- pat_data$posteam_spread *
    exp(-4 * pat_data$elapsed_share)

  # the PAT data if the team scores 0, 1 or 2 points
  pat_scored <- function(points) {
    x <- pat_data
    if (points != 0) {
      x$score_differential <- x$score_differential - points
    }
    add_esdtr(x)
  }

  # the predictions below are only needed for PATs in regulation
  pat_rows <- sort(c(pat_i, two_pt_i))

  ## start with spread version
  # get pat if 0, 1, or 2
  pat_0 <- predict_rows(get_preds_wp_spread, pat_scored(0), pat_rows)
  pat_1 <- predict_rows(get_preds_wp_spread, pat_scored(1), pat_rows)
  pat_2 <- predict_rows(get_preds_wp_spread, pat_scored(2), pat_rows)

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 -
    (two_point_make_prob * pat_2 + (1 - two_point_make_prob) * pat_0)

  OffWinProb_spread[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb_spread[pat_i] <- pat_go_for_1[pat_i]

  ## repeat for non-spread version
  # get pat if 0, 1, or 2
  pat_0 <- predict_rows(get_preds_wp, pat_scored(0), pat_rows)
  pat_1 <- predict_rows(get_preds_wp, pat_scored(1), pat_rows)
  pat_2 <- predict_rows(get_preds_wp, pat_scored(2), pat_rows)

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 -
    (two_point_make_prob * pat_2 + (1 - two_point_make_prob) * pat_0)

  OffWinProb[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb[pat_i] <- pat_go_for_1[pat_i]

  ## end PAT fix

  ## now we need to fix WP on kickoffs, which will be the WP of the receiving
  ## team's first snap: 1st and 10 at the typical start after a kickoff in the
  ## kickoff rules of that season (see wp_kickoff_start())
  kickoff_data <- as.list(pbp_data)
  kickoff_data$yardline_100 <- wp_kickoff_start(kickoff_data$season)
  # Now first down:
  kickoff_data$down <- rep(1, nrow(pbp_data))
  kickoff_data$down1 <- rep(1, nrow(pbp_data))
  kickoff_data$down2 <- rep(0, nrow(pbp_data))
  kickoff_data$down3 <- rep(0, nrow(pbp_data))
  kickoff_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10, nrow(pbp_data))

  # Find the kickoffs in regulation:
  kickoff_i <- which(
    (pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1) &
      pbp_data$qtr <= 4
  )

  # Get the new predicted probabilites:
  kickoff_preds <- predict_rows(get_preds_wp, kickoff_data, kickoff_i)
  kickoff_preds_spread <- predict_rows(
    get_preds_wp_spread,
    kickoff_data,
    kickoff_i
  )

  # Now update the probabilities:
  OffWinProb[kickoff_i] <- kickoff_preds[kickoff_i]
  OffWinProb_spread[kickoff_i] <- kickoff_preds_spread[kickoff_i]

  ## end fix for kickoffs

  ## overtime: separate rule aware model
  kickoff_i_ot <- which(
    (pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1) &
      pbp_data$qtr > 4
  )
  ot_i <- which(pbp_data$qtr > 4)
  if (length(ot_i) > 0) {
    # probability that a field goal from the current spot is good
    fg_prob_ot <- rep(NA_real_, nrow(pbp_data))
    fg_prob_ot[ot_i] <- fg_make_prob(
      pbp_data$season[ot_i],
      pbp_data$roof[ot_i],
      pbp_data$yardline_100[ot_i]
    )
    ot_wp <- add_ot_wp_vectors(
      pbp_data,
      kickoff_i = kickoff_i_ot,
      pat_i = pat_i_ot,
      two_pt_i = two_pt_i_ot,
      make_pat_prob = make_pat_prob,
      fg_prob = fg_prob_ot
    )
    OffWinProb[ot_i] <- ot_wp$wp[ot_i]
    OffWinProb_spread[ot_i] <- ot_wp$vegas_wp[ot_i]
  }

  # Now create the win probability columns and return:
  data.table::set(pbp_data, j = "wp", value = OffWinProb)
  data.table::set(pbp_data, j = "vegas_wp", value = OffWinProb_spread)
  # for figuring out posteam on NA wp lines. The team has to come from the
  # same row that wp is filled from below. Non-plays (end of quarter notes,
  # captains announcements, ...) can carry a stale posteam, which flipped
  # home_wp for them when the next play belongs to the other team.
  data.table::set(
    pbp_data,
    j = "tmp_posteam",
    value = data.table::fifelse(
      is.na(OffWinProb) | is.na(pbp_data$posteam),
      NA_character_,
      pbp_data$posteam
    )
  )
  # fill within a game so the last rows of a game can never pick up values
  # from the first play of the next game when several games are processed
  pbp_data[,
    c("wp", "vegas_wp", "tmp_posteam") := list(
      data.table::nafill(wp, type = "nocb"),
      data.table::nafill(vegas_wp, type = "nocb"),
      # nafill() only works for numbers, so we fill the row index instead
      tmp_posteam[data.table::nafill(
        data.table::fifelse(is.na(tmp_posteam), NA_integer_, seq_len(.N)),
        type = "nocb"
      )]
    ),
    by = "game_id"
  ]

  home_has_ball <- pbp_data$tmp_posteam == pbp_data$home_team
  desc_lower <- tolower(pbp_data$desc)

  #add columns for home WP
  data.table::set(
    pbp_data,
    j = "home_wp",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$wp,
      1 - pbp_data$wp
    )
  )
  data.table::set(
    pbp_data,
    j = "vegas_home_wp",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$vegas_wp,
      1 - pbp_data$vegas_wp
    )
  )
  # convenience to mark end of game
  data.table::set(
    pbp_data,
    j = "end_game",
    value = as.numeric(
      stringi::stri_detect_regex(desc_lower, "(end of game)|(end game)")
    )
  )
  # convenience for marking home win prob on last line
  data.table::set(
    pbp_data,
    j = "final_value",
    value = data.table::fcase(
      pbp_data$home_score > pbp_data$away_score,
      1,
      pbp_data$away_score > pbp_data$home_score,
      0,
      pbp_data$home_score == pbp_data$away_score,
      0.5
    )
  )
  #make 1 or 0 the final win prob
  pbp_data[
    end_game == 1,
    c("vegas_home_wp", "home_wp") := list(final_value, final_value)
  ]
  data.table::set(pbp_data, j = "away_wp", value = 1 - pbp_data$home_wp)
  # make wp of posteam on last line NA because there's no posteam
  pbp_data[end_game == 1, c("vegas_wp", "wp") := NA_real_]
  data.table::set(pbp_data, j = "def_wp", value = 1 - pbp_data$wp)

  # make wpa
  kneel_or_end <- stringi::stri_detect_regex(
    desc_lower,
    "( kneels )|(end of game)|(end game)"
  )
  pbp_data[,
    vegas_home_wpa := data.table::shift(vegas_home_wp, type = "lead") -
      vegas_home_wp,
    by = "game_id"
  ]
  data.table::set(
    pbp_data,
    j = "vegas_wpa",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$vegas_home_wpa,
      -pbp_data$vegas_home_wpa
    )
  )
  pbp_data[kneel_or_end, vegas_wpa := NA_real_]
  # home wpa isn't saved but needed for next line
  pbp_data[,
    home_wpa := data.table::shift(home_wp, type = "lead") - home_wp,
    by = "game_id"
  ]
  data.table::set(
    pbp_data,
    j = "wpa",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$home_wpa,
      -pbp_data$home_wpa
    )
  )
  pbp_data[kneel_or_end, wpa := NA_real_]

  # Home and Away post:

  # same team that wpa is relative to
  post_team <- data.table::fifelse(
    is.na(pbp_data$posteam),
    NA_character_,
    pbp_data$tmp_posteam
  )
  data.table::set(
    pbp_data,
    j = "home_wp_post",
    value = data.table::fifelse(
      post_team == pbp_data$home_team,
      pbp_data$home_wp + pbp_data$wpa,
      pbp_data$home_wp - pbp_data$wpa
    )
  )
  data.table::set(
    pbp_data,
    j = "away_wp_post",
    value = data.table::fifelse(
      post_team == pbp_data$away_team,
      pbp_data$away_wp + pbp_data$wpa,
      pbp_data$away_wp - pbp_data$wpa
    )
  )

  # If next thing is end of game, and post score differential is tied because
  # it's overtime then the game ended in a tie, which counts as 0.5 for both
  # teams:
  ot_tie <- pbp_data$qtr >= 5 &
    stringi::stri_detect_regex(
      data.table::shift(desc_lower, type = "lead"),
      "(end of game)|(end game)"
    ) &
    pbp_data$score_differential_post == 0
  data.table::set(
    pbp_data,
    j = "home_wp_post",
    value = data.table::fifelse(
      ot_tie,
      0.5,
      pbp_data$home_wp_post
    )
  )
  data.table::set(
    pbp_data,
    j = "away_wp_post",
    value = data.table::fifelse(
      ot_tie,
      0.5,
      pbp_data$away_wp_post
    )
  )

  # For plays with playtype of End of Game, use the previous play's WP_post
  # columns as the pre and post, since those are already set to be 1 and 0:
  end_of_game <- pbp_data$end_game == 1
  two_no_plays <- data.table::shift(pbp_data$play_type) == "no_play" &
    pbp_data$play_type == "no_play"
  # (fifelse() because a missing two_no_plays makes the values missing)
  use_prior <- end_of_game | two_no_plays
  pbp_data[,
    c("home_wp_post", "away_wp_post") := list(
      data.table::fifelse(
        use_prior,
        data.table::shift(home_wp_post),
        home_wp_post
      ),
      data.table::fifelse(
        use_prior,
        data.table::shift(away_wp_post),
        away_wp_post
      )
    )
  ]

  # Generate columns to keep track of cumulative rushing and passing WPA
  # values within each game:
  add_team_totals(pbp_data, pbp_data$wpa, "wpa")
}


# helper function to get expected score diff to time ratio
# needed after flipping teams in WP for getting PAT WP
add_esdtr <- function(data) {
  data$Diff_Time_Ratio <- data$score_differential /
    (exp(-4 * data$elapsed_share))
  data
}


#################################################################
# air and YAC EP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_ep_variables <- function(pbp_data) {
  pass_plays_i <- which(
    !is.na(pbp_data$air_yards) & pbp_data$play_type == "pass"
  )
  # the pass plays as list of columns, the state after the air yards replaces
  # the state before the play
  pass_pbp_data <- subset_columns(
    pbp_data,
    pass_plays_i,
    c(
      ep_model_features,
      "location",
      "season",
      "down",
      "air_yards",
      "ep",
      "epa",
      "two_point_attempt",
      "penalty",
      "yards_after_catch",
      "complete_pass",
      "ep_v",
      "ep_w",
      "ep_k_td_own",
      "ep_k_td_opp",
      "ep_two_td_own",
      "ep_two_td_opp",
      "ep_c_td_own",
      "ep_c_fg_own",
      "ep_c_saf_own",
      "ep_c_td_opp",
      "ep_c_fg_opp",
      "ep_c_saf_opp"
    )
  )
  old_yrdline100 <- pass_pbp_data$yardline_100
  old_ydstogo <- pass_pbp_data$ydstogo
  old_down <- pass_pbp_data$down
  air_yards <- pass_pbp_data$air_yards
  posteam_timeouts_pre <- pass_pbp_data$posteam_timeouts_remaining
  defeam_timeouts_pre <- pass_pbp_data$defteam_timeouts_remaining

  Turnover_Ind <- data.table::fifelse(
    old_down == 4 & air_yards < old_ydstogo,
    1,
    0
  )
  first_down <- air_yards >= old_ydstogo | Turnover_Ind == 1
  pass_pbp_data$yardline_100 <- data.table::fifelse(
    Turnover_Ind == 0,
    old_yrdline100 - air_yards,
    100 - (old_yrdline100 - air_yards)
  )
  pass_pbp_data$ydstogo <- data.table::fifelse(
    first_down,
    10,
    old_ydstogo - air_yards
  )
  down <- data.table::fifelse(first_down, 1, as.numeric(old_down) + 1)
  pass_pbp_data$half_seconds_remaining <- pass_pbp_data$half_seconds_remaining -
    5.704673
  pass_pbp_data$down1 <- data.table::fifelse(down == 1, 1, 0)
  pass_pbp_data$down2 <- data.table::fifelse(down == 2, 1, 0)
  pass_pbp_data$down3 <- data.table::fifelse(down == 3, 1, 0)
  pass_pbp_data$down4 <- data.table::fifelse(down == 4, 1, 0)
  # the other team has the ball after a turnover on downs, so home and
  # the timeouts are from its view
  pass_pbp_data$home <- data.table::fifelse(
    Turnover_Ind == 1,
    1 - pass_pbp_data$home,
    pass_pbp_data$home
  )
  pass_pbp_data$posteam_timeouts_remaining <- data.table::fifelse(
    Turnover_Ind == 1,
    defeam_timeouts_pre,
    posteam_timeouts_pre
  )
  pass_pbp_data$defteam_timeouts_remaining <- data.table::fifelse(
    Turnover_Ind == 1,
    posteam_timeouts_pre,
    defeam_timeouts_pre
  )

  preds <- get_preds(pass_pbp_data)

  # Value of the possession that follows a score (see get_kickoff_values()),
  # reused from add_ep_variables() instead of predicting again. The air state
  # is 5.7 seconds later, which only matters when the value changes quickly
  # at the end of a half, so those few rows are recomputed at the air clock.
  # The values are the same for both teams, so it does not matter that the
  # timeouts and home are from the other team's view after a turnover on downs.
  # (also the rows where a safety is likely or can be the end of the play, which
  # need the exact free kick value)
  snap_values <- list(
    v = pass_pbp_data$ep_v,
    w = pass_pbp_data$ep_w,
    k_td_own = pass_pbp_data$ep_k_td_own,
    k_td_opp = pass_pbp_data$ep_k_td_opp,
    two_td_own = pass_pbp_data$ep_two_td_own,
    two_td_opp = pass_pbp_data$ep_two_td_opp,
    c_td_own = pass_pbp_data$ep_c_td_own,
    c_fg_own = pass_pbp_data$ep_c_fg_own,
    c_saf_own = pass_pbp_data$ep_c_saf_own,
    c_td_opp = pass_pbp_data$ep_c_td_opp,
    c_fg_opp = pass_pbp_data$ep_c_fg_opp,
    c_saf_opp = pass_pbp_data$ep_c_saf_opp
  )
  late_i <- which(
    pass_pbp_data$half_seconds_remaining <= 180 |
      ep_exact_w(preds$Safety, preds$Opp_Safety) |
      (old_yrdline100 - air_yards > 99) %in% TRUE
  )
  if (length(late_i) > 0) {
    late_values <- get_kickoff_values(
      lapply(pass_pbp_data, `[`, late_i),
      exact_w = TRUE
    )
    snap_values$v[late_i] <- late_values$v
    snap_values$w[late_i] <- late_values$w
  }
  snap_sv <- score_values(snap_values, pass_pbp_data$season)

  # After a turnover on downs the other team has the ball in the air state
  turnover <- Turnover_Ind %in% 1
  swapped_sv <- swap_score_values(snap_sv)
  air_sv <- lapply(
    stats::setNames(nm = names(snap_sv)),
    function(nm) data.table::fifelse(turnover, swapped_sv[[nm]], snap_sv[[nm]])
  )

  airEP <- ep_from_probs(
    preds$Touchdown,
    preds$Opp_Touchdown,
    preds$Field_Goal,
    preds$Opp_Field_Goal,
    preds$Safety,
    preds$Opp_Safety,
    air_sv
  )
  airEP[which(pass_pbp_data$half_seconds_remaining <= 0)] <- 0

  ep <- pass_pbp_data$ep
  airEPA <- data.table::fifelse(
    old_yrdline100 - air_yards <= 0,
    snap_sv$td_own - ep,
    data.table::fifelse(
      old_yrdline100 - air_yards > 99,
      snap_sv$saf_opp - ep,
      data.table::fifelse(Turnover_Ind == 1, (-1 * airEP) - ep, airEP - ep)
    )
  )
  airEPA <- data.table::fifelse(
    pass_pbp_data$two_point_attempt == 1,
    NA,
    airEPA
  )
  yacEPA <- pass_pbp_data$epa - airEPA
  no_yac <- pass_pbp_data$penalty == 0 &
    pass_pbp_data$yards_after_catch == 0 &
    pass_pbp_data$complete_pass == 1
  yacEPA <- data.table::fifelse(no_yac, 0, yacEPA)
  airEPA <- data.table::fifelse(no_yac, pass_pbp_data$epa, airEPA)

  air_epa <- rep(NA_real_, nrow(pbp_data))
  yac_epa <- rep(NA_real_, nrow(pbp_data))
  air_epa[pass_plays_i] <- airEPA
  yac_epa[pass_plays_i] <- yacEPA
  data.table::set(
    pbp_data,
    j = c("air_epa", "yac_epa"),
    value = list(air_epa, yac_epa)
  )

  add_air_yac_totals(pbp_data, "epa")
}

# Completion and raw air and yac values per team and their cumulative totals
# within each game. type is "epa" or "wpa".
add_air_yac_totals <- function(pbp, type) {
  air <- pbp[[paste0("air_", type)]]
  yac <- pbp[[paste0("yac_", type)]]
  comp_air <- data.table::fifelse(pbp$complete_pass == 1, air, 0)
  comp_yac <- data.table::fifelse(pbp$complete_pass == 1, yac, 0)
  home <- pbp$posteam == pbp$home_team
  away <- pbp$posteam == pbp$away_team
  # the value from the point of view of a team, 0 if missing
  team_value <- function(is_team, x) {
    data.table::nafill(data.table::fifelse(is_team, x, -x), fill = 0)
  }
  team_cols <- list(
    home_team_comp_air = team_value(home, comp_air),
    away_team_comp_air = team_value(away, comp_air),
    home_team_comp_yac = team_value(home, comp_yac),
    away_team_comp_yac = team_value(away, comp_yac),
    home_team_raw_air = team_value(home, air),
    away_team_raw_air = team_value(away, air),
    home_team_raw_yac = team_value(home, yac),
    away_team_raw_yac = team_value(away, yac)
  )
  names(team_cols) <- paste0(names(team_cols), "_", type)
  data.table::set(
    pbp,
    j = paste0(c("comp_air_", "comp_yac_"), type),
    value = list(comp_air, comp_yac)
  )
  data.table::set(pbp, j = names(team_cols), value = team_cols)
  total_cols <- sub("_team_", "_", paste0("total_", names(team_cols)))
  pbp[,
    (total_cols) := lapply(.SD, cumsum),
    by = "game_id",
    .SDcols = names(team_cols)
  ]
  pbp
}


#################################################################
# air and YAC WP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_wp_variables <- function(pbp_data) {
  pass_plays_i <- which(
    !is.na(pbp_data$air_yards) & pbp_data$play_type == "pass"
  )
  # the pass plays as list of columns, the state after the air yards replaces
  # the state before the play
  pass_pbp_data <- subset_columns(
    pbp_data,
    pass_plays_i,
    c(
      wp_model_features,
      "air_yards",
      "wp",
      "wpa",
      "two_point_attempt",
      "qtr",
      "penalty",
      "yards_after_catch",
      "complete_pass",
      # for add_ot_air_wp()
      "game_id",
      "season"
    )
  )
  posteam_timeouts_pre <- pass_pbp_data$posteam_timeouts_remaining
  defeam_timeouts_pre <- pass_pbp_data$defteam_timeouts_remaining
  # time of the pass (overtime below uses these rows with the reduced clock and
  # moves the ball itself)
  pass_pbp_data$half_seconds_remaining <- pass_pbp_data$half_seconds_remaining -
    5.704673
  pass_pbp_data$game_seconds_remaining <- pass_pbp_data$game_seconds_remaining -
    5.704673
  Turnover_Ind <- data.table::fifelse(
    pass_pbp_data$down == 4 & pass_pbp_data$air_yards < pass_pbp_data$ydstogo,
    1,
    0
  )
  turnover <- Turnover_Ind == 1
  qtr <- pass_pbp_data$qtr
  # the end of the first half: the value with no time left in it (qtr is the
  # quarter of the rows of x)
  air_clock <- function(x, qtr) {
    x$half_seconds_remaining <- pmax(x$half_seconds_remaining, 0)
    x$game_seconds_remaining <- data.table::fifelse(
      qtr <= 2,
      pmax(x$game_seconds_remaining, 1800),
      x$game_seconds_remaining
    )
    elapsed_share <- (3600 - pmax(x$game_seconds_remaining, 0)) / 3600
    x$Diff_Time_Ratio <- x$score_differential / (exp(-4 * elapsed_share))
    x
  }

  # The state where the ball is caught (or lands): the ball moves by the air
  # yards, which is a first down if it reaches the line to gain. On a 4th down
  # throw short of the line to gain the other team takes over, so the whole
  # state is seen from its side and the WP is flipped back below. The same as in
  # add_air_yac_ep_variables() and, for overtime, add_ot_air_wp().
  air_state <- pass_pbp_data
  first_down_air <- pass_pbp_data$air_yards >= pass_pbp_data$ydstogo
  new_yardline <- pass_pbp_data$yardline_100 - pass_pbp_data$air_yards
  # a throw into the end zone (or behind the own goal line) is valued at the 1
  # (or the 99)
  air_state$yardline_100 <- pmin(
    pmax(data.table::fifelse(turnover, 100 - new_yardline, new_yardline), 1),
    99
  )
  air_state$down <- data.table::fifelse(
    first_down_air | turnover,
    1,
    as.numeric(pass_pbp_data$down) + 1
  )
  air_state$ydstogo <- pmin(
    data.table::fifelse(
      first_down_air | turnover,
      10,
      pass_pbp_data$ydstogo - pass_pbp_data$air_yards
    ),
    air_state$yardline_100
  )
  air_state$score_differential <- data.table::fifelse(
    turnover,
    -pass_pbp_data$score_differential,
    pass_pbp_data$score_differential
  )
  air_state$home <- data.table::fifelse(
    turnover,
    1 - pass_pbp_data$home,
    pass_pbp_data$home
  )
  air_state$receive_2h_ko <- data.table::fifelse(
    turnover & qtr <= 2,
    1 - pass_pbp_data$receive_2h_ko,
    pass_pbp_data$receive_2h_ko
  )
  air_state$posteam_timeouts_remaining <- data.table::fifelse(
    turnover,
    defeam_timeouts_pre,
    posteam_timeouts_pre
  )
  air_state$defteam_timeouts_remaining <- data.table::fifelse(
    turnover,
    posteam_timeouts_pre,
    defeam_timeouts_pre
  )

  airWP <- get_preds_wp(air_clock(air_state, qtr))
  airWP <- data.table::fifelse(turnover, 1 - airWP, airWP)

  # A throw caught in the end zone is a touchdown. Value it like the PAT fix in
  # add_wp_variables(): after the try the other team has the ball at the
  # typical start after a kickoff.
  ez <- which(
    Turnover_Ind == 0 &
      pass_pbp_data$yardline_100 - pass_pbp_data$air_yards <= 0 &
      qtr <= 4
  )
  if (length(ez) > 0) {
    x <- lapply(pass_pbp_data, `[`, ez)
    n_ez <- length(ez)
    after_td <- function(pts) {
      s <- x
      s$score_differential <- -(x$score_differential + pts)
      s$home <- 1 - x$home
      s$receive_2h_ko <- data.table::fifelse(
        qtr[ez] <= 2,
        1 - x$receive_2h_ko,
        x$receive_2h_ko
      )
      s$yardline_100 <- wp_kickoff_start(x$season)
      s$down <- rep(1, n_ez)
      s$ydstogo <- rep(10, n_ez)
      s$posteam_timeouts_remaining <- defeam_timeouts_pre[ez]
      s$defteam_timeouts_remaining <- posteam_timeouts_pre[ez]
      get_preds_wp(air_clock(s, qtr[ez]))
    }
    airWP[ez] <- 1 - (0.94 * after_td(7) + 0.06 * after_td(6))
  }

  # If the game clock runs out in regulation the game is decided by the score
  # (a tie goes to overtime, valued as a coin flip)
  end_reg <- which(qtr == 4 & pass_pbp_data$game_seconds_remaining <= 0)
  if (length(end_reg) > 0) {
    result_value <- function(sd) {
      data.table::fcase(sd > 0, 1, sd < 0, 0, !is.na(sd), 0.5)
    }
    sd <- pass_pbp_data$score_differential[end_reg]
    airWP[end_reg] <- data.table::fifelse(
      end_reg %in% ez,
      # touchdown at the gun: the try is still played
      0.94 * result_value(sd + 7) + 0.06 * result_value(sd + 6),
      result_value(sd)
    )
  }

  airWPA <- airWP - pass_pbp_data$wp
  yacWPA <- pass_pbp_data$wpa - airWPA
  airWPA <- data.table::fifelse(
    pass_pbp_data$two_point_attempt == 1,
    NA,
    airWPA
  )
  yacWPA <- data.table::fifelse(
    pass_pbp_data$two_point_attempt == 1,
    NA,
    yacWPA
  )

  ot_pass_i <- which(pass_pbp_data$qtr > 4)
  if (length(ot_pass_i) > 0) {
    ot_air <- add_ot_air_wp(
      data.table::setDT(pass_pbp_data),
      ot_possession_number(pbp_data)[pass_plays_i]
    )
    airWPA[ot_pass_i] <- ot_air[ot_pass_i] - pass_pbp_data$wp[ot_pass_i]
    yacWPA[ot_pass_i] <- pass_pbp_data$wpa[ot_pass_i] - airWPA[ot_pass_i]
    two_pt <- ot_pass_i[which(pass_pbp_data$two_point_attempt[ot_pass_i] == 1)]
    airWPA[two_pt] <- NA
    yacWPA[two_pt] <- NA
  }

  no_yac <- pass_pbp_data$penalty == 0 &
    pass_pbp_data$yards_after_catch == 0 &
    pass_pbp_data$complete_pass == 1
  yacWPA <- data.table::fifelse(no_yac, 0, yacWPA)
  airWPA <- data.table::fifelse(no_yac, pass_pbp_data$wpa, airWPA)

  air_wpa <- rep(NA_real_, nrow(pbp_data))
  yac_wpa <- rep(NA_real_, nrow(pbp_data))
  air_wpa[pass_plays_i] <- airWPA
  yac_wpa[pass_plays_i] <- yacWPA
  data.table::set(
    pbp_data,
    j = c("air_wpa", "yac_wpa"),
    value = list(air_wpa, yac_wpa)
  )

  add_air_yac_totals(pbp_data, "wpa")
}
