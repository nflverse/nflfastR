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
  if (nrow(pbp |> dplyr::filter(!is.na(.data$air_yards))) == 0) {
    out <- pbp |>
      dplyr::mutate(
        air_epa = NA_real_,
        yac_epa = NA_real_,
        comp_air_epa = NA_real_,
        comp_yac_epa = NA_real_,
        home_team_comp_air_epa = NA_real_,
        away_team_comp_air_epa = NA_real_,
        home_team_comp_yac_epa = NA_real_,
        away_team_comp_yac_epa = NA_real_,
        total_home_comp_air_epa = NA_real_,
        total_away_comp_air_epa = NA_real_,
        total_home_comp_yac_epa = NA_real_,
        total_away_comp_yac_epa = NA_real_,
        home_team_raw_air_epa = NA_real_,
        away_team_raw_air_epa = NA_real_,
        home_team_raw_yac_epa = NA_real_,
        away_team_raw_yac_epa = NA_real_,
        total_home_raw_air_epa = NA_real_,
        total_away_raw_air_epa = NA_real_,
        total_home_raw_yac_epa = NA_real_,
        total_away_raw_yac_epa = NA_real_
      )
    user_message(
      "No non-NA air_yards detected. air_yac_ep variables set to NA",
      "info"
    )
  } else {
    out <- pbp |> add_air_yac_ep_variables()
    user_message("added air_yac_ep variables", "done")
  }
  return(out)
}

add_wp <- function(pbp) {
  out <- pbp |> add_wp_variables()
  user_message("added wp variables", "done")
  return(out)
}

add_air_yac_wp <- function(pbp) {
  if (nrow(pbp |> dplyr::filter(!is.na(.data$air_yards))) == 0) {
    out <- pbp |>
      dplyr::mutate(
        air_wpa = NA_real_,
        yac_wpa = NA_real_,
        comp_air_wpa = NA_real_,
        comp_yac_wpa = NA_real_,
        home_team_comp_air_wpa = NA_real_,
        away_team_comp_air_wpa = NA_real_,
        home_team_comp_yac_wpa = NA_real_,
        away_team_comp_yac_wpa = NA_real_,
        total_home_comp_air_wpa = NA_real_,
        total_away_comp_air_wpa = NA_real_,
        total_home_comp_yac_wpa = NA_real_,
        total_away_comp_yac_wpa = NA_real_,
        home_team_raw_air_wpa = NA_real_,
        away_team_raw_air_wpa = NA_real_,
        home_team_raw_yac_wpa = NA_real_,
        away_team_raw_yac_wpa = NA_real_,
        total_home_raw_air_wpa = NA_real_,
        total_away_raw_air_wpa = NA_real_,
        total_home_raw_yac_wpa = NA_real_,
        total_away_raw_yac_wpa = NA_real_
      )
    user_message(
      "No non-NA air_yards detected. air_yac_wp variables set to NA",
      "info"
    )
  } else {
    out <- pbp |> add_air_yac_wp_variables()
    user_message("added air_yac_wp variables", "done")
  }
  return(out)
}

#get the model inputs for the EP model (neutral site games are treated as
#away games)
ep_model_input <- function(pbp) {
  if ("location" %in% names(pbp)) {
    pbp <- pbp |>
      dplyr::mutate(
        home = dplyr::if_else(.data$location == "Neutral", 0, .data$home)
      )
  }
  pbp |> ep_model_select()
}

#get predictions for a set of pbp data
#for predict stage of EP
get_preds <- function(pbp) {
  predict_ep(ep_model_input(pbp))
}

#predict from model inputs created by ep_model_input()
predict_ep <- function(model_input) {
  preds <- stats::predict(
    load_model("ep"),
    model_input |> as.matrix()
  )

  # xgboost v3 returns a matrix of predictions instead of a vector as returned
  # by xgboost v1.
  if (is.vector(preds)) {
    preds <- preds |>
      matrix(ncol = 7, byrow = TRUE) |>
      as.data.frame()
  } else if (is.matrix(preds)) {
    preds <- as.data.frame(preds)
  }

  colnames(preds) <- c(
    "Touchdown",
    "Opp_Touchdown",
    "Field_Goal",
    "Opp_Field_Goal",
    "Safety",
    "Opp_Safety",
    "No_Score"
  )

  return(preds)
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
  dplyr::case_when(
    is.na(season) ~ NA_real_,
    season <= 2014 ~ 0.99,
    season <= 2022 ~ 0.94,
    TRUE ~ 0.96
  )
}

# Probability of making a two point conversion: 0.49 for tries from the 2 yard
# line since 2004, leaving out kneel downs and botched one point tries
two_point_make_prob <- 0.49

# Where the receiving team's drive starts after a kickoff touchback, as
# yardline_100, by season. Changes with kickoff rules:
#   <= 2015: own 20, 2016-2023: own 25, 2024: own 30, 2025+: own 35
kickoff_touchback_yardline <- function(season) {
  dplyr::case_when(
    is.na(season) ~ NA_real_,
    season < 2016 ~ 80,
    season < 2024 ~ 75,
    season == 2024 ~ 70,
    TRUE ~ 65
  )
}

# Where the scoring team's drive starts after a safety free kick, as
# yardline_100 for the receiving (scoring) team. This is a data-informed guess:
# the first scrimmage play after the free kick was on average at yardline_100
# 61.4 (median 63, n = 256 safeties, 2009-2025, and about the same before and
# after 2016), so own 39 is used
safety_free_kick_yardline <- 61

# Expected points per yard of field position between a kickoff touchback and
# the safety free kick spot. The EP model gives about 0.05 points per yard
# between those two states, so the value of a free kick is the kickoff value
# plus this slope times the yards. This saves model predictions on the rows
# where a safety is unlikely, which it can get wrong by up to 0.24 points
# (older seasons, where the yards are the most), so the rows where a safety is
# likely or is the play get the value from the model (see get_kickoff_values()).
safety_free_kick_ep_per_yard <- 0.05

# The value of a scoring event is not only its points: the other team gets the
# ball afterwards, so a touchdown is worth 6 + xp - V where V is the expected
# points of the opponent's kickoff possession at the current clock. A safety
# gives the scoring team the ball on a free kick, so it is worth 2 + W.
#
# V is the EP of 1st and 10 at the kickoff touchback spot and W is the EP of
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
# `kickoff_preds` are the predictions for the possession team at the touchback
# spot if the caller already has them, so the only new prediction is the other
# team's kickoff state. The free kick (safety) value is the kickoff value
# plus the EP of the extra field position (see safety_free_kick_ep_per_yard),
# except for the rows flagged in `exact_w` (where a safety is likely or is the
# play), where it comes from the model (two more predictions on those rows
# only). Only rows that can have a kickoff after a score are predicted, and the
# model inputs are prepared once.
#
# Returns V and W as columns `v` and `w`
get_kickoff_values <- function(pbp_data, kickoff_preds = NULL, exact_w = FALSE) {
  n <- nrow(pbp_data)
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

  # only keep what the model needs (and what neutral site games need)
  model_cols <- names(ep_model_select(pbp_data[0, ]))
  keep_cols <- intersect(c(model_cols, "location"), names(pbp_data))
  base <- as.data.frame(pbp_data[can_kick, keep_cols])
  m <- nrow(base)
  neutral <- if ("location" %in% names(base)) {
    base$location == "Neutral"
  } else {
    rep(FALSE, m)
  }

  # predictions for the kickoff state of the possession team (flip = FALSE) or
  # of the other team (flip = TRUE), for the rows `rows` of `base`
  state_preds <- function(yardline_100, flip, rows = seq_len(m)) {
    d <- base[rows, ]
    home <- if (flip) 1 - base$home[rows] else base$home[rows]
    d$home <- dplyr::if_else(neutral[rows], 0, home)
    if (flip) {
      d$posteam_timeouts_remaining <- base$defteam_timeouts_remaining[rows]
      d$defteam_timeouts_remaining <- base$posteam_timeouts_remaining[rows]
    }
    d$yardline_100 <- yardline_100
    d$down1 <- rep(1, length(rows))
    d$down2 <- rep(0, length(rows))
    d$down3 <- rep(0, length(rows))
    d$down4 <- rep(0, length(rows))
    d$ydstogo <- rep(10, length(rows))
    predict_ep(ep_model_select(d))
  }

  ko_yardline <- kickoff_touchback_yardline(pbp_data$season[can_kick])
  k_own <- if (is.null(kickoff_preds)) {
    state_preds(ko_yardline, flip = FALSE)
  } else {
    kickoff_preds[can_kick, ]
  }
  k_opp <- state_preds(ko_yardline, flip = TRUE)

  # EP of a state = a + b V + d W on the average of the two teams' states
  xp <- conversion_value(pbp_data$season[can_kick])
  coefs <- function(p_own, p_opp, xp) {
    p <- (p_own + p_opp) / 2
    td <- p$Touchdown - p$Opp_Touchdown
    fg <- p$Field_Goal - p$Opp_Field_Goal
    sf <- p$Safety - p$Opp_Safety
    list(a = td * (6 + xp) + 3 * fg + 2 * sf, b = -(td + fg), d = sf)
  }
  k <- coefs(k_own, k_opp, xp)

  # W = V + g, so that V = (a + d g) / (1 - b - d)
  g <- safety_free_kick_ep_per_yard * (ko_yardline - safety_free_kick_yardline)
  v <- (k$a + k$d * g) / (1 - k$b - k$d)
  w <- v + g

  # The difference between W and V depends on the season and the clock, and the
  # shortcut can be off by 0.24 points in the early 2000s, so for the rows where
  # W matters (`exact_w`: a safety is likely or is the play) W comes from the
  # model: V and W are the solution of the two linear equations for the
  # kickoff state and the free kick state
  late <- which(exact_w[can_kick])
  if (length(late) > 0) {
    fk_yardline <- rep(safety_free_kick_yardline, length(late))
    f <- coefs(
      state_preds(fk_yardline, flip = FALSE, rows = late),
      state_preds(fk_yardline, flip = TRUE, rows = late),
      xp[late]
    )
    kl <- lapply(k, function(x) x[late])
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
  post <- week > dplyr::if_else(season >= 2021, 18L, 17L)
  post[is.na(post)] <- FALSE
  dplyr::case_when(
    post & season >= 2022 ~ "BOTH",
    !post & season >= 2025 ~ "BOTH",
    post & season >= 2010 ~ "MSD",
    !post & season >= 2012 ~ "MSD",
    TRUE ~ "SD"
  )
}

# Does a kickoff (or free kick) follow a score? In regulation always. In
# overtime it depends on the rule, the number of the possession in which the
# score happens (1, 2, 3 for the third and later ones) and the score
# differential (for the scoring team, before the score).
# pts is the value of the score: 7 touchdown, 3 field goal, 2 safety
ep_ot_continues <- function(rule, possession, diff, pts) {
  dplyr::case_when(
    is.na(possession) | is.na(diff) ~ 1,
    rule == "SD" ~ 0,
    possession >= 3 ~ 0,
    # a safety ends the game on the first possession, a touchdown does only
    # if the other team does not get a possession
    possession == 1 & pts == 2 ~ 0,
    possession == 1 & pts == 7 & rule == "MSD" ~ 0,
    possession == 1 ~ 1,
    # second possession: the game continues only if the score ties it. A score
    # that wins it ends the game, and so does one that still trails because
    # both teams have had their possession
    diff + pts == 0 ~ 1,
    TRUE ~ 0
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
  n <- nrow(pbp_data)
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
  if (!all(needed %in% names(pbp_data))) {
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
  conv_own <- dplyr::if_else(kv$two_td_own, two, conv)
  conv_opp <- dplyr::if_else(kv$two_td_opp, two, conv)
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
  preds <- stats::predict(load_model("wp"), as.matrix(pbp |> wp_model_select()))

  return(preds)
}

#get predictions for a set of pbp data
#for predict stage
get_preds_wp_spread <- function(pbp) {
  preds <- stats::predict(
    load_model("wp_spread"),
    as.matrix(pbp |> wp_spread_model_select())
  )

  return(preds)
}


#get the columns needed for ep predictions
#making sure they're in the right order
ep_model_select <- function(pbp) {
  pbp <- pbp |>
    dplyr::select(
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
      "defteam_timeouts_remaining",
    )

  return(pbp)
}

#get the columns needed for wp predictions
#making sure they're in the right order
wp_model_select <- function(pbp) {
  pbp <- pbp |>
    dplyr::select(
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

  return(pbp)
}

#get the columns needed for wp predictions
#making sure they're in the right order
wp_spread_model_select <- function(pbp) {
  pbp <- pbp |>
    dplyr::select(
      "receive_2h_ko",
      "spread_time",
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

  return(pbp)
}
prepare_wp_data <- function(pbp) {
  if (any(is.na(pbp$spread_line))) {
    broken_games <- pbp |>
      dplyr::filter(is.na(.data$spread_line)) |>
      dplyr::pull(.data$game_id) |>
      unique() |>
      sort()
    cli::cli_alert_danger(
      "The following game{?s} {?is/are} missing valid spread lines: {.val {broken_games}}."
    )
    cli::cli_alert_warning(
      "nflfastR will manually set the spread for the home team to {.val 1.5} points!"
    )
    cli::cli_alert_warning(
      "If you see this, please reach out to the package maintainers {.url https://github.com/nflverse/nflfastR/issues}"
    )
    pbp$spread_line[is.na(pbp$spread_line)] <- 1.5
  }

  pbp <- pbp |>
    dplyr::group_by(.data$game_id) |>
    dplyr::mutate(
      receive_2h_ko = dplyr::if_else(
        .data$qtr <= 2 &
          .data$posteam == dplyr::first(stats::na.omit(.data$defteam)),
        1,
        0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      posteam_spread = dplyr::if_else(
        .data$home == 1,
        .data$spread_line,
        -1 * .data$spread_line
      ),
      elapsed_share = (3600 - .data$game_seconds_remaining) / 3600,
      spread_time = .data$posteam_spread * exp(-4 * .data$elapsed_share),
      Diff_Time_Ratio = .data$score_differential /
        (exp(-4 * .data$elapsed_share))
    )

  return(pbp)
}


#add ep variables
#All of these are heavily borrowed from nflscrapR (Maksim Horowitz, Ronald Yurko, and Samuel Ventura)
add_ep_variables <- function(pbp_data) {
  #testing
  #pbp_data <- g

  #this function is below
  base_ep_preds <- get_preds(pbp_data)

  # ----------------------------------------------------------------------------
  # ---- special case: deal with FG attempts
  # Now make another dataset that to get the EP probabilities from a missed FG:
  missed_fg_data <- pbp_data
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
  missed_fg_timeouts <- missed_fg_data$posteam_timeouts_remaining
  missed_fg_data$posteam_timeouts_remaining <-
    missed_fg_data$defteam_timeouts_remaining
  missed_fg_data$defteam_timeouts_remaining <- missed_fg_timeouts
  # Now first down:
  missed_fg_data$down1 <- rep(1, nrow(pbp_data))
  missed_fg_data$down2 <- rep(0, nrow(pbp_data))
  missed_fg_data$down3 <- rep(0, nrow(pbp_data))
  missed_fg_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  missed_fg_data$ydstogo <- rep(10, nrow(pbp_data))

  # Get the new predicted probabilites:
  if (nrow(missed_fg_data) > 1) {
    missed_fg_ep_preds <- get_preds(missed_fg_data)
  } else {
    missed_fg_ep_preds <- get_preds(missed_fg_data)
  }

  # Find the rows where TimeSecs_Remaining became 0 or negative and make all the probs equal to 0:
  end_game_i <- which(missed_fg_data$half_seconds_remaining <= 0)
  missed_fg_ep_preds[end_game_i, ] <- rep(0, ncol(missed_fg_ep_preds))

  # if the half ends, no one scored
  missed_fg_ep_preds[end_game_i, "No_Score"] <- 1

  # Get the probability of making the field goal:
  make_fg_prob <- as.numeric(mgcv::predict.bam(
    fastrmodels::fg_model,
    newdata = pbp_data,
    type = "response"
  ))

  # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
  missed_fg_ep_preds <- missed_fg_ep_preds * (1 - make_fg_prob)
  # Find the FG attempts:
  fg_attempt_i <- which(pbp_data$play_type == "field_goal")

  # Now update the probabilities for the FG attempts (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
  base_ep_preds[fg_attempt_i, "Field_Goal"] <- make_fg_prob[fg_attempt_i] +
    missed_fg_ep_preds[fg_attempt_i, "Opp_Field_Goal"]
  # Update the other columns based on the opposite possession:
  base_ep_preds[fg_attempt_i, "Touchdown"] <- missed_fg_ep_preds[
    fg_attempt_i,
    "Opp_Touchdown"
  ]
  base_ep_preds[fg_attempt_i, "Opp_Field_Goal"] <- missed_fg_ep_preds[
    fg_attempt_i,
    "Field_Goal"
  ]
  base_ep_preds[fg_attempt_i, "Opp_Touchdown"] <- missed_fg_ep_preds[
    fg_attempt_i,
    "Touchdown"
  ]
  base_ep_preds[fg_attempt_i, "Safety"] <- missed_fg_ep_preds[
    fg_attempt_i,
    "Opp_Safety"
  ]
  base_ep_preds[fg_attempt_i, "Opp_Safety"] <- missed_fg_ep_preds[
    fg_attempt_i,
    "Safety"
  ]
  base_ep_preds[fg_attempt_i, "No_Score"] <- missed_fg_ep_preds[
    fg_attempt_i,
    "No_Score"
  ]

  # ----------------------------------------------------------------------------------
  # ---- special case: deal with kickoffs
  # Calculate the EP for receiving a touchback (from the point of view for recieving team)
  # and update the columns for Kickoff plays:
  kickoff_data <- pbp_data

  # Change the yard line to the touchback spot, which depends on the kickoff
  # rules in the season (see kickoff_touchback_yardline())
  kickoff_data$yardline_100 <- kickoff_touchback_yardline(kickoff_data$season)
  # Now first down:
  kickoff_data$down1 <- rep(1, nrow(pbp_data))
  kickoff_data$down2 <- rep(0, nrow(pbp_data))
  kickoff_data$down3 <- rep(0, nrow(pbp_data))
  kickoff_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10, nrow(pbp_data))

  # Get the new predicted probabilites:
  kickoff_preds <- get_preds(kickoff_data)

  # Find the kickoffs:
  kickoff_i <- which(
    pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1
  )

  # Now update the probabilities:
  base_ep_preds[kickoff_i, "Field_Goal"] <- kickoff_preds[
    kickoff_i,
    "Field_Goal"
  ]
  base_ep_preds[kickoff_i, "Touchdown"] <- kickoff_preds[kickoff_i, "Touchdown"]
  base_ep_preds[kickoff_i, "Opp_Field_Goal"] <- kickoff_preds[
    kickoff_i,
    "Opp_Field_Goal"
  ]
  base_ep_preds[kickoff_i, "Opp_Touchdown"] <- kickoff_preds[
    kickoff_i,
    "Opp_Touchdown"
  ]
  base_ep_preds[kickoff_i, "Safety"] <- kickoff_preds[kickoff_i, "Safety"]
  base_ep_preds[kickoff_i, "Opp_Safety"] <- kickoff_preds[
    kickoff_i,
    "Opp_Safety"
  ]
  base_ep_preds[kickoff_i, "No_Score"] <- kickoff_preds[kickoff_i, "No_Score"]

  # The free kick after a safety starts the scoring team's drive further up the
  # field than a kickoff touchback does. The safety was valued with this spot
  # (see safety_free_kick_yardline), so the free kick has to start from it too
  game_row <- seq_len(nrow(pbp_data))
  is_safety <- pbp_data$safety %in% 1
  after_safety <- rep(FALSE, nrow(pbp_data))
  for (back in 1:3) {
    prev <- game_row - back
    has_prev <- prev >= 1
    after_safety[has_prev] <- after_safety[has_prev] |
      (is_safety[prev[has_prev]] &
        pbp_data$game_id[prev[has_prev]] == pbp_data$game_id[has_prev])
  }
  free_kick_i <- intersect(kickoff_i, which(after_safety))
  if (length(free_kick_i) > 0) {
    free_kick_data <- kickoff_data[free_kick_i, ]
    free_kick_data$yardline_100 <- safety_free_kick_yardline
    free_kick_preds <- get_preds(free_kick_data)
    base_ep_preds[free_kick_i, ] <- free_kick_preds[, names(base_ep_preds)]
  }

  # A kickoff from midfield (after a penalty) that is a touchback is spotted at
  # the receiving team's 20 since 2026, not at the usual touchback spot. This is
  # only known on the actual kickoff, and not for a kickoff in the future
  midfield_i <- intersect(
    kickoff_i,
    which(pbp_data$season >= 2026 & pbp_data$yardline_100 == 50)
  )
  if (length(midfield_i) > 0) {
    midfield_data <- kickoff_data[midfield_i, ]
    midfield_data$yardline_100 <- 80
    midfield_preds <- get_preds(midfield_data)
    base_ep_preds[midfield_i, ] <- midfield_preds[, names(base_ep_preds)]
  }

  # ----------------------------------------------------------------------------------
  # Insert probabilities of 0 for everything but No_Score for QB Kneels that
  # occur on the possession team's side of the field:
  # Find these QB Kneels:
  qb_kneels_i <- which(
    pbp_data$play_type == "qb_kneel" & pbp_data$yardline_100 > 50
  )

  # Now update the probabilities:
  base_ep_preds[qb_kneels_i, "Field_Goal"] <- 0
  base_ep_preds[qb_kneels_i, "Touchdown"] <- 0
  base_ep_preds[qb_kneels_i, "Opp_Field_Goal"] <- 0
  base_ep_preds[qb_kneels_i, "Opp_Touchdown"] <- 0
  base_ep_preds[qb_kneels_i, "Safety"] <- 0
  base_ep_preds[qb_kneels_i, "Opp_Safety"] <- 0
  base_ep_preds[qb_kneels_i, "No_Score"] <- 1

  # ----------------------------------------------------------------------------------
  # Create two new columns, ExPoint_Prob and TwoPoint_Prob, for the PAT events:
  base_ep_preds$ExPoint_Prob <- 0
  base_ep_preds$TwoPoint_Prob <- 0

  # Find the indices for these types of plays:
  extrapoint_i <- which(
    (pbp_data$play_type == "extra_point" |
      pbp_data$play_type_nfl == "XP_KICK") &
      (is.na(pbp_data$play_type_nfl) | pbp_data$play_type_nfl != "PAT2")
  )
  twopoint_i <- which(pbp_data$two_point_attempt == 1)

  #new: special case for PAT or kickoff with penalty
  #for inserting NAs
  st_penalty_i_1 <- which(
    # pat: prior play was TD or PAT or Timeout and next play is PAT and this play isn't a td and it's not a regular down
    (pbp_data$touchdown == 0 &
      is.na(pbp_data$down) &
      (dplyr::lag(pbp_data$touchdown) == 1 |
        dplyr::lag(pbp_data$play_type_nfl) == "XP_KICK" |
        dplyr::lag(pbp_data$timeout) == 1) &
      (dplyr::lead(pbp_data$two_point_attempt) == 1 |
        dplyr::lead(pbp_data$extra_point_attempt) == 1 |
        dplyr::lead(pbp_data$play_type_nfl) == "XP_KICK")) |
      #kickoff: prior play was PAT and next play is kickoff, unless it is a
      #kickoff that ended in a safety (the next kickoff is the free kick after it)
      ((dplyr::lag(pbp_data$two_point_attempt) == 1 |
        dplyr::lag(pbp_data$extra_point_attempt) == 1) &
        dplyr::lead(pbp_data$kickoff_attempt == 1) &
        !(pbp_data$safety %in% 1))
  )

  st_penalty_i_2 <- which(
    is.na(dplyr::lead(pbp_data$down)) &
      # has a key term in desc
      (((stringr::str_detect(pbp_data$desc, 'Kick formation') &
        is.na(pbp_data$down) &
        pbp_data$play_type == 'no_play') |
        (stringr::str_detect(pbp_data$desc, 'Pass formation') &
          is.na(pbp_data$down) &
          pbp_data$play_type == 'no_play') |
        (stringr::str_detect(pbp_data$desc, 'kicks onside') &
          is.na(pbp_data$down) &
          pbp_data$play_type == 'no_play') |
        (stringr::str_detect(pbp_data$desc, 'Offside on Free Kick') &
          is.na(pbp_data$down) &
          pbp_data$play_type == 'no_play') |
        (stringr::str_detect(pbp_data$desc, 'TWO-POINT CONVERSION')) &
          # down is NA and play type no play and next play isn't a kickoff
          is.na(pbp_data$down) &
          pbp_data$play_type == 'no_play' &
          dplyr::lead(pbp_data$kickoff_attempt) == 0))
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
      !stringr::str_detect(pbp_data$desc, ' pass ') &
      !stringr::str_detect(pbp_data$desc, ' sacked ') &
      !stringr::str_detect(pbp_data$desc, ' scramble ') &
      !stringr::str_detect(pbp_data$desc, ' punts ') &
      !stringr::str_detect(pbp_data$desc, ' up the middle ') &
      !stringr::str_detect(pbp_data$desc, ' left end ') &
      !stringr::str_detect(pbp_data$desc, ' left guard ') &
      !stringr::str_detect(pbp_data$desc, ' left tackle ') &
      !stringr::str_detect(pbp_data$desc, ' right end ') &
      !stringr::str_detect(pbp_data$desc, ' right guard ') &
      !stringr::str_detect(pbp_data$desc, ' right tackle ')) |
      is.na(pbp_data$play_type)
  )

  # Now update the probabilities for missing and PATs:
  base_ep_preds$Field_Goal[c(
    missing_i,
    extrapoint_i,
    twopoint_i,
    st_penalty_i_1,
    st_penalty_i_2
  )] <- 0
  base_ep_preds$Touchdown[c(
    missing_i,
    extrapoint_i,
    twopoint_i,
    st_penalty_i_1,
    st_penalty_i_2
  )] <- 0
  base_ep_preds$Opp_Field_Goal[c(
    missing_i,
    extrapoint_i,
    twopoint_i,
    st_penalty_i_1,
    st_penalty_i_2
  )] <- 0
  base_ep_preds$Opp_Touchdown[c(
    missing_i,
    extrapoint_i,
    twopoint_i,
    st_penalty_i_1,
    st_penalty_i_2
  )] <- 0
  base_ep_preds$Safety[c(
    missing_i,
    extrapoint_i,
    twopoint_i,
    st_penalty_i_1,
    st_penalty_i_2
  )] <- 0
  base_ep_preds$Opp_Safety[c(
    missing_i,
    extrapoint_i,
    twopoint_i,
    st_penalty_i_1,
    st_penalty_i_2
  )] <- 0
  base_ep_preds$No_Score[c(
    missing_i,
    extrapoint_i,
    twopoint_i,
    st_penalty_i_1,
    st_penalty_i_2
  )] <- 0

  # Rename the events to all have _Prob at the end of them:
  base_ep_preds <- dplyr::rename(
    base_ep_preds,
    Field_Goal_Prob = "Field_Goal",
    Touchdown_Prob = "Touchdown",
    Opp_Field_Goal_Prob = "Opp_Field_Goal",
    Opp_Touchdown_Prob = "Opp_Touchdown",
    Safety_Prob = "Safety",
    Opp_Safety_Prob = "Opp_Safety",
    No_Score_Prob = "No_Score"
  )

  # Join them together:
  pbp_data <- cbind(pbp_data, base_ep_preds)

  # Value of the kickoff (or safety free kick) possession that follows a score
  # (the columns are kept for add_air_yac_ep_variables() and removed by
  # select_variables() at the end of the pipeline)
  # The free kick value (W) is exact where a safety is likely (more than 1% of
  # the next score) or is the play. Elsewhere the shortcut is off by at most
  # 0.24 points, which is less than 0.003 points in EP at that probability
  kick_values <- cbind(
    get_kickoff_values(
      pbp_data,
      kickoff_preds = kickoff_preds,
      exact_w = (pbp_data$Safety_Prob + pbp_data$Opp_Safety_Prob > 0.01) %in%
        TRUE |
        pbp_data$safety %in% 1
    ),
    ep_continue_factors(pbp_data)
  )
  score_vals <- score_values(kick_values, pbp_data$season)
  names(kick_values) <- paste0("ep_", names(kick_values))
  names(score_vals) <- paste0("ep_sv_", names(score_vals))
  pbp_data <- cbind(pbp_data, kick_values, score_vals)

  # Calculate the ExpPts. The value of a score is its points plus or minus the
  # value of the possession the other team gets afterwards (see
  # get_kickoff_values()). The extra point and two point rows are not adjusted
  # because the touchdown value already accounts for the kickoff.
  pbp_data_ep <- dplyr::mutate(
    pbp_data,
    ExpPts = (0 * .data$No_Score_Prob) +
      (.data$Touchdown_Prob * .data$ep_sv_td_own) +
      (.data$Opp_Touchdown_Prob * .data$ep_sv_td_opp) +
      (.data$Field_Goal_Prob * .data$ep_sv_fg_own) +
      (.data$Opp_Field_Goal_Prob * .data$ep_sv_fg_opp) +
      (.data$Safety_Prob * .data$ep_sv_saf_own) +
      (.data$Opp_Safety_Prob * .data$ep_sv_saf_opp) +
      (1 * .data$ExPoint_Prob) +
      (2 * .data$TwoPoint_Prob)
  )

  #just going to set these to NA bc we have no way of calculating EPA for them
  if (length(st_penalty_i_1) > 0) {
    pbp_data_ep$ExpPts[st_penalty_i_1] <- NA_real_
  }

  if (length(st_penalty_i_2) > 0) {
    pbp_data_ep$ExpPts[st_penalty_i_2] <- NA_real_
  }

  pbp_data_ep$ExpPts[missing_i] <- NA_real_

  #################################################################
  # Calculate EPA:

  ### Adding Expected Points Added (EPA) column

  # Create multiple types of EPA columns
  # for each of the possible cases,
  # grouping by GameID (will then just use
  # an ifelse statement to decide which one
  # to use as the final EPA):
  pbp_data_ep |>
    dplyr::group_by(.data$game_id) |>
    dplyr::mutate(
      # Now conditionally assign the EPA, first for possession team
      # touchdowns:
      ep = .data$ExpPts,
      tmp_posteam = .data$posteam
    ) |>
    tidyr::fill(
      .data$ep,
      .direction = "up"
    ) |>
    tidyr::fill(
      .data$tmp_posteam,
      .direction = "up"
    ) |>
    dplyr::mutate(
      # the conversion value in the touchdown's value, which is what the
      # conversion that follows is measured against
      ep_conversion = dplyr::if_else(
        .data$ep_two_try,
        2 * two_point_make_prob,
        conversion_value(.data$season)
      ),
      # a score on the last play of the game has no extra point and no kickoff
      # after it, so it is worth just its points
      ep_game_over = dplyr::lead(
        stringr::str_detect(tolower(.data$desc), "(end of game)|(end game)"),
        default = TRUE
      ) %in%
        TRUE,
      # get epa for non-scoring plays
      home_ep = dplyr::if_else(
        .data$tmp_posteam == .data$home_team,
        .data$ep,
        -.data$ep
      ),
      home_epa = dplyr::lead(.data$home_ep) - .data$home_ep,
      epa = dplyr::if_else(
        .data$tmp_posteam == .data$home_team,
        .data$home_epa,
        -.data$home_epa
      ),

      # td
      epa = dplyr::if_else(
        !is.na(.data$td_team),
        dplyr::if_else(
          .data$td_team == .data$posteam,
          dplyr::if_else(.data$ep_game_over, 6, .data$ep_sv_td_own) - .data$ep,
          dplyr::if_else(.data$ep_game_over, -6, .data$ep_sv_td_opp) - .data$ep
        ),
        .data$epa
      ),
      # Offense field goal:
      epa = dplyr::if_else(
        is.na(.data$td_team) & .data$field_goal_made == 1,
        dplyr::if_else(.data$ep_game_over, 3, .data$ep_sv_fg_own) - .data$ep,
        .data$epa,
        missing = .data$epa
      ),
      # Conversions (extra point and two point tries): the net points minus the
      # conversion value in the touchdown's value, so a touchdown and its
      # conversion add up to 6 + the conversion's points - V whichever action was
      # taken (this includes the choice between a kick and a two point try, the
      # difference between that action's expected points and the average)
      # Offense extra-point:
      epa = dplyr::if_else(
        is.na(.data$td_team) &
          .data$field_goal_made == 0 &
          .data$extra_point_good == 1,
        1 - .data$ep_conversion,
        .data$epa,
        missing = .data$epa
      ),
      # Offense two-point conversion:
      epa = dplyr::if_else(
        is.na(.data$td_team) &
          .data$field_goal_made == 0 &
          .data$extra_point_good == 0 &
          (.data$two_point_rush_good == 1 |
            .data$two_point_pass_good == 1 |
            .data$two_point_pass_reception_good == 1),
        2 - .data$ep_conversion,
        .data$epa,
        missing = .data$epa
      ),
      # Failed PAT (both 1 and 2):
      epa = dplyr::if_else(
        is.na(.data$td_team) &
          .data$field_goal_made == 0 &
          .data$extra_point_good == 0 &
          ((.data$extra_point_failed == 1 |
            .data$extra_point_blocked == 1 |
            .data$extra_point_aborted == 1) |
            (.data$two_point_rush_failed == 1 |
              .data$two_point_pass_failed == 1 |
              .data$two_point_pass_reception_failed == 1)),
        0 - .data$ep_conversion,
        .data$epa,
        missing = .data$epa
      ),
      # Opponent scores defensive 2 point:
      epa = dplyr::if_else(
        .data$defensive_two_point_conv == 1,
        -2 - .data$ep_conversion,
        .data$epa,
        missing = .data$epa
      ),
      # Safety:
      epa = dplyr::case_when(
        !is.na(.data$safety_team) & .data$safety_team == .data$posteam ~
          dplyr::if_else(.data$ep_game_over, 2, .data$ep_sv_saf_own) - .data$ep,
        !is.na(.data$safety_team) & .data$safety_team == .data$defteam ~
          dplyr::if_else(.data$ep_game_over, -2, .data$ep_sv_saf_opp) -
          .data$ep,
        TRUE ~ .data$epa
      )
    ) |>
    # Now rename each of the expected points columns to match the style of
    # the updated code:
    dplyr::rename(
      no_score_prob = "No_Score_Prob",
      opp_fg_prob = "Opp_Field_Goal_Prob",
      opp_safety_prob = "Opp_Safety_Prob",
      opp_td_prob = "Opp_Touchdown_Prob",
      fg_prob = "Field_Goal_Prob",
      safety_prob = "Safety_Prob",
      td_prob = "Touchdown_Prob",
      extra_point_prob = "ExPoint_Prob",
      two_point_conversion_prob = "TwoPoint_Prob"
    ) |>
    # Create columns with cumulative epa totals for both teams:
    dplyr::mutate(
      # helper for end of game
      end_game = ifelse(
        stringr::str_detect(tolower(.data$desc), "(end of game)|(end game)"),
        1,
        0
      ),

      # Change epa for plays occurring at end of half with no scoring
      # plays to be just the difference between 0 and starting ep:
      epa = dplyr::if_else(
        ((.data$qtr == 2 &
          (dplyr::lead(.data$qtr) == 3 |
            dplyr::lead(.data$desc) == "END QUARTER 2")) |
          (.data$qtr == 4 &
            (dplyr::lead(.data$qtr) == 5 |
              dplyr::lead(.data$desc) == "END QUARTER 4" |
              dplyr::lead(.data$end_game) == 1))) &
          .data$sp == 0 &
          !is.na(.data$play_type),
        0 - .data$ep,
        .data$epa
      ),
      # last play of OT
      epa = dplyr::if_else(
        .data$qtr > 4 & dplyr::lead(.data$end_game) == 1 & .data$sp == 0,
        0 - .data$ep,
        .data$epa
      ),
      epa = dplyr::if_else(.data$desc == "END QUARTER 2", NA_real_, .data$epa),
      epa = dplyr::if_else(.data$end_game == 1, NA_real_, .data$epa),
      ep = dplyr::if_else(.data$desc == "END QUARTER 2", NA_real_, .data$ep),
      ep = dplyr::if_else(.data$end_game == 1, NA_real_, .data$ep),
      home_team_epa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$epa,
        -.data$epa
      ),
      away_team_epa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$epa,
        -.data$epa
      ),
      home_team_epa = dplyr::if_else(
        is.na(.data$home_team_epa),
        0,
        .data$home_team_epa
      ),
      away_team_epa = dplyr::if_else(
        is.na(.data$away_team_epa),
        0,
        .data$away_team_epa
      ),
      total_home_epa = cumsum(.data$home_team_epa),
      total_away_epa = cumsum(.data$away_team_epa),
      # Same thing but separating passing and rushing:
      home_team_rush_epa = dplyr::if_else(
        .data$play_type == "run",
        .data$home_team_epa,
        0
      ),
      away_team_rush_epa = dplyr::if_else(
        .data$play_type == "run",
        .data$away_team_epa,
        0
      ),
      home_team_rush_epa = dplyr::if_else(
        is.na(.data$home_team_rush_epa),
        0,
        .data$home_team_rush_epa
      ),
      away_team_rush_epa = dplyr::if_else(
        is.na(.data$away_team_rush_epa),
        0,
        .data$away_team_rush_epa
      ),
      total_home_rush_epa = cumsum(.data$home_team_rush_epa),
      total_away_rush_epa = cumsum(.data$away_team_rush_epa),
      home_team_pass_epa = dplyr::if_else(
        .data$play_type == "pass",
        .data$home_team_epa,
        0
      ),
      away_team_pass_epa = dplyr::if_else(
        .data$play_type == "pass",
        .data$away_team_epa,
        0
      ),
      home_team_pass_epa = dplyr::if_else(
        is.na(.data$home_team_pass_epa),
        0,
        .data$home_team_pass_epa
      ),
      away_team_pass_epa = dplyr::if_else(
        is.na(.data$away_team_pass_epa),
        0,
        .data$away_team_pass_epa
      ),
      total_home_pass_epa = cumsum(.data$home_team_pass_epa),
      total_away_pass_epa = cumsum(.data$away_team_pass_epa)
    ) |>
    dplyr::ungroup()
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

  # First check if there's any overtime plays:
  if (any(pbp_data$qtr > 4)) {
    # Find the rows that are overtime:
    overtime_i <- which(pbp_data$qtr > 4)

    # Separate the dataset into regular_df and overtime_df:
    overtime_df <- pbp_data[overtime_i, ]

    # Separate routine for overtime:

    # Create a column that is just the first drive of overtime repeated:
    overtime_df$First_Drive <- rep(
      min(overtime_df$drive, na.rm = TRUE),
      nrow(overtime_df)
    )

    # Calculate the difference in drive number
    overtime_df <- dplyr::mutate(
      overtime_df,
      Drive_Diff = .data$drive - .data$First_Drive
    )

    # Create an indicator column that means the posteam is losing by 3 and
    # its the second drive of overtime:
    overtime_df$One_FG_Game <- ifelse(
      overtime_df$score_differential == -3 &
        overtime_df$Drive_Diff == 1,
      1,
      0
    )

    # Now create a copy of the dataset to then make the EP predictions for when
    # a field goal is scored and its not sudden death:
    overtime_df_ko <- overtime_df

    overtime_df_ko$yrdline100 <- with(
      overtime_df_ko,
      ifelse(
        game_year < 2016 |
          (game_year == 2016 & game_month < 4),
        80,
        75
      )
    )

    # Now first down:
    overtime_df_ko$down1 <- rep(1, nrow(overtime_df_ko))
    overtime_df_ko$down2 <- rep(0, nrow(overtime_df_ko))
    overtime_df_ko$down3 <- rep(0, nrow(overtime_df_ko))
    overtime_df_ko$down4 <- rep(0, nrow(overtime_df_ko))
    # 10 ydstogo:
    overtime_df_ko$ydstogo <- rep(10, nrow(overtime_df_ko))

    # Get the predictions from the EP model and calculate the necessary probability:
    overtime_df_ko_preds <- get_preds(overtime_df_ko)

    overtime_df_ko_preds <- dplyr::mutate(
      overtime_df_ko_preds,
      Win_Back = .data$No_Score +
        .data$Opp_Field_Goal +
        .data$Opp_Safety +
        .data$Opp_Touchdown
    )

    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    overtime_df$Sudden_Death_WP <- overtime_df$fg_prob +
      overtime_df$td_prob +
      overtime_df$safety_prob
    overtime_df$One_FG_WP <- overtime_df$td_prob +
      (overtime_df$fg_prob * overtime_df_ko_preds$Win_Back)

    # Decide which win probability to use:
    OffWinProb[overtime_i] <- ifelse(
      overtime_df$game_year >= 2012 &
        (overtime_df$Drive_Diff == 0 |
          (overtime_df$Drive_Diff == 1 & overtime_df$One_FG_Game == 1)),
      overtime_df$One_FG_WP,
      overtime_df$Sudden_Death_WP
    )
    OffWinProb_spread[overtime_i] <- OffWinProb[overtime_i]
  }

  #regulation plays
  regular_i <- which(pbp_data$qtr <= 4)

  # df of just the regulation plays:
  regular_df <- pbp_data[regular_i, ]

  # do predictions for the regular df
  OffWinProb[regular_i] <- get_preds_wp(regular_df)
  OffWinProb_spread[regular_i] <- get_preds_wp_spread(regular_df)

  ## set to NA WP for plays down is missing
  # for kickoffs and PATs, these will get overwritten by the fixes after this

  down_na <- which(is.na(pbp_data$down))
  OffWinProb[down_na] <- NA_real_
  OffWinProb_spread[down_na] <- NA_real_

  ## start PAT fix

  make_pat_prob <- as.numeric(
    mgcv::predict.bam(
      fastrmodels::fg_model,
      newdata = pbp_data |>
        mutate(
          yardline_100 = ifelse(.data$season >= 2015, 15, 3)
        ),
      type = "response"
    )
  )

  # plays with 1 point PAT attempts
  pat_i <- which(
    (pbp_data$kickoff_attempt == 0 &
      !(stringr::str_detect(pbp_data$desc, 'Onside Kick')) &
      (stringr::str_detect(pbp_data$desc, 'Kick formation')) &
      is.na(pbp_data$down)) |
      # or has PAT indicators
      stringr::str_detect(pbp_data$desc, 'extra point') |
      !is.na(pbp_data$extra_point_result)
  )

  # plays with 2 point PAT attempts
  two_pt_i <- which(
    (pbp_data$kickoff_attempt == 0 &
      !(stringr::str_detect(pbp_data$desc, 'Onside Kick')) &
      (stringr::str_detect(pbp_data$desc, 'Pass formation')) &
      is.na(pbp_data$down)) |
      # or has PAT indicators
      stringr::str_detect(pbp_data$desc, 'TWO-POINT CONVERSION ATTEMPT') |
      !is.na(pbp_data$two_point_conv_result)
  )

  # some rare 2 point PAT attempts have duplicated matches in 1 point PAT attempts
  # so we remove them in the next line
  pat_i <- pat_i[!pat_i %in% two_pt_i]

  # make df of post-PAT plays
  pat_data <- pbp_data |>
    dplyr::mutate(
      # swap timeouts
      to_pos = .data$posteam_timeouts_remaining,
      to_def = .data$defteam_timeouts_remaining,
      posteam_timeouts_remaining = .data$to_def,
      defteam_timeouts_remaining = .data$to_pos,
      # swap score
      score_differential = -.data$score_differential,
      # 1st and 10
      down = 1,
      ydstogo = 10,
      # flip receive_2h_ko var
      receive_2h_ko = case_when(
        .data$qtr <= 2 & .data$receive_2h_ko == 0 ~ 1,
        .data$qtr <= 2 & .data$receive_2h_ko == 1 ~ 0,
        TRUE ~ .data$receive_2h_ko
      ),
      # switch posteam
      posteam = if_else(
        .data$home_team == .data$posteam,
        .data$away_team,
        .data$home_team
      ),
      yardline_100 = 75
    ) |>
    dplyr::mutate(
      home = case_when(
        .data$home == 0 ~ 1,
        .data$home == 1 ~ 0
      ),
      posteam_spread = dplyr::if_else(
        .data$home == 1,
        .data$spread_line,
        -1 * .data$spread_line
      ),
      elapsed_share = (3600 - .data$game_seconds_remaining) / 3600,
      spread_time = .data$posteam_spread * exp(-4 * .data$elapsed_share)
    )

  ## start with spread version
  # get pat if 0, 1, or 2
  pat_0 <- get_preds_wp_spread(pat_data |> add_esdtr())
  pat_1 <- get_preds_wp_spread(
    pat_data |>
      dplyr::mutate(score_differential = .data$score_differential - 1) |>
      add_esdtr()
  )
  pat_2 <- get_preds_wp_spread(
    pat_data |>
      dplyr::mutate(score_differential = .data$score_differential - 2) |>
      add_esdtr()
  )

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 -
    (two_point_make_prob * pat_2 + (1 - two_point_make_prob) * pat_0)

  OffWinProb_spread[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb_spread[pat_i] <- pat_go_for_1[pat_i]

  ## repeat for non-spread version
  # get pat if 0, 1, or 2
  pat_0 <- get_preds_wp(pat_data |> add_esdtr())
  pat_1 <- get_preds_wp(
    pat_data |>
      dplyr::mutate(score_differential = .data$score_differential - 1) |>
      add_esdtr()
  )
  pat_2 <- get_preds_wp(
    pat_data |>
      dplyr::mutate(score_differential = .data$score_differential - 2) |>
      add_esdtr()
  )

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 -
    (two_point_make_prob * pat_2 + (1 - two_point_make_prob) * pat_0)

  OffWinProb[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb[pat_i] <- pat_go_for_1[pat_i]

  ## end PAT fix

  ## now we need to fix WP on kickoffs, which will be WP associated with touchback
  kickoff_data <- pbp_data

  # Change the yard line to be 80 for 2009-2015 and 75 otherwise
  kickoff_data$yardline_100 <- with(kickoff_data, ifelse(season < 2016, 80, 75))
  # Now first down:
  kickoff_data$down <- rep(1, nrow(pbp_data))
  kickoff_data$down1 <- rep(1, nrow(pbp_data))
  kickoff_data$down2 <- rep(0, nrow(pbp_data))
  kickoff_data$down3 <- rep(0, nrow(pbp_data))
  kickoff_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10, nrow(pbp_data))

  # Get the new predicted probabilites:
  kickoff_preds <- get_preds_wp(kickoff_data)
  kickoff_preds_spread <- get_preds_wp_spread(kickoff_data)

  # Find the kickoffs in regulation:
  kickoff_i <- which(
    (pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1) &
      pbp_data$qtr <= 4
  )

  # Now update the probabilities:
  OffWinProb[kickoff_i] <- kickoff_preds[kickoff_i]
  OffWinProb_spread[kickoff_i] <- kickoff_preds_spread[kickoff_i]

  ## end fix for kickoffs

  # Now create the win probability columns and return:
  pbp_data <- pbp_data |>
    dplyr::mutate(
      wp = OffWinProb,
      vegas_wp = OffWinProb_spread,
      # for figuring out posteam on NA posteam lines
      tmp_posteam = .data$posteam
    ) |>
    tidyr::fill(
      .data$wp,
      .direction = "up"
    ) |>
    tidyr::fill(
      .data$vegas_wp,
      .direction = "up"
    ) |>
    tidyr::fill(
      .data$tmp_posteam,
      .direction = "up"
    ) |>
    dplyr::group_by(.data$game_id) |>
    dplyr::mutate(
      #add columns for home WP
      home_wp = dplyr::if_else(
        .data$tmp_posteam == .data$home_team,
        .data$wp,
        1 - .data$wp
      ),
      vegas_home_wp = dplyr::if_else(
        .data$tmp_posteam == .data$home_team,
        .data$vegas_wp,
        1 - .data$vegas_wp
      ),

      # convenience to mark end of game
      end_game = ifelse(
        stringr::str_detect(tolower(.data$desc), "(end of game)|(end game)"),
        1,
        0
      ),

      # convenience for marking home win prob on last line
      final_value = dplyr::case_when(
        .data$home_score > .data$away_score ~ 1,
        .data$away_score > .data$home_score ~ 0,
        .data$home_score == .data$away_score ~ .5
      ),

      #make 1 or 0 the final win prob
      vegas_home_wp = dplyr::if_else(
        .data$end_game == 1,
        .data$final_value,
        .data$vegas_home_wp
      ),

      # can we make this and the above into a function? feels like a lot of repitition
      home_wp = dplyr::if_else(
        .data$end_game == 1,
        .data$final_value,
        .data$home_wp
      ),

      away_wp = 1 - .data$home_wp,

      # make wp of posteam on last line NA because there's no posteam
      vegas_wp = dplyr::if_else(
        .data$end_game == 1,
        NA_real_,
        .data$vegas_wp
      ),

      wp = dplyr::if_else(
        .data$end_game == 1,
        NA_real_,
        .data$wp
      ),

      def_wp = 1 - .data$wp,

      # make wpa
      vegas_home_wpa = dplyr::lead(.data$vegas_home_wp) - .data$vegas_home_wp,
      vegas_wpa = dplyr::if_else(
        .data$tmp_posteam == .data$home_team,
        .data$vegas_home_wpa,
        -.data$vegas_home_wpa
      ),
      vegas_wpa = dplyr::if_else(
        stringr::str_detect(
          tolower(.data$desc),
          "( kneels )|(end of game)|(end game)"
        ),
        NA_real_,
        .data$vegas_wpa
      ),

      # home wpa isn't saved but needed for next line
      home_wpa = dplyr::lead(.data$home_wp) - .data$home_wp,
      wpa = dplyr::if_else(
        .data$tmp_posteam == .data$home_team,
        .data$home_wpa,
        -.data$home_wpa
      ),
      wpa = dplyr::if_else(
        stringr::str_detect(
          tolower(.data$desc),
          "( kneels )|(end of game)|(end game)"
        ),
        NA_real_,
        .data$wpa
      )
    ) |>
    dplyr::ungroup()

  # Home and Away post:

  pbp_data$home_wp_post <- ifelse(
    pbp_data$posteam == pbp_data$home_team,
    pbp_data$home_wp + pbp_data$wpa,
    pbp_data$home_wp - pbp_data$wpa
  )
  pbp_data$away_wp_post <- ifelse(
    pbp_data$posteam == pbp_data$away_team,
    pbp_data$away_wp + pbp_data$wpa,
    pbp_data$away_wp - pbp_data$wpa
  )

  # If next thing is end of game, and post score differential is tied because it's
  # overtime then make both the home_wp_post and away_wp_post equal to 0:
  pbp_data <- pbp_data |>
    dplyr::mutate(
      home_wp_post = dplyr::if_else(
        .data$qtr == 5 &
          stringr::str_detect(
            tolower(dplyr::lead(.data$desc)),
            "(end of game)|(end game)"
          ) &
          .data$score_differential_post == 0,
        0,
        .data$home_wp_post
      ),
      away_wp_post = dplyr::if_else(
        .data$qtr == 5 &
          stringr::str_detect(
            tolower(dplyr::lead(.data$desc)),
            "(end of game)|(end game)"
          ) &
          .data$score_differential_post == 0,
        0,
        .data$away_wp_post
      )
    )

  # For plays with playtype of End of Game, use the previous play's WP_post columns
  # as the pre and post, since those are already set to be 1 and 0:

  pbp_data$home_wp_post <- with(
    pbp_data,
    ifelse(
      stringr::str_detect(tolower(desc), "(end of game)|(end game)"),
      dplyr::lag(home_wp_post),
      ifelse(
        dplyr::lag(play_type) == "no_play" & play_type == "no_play",
        dplyr::lag(home_wp_post),
        home_wp_post
      )
    )
  )

  pbp_data$away_wp_post <- with(
    pbp_data,
    ifelse(
      stringr::str_detect(tolower(desc), "(end of game)|(end game)"),
      dplyr::lag(away_wp_post),
      ifelse(
        dplyr::lag(play_type) == "no_play" & play_type == "no_play",
        dplyr::lag(away_wp_post),
        away_wp_post
      )
    )
  )

  # Now drop the unnecessary columns, rename variables back, and return:
  pbp_data |>
    dplyr::group_by(.data$game_id) |>
    dplyr::mutate(
      # Generate columns to keep track of cumulative rushing and
      # passing WPA values:
      home_team_wpa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$wpa,
        -.data$wpa
      ),
      away_team_wpa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$wpa,
        -.data$wpa
      ),
      home_team_wpa = dplyr::if_else(
        is.na(.data$home_team_wpa),
        0,
        .data$home_team_wpa
      ),
      away_team_wpa = dplyr::if_else(
        is.na(.data$away_team_wpa),
        0,
        .data$away_team_wpa
      ),
      # Same thing but separating passing and rushing:
      home_team_rush_wpa = dplyr::if_else(
        .data$play_type == "run",
        .data$home_team_wpa,
        0
      ),
      away_team_rush_wpa = dplyr::if_else(
        .data$play_type == "run",
        .data$away_team_wpa,
        0
      ),
      home_team_rush_wpa = dplyr::if_else(
        is.na(.data$home_team_rush_wpa),
        0,
        .data$home_team_rush_wpa
      ),
      away_team_rush_wpa = dplyr::if_else(
        is.na(.data$away_team_rush_wpa),
        0,
        .data$away_team_rush_wpa
      ),
      total_home_rush_wpa = cumsum(.data$home_team_rush_wpa),
      total_away_rush_wpa = cumsum(.data$away_team_rush_wpa),
      home_team_pass_wpa = dplyr::if_else(
        .data$play_type == "pass",
        .data$home_team_wpa,
        0
      ),
      away_team_pass_wpa = dplyr::if_else(
        .data$play_type == "pass",
        .data$away_team_wpa,
        0
      ),
      home_team_pass_wpa = dplyr::if_else(
        is.na(.data$home_team_pass_wpa),
        0,
        .data$home_team_pass_wpa
      ),
      away_team_pass_wpa = dplyr::if_else(
        is.na(.data$away_team_pass_wpa),
        0,
        .data$away_team_pass_wpa
      ),
      total_home_pass_wpa = cumsum(.data$home_team_pass_wpa),
      total_away_pass_wpa = cumsum(.data$away_team_pass_wpa)
    ) |>
    dplyr::ungroup()
}


# helper function to get expected score diff to time ratio
# needed after flipping teams in WP for getting PAT WP
add_esdtr <- function(data) {
  data |>
    dplyr::mutate(
      Diff_Time_Ratio = .data$score_differential /
        (exp(-4 * .data$elapsed_share))
    )
}


#################################################################
# air and YAC EP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_ep_variables <- function(pbp_data) {
  #testing
  #pbp_data <- g

  # Final all pass attempts that are not sacks:
  pass_plays_i <- which(
    !is.na(pbp_data$air_yards) & pbp_data$play_type == 'pass'
  )
  pass_pbp_data <- pbp_data[pass_plays_i, ]

  # Using the air_yards need to update the following:
  # - yrdline100
  # - TimeSecs_Remaining
  # - ydstogo
  # - down
  # - timeouts

  # Get everything set up for calculation
  pass_pbp_data <- pass_pbp_data |>
    dplyr::mutate(
      posteam_timeouts_pre = .data$posteam_timeouts_remaining,
      defeam_timeouts_pre = .data$defteam_timeouts_remaining
    ) |>
    # Rename the old columns to update for calculating the EP from the air:
    dplyr::rename(
      old_yrdline100 = .data$yardline_100,
      old_ydstogo = .data$ydstogo,
      old_TimeSecs_Remaining = .data$half_seconds_remaining,
      old_down = .data$down
    ) |>
    dplyr::mutate(
      Turnover_Ind = dplyr::if_else(
        .data$old_down == 4 & .data$air_yards < .data$old_ydstogo,
        1,
        0
      ),
      yardline_100 = dplyr::if_else(
        .data$Turnover_Ind == 0,
        .data$old_yrdline100 - .data$air_yards,
        100 - (.data$old_yrdline100 - .data$air_yards)
      ),
      ydstogo = dplyr::if_else(
        .data$air_yards >= .data$old_ydstogo |
          .data$Turnover_Ind == 1,
        10,
        .data$old_ydstogo - .data$air_yards
      ),
      down = dplyr::if_else(
        .data$air_yards >= .data$old_ydstogo |
          .data$Turnover_Ind == 1,
        1,
        as.numeric(.data$old_down) + 1
      ),
      half_seconds_remaining = .data$old_TimeSecs_Remaining - 5.704673,
      down1 = dplyr::if_else(.data$down == 1, 1, 0),
      down2 = dplyr::if_else(.data$down == 2, 1, 0),
      down3 = dplyr::if_else(.data$down == 3, 1, 0),
      down4 = dplyr::if_else(.data$down == 4, 1, 0),
      # the other team has the ball after a turnover on downs, so home and
      # the timeouts are from its view
      home = dplyr::if_else(
        .data$Turnover_Ind == 1,
        1 - .data$home,
        .data$home
      ),
      posteam_timeouts_remaining = dplyr::if_else(
        .data$Turnover_Ind == 1,
        .data$defeam_timeouts_pre,
        .data$posteam_timeouts_pre
      ),
      defteam_timeouts_remaining = dplyr::if_else(
        .data$Turnover_Ind == 1,
        .data$posteam_timeouts_pre,
        .data$defeam_timeouts_pre
      )
    )

  #get EP predictions
  pass_pbp_data_preds <- get_preds(pass_pbp_data)

  # Value of the possession that follows a score (see get_kickoff_values()),
  # reused from add_ep_variables() instead of predicting again. The air state
  # is 5.7 seconds later, which only matters when the value changes quickly
  # at the end of a half, so those few rows are recomputed at the air clock.
  # The values are the same for both teams, so it does not matter that the
  # timeouts and home are from the other team's view after a turnover on downs.
  snap_values <- data.frame(
    v = pass_pbp_data$ep_v,
    w = pass_pbp_data$ep_w
  )
  # (also the rows where a safety is likely or can be the end of the play, which
  # need the exact free kick value)
  late_i <- which(
    pass_pbp_data$half_seconds_remaining <= 180 |
      ((pass_pbp_data_preds$Safety + pass_pbp_data_preds$Opp_Safety) > 0.01) %in%
        TRUE |
      (pass_pbp_data$old_yrdline100 - pass_pbp_data$air_yards > 99) %in% TRUE
  )
  if (length(late_i) > 0) {
    snap_values[late_i, c("v", "w")] <- get_kickoff_values(
      pass_pbp_data[late_i, ],
      exact_w = TRUE
    )
  }
  snap_values$k_td_own <- pass_pbp_data$ep_k_td_own
  snap_values$k_td_opp <- pass_pbp_data$ep_k_td_opp
  snap_values$two_td_own <- pass_pbp_data$ep_two_td_own
  snap_values$two_td_opp <- pass_pbp_data$ep_two_td_opp
  snap_values$c_td_own <- pass_pbp_data$ep_c_td_own
  snap_values$c_fg_own <- pass_pbp_data$ep_c_fg_own
  snap_values$c_saf_own <- pass_pbp_data$ep_c_saf_own
  snap_values$c_td_opp <- pass_pbp_data$ep_c_td_opp
  snap_values$c_fg_opp <- pass_pbp_data$ep_c_fg_opp
  snap_values$c_saf_opp <- pass_pbp_data$ep_c_saf_opp
  snap_sv <- score_values(snap_values, pass_pbp_data$season)

  # After a turnover on downs the other team has the ball in the air state
  turnover <- pass_pbp_data$Turnover_Ind %in% 1
  swapped_sv <- swap_score_values(snap_sv)
  air_sv <- as.data.frame(lapply(
    names(snap_sv),
    function(nm) dplyr::if_else(turnover, swapped_sv[[nm]], snap_sv[[nm]])
  ))
  names(air_sv) <- names(snap_sv)

  # Convert to air EP:
  pass_pbp_data_preds <- dplyr::mutate(
    pass_pbp_data_preds,
    airEP = ep_from_probs(
      .data$Touchdown,
      .data$Opp_Touchdown,
      .data$Field_Goal,
      .data$Opp_Field_Goal,
      .data$Safety,
      .data$Opp_Safety,
      air_sv
    )
  )

  # Return back to the passing data:
  pass_pbp_data$airEP <- pass_pbp_data_preds$airEP

  # For the plays that have TimeSecs_Remaining 0 or less, set airEP to 0:
  pass_pbp_data$airEP[which(pass_pbp_data$half_seconds_remaining <= 0)] <- 0

  # Calculate the airEPA based on 4 scenarios:
  pass_pbp_data$airEPA <- with(
    pass_pbp_data,
    ifelse(
      old_yrdline100 - air_yards <= 0,
      snap_sv$td_own - ep,
      ifelse(
        old_yrdline100 - air_yards > 99,
        snap_sv$saf_opp - ep,
        ifelse(Turnover_Ind == 1, (-1 * airEP) - ep, airEP - ep)
      )
    )
  )

  # If the play is a two-point conversion then change the airEPA to NA since
  # no air yards are provided:
  pass_pbp_data$airEPA <- with(
    pass_pbp_data,
    ifelse(two_point_attempt == 1, NA, airEPA)
  )
  # Calculate the yards after catch EPA:
  pass_pbp_data <- dplyr::mutate(
    pass_pbp_data,
    yacEPA = .data$epa - .data$airEPA
  )

  # if Yards after catch is 0 make yacEPA set to 0:
  pass_pbp_data$yacEPA <- ifelse(
    pass_pbp_data$penalty == 0 &
      pass_pbp_data$yards_after_catch == 0 &
      pass_pbp_data$complete_pass == 1,
    0,
    pass_pbp_data$yacEPA
  )

  # if Yards after catch is 0 make airEPA set to EPA:
  pass_pbp_data$airEPA <- ifelse(
    pass_pbp_data$penalty == 0 &
      pass_pbp_data$yards_after_catch == 0 &
      pass_pbp_data$complete_pass == 1,
    pass_pbp_data$epa,
    pass_pbp_data$airEPA
  )

  # Now add airEPA and yacEPA to the original dataset:
  pbp_data$airEPA <- NA
  pbp_data$yacEPA <- NA
  pbp_data$airEPA[pass_plays_i] <- pass_pbp_data$airEPA
  pbp_data$yacEPA[pass_plays_i] <- pass_pbp_data$yacEPA

  # Now change the names to be the right style, calculate the completion form
  # of the variables, as well as the cumulative totals and return:
  pbp_data |>
    dplyr::rename(air_epa = "airEPA", yac_epa = "yacEPA") |>
    dplyr::group_by(.data$game_id) |>
    dplyr::mutate(
      comp_air_epa = dplyr::if_else(.data$complete_pass == 1, .data$air_epa, 0),
      comp_yac_epa = dplyr::if_else(.data$complete_pass == 1, .data$yac_epa, 0),
      home_team_comp_air_epa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$comp_air_epa,
        -.data$comp_air_epa
      ),
      away_team_comp_air_epa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$comp_air_epa,
        -.data$comp_air_epa
      ),
      home_team_comp_yac_epa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$comp_yac_epa,
        -.data$comp_yac_epa
      ),
      away_team_comp_yac_epa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$comp_yac_epa,
        -.data$comp_yac_epa
      ),
      home_team_comp_air_epa = dplyr::if_else(
        is.na(.data$home_team_comp_air_epa),
        0,
        .data$home_team_comp_air_epa
      ),
      away_team_comp_air_epa = dplyr::if_else(
        is.na(.data$away_team_comp_air_epa),
        0,
        .data$away_team_comp_air_epa
      ),
      home_team_comp_yac_epa = dplyr::if_else(
        is.na(.data$home_team_comp_yac_epa),
        0,
        .data$home_team_comp_yac_epa
      ),
      away_team_comp_yac_epa = dplyr::if_else(
        is.na(.data$away_team_comp_yac_epa),
        0,
        .data$away_team_comp_yac_epa
      ),
      total_home_comp_air_epa = cumsum(.data$home_team_comp_air_epa),
      total_away_comp_air_epa = cumsum(.data$away_team_comp_air_epa),
      total_home_comp_yac_epa = cumsum(.data$home_team_comp_yac_epa),
      total_away_comp_yac_epa = cumsum(.data$away_team_comp_yac_epa),
      # Same but for raw - not just completions:
      home_team_raw_air_epa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$air_epa,
        -.data$air_epa
      ),
      away_team_raw_air_epa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$air_epa,
        -.data$air_epa
      ),
      home_team_raw_yac_epa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$yac_epa,
        -.data$yac_epa
      ),
      away_team_raw_yac_epa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$yac_epa,
        -.data$yac_epa
      ),
      home_team_raw_air_epa = dplyr::if_else(
        is.na(.data$home_team_raw_air_epa),
        0,
        .data$home_team_raw_air_epa
      ),
      away_team_raw_air_epa = dplyr::if_else(
        is.na(.data$away_team_raw_air_epa),
        0,
        .data$away_team_raw_air_epa
      ),
      home_team_raw_yac_epa = dplyr::if_else(
        is.na(.data$home_team_raw_yac_epa),
        0,
        .data$home_team_raw_yac_epa
      ),
      away_team_raw_yac_epa = dplyr::if_else(
        is.na(.data$away_team_raw_yac_epa),
        0,
        .data$away_team_raw_yac_epa
      ),
      total_home_raw_air_epa = cumsum(.data$home_team_raw_air_epa),
      total_away_raw_air_epa = cumsum(.data$away_team_raw_air_epa),
      total_home_raw_yac_epa = cumsum(.data$home_team_raw_yac_epa),
      total_away_raw_yac_epa = cumsum(.data$away_team_raw_yac_epa)
    ) |>
    dplyr::ungroup()
}


#################################################################
# air and YAC WP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_wp_variables <- function(pbp_data) {
  #testing
  #pbp_data <- g

  # Change the names to reflect the old style - will update this later on:
  pbp_data <- pbp_data |>
    dplyr::mutate(
      posteam_timeouts_pre = .data$posteam_timeouts_remaining,
      defeam_timeouts_pre = .data$defteam_timeouts_remaining
    )

  # Final all pass attempts that are not sacks:
  pass_plays_i <- which(
    !is.na(pbp_data$air_yards) & pbp_data$play_type == 'pass'
  )
  pass_pbp_data <- pbp_data[pass_plays_i, ]

  pass_pbp_data <- pass_pbp_data |>
    dplyr::mutate(
      half_seconds_remaining = .data$half_seconds_remaining - 5.704673,
      game_seconds_remaining = .data$game_seconds_remaining - 5.704673,
      Diff_Time_Ratio = .data$score_differential /
        (exp(-4 * .data$elapsed_share)),
      Turnover_Ind = dplyr::if_else(
        .data$down == 4 & .data$air_yards < .data$ydstogo,
        1,
        0
      ),
      Diff_Time_Ratio = dplyr::if_else(
        .data$Turnover_Ind == 1,
        -1 * .data$Diff_Time_Ratio,
        .data$Diff_Time_Ratio
      ),
      posteam_timeouts_remaining = dplyr::if_else(
        .data$Turnover_Ind == 1,
        .data$defeam_timeouts_pre,
        .data$posteam_timeouts_pre
      ),
      defteam_timeouts_remaining = dplyr::if_else(
        .data$Turnover_Ind == 1,
        .data$posteam_timeouts_pre,
        .data$defeam_timeouts_pre
      )
    )

  # Calculate the airWP:
  pass_pbp_data$airWP <- get_preds_wp(pass_pbp_data)

  # Now for plays marked with Turnover_Ind, use 1 - airWP to flip back to the original
  # team with possession:
  pass_pbp_data$airWP <- ifelse(
    pass_pbp_data$Turnover_Ind == 1,
    1 - pass_pbp_data$airWP,
    pass_pbp_data$airWP
  )

  # For the plays that have TimeSecs_Remaining 0 or less, set airWP to 0:
  pass_pbp_data$airWP[which(pass_pbp_data$half_seconds_remaining <= 0)] <- 0
  pass_pbp_data$airWP[which(pass_pbp_data$game_seconds_remaining <= 0)] <- 0

  # Calculate the airWPA and yacWPA:
  pass_pbp_data <- dplyr::mutate(
    pass_pbp_data,
    airWPA = .data$airWP - .data$wp,
    yacWPA = .data$wpa - .data$airWPA
  )

  # If the play is a two-point conversion then change the airWPA to NA since
  # no air yards are provided:
  pass_pbp_data$airWPA <- with(
    pass_pbp_data,
    ifelse(two_point_attempt == 1, NA, airWPA)
  )
  pass_pbp_data$yacWPA <- with(
    pass_pbp_data,
    ifelse(two_point_attempt == 1, NA, yacWPA)
  )

  # Check to see if there is any overtime plays, if so then need to calculate
  # by essentially taking the same process as the airEP calculation and using
  # the resulting probabilities for overtime:

  # First check if there's any overtime plays:
  if (any(pass_pbp_data$qtr == 5 | pass_pbp_data$qtr == 6)) {
    # Find the rows that are overtime:
    pass_overtime_i <- which(pass_pbp_data$qtr == 5 | pass_pbp_data$qtr == 6)
    pass_overtime_df <- pass_pbp_data[pass_overtime_i, ]

    # Find the rows that are overtime:

    # Need to generate same overtime scenario data as before in the wp function:
    # Find the rows that are overtime:
    overtime_i <- which(pbp_data$qtr == 5 | pbp_data$qtr == 6)

    overtime_df <- pbp_data[overtime_i, ]

    # Separate routine for overtime:

    # Create a column that is just the first drive of overtime repeated:
    overtime_df$First_Drive <- rep(
      min(overtime_df$drive, na.rm = TRUE),
      nrow(overtime_df)
    )

    # Calculate the difference in drive number
    overtime_df <- dplyr::mutate(
      overtime_df,
      Drive_Diff = .data$drive - .data$First_Drive
    )

    # Create an indicator column that means the posteam is losing by 3 and
    # its the second drive of overtime:
    overtime_df$One_FG_Game <- ifelse(
      overtime_df$score_differential == -3 &
        overtime_df$Drive_Diff == 1,
      1,
      0
    )

    # Now create a copy of the dataset to then make the EP predictions for when
    # a field goal is scored and its not sudden death:
    overtime_df_ko <- overtime_df

    overtime_df_ko$yardline_100 <- with(
      overtime_df_ko,
      ifelse(
        game_year < 2016 |
          (game_year == 2016 & game_month < 4),
        80,
        75
      )
    )

    # Now first down:
    overtime_df_ko$down1 <- rep(1, nrow(overtime_df_ko))
    overtime_df_ko$down2 <- rep(0, nrow(overtime_df_ko))
    overtime_df_ko$down3 <- rep(0, nrow(overtime_df_ko))
    overtime_df_ko$down4 <- rep(0, nrow(overtime_df_ko))
    # 10 ydstogo:
    overtime_df_ko$ydstogo <- rep(10, nrow(overtime_df_ko))

    # Get the predictions from the EP model and calculate the necessary probability:
    if (nrow(overtime_df_ko) > 1) {
      overtime_df_ko_preds <- get_preds(overtime_df_ko)
    } else {
      overtime_df_ko_preds <- get_preds(overtime_df_ko)
    }

    overtime_df_ko_preds <- dplyr::mutate(
      overtime_df_ko_preds,
      Win_Back = .data$No_Score +
        .data$Opp_Field_Goal +
        .data$Opp_Safety +
        .data$Opp_Touchdown
    )

    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    overtime_df$Sudden_Death_WP <- overtime_df$fg_prob +
      overtime_df$td_prob +
      overtime_df$safety_prob
    overtime_df$One_FG_WP <- overtime_df$td_prob +
      (overtime_df$fg_prob * overtime_df_ko_preds$Win_Back)

    # Find all Pass Attempts that are also actual plays in overtime:
    overtime_pass_plays_i <- which(
      overtime_df$play_type == "pass" &
        !is.na(overtime_df$air_yards)
    )

    overtime_pass_df <- overtime_df[overtime_pass_plays_i, ]
    overtime_df_ko_preds_pass <- overtime_df_ko_preds[overtime_pass_plays_i, ]

    # Using the AirYards need to update the following:
    # - yardline_100
    # - half_seconds_remaining
    # - ydstogo
    # - down

    # First rename the old columns to update for calculating the EP from the air:
    overtime_pass_df <- dplyr::rename(
      overtime_pass_df,
      old_yrdline100 = "yardline_100",
      old_ydstogo = "ydstogo",
      old_TimeSecs_Remaining = "half_seconds_remaining",
      old_down = "down"
    )

    # Create an indicator column for the air yards failing to convert the first down:
    overtime_pass_df$Turnover_Ind <- ifelse(
      overtime_pass_df$old_down == 4 &
        overtime_pass_df$air_yards < overtime_pass_df$old_ydstogo,
      1,
      0
    )
    # Adjust the field position variables:
    overtime_pass_df$yardline_100 <- ifelse(
      overtime_pass_df$Turnover_Ind == 0,
      overtime_pass_df$old_yrdline100 - overtime_pass_df$air_yards,
      100 - (overtime_pass_df$old_yrdline100 - overtime_pass_df$air_yards)
    )

    overtime_pass_df$ydstogo <- ifelse(
      overtime_pass_df$air_yards >= overtime_pass_df$old_ydstogo |
        overtime_pass_df$Turnover_Ind == 1,
      10,
      overtime_pass_df$old_ydstogo - overtime_pass_df$air_yards
    )

    overtime_pass_df$down <- ifelse(
      overtime_pass_df$air_yards >= overtime_pass_df$old_ydstogo |
        overtime_pass_df$Turnover_Ind == 1,
      1,
      as.numeric(overtime_pass_df$old_down) + 1
    )

    # Adjust the time with the average incomplete pass time:
    overtime_pass_df$half_seconds_remaining <- overtime_pass_df$old_TimeSecs_Remaining -
      5.704673

    overtime_pass_df <- overtime_pass_df |>
      dplyr::mutate(
        down1 = dplyr::if_else(.data$down == 1, 1, 0),
        down2 = dplyr::if_else(.data$down == 2, 1, 0),
        down3 = dplyr::if_else(.data$down == 3, 1, 0),
        down4 = dplyr::if_else(.data$down == 4, 1, 0)
      )

    # Get the predictions from the EP model and calculate the necessary probability:
    if (nrow(overtime_df_ko) > 1) {
      overtime_pass_data_preds <- get_preds(overtime_pass_df)
    } else {
      overtime_pass_data_preds <- get_preds(overtime_pass_df)
    }

    # For the turnover plays flip the scoring probabilities:
    overtime_pass_data_preds <- dplyr::mutate(
      overtime_pass_data_preds,
      old_Opp_Field_Goal = .data$Opp_Field_Goal,
      old_Opp_Safety = .data$Opp_Safety,
      old_Opp_Touchdown = .data$Opp_Touchdown,
      old_Field_Goal = .data$Field_Goal,
      old_Safety = .data$Safety,
      old_Touchdown = .data$Touchdown
    )
    overtime_pass_data_preds$Opp_Field_Goal <- ifelse(
      overtime_pass_df$Turnover_Ind == 1,
      overtime_pass_data_preds$old_Field_Goal,
      overtime_pass_data_preds$Opp_Field_Goal
    )
    overtime_pass_data_preds$Opp_Safety <- ifelse(
      overtime_pass_df$Turnover_Ind == 1,
      overtime_pass_data_preds$old_Safety,
      overtime_pass_data_preds$Opp_Safety
    )
    overtime_pass_data_preds$Opp_Touchdown <- ifelse(
      overtime_pass_df$Turnover_Ind == 1,
      overtime_pass_data_preds$old_Touchdown,
      overtime_pass_data_preds$Opp_Touchdown
    )
    overtime_pass_data_preds$Field_Goal <- ifelse(
      overtime_pass_df$Turnover_Ind == 1,
      overtime_pass_data_preds$old_Opp_Field_Goal,
      overtime_pass_data_preds$Field_Goal
    )
    overtime_pass_data_preds$Safety <- ifelse(
      overtime_pass_df$Turnover_Ind == 1,
      overtime_pass_data_preds$old_Opp_Safety,
      overtime_pass_data_preds$Safety
    )
    overtime_pass_data_preds$Touchdown <- ifelse(
      overtime_pass_df$Turnover_Ind == 1,
      overtime_pass_data_preds$old_Opp_Touchdown,
      overtime_pass_data_preds$Touchdown
    )

    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    pass_overtime_df$Sudden_Death_airWP <- with(
      overtime_pass_data_preds,
      Field_Goal + Touchdown + Safety
    )
    pass_overtime_df$One_FG_airWP <- overtime_pass_data_preds$Touchdown +
      (overtime_pass_data_preds$Field_Goal * overtime_df_ko_preds_pass$Win_Back)

    # Decide which win probability to use:
    pass_overtime_df$airWP <- ifelse(
      overtime_pass_df$game_year >= 2012 &
        (overtime_pass_df$Drive_Diff == 0 |
          (overtime_pass_df$Drive_Diff == 1 &
            overtime_pass_df$One_FG_Game == 1)),
      pass_overtime_df$One_FG_airWP,
      pass_overtime_df$Sudden_Death_airWP
    )

    # For the plays that have TimeSecs_Remaining 0 or less, set airWP to 0:
    pass_overtime_df$airWP[which(
      overtime_pass_df$half_seconds_remaining <= 0
    )] <- 0

    # Calculate the airWPA and yacWPA:
    pass_overtime_df <- dplyr::mutate(
      pass_overtime_df,
      airWPA = .data$airWP - .data$wp,
      yacWPA = .data$wpa - .data$airWPA
    )

    # If the play is a two-point conversion then change the airWPA to NA since
    # no air yards are provided:
    pass_overtime_df$airWPA <- with(
      pass_overtime_df,
      ifelse(two_point_attempt == 1, NA, airWPA)
    )
    pass_overtime_df$yacWPA <- with(
      pass_overtime_df,
      ifelse(two_point_attempt == 1, NA, yacWPA)
    )

    pass_overtime_df <- pass_pbp_data[pass_overtime_i, ]

    # Now update the overtime rows in the original pass_pbp_data for airWPA and yacWPA:
    pass_pbp_data$airWPA[pass_overtime_i] <- pass_overtime_df$airWPA
    pass_pbp_data$yacWPA[pass_overtime_i] <- pass_overtime_df$yacWPA
  }

  # if Yards after catch is 0 make yacWPA set to 0:
  pass_pbp_data$yacWPA <- ifelse(
    pass_pbp_data$penalty == 0 &
      pass_pbp_data$yards_after_catch == 0 &
      pass_pbp_data$complete_pass == 1,
    0,
    pass_pbp_data$yacWPA
  )
  # if Yards after catch is 0 make airWPA set to WPA:
  pass_pbp_data$airWPA <- ifelse(
    pass_pbp_data$penalty == 0 &
      pass_pbp_data$yards_after_catch == 0 &
      pass_pbp_data$complete_pass == 1,
    pass_pbp_data$wpa,
    pass_pbp_data$airWPA
  )

  # Now add airWPA and yacWPA to the original dataset:
  pbp_data$airWPA <- NA
  pbp_data$yacWPA <- NA
  pbp_data$airWPA[pass_plays_i] <- pass_pbp_data$airWPA
  pbp_data$yacWPA[pass_plays_i] <- pass_pbp_data$yacWPA

  # Now change the names to be the right style, calculate the completion form
  # of the variables, as well as the cumulative totals and return:
  pbp_data |>
    dplyr::rename(air_wpa = "airWPA", yac_wpa = "yacWPA") |>
    dplyr::group_by(.data$game_id) |>
    dplyr::mutate(
      comp_air_wpa = dplyr::if_else(.data$complete_pass == 1, .data$air_wpa, 0),
      comp_yac_wpa = dplyr::if_else(.data$complete_pass == 1, .data$yac_wpa, 0),
      home_team_comp_air_wpa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$comp_air_wpa,
        -.data$comp_air_wpa
      ),
      away_team_comp_air_wpa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$comp_air_wpa,
        -.data$comp_air_wpa
      ),
      home_team_comp_yac_wpa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$comp_yac_wpa,
        -.data$comp_yac_wpa
      ),
      away_team_comp_yac_wpa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$comp_yac_wpa,
        -.data$comp_yac_wpa
      ),
      home_team_comp_air_wpa = dplyr::if_else(
        is.na(.data$home_team_comp_air_wpa),
        0,
        .data$home_team_comp_air_wpa
      ),
      away_team_comp_air_wpa = dplyr::if_else(
        is.na(.data$away_team_comp_air_wpa),
        0,
        .data$away_team_comp_air_wpa
      ),
      home_team_comp_yac_wpa = dplyr::if_else(
        is.na(.data$home_team_comp_yac_wpa),
        0,
        .data$home_team_comp_yac_wpa
      ),
      away_team_comp_yac_wpa = dplyr::if_else(
        is.na(.data$away_team_comp_yac_wpa),
        0,
        .data$away_team_comp_yac_wpa
      ),
      total_home_comp_air_wpa = cumsum(.data$home_team_comp_air_wpa),
      total_away_comp_air_wpa = cumsum(.data$away_team_comp_air_wpa),
      total_home_comp_yac_wpa = cumsum(.data$home_team_comp_yac_wpa),
      total_away_comp_yac_wpa = cumsum(.data$away_team_comp_yac_wpa),
      # Same but for raw - not just completions:
      home_team_raw_air_wpa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$air_wpa,
        -.data$air_wpa
      ),
      away_team_raw_air_wpa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$air_wpa,
        -.data$air_wpa
      ),
      home_team_raw_yac_wpa = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$yac_wpa,
        -.data$yac_wpa
      ),
      away_team_raw_yac_wpa = dplyr::if_else(
        .data$posteam == .data$away_team,
        .data$yac_wpa,
        -.data$yac_wpa
      ),
      home_team_raw_air_wpa = dplyr::if_else(
        is.na(.data$home_team_raw_air_wpa),
        0,
        .data$home_team_raw_air_wpa
      ),
      away_team_raw_air_wpa = dplyr::if_else(
        is.na(.data$away_team_raw_air_wpa),
        0,
        .data$away_team_raw_air_wpa
      ),
      home_team_raw_yac_wpa = dplyr::if_else(
        is.na(.data$home_team_raw_yac_wpa),
        0,
        .data$home_team_raw_yac_wpa
      ),
      away_team_raw_yac_wpa = dplyr::if_else(
        is.na(.data$away_team_raw_yac_wpa),
        0,
        .data$away_team_raw_yac_wpa
      ),
      total_home_raw_air_wpa = cumsum(.data$home_team_raw_air_wpa),
      total_away_raw_air_wpa = cumsum(.data$away_team_raw_air_wpa),
      total_home_raw_yac_wpa = cumsum(.data$home_team_raw_yac_wpa),
      total_away_raw_yac_wpa = cumsum(.data$away_team_raw_yac_wpa)
    ) |>
    dplyr::ungroup()
}
