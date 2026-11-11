# Builds the field goal make probability table that lives in R/sysdata.rda
# (object `fg_prob_table`). See R/helper_fg_model.R for how it is used. It
# replaces fastrmodels::fg_model, whose newest era starts in 2014 and so
# understates today's kickers from 45+ yards.
#
# The method is nfl4th's (data-raw/_punt_and_fg_models.R there, nfl4th#63),
# with more eras so that it covers every season since 1999:
#
# 1. A smooth curve in distance per kicking era and roof (outdoors or not).
# 2. A correction for selection at long range: only teams who trust their
#    kicker attempt very long field goals, so the observed make rate out there
#    describes an elite subset rather than the average team. The attempt rate
#    on 4th downs tells how selected the kicks at each distance are; where it
#    falls, the curve is blended in log odds toward a curve fit on the range
#    where (almost) everyone kicks. The blend can only lower the probability.
# 3. Make probability falls with distance, and is tapered to zero from 62 to
#    71 yards, where there is too little data to say anything.
#
# Run from the package root. Loads one season at a time, takes a few minutes.

library(dplyr)
source("R/helper_fg_model.R")

WEIGHT_FLOOR <- 0.80 # a distance is "unselected" while the attempt rate is
# at least this share of the group's plateau
TAPER_FROM <- 62 # yards; last distance with enough data to say anything
TAPER_ZERO <- 71 # yards; beyond here, call it zero
PLATEAU_MAX <- 51 # yards; every group is still unselected below this

# ---- data: field goal attempts and 4th downs ---------------------------------
pbp <- bind_rows(lapply(1999:nflreadr::most_recent_season(), function(s) {
  x <- nflreadr::load_pbp(s) |>
    filter(down == 4 | play_type == "field_goal") |>
    select(
      season,
      roof,
      play_type,
      down,
      ydstogo,
      yardline_100,
      half_seconds_remaining,
      field_goal_result
    )
  gc(FALSE)
  x
})) |>
  mutate(
    fg_group = paste0(
      "era",
      fg_era(season),
      ifelse(is.na(roof) | roof == "outdoors", "_outdoors", "_indoors")
    ),
    fg_group = factor(fg_group, levels = sort(unique(fg_group))),
    fg_dist = yardline_100 + 17
  )

kicks <- pbp |>
  filter(
    play_type == "field_goal",
    !is.na(field_goal_result),
    !is.na(yardline_100)
  ) |>
  mutate(made = as.numeric(field_goal_result == "made"))
cat("field goal attempts by group:\n")
print(table(kicks$fg_group))

# ---- 1. observed make rate ---------------------------------------------------
fg_fit <- mgcv::bam(
  made ~ s(yardline_100, by = fg_group) + fg_group,
  data = kicks,
  family = "binomial"
)

# ---- 2. long range selection correction --------------------------------------
# How often is a field goal attempted, by distance and group? Restricted to 4th
# downs where going for it is unattractive and it is not an end-of-half heave,
# so the attempt rate reflects confidence in making the kick rather than
# strategy.
fourth_downs <- pbp |>
  filter(
    down == 4,
    ydstogo >= 5,
    fg_dist >= 30,
    fg_dist <= 75,
    is.na(half_seconds_remaining) | half_seconds_remaining >= 30
  ) |>
  mutate(attempted = play_type %in% "field_goal")

attempt_fit <- mgcv::bam(
  attempted ~ s(fg_dist, by = fg_group) + fg_group,
  data = fourth_downs,
  family = "binomial"
)

propensity <- tidyr::expand_grid(
  fg_dist = 30:75,
  fg_group = levels(kicks$fg_group)
) |>
  mutate(fg_group = factor(fg_group, levels = levels(kicks$fg_group)))
propensity$attempt_rate <- as.numeric(
  mgcv::predict.bam(attempt_fit, newdata = propensity, type = "response")
)

# The attempt rate plateaus below 1 even at short range, because some teams go
# for it instead. That is strategy, not kicker doubt, so rescale each group
# against its own plateau and let the weight fall only where it really falls.
propensity <- propensity |>
  group_by(fg_group) |>
  mutate(
    plateau = stats::median(attempt_rate[fg_dist <= PLATEAU_MAX]),
    weight = pmin(attempt_rate / plateau, 1)
  ) |>
  ungroup()

unselected_max <- propensity |>
  filter(weight >= WEIGHT_FLOOR) |>
  group_by(fg_group) |>
  summarise(max_dist = max(fg_dist), .groups = "drop")
cat("\nattempt rate relative to the plateau, by distance:\n")
print(
  propensity |>
    filter(fg_dist %in% c(35, 40, 45, 48, 50, 52, 54, 56, 58, 60)) |>
    select(fg_group, fg_dist, weight) |>
    tidyr::pivot_wider(names_from = fg_dist, values_from = weight) |>
    as.data.frame(),
  digits = 2
)
cat("\nlongest unselected distance by group:\n")
print(as.data.frame(unselected_max))

logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))
squeeze <- function(p) pmin(pmax(p, 1e-6), 1 - 1e-6)

# Only compute out to TAPER_ZERO; the spline would extrapolate nonsensically
# past the data and everything beyond is zero anyway.
fg <- tidyr::expand_grid(
  yardline_100 = 1:(TAPER_ZERO - 17),
  fg_group = levels(kicks$fg_group)
) |>
  mutate(
    fg_group = factor(fg_group, levels = levels(kicks$fg_group)),
    fg_dist = yardline_100 + 17
  ) |>
  left_join(
    propensity |> select(fg_dist, fg_group, weight),
    by = c("fg_dist", "fg_group")
  ) |>
  mutate(weight = pmin(pmax(dplyr::coalesce(weight, 1), 0), 1))
fg$p_observed <- as.numeric(
  mgcv::predict.bam(fg_fit, newdata = fg, type = "response")
)

# Curve from the unselected range only, fit per group on that group's own
# range. Logit-linear rather than a spline so that it extrapolates
# monotonically instead of curling back up.
fg$p_unselected <- NA_real_
for (group in levels(kicks$fg_group)) {
  limit <- unselected_max$max_dist[unselected_max$fg_group == group]
  unselected_fit <- stats::glm(
    made ~ fg_dist,
    data = kicks |> filter(fg_group == group, fg_dist <= limit),
    family = stats::binomial()
  )
  rows <- fg$fg_group == group
  fg$p_unselected[rows] <- as.numeric(
    stats::predict(unselected_fit, newdata = fg[rows, ], type = "response")
  )
}

fg <- fg |>
  mutate(
    blended = inv_logit(
      weight *
        logit(squeeze(p_observed)) +
        (1 - weight) * logit(squeeze(p_unselected))
    ),
    # the correction may only pull down, never up
    p = pmin(p_observed, blended)
  ) |>
  arrange(fg_group, yardline_100) |>
  group_by(fg_group) |>
  # make probability has to fall with distance
  mutate(p = cummin(p), anchor = p[fg_dist == TAPER_FROM]) |>
  ungroup() |>
  mutate(
    p = case_when(
      fg_dist <= TAPER_FROM ~ p,
      fg_dist >= TAPER_ZERO ~ 0,
      TRUE ~ anchor * (TAPER_ZERO - fg_dist) / (TAPER_ZERO - TAPER_FROM)
    )
  )

# yardline_100 1 to 99 by group, zero past TAPER_ZERO
fg_prob_table <- matrix(
  0,
  nrow = 99,
  ncol = nlevels(kicks$fg_group),
  dimnames = list(NULL, levels(kicks$fg_group))
)
fg_prob_table[cbind(
  fg$yardline_100,
  as.integer(fg$fg_group)
)] <- fg$p
fg_prob_table <- round(fg_prob_table, 5)

stopifnot(
  identical(
    colnames(fg_prob_table),
    as.vector(outer(
      c("_indoors", "_outdoors"),
      paste0("era", 1:5),
      function(r, e) paste0(e, r)
    ))
  ),
  all(fg_prob_table >= 0 & fg_prob_table <= 1),
  # make probability must fall with distance in every group
  all(apply(fg_prob_table, 2, function(p) all(diff(p) <= 1e-12)))
)

cat("\nmake probability by distance:\n")
print(
  data.frame(
    dist = c(25, 30, 35, 40, 45, 50, 55, 60, 65),
    fg_prob_table[c(25, 30, 35, 40, 45, 50, 55, 60, 65) - 17, ]
  ),
  digits = 2
)

# ---- save, keeping the other internal data ----------------------------------
# R/sysdata.rda holds other objects. Load them and write them back untouched so
# that only this table changes.
e <- new.env()
load("R/sysdata.rda", envir = e)
untouched <- setdiff(ls(e), "fg_prob_table")
before <- mget(untouched, envir = e)
e$fg_prob_table <- fg_prob_table
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
