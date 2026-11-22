# The expected points model
#
# The model gives the probabilities of the seven ways the next score of the
# half can go (a touchdown, field goal or safety for either team, or no score)
# for a game state. It is the average of three small neural networks (three
# hidden layers of 48 units), `ep_model` in R/sysdata.rda, built in
# data-raw/ep_model.R: boosted trees are fit to the next score of the plays
# since 2001, and the networks are fit to the probabilities of the trees. One
# network depends on the random draws of its fit, so the model averages three
# that are fit with different draws. It is as accurate as the trees but a
# smooth function of the game state, where trees move in steps (and often in
# the wrong direction) when the ball moves a yard, the distance changes by a
# yard or the clock runs.
#
# EP is the value of a state for an average team. Good teams reach the red
# zone more often, and bad teams face more 3rd and longs and are backed up more
# often, so the next score of the plays in a state also reflects who gets
# there. The trees are fit to plays that are weighted so that in every state
# the strength of the teams (from the pregame lines) is that of all games. The
# model doesn't need team strength to predict.
#
# Predictions have to be fast: a game needs the EP of about 3,700 states.
# Everything is laid out for that. The plays are the columns of all matrices
# (inputs and units are the rows), so a block of plays is contiguous in memory
# and the bias of a layer is added by recycling. The plays are processed in
# blocks that fit into the CPU cache, the activation only needs a square root,
# and the stored weights of the first layer include the standardization of the
# inputs.

# The columns of the game state that the EP model needs. Its eras are the
# seasons through 2005 (`era0` and `era1` together), 2006 to 2013 (`era2`),
# 2014 to 2017 (`era3`), 2018 to 2021 and the seasons since 2022 (`era5`). 2018
# to 2021 and a retractable roof are the reference, so `era4` (which includes
# the seasons since 2022) and `retractable` aren't needed.
ep_model_features <- c(
  "half_seconds_remaining",
  "yardline_100",
  "home",
  "dome",
  "outdoors",
  "ydstogo",
  "era0",
  "era1",
  "era2",
  "era3",
  "era5",
  "down1",
  "down2",
  "down3",
  "down4",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# The inputs of the network (one row per input, one column per play) from a
# matrix with the columns `ep_model_features`. Yard line, distance and clock
# are also given on a log scale because EP changes fastest near the goal line,
# on short distances and at the end of a half.
#
# The package also asks for states outside of the range of the plays the model
# was built with. They get the value of the nearest state inside of it, as they
# did from the trees of the previous model: a yard line of 0 (a touchdown in
# the xYAC variables), 0 yards to go, a clock below 0 (time taken off for a
# play), and distances over 40 yards.
ep_model_inputs <- function(features) {
  yardline <- pmin(pmax(features[, "yardline_100"], 1), 99)
  ydstogo <- pmin(pmax(features[, "ydstogo"], 1), 40)
  clock <- pmin(pmax(features[, "half_seconds_remaining"], 0), 1800)
  rbind(
    yardline = yardline / 100,
    log_yardline = log(yardline) / log(100),
    ydstogo = ydstogo / 20,
    log_ydstogo = log(ydstogo) / log(40),
    down1 = features[, "down1"],
    down2 = features[, "down2"],
    down3 = features[, "down3"],
    down4 = features[, "down4"],
    clock = clock / 1800,
    log_clock = log1p(clock) / log(1801),
    sqrt_clock = sqrt(clock / 1800),
    posteam_timeouts = features[, "posteam_timeouts_remaining"] / 3,
    defteam_timeouts = features[, "defteam_timeouts_remaining"] / 3,
    home = features[, "home"],
    dome = features[, "dome"],
    outdoors = features[, "outdoors"],
    era1 = features[, "era0"] + features[, "era1"],
    era2 = features[, "era2"],
    era3 = features[, "era3"],
    era5 = features[, "era5"]
  )
}

# The activation of the hidden units: a smooth step from -1 to 1 like tanh,
# which takes three times as long to compute
ep_model_activation <- function(x) {
  x / sqrt(1 + x * x)
}

# The probabilities of the next score (one row per play, one column per
# outcome) for a matrix with the columns `ep_model_features`: the average of the
# probabilities of the networks in `model$students`. They are NA for plays with
# a missing feature.
#
# Every distinct state is predicted once. With an optimized BLAS a matrix
# product can round the last digit of a column differently depending on where
# the column is in the matrix, so the same state at two places would get EP
# that differs by about 1e-16. EPA of plays that don't change the state (e.g.
# a touchback on a kickoff or a timeout) would then be slightly positive or
# negative instead of 0, and `success` would depend on the platform. Plays
# with a missing feature are left out of the matrix products, which are much
# slower with missing values.
ep_model_predict <- function(features, model = ep_model, block = 1024L) {
  # the same number for the same state, NA for a state with a missing feature
  state <- data.table::frankv(
    data.table::as.data.table(features),
    ties.method = "dense",
    na.last = "keep"
  )
  first <- which(!duplicated(state) & !is.na(state))
  x <- ep_model_inputs(features[first, , drop = FALSE])
  n <- ncol(x)
  probs <- matrix(0, length(model$classes), n)
  for (b in seq_len(ceiling(n / block))) {
    i <- ((b - 1L) * block + 1L):min(b * block, n)
    for (layers in model$students) {
      last <- length(layers)
      h <- x[, i, drop = FALSE]
      for (l in seq_len(last - 1L)) {
        h <- ep_model_activation(layers[[l]]$weights %*% h + layers[[l]]$bias)
      }
      # Softmax. exp() can't overflow: the hidden units are between -1 and 1,
      # so the weights of the last layer bound its input (data-raw/ep_model.R
      # checks that).
      e <- exp(layers[[last]]$weights %*% h + layers[[last]]$bias)
      probs[, i] <- probs[, i] + e / rep(colSums(e), each = nrow(e))
    }
  }
  probs <- t(probs) / length(model$students)
  colnames(probs) <- model$classes
  # a state with a missing feature isn't in state[first], so it gets a row of NA
  probs[match(state, state[first]), , drop = FALSE]
}
