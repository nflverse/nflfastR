test_that("add_game_data() fails without the game data", {
  pbp <- data.table::data.table(game_id = c("2019_01_GB_CHI", "2019_02_GB_CHI"))
  g <- readRDS(file.path(test_dir, "games.rds"))
  expect_snapshot(add_game_data(pbp, games = g), error = TRUE)
  expect_snapshot(add_game_data(pbp, games = g[0, ]), error = TRUE)
  expect_snapshot(add_game_data(pbp, games = data.frame()), error = TRUE)
})
