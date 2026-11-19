test_that("update_pbp_db() only warns about games that are still missing", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(
    con,
    "pbp",
    data.frame(game_id = "2024_01_BAL_KC", season = 2024)
  )
  ready <- c("2024_01_BAL_KC", "2024_01_GB_PHI")
  local_mocked_bindings(
    valid_seasons = \() 2024,
    db_drop_seasons = \(...) invisible(TRUE),
    db_write_pbp_seasons = \(...) invisible(TRUE),
    db_remove_dummy = \(...) invisible(TRUE),
    completed_game_ids = \(seasons) c(ready, "2024_01_PIT_ATL"),
    build_nflfastR_pbp = \(game_ids, ...) {
      ids <- intersect(game_ids, ready)
      data.frame(game_id = ids, season = rep(2024, length(ids)))
    }
  )
  withr::local_options(nflfastR.verbose = FALSE)
  mask <- \(x) {
    x <- gsub("[0-9]{2}:[0-9]{2}:[0-9]{2}", "<time>", x)
    gsub("nflfastR version [0-9.]+", "nflfastR version <version>", x)
  }

  expect_snapshot(
    update_pbp_db(con, name = "pbp", seasons = 2024),
    transform = mask
  )
  ready <- c(ready, "2024_01_PIT_ATL")
  expect_snapshot(
    update_pbp_db(con, name = "pbp", seasons = 2024),
    transform = mask
  )
})
