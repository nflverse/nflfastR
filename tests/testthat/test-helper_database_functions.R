test_that("update_db() is deprecated", {
  withr::local_options(lifecycle_verbosity = "error")
  expect_error(update_db(), class = "defunctError")
})

test_that("update_db() creates the database and appends missing games", {
  skip_if_not_installed("RSQLite")
  ids <- db_test_schedule()$game_id
  dir <- file.path(withr::local_tempdir(), "new_dir")
  local_update_db_mocks(available = ids[1:38], built = ids[39:40])

  res <- run_update_db(dir)

  expect_identical(
    res[c("play_id", "game_id", "season", "epa")],
    db_test_pbp(ids[c(1:38, 40, 39)])
  )
  other <- setdiff(
    names(default_play),
    c("play_id", "game_id", "season", "epa")
  )
  expect_all_true(vapply(res[other], \(x) all(is.na(x)), logical(1)))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "nflfastR_pbp", default_play)
  expect_identical(
    db_test_columns(dir),
    DBI::dbGetQuery(con, "PRAGMA table_info(nflfastR_pbp)")[c("name", "type")]
  )
})

test_that("update_db() rebuilds seasons", {
  skip_if_not_installed("RSQLite")
  ids <- db_test_schedule()$game_id
  dir <- withr::local_tempdir()
  local_update_db_mocks(available = ids[1:40])
  run_update_db(dir)

  local_update_db_mocks(available = ids[1:40], epa = 1)
  res <- run_update_db(dir, force_rebuild = 2002)
  expect_identical(
    res[c("game_id", "epa")],
    data.frame(
      game_id = rep(ids[1:40], each = 2),
      epa = rep(c(0.5, 1), each = 40)
    )
  )

  local_update_db_mocks(available = ids[1:40], epa = 2)
  res <- run_update_db(dir, force_rebuild = TRUE)
  expect_identical(
    res[c("game_id", "epa")],
    db_test_pbp(ids[1:40], 2)[c("game_id", "epa")]
  )

  # an invalid season changes nothing
  res <- run_update_db(dir, force_rebuild = 1990)
  expect_identical(
    res[c("game_id", "epa")],
    db_test_pbp(ids[1:40], 2)[c("game_id", "epa")]
  )
})

test_that("update_db() rebuilds a season with more than 16 missing games", {
  skip_if_not_installed("RSQLite")
  ids <- db_test_schedule()$game_id
  dir <- withr::local_tempdir()
  local_update_db_mocks(available = ids[1:40])
  run_update_db(dir)
  db_test_execute(dir, "DELETE FROM nflfastR_pbp WHERE game_id < '2001_01_T18'")

  local_update_db_mocks(available = ids[1:40], epa = 3)
  res <- run_update_db(dir)
  expect_identical(
    res[c("game_id", "epa")],
    data.frame(
      game_id = rep(ids[c(21:40, 1:20)], each = 2),
      epa = rep(c(0.5, 3), each = 40)
    )
  )
})

test_that("update_db() skips games without raw data", {
  skip_if_not_installed("RSQLite")
  ids <- db_test_schedule()$game_id
  dir <- withr::local_tempdir()
  local_update_db_mocks(available = ids[1:39])
  res <- run_update_db(dir)
  expect_identical(res$game_id, rep(ids[1:39], each = 2))
})

test_that("update_db() uses and keeps a given connection", {
  skip_if_not_installed("RSQLite")
  withr::local_options(lifecycle_verbosity = "quiet")
  ids <- db_test_schedule()$game_id
  local_update_db_mocks(available = ids[1:40])
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  quiet_update_db(tblname = "pbp", db_connection = con)

  expect_true(DBI::dbIsValid(con))
  expect_identical(
    DBI::dbGetQuery(con, "SELECT game_id FROM pbp ORDER BY rowid")$game_id,
    rep(ids[1:40], each = 2)
  )
})
