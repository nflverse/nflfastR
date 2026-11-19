test_that("missing_raw_pbp() returns finished games without local file", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "2023"))
  file.create(file.path(dir, "2023", "2023_01_DET_KC.rds"))
  local_mocked_bindings(
    raw_pbp_load_schedules = function(seasons) {
      data.frame(
        game_id = c("2023_01_DET_KC", "2023_01_CAR_ATL", "2023_02_GB_ATL"),
        result = c(-1, 14, NA)
      )
    }
  )

  expect_snapshot(missing <- missing_raw_pbp(dir))
  expect_identical(missing, "2023_01_CAR_ATL")
  expect_identical(missing_raw_pbp(dir, verbose = FALSE), "2023_01_CAR_ATL")
})

test_that("missing_raw_pbp() handles empty dir and no missing games", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(
    raw_pbp_load_schedules = function(seasons) {
      data.frame(game_id = c("2023_01_DET_KC", "2023_02_GB_ATL"), result = NA)
    }
  )

  expect_snapshot(out <- missing_raw_pbp(dir))
  expect_null(out)
})

test_that("missing_raw_pbp() and save_raw_pbp() validate dir", {
  expect_snapshot(error = TRUE, {
    missing_raw_pbp(dir = NULL)
    missing_raw_pbp(dir = "does/not/exist")
    save_raw_pbp("2023_01_DET_KC", dir = NULL)
    save_raw_pbp("2023_01_DET_KC", dir = "does/not/exist")
  })
})

test_that("verify_game_ids() flags invalid game IDs", {
  expect_null(verify_game_ids(c("2021_02_LAC_KC", "2002_17_OAK_SD")))
  expect_snapshot(error = TRUE, {
    verify_game_ids(c(
      "2021_02_LAC_KC",
      "Hello World",
      "2028_01_LAC_JAX",
      "2022_27_LAC_BUF",
      "2021_02_LAC_KAC"
    ))
  })
})
