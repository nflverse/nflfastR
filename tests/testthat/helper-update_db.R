# Fake schedule for update_db(): 20 games in 2001 and 2002 and an unfinished
# 2003. Within a season, gameday runs opposite to game_id.
db_test_schedule <- function() {
  s <- data.frame(
    season = rep(2001:2003, each = 20),
    i = rep(1:20, 3)
  )
  s$game_id <- sprintf("%d_01_T%02d_U%02d", s$season, s$i, s$i)
  s$gameday <- as.character(
    as.Date(sprintf("%d-09-01", s$season)) + (20 - s$i)
  )
  s$result <- ifelse(s$season == 2003, NA_real_, 3)
  s$i <- NULL
  s
}

# Fake pbp with two plays per game
db_test_pbp <- function(game_ids, epa = 0.5) {
  n <- 2 * length(game_ids)
  data.frame(
    play_id = rep(c(1, 2), length(game_ids)),
    game_id = rep(game_ids, each = 2),
    season = as.integer(substr(rep(game_ids, each = 2), 1, 4)),
    epa = rep(epa, n)
  )
}

# Mocks the loaders of update_db(). load_pbp() returns all games in
# `available`, build_nflfastR_pbp() the games in `built`.
local_update_db_mocks <- function(
  available,
  built = character(),
  epa = 0.5,
  env = parent.frame()
) {
  sched <- db_test_schedule()
  local_mocked_bindings(
    load_schedules = \(...) sched,
    load_pbp = function(seasons, ...) {
      ids <- sched$game_id[sched$season %in% seasons]
      db_test_pbp(ids[ids %in% available], epa)
    },
    build_nflfastR_pbp = \(game_ids, ...) {
      db_test_pbp(game_ids[game_ids %in% built], epa)
    },
    .env = env
  )
}

quiet_update_db <- function(...) {
  utils::capture.output(suppressMessages(update_db(...)))
  invisible()
}

# Runs update_db() quietly in `dir` and returns the table in insert order
run_update_db <- function(dir, ...) {
  withr::local_options(lifecycle_verbosity = "quiet")
  quiet_update_db(dbdir = dir, ...)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, "pbp_db"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "SELECT * FROM nflfastR_pbp ORDER BY rowid")
}

db_test_columns <- function(dir) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, "pbp_db"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "PRAGMA table_info(nflfastR_pbp)")[c("name", "type")]
}

db_test_execute <- function(dir, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, "pbp_db"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sql)
}
