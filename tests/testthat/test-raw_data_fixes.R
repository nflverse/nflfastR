test_that("scores are right in games with broken team fields in the raw data", {
  g <- readRDS(file.path(test_dir, "games_raw_fixes.rds"))
  pbp <- suppressMessages(build_nflfastR_pbp(
    c("2002_05_PHI_JAX", "2011_13_DET_NO"),
    dir = test_dir,
    games = g
  ))
  fin <- tapply(pbp$total_home_score, pbp$game_id, max, na.rm = TRUE)
  expect_equal(as.vector(fin[c("2002_05_PHI_JAX", "2011_13_DET_NO")]), c(28, 31))
  fin <- tapply(pbp$total_away_score, pbp$game_id, max, na.rm = TRUE)
  expect_equal(as.vector(fin[c("2002_05_PHI_JAX", "2011_13_DET_NO")]), c(25, 17))

  # 2002: the raw data name PHI on every play. Jacksonville's touchdowns and
  # penalties belong to Jacksonville
  jax <- pbp[pbp$game_id == "2002_05_PHI_JAX", ]
  td <- jax[jax$touchdown %in% 1, ]
  expect_equal(td$td_team[grepl("Brunell|Mack", td$desc)], c("JAX", "JAX"))
  pen <- jax[!is.na(jax$penalty_team), ]
  in_desc <- stringr::str_extract(
    pen$desc,
    "(?<=PENALTY on )[A-Z]{2,3}(?=[-,])"
  )
  expect_equal(pen$penalty_team, ifelse(in_desc == "JAC", "JAX", in_desc))
  # PHI trailed 28-18 before its last touchdown
  last_td <- jax[jax$touchdown %in% 1 & jax$qtr == 4, ]
  expect_equal(utils::tail(last_td$score_differential, 1), -10)

  # 2011: two touchdowns are in the raw data twice
  no <- pbp[pbp$game_id == "2011_13_DET_NO", ]
  expect_equal(sum(no$touchdown, na.rm = TRUE), 6)
})
