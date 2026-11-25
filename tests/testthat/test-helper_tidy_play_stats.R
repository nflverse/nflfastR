test_that("the switch branches of tidy_play_stats match stat_ids", {
  loop <- Find(
    \(e) is.call(e) && identical(e[[1]], as.name("for")),
    as.list(body(tidy_play_stats))
  )
  sw <- Find(
    \(e) is.call(e) && identical(e[[1]], as.name("switch")),
    as.list(loop[[4]])
  )
  branches <- names(as.list(sw))[-(1:2)]
  expect_identical(as.integer(branches), stat_ids$stat_id)
})

test_that("games without play stats give an empty play stats table", {
  plays <- data.frame(playId = c(1, 2))
  plays$playStats <- list(NULL, data.frame())
  stats <- unnest_play_stats(plays)
  expect_identical(nrow(stats), 0L)
  play_stats <- tidy_play_stats(stats)
  expect_identical(nrow(play_stats), 0L)
  expect_type(play_stats$play_id, "integer")
})
