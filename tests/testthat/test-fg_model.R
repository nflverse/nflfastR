test_that("kicking eras follow the season", {
  s <- c(1999, 2003, 2004, 2008, 2009, 2013, 2014, 2022, 2023, 2026)
  expect_equal(fg_era(s), c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
})

test_that("field goal make probability falls with distance and ends at 71 yards", {
  for (season in c(2000, 2010, 2018, 2025)) {
    for (roof in c("outdoors", "dome")) {
      p <- fg_make_prob(season, roof, 1:99)
      expect_true(all(p >= 0 & p <= 1))
      expect_true(all(diff(p) <= 0))
      # a 72+ yard field goal is never good
      expect_equal(p[55:99], rep(0, 45))
    }
  }
})

test_that("today's kickers are better than 1999's, and indoors is easier", {
  yl <- c(10, 20, 30, 35)
  expect_true(all(
    fg_make_prob(2025, "dome", yl) > fg_make_prob(2000, "dome", yl)
  ))
  expect_true(all(
    fg_make_prob(2025, "outdoors", yl) > fg_make_prob(2000, "outdoors", yl)
  ))
  # from 40+ yards (at short range the two are within noise)
  yl <- c(25, 30, 35, 40)
  expect_true(all(
    fg_make_prob(2025, "dome", yl) > fg_make_prob(2025, "outdoors", yl)
  ))
})

test_that("field goal make probability handles missing inputs", {
  now <- nflreadr::most_recent_season()
  # no season is the most recent season, no roof is outdoors
  expect_equal(
    fg_make_prob(NULL, NULL, c(20, NA)),
    c(fg_make_prob(now, "outdoors", 20), NA)
  )
  expect_equal(
    fg_make_prob(c(NA, 2010), c(NA, "dome"), c(20, 20)),
    c(fg_make_prob(now, "outdoors", 20), fg_make_prob(2010, "dome", 20))
  )
  # open and closed retractable roofs are not outdoors
  expect_equal(
    fg_make_prob(2025, c("open", "closed"), 30),
    rep(fg_make_prob(2025, "dome", 30), 2)
  )
})
