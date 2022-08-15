test_that("fixtures data can be queried", {
  fixtures <- query_fixtures_data()

  expect_s3_class(fixtures, "tbl_df")
  expect_length(fixtures, 380)
})

test_that("fixture list can be obtained", {
  fixtures <- get_fixture_list()

  expect_s3_class(fixtures, "tbl_df")
  expect_equal(nrow(fixtures), 380)
  expect_named(fixtures, c("id",
                           "gameweek",
                           "finished",
                           "kickoff_time",
                           "minutes",
                           "home_team",
                           "home_score",
                           "home_difficulty",
                           "away_team",
                           "away_score",
                           "away_difficulty"))
})

test_that("fixture stats can be obtained", {
  fixture_stats <- get_fixture_stats()

  expect_s3_class(fixture_stats, "tbl_df")
  expect_gt(nrow(fixture_stats), 812)
  expect_named(fixture_stats, c("fixture_id", "stat", "team", "player", "value"))

})
