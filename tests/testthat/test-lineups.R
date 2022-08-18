test_that("predicted lineups can be obtained from fantasyfootballscout.co.uk", {
  lineups <- get_predicted_lineups()

  expect_s3_class(lineups, "tbl_df")
  expect_equal(nrow(lineups), 220)
  expect_named(lineups, c("team", "selection", "player"))
  expect_equal(count(lineups, team)$n, rep(11, 20))
})
