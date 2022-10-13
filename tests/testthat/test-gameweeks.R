test_that("gameweek info can be extracted", {
  gw_info <- get_gameweek_info()

  expect_s3_class(gw_info, "tbl_df")
  expect_named(gw_info)
  expect_equal(nrow(gw_info), 38)
})

test_that("gw arg works for gameweek info", {
  second_week <- get_gameweek_info(gw = 2)

  expect_s3_class(second_week, "tbl_df")
  expect_named(second_week)
  expect_equal(nrow(second_week), 1)
  expect_equal(second_week$gameweek, 2)
  expect_equal(second_week$highest_scoring_player, "Jesus")
})
