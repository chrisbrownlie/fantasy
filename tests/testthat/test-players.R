test_that("get_players works", {
  players <- get_players()

  expect_equal(nrow(players), 580)
  expect_named(players)
})
