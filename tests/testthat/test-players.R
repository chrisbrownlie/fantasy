test_that("get_players works", {
  players <- get_players()

  expect_gte(nrow(players), 570)
  expect_named(players)
})
