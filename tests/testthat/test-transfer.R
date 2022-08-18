test_that("team_transfer works", {

  transferred_team <- team_transfer(valid_team,
                                    p_out = 465,
                                    p_in = 13)

  expect_setequal(transferred_team$id, c(valid_team$id[valid_team$id != 465], 13))
  expect_equal(attr(transferred_team, "bank"), 0.5)
})

test_that("team_transfer fails when not enough money", {

  expect_error(
    transferred_team <- team_transfer(valid_team,
                                      p_out = 465,
                                      p_in = 301),
    regexp = "do not have the funds"
  )
})

test_that("team_transfer fails when trying to transfer captain or vice captain", {

  expect_error(
    transferred_team <- team_transfer(valid_team,
                                      p_out = 283,
                                      p_in = 13),
    regexp = "cannot transfer out the team captain"
  )
  expect_error(
    transferred_team <- team_transfer(valid_team,
                                      p_out = 428,
                                      p_in = 13),
    regexp = "cannot transfer out the team vice-captain"
  )
})

test_that("show_transfer_targets works as expected", {

  players <- get_players()
  player_cost <- players$cost[players$id == 283]

  targets <- show_transfer_targets(x = valid_team,
                                   p_out = 283)

  expect_s3_class(targets, "tbl_df")
  expect_named(targets)
  expect_gt(nrow(targets), 240)
  expect_lte(max(targets$cost), player_cost)
  expect_true(all(!valid_team$id %in% targets$id))
})
