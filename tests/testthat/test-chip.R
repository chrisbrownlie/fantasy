test_that("activate_chip works", {

  freehit_team <- activate_chip(valid_team, "freehit")
  wildcard_team <- activate_chip(valid_team, "wildcard")
  triplecap_team <- activate_chip(valid_team, "3xc")
  bboost_team <- activate_chip(valid_team, "bboost")

  expect_equal(attr(freehit_team, "chip"), all_chips(freehit = FALSE))
  expect_equal(attr(wildcard_team, "chip"), all_chips(wildcard = FALSE))
  expect_equal(attr(triplecap_team, "chip"), all_chips(triple_cap = FALSE))
  expect_equal(attr(bboost_team, "chip"), all_chips(bboost = FALSE))

  expect_identical(team_to_df(freehit_team), team_to_df(valid_team))
  expect_identical(team_to_df(wildcard_team), team_to_df(valid_team))
  expect_identical(team_to_df(triplecap_team), team_to_df(valid_team))
  expect_identical(team_to_df(bboost_team), team_to_df(valid_team))

})

test_that("activate_chip fails when necessary", {
  expect_error(
    activate_chip(valid_team,
                  "not a chip"),
    regexp = 'must be one of: "3xc", "bboost", "wildcard" or "freehit"'
  )

  freehit_team <- activate_chip(valid_team, "freehit")
  expect_error(
    activate_chip(freehit_team,
                  "freehit"),
    regexp = "already activated for this gameweek"
  )
  expect_error(
    activate_chip(freehit_team,
                  "wildcard"),
    regexp = 'cannot activate "wildcard" as "freehit" is already active'
  )

  pre_used_team <- team(players = valid_team_ids,
                        captain = valid_team_ids[6],
                        vc = valid_team_ids[7],
                        bank = 0,
                        transfers = 1,
                        chips = all_chips(freehit = NA, bboost = NA))

  expect_error(
    activate_chip(pre_used_team,
                  "freehit"),
    regexp = "has already been used in a previous gameweek"
  )
  expect_error(
    activate_chip(pre_used_team,
                  "bboost"),
    regexp = "has already been used in a previous gameweek"
  )
})

test_that("deactivate_chip and activate_chip are symmetrical", {

  de_freehit_team <- valid_team %>%
    activate_chip("freehit") %>%
    deactivate_chip("freehit")

  expect_identical(valid_team, de_freehit_team)
})

test_that("deactivate_chip fails when necessary", {
  expect_error(
    deactivate_chip(valid_team,
                    "not a chip"),
    regexp = 'must be one of: "3xc", "bboost", "wildcard" or "freehit"'
  )
  pre_used_team <- team(players = valid_team_ids,
                        captain = valid_team_ids[6],
                        vc = valid_team_ids[7],
                        bank = 0,
                        transfers = 1,
                        chips = all_chips(freehit = NA, bboost = NA, wildcard = FALSE))
  expect_error(
    deactivate_chip(pre_used_team,
                    "freehit"),
    regexp = '"freehit" has already been used in a previous gameweek'
  )
  expect_error(
    deactivate_chip(pre_used_team,
                    "3xc"),
    regexp = "has not been activated for this gameweek"
  )
})
