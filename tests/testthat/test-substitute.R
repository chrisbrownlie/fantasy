test_that("team_substitution works", {

  subbed_team <- team_substitute(valid_team,
                                 15,
                                 398)

  expect_setequal(valid_team$id, subbed_team$id)
  expect_setequal(attr(valid_team, "submission_order"), attr(subbed_team, "submission_order"))
  expect_equal(subbed_team$id[1], 398)
})

test_that("captain cannot be substituted", {

  expect_error(
    team_substitute(valid_team,
                    p1 = valid_team_ids[6],
                    p2 = valid_team_ids[14]),
    regexp = "is the team captain"
  )
})

test_that("vice-captain cannot be substituted", {
  expect_error(
    team_substitute(valid_team,
                    p1 = valid_team_ids[7],
                    p2 = valid_team_ids[14]),
    regexp = "is the team vice-captain"
  )
})

test_that("cannot violate positions when substituting", {

  expect_error(
    team_substitute(valid_team,
                    p1 = valid_team_ids[4],
                    p2 = valid_team_ids[12]),
    regexp = "must select.*?goalkeeper in your starting XI"
  )
})
