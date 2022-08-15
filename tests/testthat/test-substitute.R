test_that("team_substitution works", {

  valid_team_ids <- c(15, 430, 299, 10, 280, 283, 428, 305, 370, 465, 255, 398, 199, 391, 166)

  valid_team <- team_selection(players = valid_team_ids,
                               captain = valid_team_ids[6],
                               vc = valid_team_ids[7])

  subbed_team <- team_substitute(valid_team,
                                 15,
                                 398)

  expect_setequal(valid_team$id, subbed_team$id)
  expect_setequal(attr(valid_team, "submission_order"), attr(subbed_team, "submission_order"))
  expect_equal(subbed_team$id[1], 398)
})

test_that("captain cannot be substituted", {

  valid_team_ids <- c(15, 430, 299, 10, 280, 283, 428, 305, 370, 465, 255, 398, 199, 391, 166)

  valid_team <- team_selection(players = valid_team_ids,
                               captain = valid_team_ids[6],
                               vc = valid_team_ids[7])

  expect_error(
    team_substitute(valid_team,
                    p1 = valid_team_ids[6],
                    p2 = valid_team_ids[14]),
    regexp = "is the team captain"
  )
})

test_that("vice-captain cannot be substituted", {

  valid_team_ids <- c(15, 430, 299, 10, 280, 283, 428, 305, 370, 465, 255, 398, 199, 391, 166)

  valid_team <- team_selection(players = valid_team_ids,
                               captain = valid_team_ids[6],
                               vc = valid_team_ids[7])

  expect_error(
    team_substitute(valid_team,
                    p1 = valid_team_ids[7],
                    p2 = valid_team_ids[14]),
    regexp = "is the team vice-captain"
  )
})

test_that("cannot violate positions when substituting", {

  valid_team_ids <- c(15, 430, 299, 10, 280, 283, 428, 305, 370, 465, 255, 398, 199, 391, 166)

  valid_team <- team_selection(players = valid_team_ids,
                               captain = valid_team_ids[6],
                               vc = valid_team_ids[7])

  expect_error(
    team_substitute(valid_team,
                    p1 = valid_team_ids[4],
                    p2 = valid_team_ids[12]),
    regexp = "must select.*?goalkeeper in your starting XI"
  )
})
