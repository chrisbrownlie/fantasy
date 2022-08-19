test_that("team constructor works", {

  new_team <- new_team(players = 1:15,
                       captain = 8,
                       vc = 9,
                       bank = 0,
                       transfers = 0,
                       chips = 0)

  expect_s3_class(new_team, "team")
  expect_named(new_team, "id")
  expect_equal(nrow(new_team), 15)
  expect_equal(ncol(new_team), 1)
  expect_equal(attr(new_team, "captain"), 8)
  expect_equal(attr(new_team, "vc"), 9)
  expect_equal(attr(new_team, "submission_order"), 1:15)
  expect_equal(attr(new_team, "bank"), 0)
  expect_equal(attr(new_team, "transfers"), 0)
  expect_equal(attr(new_team, "chips"), 0)
})

test_that("team helper/validator works with known valid team", {

  valid_team_ids <- c(15, 430, 299, 10, 280, 283, 428, 305, 370, 465, 255, 398, 199, 391, 166)

  valid_team <- team(players = valid_team_ids,
                     captain = valid_team_ids[8],
                     vc = valid_team_ids[9],
                     bank = 0,
                     transfers = 1,
                     chips = all_chips())

  expect_s3_class(valid_team, "team")
  expect_named(valid_team, c("id", "name", "known_as", "position", "team", "points", "points_total", "form", "cost"))
  expect_equal(nrow(valid_team), 15)
  expect_equal(attr(valid_team, "captain"), 305)
  expect_equal(attr(valid_team, "vc"), 370)
  expect_equal(attr(valid_team, "submission_order"), valid_team_ids)
  expect_equal(attr(valid_team, "chips"), all_chips())
  expect_equal(attr(valid_team, "bank"), 0)
  expect_equal(attr(valid_team, "transfers"), 1)
})

test_that("team helper/validator works with random valid team", {

  players <- get_players()

  # Need to ensure no more than 3 from any one team
  teams <- sample(1:20, 7)
  keepers <- players$id[players$position == "GKP" & players$team == teams[1]][1:2]
  defs1 <- players$id[players$position == "DEF" & players$team == teams[2]][1:3]
  defs2 <- players$id[players$position == "DEF" & players$team == teams[3]][1:2]
  mids1 <- players$id[players$position == "MID" & players$team == teams[4]][1:3]
  mids2 <- players$id[players$position == "MID" & players$team == teams[5]][1:2]
  fwds1 <- players$id[players$position == "FWD" & players$team == teams[6]][1:2]
  fwds2 <- players$id[players$position == "FWD" & players$team == teams[7]][1]

  random_team_ids <- c(keepers[1],
                       defs1,
                       defs2[1],
                       mids1,
                       mids2[1],
                       fwds1,
                       keepers[2],
                       defs2[2],
                       mids2[2],
                       fwds2)


  valid_team <- team(players = random_team_ids,
                     captain = random_team_ids[8],
                     vc = random_team_ids[9],
                     bank = 0,
                     transfers = 1,
                     chips = all_chips())

  expect_s3_class(valid_team, "team")
  expect_named(valid_team, c("id", "name", "known_as", "position", "team", "points", "points_total", "form", "cost"))
  expect_equal(nrow(valid_team), 15)
  expect_equal(attr(valid_team, "captain"), random_team_ids[8])
  expect_equal(attr(valid_team, "vc"), random_team_ids[9])
  expect_equal(attr(valid_team, "submission_order"), random_team_ids)
})

test_that("team helper/validator fails with invalid teams", {

  players <- get_players()

  keepers <- players$id[players$position == "GKP"][round(runif(15, min = 1, max = length(players$id[players$position == "GKP"])))]

  # Incorrect team positions
  expect_error(
    team(players = keepers,
         captain = keepers[1],
         vc = keepers[2],
         bank = 0,
         transfers = 1,
         chips = all_chips()),
    regexp = "must select two goalkeepers"
  )
  # Incorrect starting XI positions
  expect_error(
    team(players = c(valid_team_ids[1:10],
                     valid_team_ids[12],
                     valid_team_ids[c(11,13:15)]),
         captain = valid_team_ids[1],
         vc = valid_team_ids[2],
         bank = 0,
         transfers = 1,
         chips = all_chips())
  )
  # Not 15 players
  expect_error(
    team(players = valid_team_ids[1:14],
         captain = valid_team_ids[2],
         vc = valid_team_ids[7],
         bank = 0,
         transfers = 1,
         chips = all_chips()),
    regexp = "must be length 15"
  )
  # Captain not in team
  expect_error(
    team(players = valid_team_ids,
         captain = keepers[3],
         vc = valid_team_ids[7],
         bank = 0,
         transfers = 1,
         chips = all_chips()),
    regexp = "captain is not one of the players in the team"
  )
  # Vice-captain not in team
  expect_error(
    team(players = valid_team_ids,
         captain = valid_team_ids[1],
         vc = keepers[2],
         bank = 0,
         transfers = 1,
         chips = all_chips()),
    regexp = "vice-captain is not one of the players in the team"
  )
  # Too many players from one team
  arsenal <- players %>% filter(team == 1)
  villa <- players %>% filter(team == 2)
  mixed_team <- c(arsenal$id[arsenal$position == "GKP"][1],
                  villa$id[villa$position == "DEF"][1:4],
                  arsenal$id[arsenal$position == "MID"][1:4],
                  villa$id[villa$position == "FWD"][1:2],
                  arsenal$id[arsenal$position == "GKP"][2],
                  villa$id[villa$position == "DEF"][5],
                  arsenal$id[arsenal$position == "MID"][5],
                  villa$id[villa$position == "FWD"][3])
  expect_error(
    team(players = mixed_team,
         captain = mixed_team[1],
         vc = mixed_team[11],
         bank = 0,
         transfers = 1,
         chips = all_chips()),
    regexp = "maximum of three players from any one club"
  )

  # Negative bank
  expect_error(
    team(players = valid_team_ids,
         captain = valid_team_ids[1],
         vc = valid_team_ids[2],
         bank = -10,
         transfers = 1,
         chips = all_chips()),
    regexp = "Bank balance cannot be negative"
  )
  # Invalid bank argument
  expect_error(
    team(players = valid_team_ids,
         captain = valid_team_ids[1],
         vc = valid_team_ids[2],
         bank = "loads",
         transfers = 0,
         chips = all_chips()),
    regexp = "single non-negative numeric"
  )

  # Warning that transfers will cost points
  expect_snapshot(
    {
    new_team <- team(players = valid_team_ids,
                     captain = valid_team_ids[1],
                     vc = valid_team_ids[2],
                     bank = 10,
                     transfers = -1,
                     chips = all_chips())
    }
  )
  # Invalid transfers argument
  expect_error(
    team(players = valid_team_ids,
         captain = valid_team_ids[1],
         vc = valid_team_ids[2],
         bank = 10,
         transfers = "none",
         chips = all_chips()),
    regexp = "single integer value"
  )

})
