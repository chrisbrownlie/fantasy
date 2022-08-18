valid_team_ids <- c(15, 430, 299, 10, 280, 283, 428, 305, 370, 465, 255, 398, 199, 391, 166)

valid_team <- team(players = valid_team_ids,
                   captain = valid_team_ids[6],
                   vc = valid_team_ids[7],
                   bank = 0,
                   transfers = 1,
                   chips = all_chips())
