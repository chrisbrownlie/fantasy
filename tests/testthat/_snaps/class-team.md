# team helper/validator fails with invalid teams

    Code
      new_team <- team(players = valid_team_ids, captain = valid_team_ids[1], vc = valid_team_ids[
        2], bank = 10, transfers = -1, chips = all_chips())
    Warning <rlang_warning>
      This action is not covered by free transfers so will cost 4 points.

