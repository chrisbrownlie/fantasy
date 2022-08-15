test_that("boostrap-static can be queried", {
  general <- query_general_data()

  expect_length(general, 8)
  expect_type(general, "list")
  expect_named(general, c("events", "game_settings", "phases", "teams", "total_players", "elements", "element_stats", "element_types"))
})
