test_that("assign_role works", {

  new_team <- valid_team |>
    assign_role(15, "c") |>
    assign_role(299, "vc")

  expect_s3_class(new_team, "team")
  expect_equal(attr(new_team, "captain"), 15)
  expect_equal(attr(new_team, "vc"), 299)
  expect_identical(attr(new_team, "submission_order"), attr(valid_team, "submission_order"))

  # Check non-attribute equality
  raw_valid <- valid_team
  attributes(raw_valid) <- NULL
  new_valid <- new_team
  attributes(new_valid) <- NULL
  expect_identical(raw_valid, new_valid)
})

test_that("assign_role fails when necessary", {
  expect_error(
    assign_role(valid_team,
                999,
                "c"),
    regexp = "ID of a player in the team"
  )

  expect_error(
    assign_role(valid_team,
                428,
                "c"),
    regexp = "already vice-captain so cannot be assigned as captain"
  )

  expect_error(
    assign_role(valid_team,
                283,
                "vc"),
    regexp = "already captain so cannot be assigned as vice-captain"
  )

  expect_error(
    assign_role(valid_team,
                428,
                "other"),
    regexp = "must be one of 'c' or 'vc'"
  )

  expect_error(
    assign_role(iris,
                428,
                "other"),
    regexp = "can only be used with an object of class"
  )
})
