test_that("authentication works", {

  expect_snapshot(
    authenticate(email = httr2::secret_decrypt("tPIPG8HBhvYfTY2j5dEEDfGdc2VcvclMi-TANtfDclBLYGiNYu4198rneTk",
                                               "FANTASY_KEY"),
                 password = httr2::secret_decrypt("-SJx4AhW09y51joLmPSEWWAf5ezKkVX0EluSeQ",
                                                  "FANTASY_KEY")),
    transform = function(x) gsub(pattern = "[[:alnum:][:punct:]]+@[[:alnum:][:punct:]]+",
                                 replacement = "[login]",
                                 x)
  )

})
