test_that("percentile works", {
  expect_equal(percentile(9,10,1), 10)
  expect_equal(percentile(99,100,1), 1)
  expect_equal(percentile(93333,100000,0), 6.67)
  expect_equal(percentile(86,100,5), 15)
  expect_equal(percentile(123,1000,10), 90)
  expect_equal(percentile(1,10000,5), 100)
  expect_equal(percentile(9999,10000,0), 0.01)

  expect_error(percentile(2,1,1))
  expect_error(percentile(1,2,"some"))
  expect_error(percentile(1,2,3))
})
