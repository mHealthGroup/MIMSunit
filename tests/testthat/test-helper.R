test_that("parse_epoch_string works", {
  n1 <- parse_epoch_string("1 min", sr = 50)
  expect_equal(n1, 3000)
  n2 <- parse_epoch_string("5 secs", sr = 10)
  expect_equal(n2, 50)
  n3 <- parse_epoch_string("2 hours", sr = 10)
  expect_equal(n3, 72000)
})
