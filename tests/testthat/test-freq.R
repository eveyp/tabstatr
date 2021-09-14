library(dplyr)
test_data = tibble(x = 1:4, y = rep(1:2, 2), z = 1:4)
test_data_nas = bind_rows(test_data, tibble(x = c(5:6, NA, NA, NA, NA), y = c(NA, NA, 1:2, NA, NA), z = 5:10))

test_that("one variable freq works", {
  expect_equal(
    freq(test_data, x, weights = z),
    tibble(x = 1:4, freq = 1:4)
  )

  expect_equal(
    freq(test_data, y, weights = z),
    tibble(y = 1:2, freq = c(4, 6))
  )

  expect_equal(
    freq(test_data_nas, x, weights = z),
    tibble(x = 1:6, freq = 1:6)
  )

  expect_equal(
    freq(test_data_nas, x, weights = z, use_na = "drop"),
    tibble(x = 1:6, freq = 1:6)
  )

  expect_equal(
    freq(test_data_nas, x, weights = z, use_na = "show"),
    tibble(x = c(1:6, NA), freq = c(1:6, 34))
  )

  expect_equal(
    freq(test_data_nas, y, weights = z),
    tibble(y = 1:2, freq = c(11, 14))
  )

  expect_equal(
    freq(test_data_nas, y, weights = z, use_na = "drop"),
    tibble(y = 1:2, freq = c(11, 14))
  )

  expect_equal(
    freq(test_data_nas, y, weights = z, use_na = "show"),
    tibble(y = c(1:2, NA), freq = c(11, 14, 30))
  )
})
