library(dplyr)
test_data = tibble(x = 1:4, y = rep(1:2, 2), z = 1:4)
test_data_nas = bind_rows(test_data, tibble(x = c(5:6, NA, NA, NA, NA), y = c(NA, NA, 1:2, NA, NA), z = 5:10))

test_that("error checking works", {
  expect_error(freq(test_data, weights = z), "No variables supplied.")
  expect_error(freq(test_data_nas, x, y, xna, weights = z), "More than 2 variables supplied.")
  expect_error(freq(test_data, x), "No weights supplied.")

  test_neg_weight = tibble(x = 1:4, y = c(1:3, -1))
  expect_error(freq(test_neg_weight, x, weights = y), "Cannot use negative weights.")
})

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

test_that("two variable freq works", {
  expect_equal(
    freq(test_data, x, y, weights = z),
    tibble(x = 1:4, y = rep(1:2, 2), freq = 1:4) %>% group_by(x)
  )

  expect_equal(
    freq(test_data, y, x, weights = z),
    tibble(y = c(1, 1, 2, 2), x = c(1, 3, 2, 4), freq = c(1, 3, 2, 4)) %>%
      group_by(y)
  )

  expect_equal(
    freq(test_data_nas, x, y, weights = z),
    tibble(x = 1:4, y = c(1:2, 1:2), freq = 1:4) %>%
      group_by(x)
  )

  expect_equal(
    freq(test_data_nas, x, y, weights = z, use_na = "drop"),
    tibble(x = 1:4, y = c(1:2, 1:2), freq = 1:4) %>%
      group_by(x)
  )

  expect_equal(
    freq(test_data_nas, x, y, weights = z, use_na = "show"),
    tibble(
      x = c(1:6, NA, NA, NA), y = c(1:2, 1:2, NA, NA, 1:2, NA),
      freq = c(1:8, 19)
    ) %>%
      group_by(x)
  )

  expect_equal(
    freq(test_data_nas, y, x, weights = z),
    tibble(y = c(1, 1, 2, 2), x = c(1, 3, 2, 4), freq = c(1, 3, 2, 4)) %>%
      group_by(y)
  )

  expect_equal(
    freq(test_data_nas, y, x, weights = z, use_na = "drop"),
    tibble(y = c(1, 1, 2, 2), x = c(1, 3, 2, 4), freq = c(1, 3, 2, 4)) %>%
      group_by(y)
  )

  expect_equal(
    freq(test_data_nas, y, x, weights = z, use_na = "show"),
    tibble(
      y = c(1, 1, 1, 2, 2, 2, NA, NA, NA), x = c(1, 3, NA, 2, 4, NA, 5, 6, NA),
      freq = c(1, 3, 7, 2, 4, 8, 5, 6, 19)
    ) %>%
      group_by(y)
  )
})
