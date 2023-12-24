#project3_tests
# While variables were not specified in the problem instructions
# Tests will reference the problem number and expected variable name
# Much of this project focuses on visualizations which are not tested
# by this script.

library(pacman)
p_load(testthat)
test_that("Data Cleaning P6, Variable Called books. Checking Number of Rows.", {
  expect_equal(nrow(books), 20116)
})

test_that("Data Cleaning P6, Variable Called books. Checking Number of Columns", {
  expect_equal(ncol(books), 17)
})

test_that("Data Cleaning P6, Variable Called books. Checking year column.", {
  expect_equal(books$year[5], 2003)
})

test_that("Data Cleaning P6, Variable Called books. Checking year column.", {
  expect_equal(books$year[5], 2003)
})

test_that("Data Cleaning P6, Variable Called books. Checking for no large books.", {
  expect_equal(sum(books$pages[books$pages>=1200]), 0)
})

test_that("Data Cleaning P6, Variable Called books. Checking for 1990 first year.", {
  expect_equal(min(books$year), 1990)
})
test_that("Data Cleaning P6, Variable Called books. Checking for 2020 last year.", {
  expect_equal(max(books$year), 2020)
})

test_that("Data Analysis P5, Variable Called book_publishers. Checking Random House total_books", {
  expect_equal(book_publishers$total_books[1], 2607)
})

test_that("Data Analysis P5, Variable Called book_publishers. Checking Harper Collins cum_count", {
  expect_equal(book_publishers$cum_count[2], 4119)
})

test_that("Data Analysis P5, Variable Called book_publishers. Checking Simon rel_freq", {
  expect_equal(book_publishers$rel_freq[3], .109, tolerance = 1e-3)
})

test_that("Data Analysis P5, Variable Called book_publishers. Checking MacMillan cum_freq", {
  expect_equal(book_publishers$cum_freq[4], .874, tolerance = 1e-3)
})


test_that("Data Analysis P8, Variable Called by_year. Checking 1990 total_books", {
  expect_equal(by_year$total_books[1], 340)
})

test_that("Data Analysis P8, Variable Called by_year. Checking 1995 avg_rating", {
  expect_equal(by_year$avg_rating[6], 3.97, tolerance = 1e-2)
})

test_that("Data Analysis P8, Variable Called by_year. Checking 1990 total_", {
  expect_equal(by_year$total_books[1], 340)
})


