## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
library(xfun)
test_that("get_and_or_list correct forms word lists", {
  word_vec <- xfun::numbers_to_words(1:4)
  expect_equal(get_and_or_list(word_vec), "one, two, three, and four")
  expect_equal(get_and_or_list(word_vec, conjunction = "and"),
               "one, two, three, and four")
  expect_equal(get_and_or_list(word_vec, conjunction = "or"),
               "one, two, three, or four")
  expect_equal(get_and_or_list(word_vec, punct = FALSE),
               "one, two, three and four")
  expect_equal(get_and_or_list(word_vec[1:2]),
               "one and two")
  expect_equal(get_and_or_list(word_vec[1:2], conjunction = "and"),
               "one and two")
  expect_equal(get_and_or_list(word_vec[1:2], conjunction = "or"),
               "one or two")
  expect_equal(get_and_or_list(word_vec[1]),
               "one")
  expect_equal(get_and_or_list(word_vec[1], conjunction = "and"),
               "one")
  expect_equal(get_and_or_list(word_vec[1], conjunction = "or"),
               "one")
})
