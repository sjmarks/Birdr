test_that("That get_all_birds works", {
  correct_result <- "ostric2"

  my_result <- get_all_birds("6qqv2oh5smkm")

  expect_true(correct_result %in% my_result$speciesCode)
})
