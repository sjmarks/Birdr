test_that("That get regions works", {
  correct_result <- "Alice Keck Park"

  my_result <- get_hotspots(region = "US")

  expect_true(correct_result %in% my_result$name)
})
