test_that("That get regions works", {
  correct_result <- "L718307"

  my_result <- get_hotspots(region = "US", key = "888uifcqohgh")

  expect_true(correct_result %in% my_result$locID)
})
