test_that("get regions works", {
  correct_result <- "Uganda"

  my_result <- get_regions(region_type = "country", key = "6qqv2oh5smkm")

  expect_true(correct_result %in% my_result$name)
})
