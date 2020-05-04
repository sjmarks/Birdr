test_that("That bird_day works", {
  correct_result <- "easowl1"
  
  my_result <- bird_day(year = "1999", month = "1", day = "28", key = "888uifcqohgh")
  
  expect_true(correct_result %in% my_result$speciesCode)
})
