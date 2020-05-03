# test for helper function get_notable_obs, helper to get_notable_chart

test_that("get_notable_obs returns proper tibble headers", {
  correct_result <- c("speciesCode", "comName", "sciName",
                      "locId", "locName", "obsDt", "howMany",
                      "lat", "lng", "obsValid", "obsReviewed", "locationPrivate", "subId")

  test_getnotable <- get_notable_obs(region_code = "US-CA-079", ebirdkey = "rqksong3qcbm", back = 2)

  my_result <- names(test_getnotable)

  expect_equal(my_result, correct_result)
})
