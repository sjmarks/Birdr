test_that("pickUSCode returns proper name and code", {
  correct_result <- tidyr::tibble(code = "US-CA-079", name = "San Luis Obispo")

  my_result <- pick_UScode(state = "California", county = "San Luis Obispo", ebirdkey = "rqksong3qcbm")

  expect_equal(my_result, correct_result)
})
