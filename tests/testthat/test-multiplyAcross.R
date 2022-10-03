test_that("multiplyAcross works", {
  mat = matrix(runif(10*5), nrow = 5)

  row_vec = 1:10
  row_multiplied = multiplyAcross(mat, row_vec, across = "rows")

  # spot check
  expect_equal(mat[1, 10]*10, row_multiplied[1,10])
  expect_equal(mat[2, 6]*6, row_multiplied[2,6])
  expect_equal(mat[5, 5]*5, row_multiplied[5,5])

  col_vec = 1:5
  col_multiplied = multiplyAcross(mat, col_vec, across = "cols")

  # spot check
  expect_equal(mat[1, 10]*1, col_multiplied[1,10])
  expect_equal(mat[2, 6]*2, col_multiplied[2,6])
  expect_equal(mat[5, 5]*5, col_multiplied[5,5])
})
