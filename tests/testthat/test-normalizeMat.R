test_that("noramlizeMat works", {
  mat <- matrix(runif(2*3), nrow = 2)

  mat_cols1 <- normalizeMat(mat, sum = 'cols')
  expect_equal(colSums(mat_cols1), c(1,1,1))


  mat_rows1 <- normalizeMat(mat, sum = 'rows')
  expect_equal(rowSums(mat_rows1), c(1,1))

  expect_error(normalizeMat(mat, sum = 'abc'))
})
