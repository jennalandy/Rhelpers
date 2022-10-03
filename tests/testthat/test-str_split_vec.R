test_that("str_split_vec() works", {
  expect_equal(
    str_split_vec(c('a.1','b.2','c.3'), '\\.', 2), c('1','2','3')
  )

  expect_equal(
    str_split_vec(c('a.1','b.2','c.3'), '\\.', 1), c('a','b','c')
  )

  expect_equal(
    str_split_vec(c('b..a.1','b..b.2','b..c.3'), '\\.', 3), c('a','b','c')
  )

  expect_error(str_split_vec(c(), 'a', 2))
  expect_error(str_split_vec(c(), 'a', 2))
  expect_error(str_split_vec(c('bac'), 'a', 2.3))
  expect_error(str_split_vec(c(1,2,3), 'a', 2))
  expect_error(str_split_vec(c('bac'), 1, 2))
})
