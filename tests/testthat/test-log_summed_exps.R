test_that("A simple vector", {
  x = 1:10
  expect_equal(log_summed_exps(x), log(sum(exp(x))))
})

test_that("finite test", {
  x = 1:2000
  expect_true(is.finite(log_summed_exps(x)))
})
