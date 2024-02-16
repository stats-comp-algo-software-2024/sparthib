test_that("are_all_close() case1  ", {
  expect_true(are_all_close(1, 1, abs_tol = 1e-6, rel_tol = 1e-6) )

})

test_that("are_all_close() case2 ", {
  expect_false(are_all_close(1, 1+1e-5, abs_tol = 1e-6, rel_tol = 1e-6))

})

test_that("are_all_close() case3 ", {
  expect_false(are_all_close(1 + 1e-5, 1, abs_tol = 1e-6, rel_tol = 1e-6))
})

