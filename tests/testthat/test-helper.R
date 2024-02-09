test_that("are_all_close() works ", {
  expect_true(are_all_close(1, 1, abs_tol = 1e-6, rel_tol = 1e-6) )
  expect_false(are_all_close(1, 1+1e-5, abs_tol = 1e-6, rel_tol = 1e-6))
  expect_false(are_all_close(1 + 1e-5, 1, abs_tol = 1e-6, rel_tol = 1e-6))
})


# Test the are_all_close() function. Write a test to cover at least three cases:
# the function 1) correctly returns TRUE, 2) correctly returns FALSE because the relative error
# is above rel_tol, and 3) correctly returns FALSE because the absolute error is above abs_tol.
# Make sure that the logic of the test is clear and easy to understand — tests are there to clarify
# and validate the code's behavior; unreadable tests defeat the purpose.
# <br> Remark: This is just an exercise, but in general there is nothing strange about
# testing a function you use to test another function. It is perfectly reasonable to consider
# testing any functions complicated enough to potentially contain insiduous bugs.
