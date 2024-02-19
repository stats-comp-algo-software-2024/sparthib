test_that("analytical and BFGS coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = "linear", options = list(mle_solver = "LS"))
  via_bfgs_out <- hiper_glm(design, outcome, model = "linear", options = list(mle_solver = "BFGS"))

  ## compare analytical with BFGS
  expect_true(are_all_close(coef(via_linalg_out), coef(via_bfgs_out)))

})

test_that("grad and numerical grad", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design
  outcome <- data$outcome
  betas <- hiper_glm(design, outcome, model = "linear", options = list(mle_solver = "LS"))$coefficients
  grad <- grad_linear(design, outcome, betas)
  loglik <- loglik_linear(design, outcome, betas)
  approxgrad  <- approxgrad_linear(loglik, )


  expect_true(are_all_close( grad_linear, approxgrad_linear))

})
