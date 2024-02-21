test_that("linalg and optim least-sq coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = 'linear', option = list(mle_solver = 'LS'))
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'linear', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_linalg_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})


test_that("analytical vs approx grad linear ", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  betas <- c(1,2,3,4)
  approx <- approxgrad_linear(function(x) loglik_linear(design, outcome, x),
                     betas)
  analytical <- grad_linear(design,outcome, betas)
  expect_true(are_all_close(
    approx, analytical, abs_tol = 1e-2, rel_tol = 1e-2
  ))
  })


test_that("analytical vs approx grad logistic ", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  betas <- c(1,2,3,4)
  approx <- approxgrad_linear(function(x) loglik_logistic(design, outcome, x),
                              betas)
  analytical <- numerical_grad_logistic(design,outcome, betas)
  expect_true(are_all_close(
    approx, analytical, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})



test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = 'logit', option = list(mle_solver = 'NR'))
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'logit', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
