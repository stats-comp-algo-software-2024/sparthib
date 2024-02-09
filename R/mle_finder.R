find.mle.pseudoinv <- function(design, outcome){
  sigma_inv <- t(design) %*% design
  #print(sigma_inv)
  betas <- solve(t(design) %*% design, t(design) %*% outcome)
  return(betas)
}

find.mle.bfgs <- function(design, outcome, tol=1e-6){
  betas <- rep(0, ncol(design))

  #sigma_inv <- t(design) %*% design

  loglik <- loglik.linear(design, outcome, betas)
  grad <- grad.linear(design, outcome, betas)

  hessian <- t(design) %*% design

  n <- dim(design)[[2]]
  H <- diag(n)

  serach_direction <- -H %*% grad

  rho_backtracking <- 0.95
  c <- 0.9


  n_iter <- 0

  while(norm(grad, "2") > tol){
    search_direction <- -H %*% grad
    step_size <- 1

    lhs <- loglik.linear(design, outcome, betas + step_size * search_direction)
    rhs <- loglik + c * step_size %*% t(grad) %*% search_direction

    print(lhs)
    print(rhs)

    while (lhs > rhs) {

      step_size <- rho_backtracking * step_size
      # update
      lhs <- loglik.linear(design, outcome, betas + (step_size * search_direction))
      rhs <- loglik + c * step_size %*% t(grad) %*% search_direction

    }

    betas_new <- betas + step_size * search_direction

    grad_new <- grad.linear(design, outcome, betas_new)

    s <- betas_new - betas
    y <- grad_new - grad

    rho <- 1 / (t(y) %*% s) %>%
      c()
    H <- (diag(n) - rho * s %*% t(y)) %*% H %*% (diag(n) - rho * y %*% t(s)) + rho * s %*% t(s)

    betas <- betas_new
    grad <- grad_new

    n_iter <- n_iter + 1


  }

  print(n_iter)
  return(betas)

}
