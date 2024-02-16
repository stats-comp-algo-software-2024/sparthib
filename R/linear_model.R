loglik_linear <- function(design, outcome, betas){
  mat <- outcome - design %*% betas
  loglik <- 0.5*t(mat) %*% (mat)
  return(loglik)
}

grad_linear <- function(design, outcome, betas){
  grad <- -t(design) %*% (outcome - design %*% betas)
  return(grad)
}

approxgrad_linear <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for (i in 1:length(x)){
    x_plus <- x
    x_plus[[i]] <- x_plus[[i]] + dx

    x_minus <- x
    x_minus[[i]] <- x_minus[[i]] - dx

    numerical_grad[[i]] <- (func(x_plus) - func(x_minus))/(2*dx)
  }

  return(numerical_grad)
}
gaussian_logp <- function(x, Sigma_inv) {
  logp <- - .5 * t(x) %*% Sigma_inv %*% x
  return(logp)
}
