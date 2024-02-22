find_mle_pseudoinv <- function(design, outcome){
  betas <- solve(t(design) %*% design, t(design) %*% outcome)
  return(betas)
}


min_residuals <- function(data = df, par) {

  design <- as.matrix(df[,1: ncol(df)-1])
  res <- design%*%par - as.matrix(df$outcome)
  res_sq <- t(res)%*%res
  return(res_sq)
}

find_mle_bfgs <- function(design, outcome, func = loglik_logistic, grad = numerical_grad_logistic ){
  mle = stats::optim(par = rep(0, ncol(design)),
                     fn = func , gr = grad,
                     design = design, outcome = outcome,
                     control = list(fnscale = -1),
                     method = "BFGS")
  return(mle$par)
}

