find_mle_pseudoinv <- function(design, outcome){
  betas <- solve(t(design) %*% design, t(design) %*% outcome)
  return(betas)
}




min_residuals <- function(data = df, par) {

  design <- as.matrix(df[, names(df) != "outcome"])
  res <- design%*%par - as.matrix(df$outcome)
  res_sq <- t(res)%*%res
  return(res_sq)
}


find_mle_bfgs <- function(design, outcome, tol=1e-6){


  design <- cbind(rep(1, dim(design)[1]), design)
  df <- as.data.frame(design)
  par <- as.matrix(rep_len(0.001, ncol(design)))
  df$outcome <- outcome

  result <- stats::optim(par = par, fn=min_residuals, data=df,
               method = "BFGS")
  return(result$par)


}

