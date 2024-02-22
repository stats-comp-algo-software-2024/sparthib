# Implement functions to calculate the log-likelihood and its gradient
# under the logistic model.


loglik_logistic <- function(design, outcome, betas){
  outcome <- as.matrix(outcome)
  betas <- as.matrix(betas)

  mat <- t(outcome)%*%design%*%betas

  res <- 0
  for( i in seq_len(dim(design)[1])){
    res <- res + log(1 + exp(t(design[i,])%*%betas))
  }
  loglik <- mat - res
  return(loglik)
}

numerical_grad_logistic <- function(design, outcome, betas){
  design <- as.matrix(design)
  outcome <- as.matrix(outcome)
  betas<- as.matrix(betas)
  num <- exp(design%*%betas)
  prob <- num / (1 + num)
  grad <-  t(design)%*%(outcome - prob )
  return (grad)

}

hessian_logistic <- function(design, outcome, betas){
  num <- exp(design%*%betas)
  prob <- num / (1 + num)
  probs <- as.vector(prob*(1- prob))
  W <- diag(probs)
  return( - t(design)%*%W%*%design)
}


find_mle_nr <- function(design, outcome, conv = 1e-6, max_iter = 100){


  betas_list <- list()
  betas_list[[1]] <- rep(0, ncol(design))
  betas_list[[2]] <- rep(0.05, ncol(design))

  design <- as.matrix(design)
  outcome <- as.matrix(outcome)

  i = 1
  while( dist( t(cbind( betas_list[[i]],   betas_list[[i + 1 ]])) )[1] > conv) {


     hess <- hessian_logistic(design, outcome, as.matrix(betas_list[[i+1]]))
     grad <- numerical_grad_logistic(design, outcome, as.matrix(betas_list[[i+1]]))

     betas_list[[i+2]] <- betas_list[[i+1]] - solve(hess)%*%grad
     i= i + 1
     if(i == max_iter){
       break
       print("convergence not achieved")
     }

  }

  return(as.vector(betas_list[[length(betas_list)]]))

  }


