#' @export

hiper_glm <- function(design, outcome, model = "linear", option ) {
  hglm <- list()
  class(hglm) <- "hglm"
  supported_model <- c("linear", "logistic", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }

  if(model == "linear"){
    if(option$mle_solver == "LS"){
      betas <- find_mle_pseudoinv(design,outcome)
    }else if(option$mle_solver == "BFGS"){
      betas <- find_mle_bfgs(design,outcome, func= loglik_linear, grad= grad_linear)
    }
    hglm$coefficients <- betas

  }

  if(model == "logit" || model == "logistic"){
    if(option$mle_solver == "NR"){
      betas <- find_mle_nr(design,outcome)
    }else if(option$mle_solver == "BFGS"){
      betas <- find_mle_bfgs(design,outcome, func=loglik_logistic, grad=numerical_grad_logistic)
    }
    hglm$coefficients <- betas

  }


  return(hglm)


  }


