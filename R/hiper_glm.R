#' @export

hiper_glm <- function(design, outcome, model = "linear", options ) {
  hglm <- list()
  class(hglm) <- "hglm"
  supported_model <- c("linear")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }
  if(options$mle_solver == "LS"){
    betas <- find_mle_pseudoinv(design,outcome)
  }else if(options$mle_solver == "BFGS"){
    betas <- find_mle_bfgs(design,outcome)
  }

  hglm$coefficients <- betas
  return(hglm)


  }


