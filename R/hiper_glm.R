#' @export
hiper_glm <- function (outcome, design) {
  # TODO: Maximize likelihood
  hglm <- list()
  class(hglm) <- "hglm"
  return(hglm)
}


## ITERATIVELY WEIGHTED LEAST SQUARES
# stats::optim
# method = BFGS
# usethis::use_testthat()
