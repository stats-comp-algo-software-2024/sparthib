#' @export
predict.hglm <- function (hglm) {
  warning("Yet to be implemented")
}

#' @export
coef.hglm <- function (hglm) {
  return(hglm$coefficients)
}

#' @export
vcov.hglm <- function (hglm) {
  warning("Yet to be implemented")
}

#' @export
print.hglm <- function (hglm) {
  print("Output of `hiper_glm()")
}
