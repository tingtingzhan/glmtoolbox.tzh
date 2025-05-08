
#' @title [backwardCriterion]
#' 
#' @param model,... parameters of function \link[glmtoolbox]{stepCriterion},
#' **other than `direction`**
#' 
#' @importFrom glmtoolbox stepCriterion
#' @export
backwardCriterion <- function(model, ...) {
  ret <- stepCriterion(model, direction = 'backward', ...)
  attr(ret, which = 'initial.fit') <- model
  class(ret) <- 'backwardCriterion'
  return(ret)
}


