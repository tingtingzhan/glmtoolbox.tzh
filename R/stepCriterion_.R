
#' @title \link[glmtoolbox]{stepCriterion} with returned class
#' 
#' @param model,... see function \link[glmtoolbox]{stepCriterion}
#' 
#' @importFrom glmtoolbox stepCriterion
#' @export
stepCriterion_ <- function(model, ...) {
  ret <- stepCriterion(model, ...)
  class(ret) <- 'stepCriterion'
  return(ret)
}


