
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



#' @title [textCriterion]
#' 
#' @param x returned object from function \link[glmtoolbox]{stepCriterion}
#' 
#' @keywords internal
#' @export
textCriterion <- function(x) {
  switch(
    EXPR = x$criterion, # ?stepCriterion.glmgee
    'P(Chisq>)(*)' = '$p$-values of selected predictors',
    AGPC = 'Akaike-type penalized Gaussian pseudo-likelihood criterion'
  )
}