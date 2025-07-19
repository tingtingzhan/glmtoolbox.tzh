
#' @title [backwardCriterion]
#' 
#' @param model,trace,... parameters of function \link[glmtoolbox]{stepCriterion},
#' **other than `direction`**
#' 
#' @keywords internal
#' @importFrom glmtoolbox stepCriterion
#' @export
backwardCriterion <- function(model, trace = FALSE, ...) {
  ret <- stepCriterion(model, direction = 'backward', trace = trace, ...)
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
    EXPR = x$criterion, 
    # ?glmtoolbox:::stepCriterion.glmgee
    'P(Chisq>)(*)' = {
      # `criterion = 'p-value'`
      '$p$-values of selected predictors'
    }, 
    QIC = {
      # `criterion = 'qic'`
      'quasilikelihood under the independence model criterion'
    },
    QICu = 'approximated and simplified quasilikelihood under the independence model criterion', # `criterion = 'qicu'`
    AGPC = 'Akaike-type penalized Gaussian pseudo-likelihood criterion', # `criterion = 'agpc'`
    SGPC = 'Schwarz-type penalized Gaussian pseudo-likelihood criterion', # `criterion = 'sgpc'`
    # additional options in ?stepCriterion.glm
    AIC = 'Akaike information criterion', # `criterion = 'aic'`
    BIC = 'Bayesian information criterion', # `criterion = 'bic'`
    'adj.R-squared' = 'Adjusted $R^2$', # `criterion = 'adjr2'`
    # additional options in ?stepCriterion.lm
    'prd.R-squared' = 'predicted $R^2$', # `criterion = 'prdr2'`,
    'Mallows\' CP' = '[Mallows\' $C_p$](https://en.wikipedia.org/wiki/Mallows%27s_Cp)', # `criterion = 'cp'`,
    # additional options in ?stepCriterion.overglm
    # [none]
    stop('write some more')
  ) # all tested good with \pkg{rmarkdown}
}