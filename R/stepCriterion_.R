
#' @title [backwardCriterion]
#' 
#' @param model,trace,... parameters of function \link[glmtoolbox]{stepCriterion},
#' **other than `direction`**
#' 
#' @examples
#' library(glmtoolbox)
#' # ?stepCriterion
#' data(depression)
#' m = glmgee(depressd ~ visit * group, id=subj, family=binomial(probit), 
#'  corstr="AR-M-dependent", data=depression)
#' list(
#'  'back' = m |> 
#'    backwardCriterion()
#' ) |> fastmd::render2html()
#' 
#' @keywords internal
#' @importFrom glmtoolbox stepCriterion
#' @export
backwardCriterion <- function(model, trace = FALSE, ...) {
  ret <- model |>
    stepCriterion(direction = 'backward', trace = trace, ...)
  # glmtoolbox:::stepCriterion.glmgee
  # does not keep a lot of tuning parameters :(
  attr(ret, which = 'initial.fit') <- model
  class(ret) <- c('backwardCriterion', class(ret)) |>
    unique.default()
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
      '$p>.05$'
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






#' @method as.matrix backwardCriterion
#' @export
as.matrix.backwardCriterion <- function(x, ...) {
  
  # '\u274c' # unicode 'Cross Mark'
  # '\U1f6ab' # unicode 'No Entry Sign'
  
  z <- list(
    x |> attr(which = 'initial.fit', exact = TRUE),
    x$final.fit
  ) |> 
    lapply(FUN = ecip)
  
  m <- list(
    z[[1L]] |> # `initial` model, only print p-values
      as.matrix(type = 'p_only'), # ecip::as.matrix.ecip
    z[[2L]] |> # `final` model
      as.matrix(type = 'ncol1') # ecip::as.matrix.ecip
  )
  
  r <- m |>
    lapply(FUN = rownames) |>
    unlist(use.names = FALSE) |>
    unique.default() # do *not* ?base::sort !!!
  
  out <- array(data = '\u274c', dim = c(length(r), 2L), dimnames = list(
    r, 
    paste(c('(Initial)', '(Final)'), vapply(m, FUN = colnames, FUN.VALUE = ''), sep = '\n')
  ))
  for (i in 1:2) {
    out[match(x = rownames(m[[i]]), table = r), i] <- m[[i]]
  }
  
  attr(out, which = 'row.title') <- x$final.fit |> 
    endpoint() |> 
    deparse1()
  return(out)
  
}



#' @importFrom flextable as_flextable add_footer_lines color
#' @importFrom ftExtra as_paragraph_md colformat_md 
#' @export
as_flextable.backwardCriterion <- function(
    x, 
    row.title = z |> attr(which = 'row.title', exact = TRUE),
    ...
) {
  z <- as.matrix.backwardCriterion(x, ...)
  z |>
    as_flextable( # fastmd:::as_flextable.matrix
      row.title = row.title,
    ) |>
    color(j = 2L, color = 'grey60', part = 'all') |>
    add_footer_lines(values = c(
      x |> 
        textCriterion() |> 
        sprintf(fmt = '\u274c: predictor(s) removed by %s') #|>
        #as_paragraph_md()
    )) #|>
    #colformat_md(part = 'all') # why does not work??

}


if (FALSE) {
  '$p>.3$' |> ftExtra::as_paragraph_md() # error
}


#' @export
md_.backwardCriterion <- function(x, xnm, ...) {
  
  z1 <- x |>
    attr(which = 'initial.fit', exact = TRUE) |>
    md_regression_()
  
  z2 <- x |>
    textCriterion() |> 
    sprintf(fmt = 'Backward stepwise variable selection is performed by %s.') |>
    new(Class = 'md_lines')
  
  z3 <- md_int(x = x, xnm = xnm, engine = 'flextable', ...)
  
  c(z1, z2, z3) # ?fastmd::c.md_lines
  
}

