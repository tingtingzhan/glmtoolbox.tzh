
#' @title [backwardCriterion]
#' 
#' @param model,trace,... parameters of function \link[glmtoolbox]{stepCriterion},
#' **other than `direction`**
#' 
#' @keywords internal
#' @importFrom glmtoolbox stepCriterion
#' @export
backwardCriterion <- function(model, trace = FALSE, ...) {
  ret <- model |>
    stepCriterion(direction = 'backward', trace = trace, ...)
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






#' @title Convert \link[glmtoolbox.tzh]{backwardCriterion} to \link[base]{matrix}
#' 
#' @param x returned object from function \link[glmtoolbox.tzh]{backwardCriterion}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @importFrom ecip ecip as.matrix.ecip endpoint
#' @method as.matrix backwardCriterion
#' @export as.matrix.backwardCriterion
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
    z[[1L]] |> as.matrix.ecip(type = 'p_only'), # `initial` model, only print p-values
    z[[2L]] |> as.matrix.ecip(type = 'ncol1') # `final` model
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



#' @title Convert \link[glmtoolbox.tzh]{backwardCriterion} to \link[flextable]{flextable}
#' 
#' @param x returned object from function \link[glmtoolbox.tzh]{backwardCriterion}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @importFrom flextable as_flextable add_footer_lines color
#' @importFrom fastmd as_flextable.matrix
#' @export as_flextable.backwardCriterion
#' @export
as_flextable.backwardCriterion <- function(
    x, 
    row.title = z |> attr(which = 'row.title', exact = TRUE),
    ...
) {
  z <- as.matrix.backwardCriterion(x, ...)
  z |>
    as_flextable.matrix(
      row.title = row.title,
    ) |>
    color(j = 2L, color = 'grey60', part = 'all') |>
    add_footer_lines(values = c(
      x |> textCriterion() |> sprintf(fmt = '\u274c: predictor(s) removed by %s')
    ))
}



#' @title R Markdown Lines for [backwardCriterion]
#' 
#' @param x,xnm,... ..
#' 
#' @keywords internal
#' @importFrom fastmd md_
#' @importClassesFrom fastmd md_lines
#' @importFrom ecip .md_reg
#' @export md_.backwardCriterion
#' @export
md_.backwardCriterion <- function(x, xnm, ...) {
  
  z1 <- x |>
    attr(which = 'initial.fit', exact = TRUE) |>
    .md_reg() |>
    new(Class = 'md_lines')
  
  z2 <- x |>
    textCriterion() |> 
    sprintf(fmt = 'Backward stepwise variable selection is performed by %s.') |>
    new(Class = 'md_lines')
  
  z3 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2, z3) # ?fastmd::c.md_lines
  
}

