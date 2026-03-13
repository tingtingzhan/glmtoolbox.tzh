
#' @title \link[nlme]{lmeObject} and \link[nlme]{glsObject}
#' 
#' @examples
#' library(nlme)
#' lme(fixed = distance ~ Sex * I(age-11), 
#'   weights = varIdent(form = ~ 1 | Sex), data = Orthodont)
#' 
#' m1 = lme(distance ~ age, data = Orthodont, keep.data = TRUE)
#' m2 = gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
#'   data = Ovary, correlation = corAR1(form = ~ 1 | Mare)) 
#'   
#' library(ecip); list(
#'  '`lme`' = m1, 
#'  '`gls`' = m2
#' ) |> fastmd::render2html()
#' @name lme_gls
NULL




#' @importFrom nlme intervals
#' @export
confint_.lme <- function(x, level = .95, ...) {
  x |>
    intervals(object = _, level = level, which = 'fixed', ...) |> # ?nlme:::intervals.lme
    confint_.intervals.lme()
}

#' @export
confint_.gls <- function(x, level = .95, ...) {
  x |>
    intervals(object = x, level = level, which = 'coef', ...) |> # ?nlme:::intervals.gls
    confint_.intervals.gls()
}

#' @method confint_ intervals.lme
#' @export
confint_.intervals.lme <- function(x, ...) {
  ci <- x[['fixed']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}

#' @method confint_ intervals.gls
#' @export
confint_.intervals.gls <- function(x, ...) {
  ci <- x[['coef']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}


#' @export
desc_.lme <- function(x) {
  # from ?nlme:::print.lme
  'linear mixed effects model' |>
    sprintf(fmt = '*%s*') |>
    new(Class = 'md_lines', package = 'nlme')
}

#' @export
desc_.gls <- function(x) {
  # from ?nlme:::print.gls
  'generalized (weighted) least squares fit' |>
    sprintf(fmt = '*%s*') |>
    new(Class = 'md_lines', package = 'nlme')
}



#' @method .pval summary.lme
#' @export
.pval.summary.lme <- function(x) {
  # `nlme:::coef.summary.lme` not show-able!!  could only see from debug(coef)
  # `nlme:::coef.summary.gls` not show-able!!  could only see from debug(coef)
  cf <- x$tTable
  ret <- cf[, 'p-value'] 
  names(ret) <- rownames(cf)
  return(ret)
}

#' @method .pval summary.gls
#' @export
.pval.summary.gls <- .pval.summary.lme




#' @export
nobsText.lme <- function(x) {
  # ?nlme:::print.lme; 'Number of Groups'
  dd <- x$dims
  ng <- dd[['ngrps']][1L:dd$Q]
  # either dd$Q == 1L or > 1L
  sprintf(fmt = '%d records from %s', dd[['N']], paste(sprintf(fmt = '%d `%s`', ng, names(ng)), collapse = ' nested in '))
}

#' @export
nobsText.gls <- function(x) {
  # what happens with nested groups ????
  groups <- x[['groups']]
  corrfom <- x$call$correlation$form
  sprintf(fmt = '%d records from %d `%s`', x$dims[['N']], nlevels(groups), deparse1(corrfom[[2L]][[3L]]))
}


# @note
# The `S3` generic \link[stats]{model.frame} is critical 
# for the function \link[stats]{.getXlevels}.
# 
# Without the `S3` method [model.frame.lme()], 
# the `S3` generic \link[stats]{model.frame}
# dispatches to the default `S3` method \link[stats]{model.frame.default} 
# and get the element `$modelStruct` 
# (as \link[nlme]{lmeObject} has no `$model` element).
#' @importFrom nlme getData
#' @method model.frame lme
#' @export
model.frame.lme <- function(
    formula, 
    data = getData(formula), # ?nlme:::getData.lme or # ?nlme:::getData.gls
    ...
) {
  model.frame.default(
    formula = formula(formula), # ?nlme:::formula.lme or ?nlme:::formula.gls
    data = data, 
    ...
  )
}

#' @method model.frame gls
#' @export
model.frame.gls <- model.frame.lme




# @note
# We already have `nlme:::coef.lme`
# [coef_.lme()] is actually `nlme:::fixef.lme`.
#' @export
coef_.lme <- function(x) x$coefficients$fixed

# ?nlme:::coef.gls is fine


#' @export
md_.lme <- md_ecip
#' @export
md_.gls <- md_ecip








