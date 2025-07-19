

#' @title \link[stats]{family} of \link[glmtoolbox]{glmgee} objects
#' 
#' @param object \link[glmtoolbox]{glmgee} object
#' 
#' @param ... S3 method dispatch holder
#' 
#' @importFrom stats family
#' @export family.glmgee
#' @export
family.glmgee <- function(object, ...) object[['family']]






#' @title Auxiliary S3 method dispatch for \link[glmtoolbox]{glmgee} objects
#' 
#' @param x \link[glmtoolbox]{glmgee} object
#' 
#' @param level ..
#' 
#' @param ... ..
#' 
#' @name glmgee_S3
#' @export
coef_.glmgee <- function(x) x$coefficients[,1L]
# do not overwrite glmtoolbox:::coef.glmgee

#' @rdname glmgee_S3
#' @importFrom stats pnorm vcov
#' @export
.pval.glmgee <- function(x) {
  # see inside
  # glmtoolbox:::summary.glmgee
  e <- x$coefficients[,1L]
  std <- vcov(x, type = 'robust') |> diag() |> sqrt()
  tval <- e/std
  return(2 * pnorm(-abs(tval)))
}


#' @rdname glmgee_S3
#' @importFrom stats confint
#' @export
confint_.glmgee <- function(x, level = .95, ...) {
  # ?glmtoolbox:::confint.glmgee
  ci <- confint(object = x, level = level, ..., verbose = FALSE, digits = 7)
  attr(ci, which = 'conf.level') <- level
  return(ci)
}



#' @rdname glmgee_S3
#' @export
nobsText.glmgee <- function(x) {
  sz <- x[['sizes']]
  if (ncol(sz) != 1L) stop('new situation?')
  id_call <- x$call$id
  sprintf(fmt = '%d records from %d `%s`', sum(sz), nrow(sz), deparse1(id_call))
}



#' @rdname glmgee_S3
#' @importFrom utils bibentry
#' @export
desc_.glmgee <- function(x, ...) {
  ret <- 'generalized estimating equations [GEE, @Liang86]'
  attr(ret, which = 'bibentry') <- bibentry(
    bibtype = 'Article', key = 'Liang86',
    author = 'Liang, Kung-Yee and Zeger, Scott L.',
    title = 'Longitudinal data analysis using generalized linear models',
    journal = 'Biometrika',
    volume = '73',
    number = '1',
    pages = '13-22',
    year = '1986',
    month = '04',
    doi = '10.1093/biomet/73.1.13'
  )
  return(ret)
}


