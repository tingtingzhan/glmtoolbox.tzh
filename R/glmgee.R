

#' @title \link[glmtoolbox]{glmgee} Objects
#' 
#' @examples
#' library(ecip); list(
#'  '`glmgee`' = glmgee(breaks ~ tension, id = wool, data = warpbreaks, corstr = 'exchangeable')
#' ) |> fastmd::render2html()
#' 
#' @name glmgee
NULL





#' @export
family.glmgee <- function(object, ...) object[['family']]



#' @export
coef_.glmgee <- function(x) {
  # x$coefficients[, 1L] # drop rownames if ncol-1L
  # x$coefficients[, 1L, drop = FALSE] # wont help either ..
  setNames(c(x$coefficients), nm = rownames(x$coefficients))
}
# do not overwrite ?glmtoolbox:::coef.glmgee


#' @export
.pval.glmgee <- function(x) {
  # see inside
  # glmtoolbox:::summary.glmgee
  e <- x$coefficients[,1L]
  std <- vcov(x, type = 'robust') |> diag() |> sqrt()
  tval <- e/std
  return(2 * pnorm(-abs(tval)))
}


#' @export
confint_.glmgee <- function(x, level = .95, ...) {
  # ?glmtoolbox:::confint.glmgee
  ci <- confint(object = x, level = level, ..., verbose = FALSE, digits = 7)
  attr(ci, which = 'conf.level') <- level
  return(ci)
}



#' @export
nobsText.glmgee <- function(x) {
  sz <- x[['sizes']]
  if (ncol(sz) != 1L) stop('new situation?')
  id_call <- x$call$id
  sprintf(fmt = '%d records from %d `%s`', sum(sz), nrow(sz), deparse1(id_call))
}



#' @export
desc_.glmgee <- function(x, ...) {
  'generalized estimating equations [GEE, @LiangZeger86]' |> 
    new(Class = 'md_lines', bibentry = .liang_zeger86())
}




#' @export
md_.glmgee <- md_ecip


