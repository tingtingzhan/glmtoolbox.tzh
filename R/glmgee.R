

#' @title \link[stats]{family} of \link[glmtoolbox]{glmgee} objects
#' 
#' @param object \link[glmtoolbox]{glmgee} object
#' 
#' @param ... S3 method dispatch holder
#' 
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
#' @keywords internal
#' @name glmgee_S3
#' @importFrom ecip coef_
#' @export coef_.glmgee
#' @export
coef_.glmgee <- function(x) {
  # x$coefficients[, 1L] # drop rownames if ncol-1L
  # x$coefficients[, 1L, drop = FALSE] # wont help either ..
  setNames(c(x$coefficients), nm = rownames(x$coefficients))
}
# do not overwrite ?glmtoolbox:::coef.glmgee

#' @rdname glmgee_S3
#' @importFrom ecip .pval
#' @export .pval.glmgee
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
#' @importFrom ecip confint_
#' @export confint_.glmgee
#' @export
confint_.glmgee <- function(x, level = .95, ...) {
  # ?glmtoolbox:::confint.glmgee
  ci <- confint(object = x, level = level, ..., verbose = FALSE, digits = 7)
  attr(ci, which = 'conf.level') <- level
  return(ci)
}



#' @rdname glmgee_S3
#' @importFrom ecip nobsText
#' @export nobsText.glmgee
#' @export
nobsText.glmgee <- function(x) {
  sz <- x[['sizes']]
  if (ncol(sz) != 1L) stop('new situation?')
  id_call <- x$call$id
  sprintf(fmt = '%d records from %d `%s`', sum(sz), nrow(sz), deparse1(id_call))
}



#' @rdname glmgee_S3
#' @importFrom methods new
#' @importClassesFrom fastmd md_lines
#' @importFrom ecip desc_
#' @export desc_.glmgee
#' @export
desc_.glmgee <- function(x, ...) {
  'generalized estimating equations [GEE, @LiangZeger86]' |> 
    new(Class = 'md_lines', bibentry = .liang_zeger86())
}




#' @title R Markdown Lines for \link[glmtoolbox]{glmgee} Object
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' library(ecip); list(
#'  '`glmgee`' = glmgee(breaks ~ tension, id = wool, data = warpbreaks, corstr = 'exchangeable')
#' ) |> fastmd::render2html()
#' 
#' @keywords internal
#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export md_.glmgee
#' @export
md_.glmgee <- md_ecip


