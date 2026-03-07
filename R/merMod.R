
#' @title \link[lme4]{merMod} Objects
#' 
#' @description
#' ..
#' 
#' @examples
#' # see ?lme4::glmer or ?lme4::cbpp
#' library(lme4)
#' m1 = glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), 
#'   data = cbpp, family = binomial)
#' 
#' startvec = c(Asym = 200, xmid = 725, scal = 350)
#' m2 = nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
#'   Orange, start = startvec)
#' 
#' #library(HSAUR3)
#' m3 = glmer(outcome ~ treatment + visit + (1|patientID), 
#'   data = toenail, family = binomial, nAGQ = 20)
#' 
#' library(ecip); list(
#'  '`glmerMod`' = list(m1, m3)#,
#'  # '`nlmerMod`' = m2 # currently error
#' ) |> fastmd::render2html()
#' @name merMod
NULL



#' @method .pval summary.merMod
#' @export
.pval.summary.merMod <- function(x) {
  # ('glmerMod' inherits from 'merMod')
  # ('nlmerMod' inherits from 'merMod')
  # returned value from ?lme4:::summary.merMod
  cf <- x$coefficients
  ret <- cf[, 'Pr(>|z|)'] # has error on 'nlmerMod' object!!
  names(ret) <- rownames(cf)
  return(ret)
}


#' @export
vterms.merMod <- function(x) {
  
  tmp. <- x |>
    terms() |> # ?lme4:::terms.merMod returns only fixed effect!!
    attr(which = 'variables', exact = TRUE) |>
    as.list.default()
  ret <- tmp.[-1L] # remove first element of quote(list)
  
  # ?lme4:::formula.merMod
  ranfom <- x |> 
    formula(random.only = TRUE)
  
  attr(ret, which = 'group') <- ranfom[[3L]]
  return(ret)
  
}






#' @export
desc_.glmerMod <- function(x) 'generalized linear mixed regression'

#' @importFrom lme4 methTitle
#' @export
desc_.merMod <- function(x) {
  # see inside ?lme4:::print.merMod
  x@devcomp$dims |> 
    methTitle() |> # lme4::methTitle
    gsub(pattern = ' fit by .*$', replacement = '') |> 
    tolower()
}



# ?lme4:::coef.merMod not want I need
#' @importFrom nlme fixef
#' @export
coef_.merMod <- function(x) fixef(x) # ?lme4:::fixef.merMod


# \link[lme4]{confint.merMod} is very slow with default `method = 'profile'`
# \link[lme4]{confint.merMod} returns 'sigmas' and cannot be suppressed
#' @importFrom lme4 confint.merMod
#' @export
confint_.merMod <- function(x, level = .95, method = 'Wald', ...) {
  ci <- x |>
    confint.merMod(level = level, method = method, ...) # ?lme4::confint.merMod
  ret <- ci[names(coef_.merMod(x)), , drop = FALSE]
  attr(ret, which = 'conf.level') <- level
  return(ret)
}



#' @export
nobsText.merMod <- function(x) {
  # ?lme4:::.prt.grps (from ?lme4:::print.merMod)
  # or ?lme4:::nobs.merMod
  dims <- x@devcomp$dims
  ng <- ngrps(x) # ?lme4:::ngrps.merMod
  sprintf(fmt = '%d records from %s', 
          dims[['n']],
          paste(sprintf(fmt = '%d `%s`', ng, names(ng)), collapse = ' nested in '))
} # same as ?nobsText.rlmerMod



# do I need this??
# @method vcov VarCorr.merMod
# @export
#vcov.VarCorr.merMod <- function(object, ...) unclass(object) # return object of ?lme4:::VarCorr.merMod




# as of 2026-03-04
# `lme4:::terms.merMod()` return does not have `'dataClasses'` attribute.
#' @export
dataClasses.merMod <- function(x) {
  x |>
    model.frame() |> # ?lme4:::model.frame.merMod
    attr(which = 'terms', exact = TRUE) |>
    # do *not* overwrite ?lme4:::terms.merMod (which does not have dataClasses)
    dataClasses() # ecip:::dataClasses.terms()
  # seems wrong for 'nlmerMod' object..
  # 'glmerMod' and 'lmerMod' should be correct
}




#' @export
md_.merMod <- md_ecip



