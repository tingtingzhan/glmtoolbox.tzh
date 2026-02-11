
#' @title bibs in glmtoolbox.tzh package
#' 
#' @param key,... \link[utils]{bibentry}
#' 
#' @keywords internal
#' @importFrom utils bibentry person
#' @name glmtoolbox_bib
#' @export
.liang_zeger86 <- \(key = 'LiangZeger86', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = c(
      person(family = 'Liang', given = 'Kung-Yee'), 
      person(family = 'Zeger', given = c('Scott', 'L.'))
    ),
    title = 'Longitudinal data analysis using generalized linear models',
    journal = 'Biometrika',
    volume = '73',
    number = '1',
    pages = '13-22',
    year = '1986',
    month = '04',
    doi = '10.1093/biomet/73.1.13'
  )
}