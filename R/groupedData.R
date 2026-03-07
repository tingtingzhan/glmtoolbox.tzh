

#' @title \link[nlme]{groupedData} Objects
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param ... ..
#' 
#' @examples 
#' library(ggplot2)
#' autoplot(Indometh)
#' autoplot(Loblolly)
#' autoplot(ChickWeight) # Chick and Diet already factor
#' 
#' autoplot(nlme::Soybean) # two outer variables
#' autoplot(nlme::Glucose2) # nested groups
#' 
#' @export
autoplot.groupedData <- function(object, ...) {
  ggplot() + 
    autolayer.groupedData(object, ...) + 
    labs(title = deparse1(substitute(object)))
}




#' @importFrom rlang .data
#' @export
autolayer.groupedData <- function(object, ...) {
  
  fom <- attr(object, which = 'formula', exact = TRUE)
  if (!is.symbol(x <- fom[[3L]][[2L]])) return(invisible())
  if (!is.symbol(y <- fom[[2L]])) return(invisible())
  
  if (!is.symbol(id <- fom[[3L]][[3L]])) {
    id <- as.call(c(list(name = quote(interaction)), lapply(all.vars(id), FUN = as.symbol)))
  }
  
  otfom <- attr(object, which = 'outer', exact = TRUE)
  
  mp <- if (length(otfom)) {
    ot <- otfom[[2L]]
    if (is.symbol(ot)) {
      aes(x = .data[[x]], y = .data[[y]], group = eval(id, .data), colour = .data[[ot]])
    } else if ((ot[[1L]] == '+') && is.symbol(ot1 <- ot[[2L]]) && is.symbol(ot2 <- ot[[3L]])) {
      # `ot` is `a + b`
      aes(x = .data[[x]], y = .data[[y]], group = eval(id, .data), colour = .data[[ot1]], linetype = .data[[ot2]])
    } else if ((ot[[1L]] == '+') && is.symbol(ot2 <- ot[[3L]]) && 
               (ot[[2L]][[1L]] == '+') && is.symbol(ot3 <- ot[[2L]][[3L]])) {
      # `ot` is `a + b + c`
      # not sure if rlang::.data works..
      eval(call(name = 'aes', x = x, y = y, group = id, colour = ot2, linetype = ot3))
    } else return(invisible())
  } else eval(call(name = 'aes', x = x, y = y, group = id))
  
  geom_path(data = object, mapping = mp, alpha = .2, linewidth = .5)
  
}


