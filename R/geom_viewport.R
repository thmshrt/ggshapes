#' {title placeholder} 
#'
#' {description placeholder} 
#' 
#' @usage
#' {usage placeholder}
#'
#' @param {param}   \[{type}\], {restrictions}
#' 
#' @return           [{type}]
#'
#' @export
#' @import magrittr rlang purrr tibble dplyr ggplot2
#' 
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(ggplot2)
#' library(grid)
#' library(magrittr)
#' # identity case
#'
#'
#' ggplot() +
#' coord_equal() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_viewport(
#'   data = tibble(xc = c(0,1),yc = c(0,1),w = c(.5,1), h = c(.5,1)),
#'   mapping = aes(xc = xc, yc = yc, w = w, h = h)
#' )


geom_viewport = function(
  mapping = NULL,
  data = NULL,
  stat = 'viewport',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomViewport,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...)
  )
}
