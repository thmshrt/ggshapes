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
#' geom_planar_pointgon(
#'   data = pts_planar_circle(),
#'   mapping = aes(
#'     x = x, y = y, z = z
#'     )
#'   ) +
#'   geom_rect(
#'     mapping = aes(
#'       xmin = -1, xmax = +1,
#'       ymin = -1, ymax = +1
#'       ),
#'     fill = NA,
#'     colour = 'black'
#'   )

geom_planar_pointgon = function(
  mapping = NULL,
  data = NULL,
  stat = 'planar_pointgon',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  degx = 0, degy = 0, degz = 0,
  xc = NA, yc = NA, zc = NA,
  viewx = NA, viewy = NA,
  respect = TRUE,
  ...
) {
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomPlanarPointgon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      degx = degx, degy = degy, degz = degz,
      viewx = viewx, viewy = viewy, respect = TRUE,
      xc = xc, yc = yc, zc = zc,
      ...)
  )
}
