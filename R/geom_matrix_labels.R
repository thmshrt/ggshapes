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
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   # geom_viewport(aes(xc = 0, yc = 0, w = 2, h = 2)) +
#'   geom_matrix(
#'     mapping = aes(xc = 0, yc = 0, lx = 4, ly = 2, lz = 3),
#'     fill = 'skyblue3', colour = 'grey3',
#'     viewx = 2, viewy = 2
#'   ) +
#'   geom_matrix_axes(
#'     mapping = aes(xc = 0, yc = 0, lx = 4, ly = 2, lz = 3),
#'     arrow = arrow(15,unit(.25,'cm')),
#'     viewx = 2, viewy = 2
#'   ) +
#'   geom_matrix_labels(
#'     mapping = aes(xc = 0, yc = 0, lx = 4, ly = 2, lz = 3),
#'     xpos = c('start','end'), xlabel = c('1','n'),
#'     viewx = 2, viewy = 2
#'   )

geom_matrix_labels = function(
  mapping = NULL,
  data = NULL,
  stat = 'matrix_labels',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  xlabel = 'x', xpos = 'middle',
  ylabel = 'y', ypos = 'middle',
  zlabel = 'z', zpos = 'middle',
  degx = 70, degy = 20, degz = 0,
  viewx = 1, viewy = 1,
  respect = TRUE,
  ...
) {
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomMatrixLabels,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      xlabel = xlabel, xpos = xpos,
      ylabel = ylabel, ypos = ypos,
      zlabel = zlabel, zpos = zpos,
      degx = degx, degy = degy, degz = degz,
      viewx = viewx, viewy = viewy, respect = TRUE,
      ...)
  )
}
