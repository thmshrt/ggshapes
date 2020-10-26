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
#'   geom_viewport(aes(xc = 0, yc = 0, w = 2, h = 2)) +
#'   geom_matrix_slice(
#'     mapping = aes(
#'       xc = 0, yc = 0,
#'       lx = 3, ly = 3, lz = 3,
#'       ix = 1, ixend = 1,
#'       iy = 1, iyend = 2,
#'       iz = 1, izend = 3),
#'     fill = 'skyblue3', colour = 'grey3',
#'     viewx = 2, viewy = 2
#'   ) +
#'   geom_matrix_slice_axes(
#'     mapping = aes(
#'       xc = 0, yc = 0,
#'       lx = 3, ly = 3, lz = 3,
#'       ix = 1, ixend = 1,
#'       iy = 1, iyend = 2,
#'       iz = 1, izend = 3),
#'     arrow = arrow(15,unit(.25,'cm')),
#'     viewx = 2, viewy = 2
#'   )

#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_viewport(aes(xc = 0, yc = 0, w = 2, h = 2)) +
#'   geom_matrix_slice(
#'     mapping = aes(
#'       xc = 0, yc = 0,
#'       lx = 3, ly = 3, lz = 3,
#'       ix = 1, ixend = 1,
#'       iy = 1, iyend = 2,
#'       iz = 1, izend = 3),
#'     fill = 'skyblue3', colour = 'grey3',
#'     viewx = 2, viewy = 2
#'   ) +
#'   geom_matrix_slice_axes(
#'     mapping = aes(
#'       xc = 0, yc = 0,
#'       lx = 3, ly = 3, lz = 3,
#'       ix = 1, ixend = 1,
#'       iy = 1, iyend = 2,
#'       iz = 1, izend = 3),
#'     arrow = arrow(15,unit(.25,'cm')),
#'     viewx = 2, viewy = 2
#'   )

geom_matrix_slice_axes = function(
  mapping = NULL,
  data = NULL,
  stat = 'matrix_slice_axes',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  degx = 70, degy = 20, degz = 0,
  viewx = 1, viewy = 1,
  respect = TRUE,
  ...
) {
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomMatrixSliceAxes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      degx = degx, degy = degy, degz = degz,
      viewx = viewx, viewy = viewy, respect = TRUE,
      ...)
  )
}
