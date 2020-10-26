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
#'   geom_point(
#'     data = pts_planar_arc(),
#'     mapping = aes(x = x, y = y),
#'     size = 0.5
#'   ) +
#'   geom_polygon(
#'     data = pts_planar_arc(),
#'     mapping = aes(x = x, y = y),
#'     fill = 'NA',
#'     colour = 'black'
#'   )
#
#' # an arc
#' library(ggplot2)
#' library(tibble)
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_point(
#'     data = pts_planar_arc(a0 = 0, a1 = 90, n = 64 / 4),
#'     mapping = aes(x = x, y = y),
#'     size = 0.5
#'   ) +
#'   geom_path(
#'     data = pts_planar_arc(a0 = 0, a1 = 90, n = 64 / 4),
#'     mapping = aes(x = x, y = y),
#'     colour = 'black',
#'     arrow = arrow(angle = 15, length = unit(0.25,'cm'))
#'   )
#
#' # reversing the direction is as simple as reversing the start and end angles
#' library(ggplot2)
#' library(tibble)
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_point(
#'     data = pts_planar_arc(a0 = 90, a1 = 0, n = 64 / 4),
#'     mapping = aes(x = x, y = y),
#'     size = 0.5
#'   ) +
#'   geom_path(
#'     data = pts_planar_arc(a0 = 90, a1 = 0, n = 64 / 4),
#'     mapping = aes(x = x, y = y),
#'     colour = 'black',
#'     arrow = arrow(angle = 15, length = unit(0.25,'cm'))
#'   )

pts_planar_arc = function(r = 1,a0 = 0, a1 = 360 - 360/64, z = 0,n = 64) {

  # BEGIN : checks
  # r is numeric or integer, nonnegative, length 1
  # a0 is numeric or integer, nonegative, length 1
  # a1 is numeric or integer, nonegative, length 1
  # z is numeric or integer, length 1
  # n is numeric or integer, nonnegative, length 1
  # END : checks

  # BEGIN: logic
  rad0 = a0 * pi / 180
  rad1 = a1 * pi / 180
  theta = seq(rad0, rad1, length.out = n)

  tibble(
    x = r * cos(theta),
    y = r * sin(theta),
    z = z,
    order = 1:length(theta)
  )
  # END: logic

}
