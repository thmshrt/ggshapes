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
#' # default is unit circle, z = 0
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_point(
#'     data = pts_planar_circle(n=32),
#'     mapping = aes(x = x, y = y)
#'   ) +
#'   geom_polygon(
#'     data = pts_planar_circle(n = 32),
#'     mapping = aes(x = x, y = y),
#'     fill = 'NA',
#'     colour = 'black'
#'   )
#'
pts_planar_circle = function(r = 1,z = 0,n = 50) {

  # BEGIN : checks
  # r is numeric or integer, nonnegative, length 1
  # z is numeric or integer, length 1
  # n is numeric or integer, nonnegative, length 1
  # END : checks

  # BEGIN: logic
  theta = seq(0, 2*pi - 2*pi/n, length.out = n)

  tibble(
    x = r * cos(theta),
    y = r * sin(theta),
    z = z,
    order = 1:length(theta)
  )
  # END: logic

}