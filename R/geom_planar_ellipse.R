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
#' geom_planar_ellipse(mapping = aes(xc = 0, yc = 0, r = 1))
#
#' # equivalent to the default parameter call
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_planar_ellipse(mapping = aes(xc = 0, yc = 0, ),degx = 0, degy = 0, degz = 0)
#
#' # add some rotation
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_planar_ellipse(mapping = aes(xc = 0, yc = 0, r = 1),degx = 70, degy = 20, degz = 0)
#
#' # removing coord_equal() results in cube being deformed according to scale
#' ggplot() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_planar_ellipse(mapping = aes(xc = 0, yc = 0, r = 1),degx = 70, degy = 20, degz = 0)
#
#' # fill and colour set as usual!
#' ggplot() +
#' coord_equal() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_planar_ellipse(
#'   data = tibble(xc = c(0,.5), yc = c(0,.5), r = c(.5,.5), fill = c(1,2) %}% as.factor()),
#'   mapping = aes(xc = xc, yc = yc, r = r, fill = fill),
#'   degx = c(70,0), degy = c(20,0), degz = 0
#'   )

geom_planar_ellipse = function(
  mapping = NULL,
  data = NULL,
  stat = 'planar_circle',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  degx = 0, degy = 0, degz = 0,
  viewx = NA, viewy = NA,
  respect = TRUE,
  ...
) {
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomPlanarEllipse,
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
