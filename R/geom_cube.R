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
#' geom_cube(mapping = aes(xc = 0, yc = 0))
#'
#' # equivalent to the default parameter call
#' ggplot() +
#' coord_equal() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_cube(mapping = aes(xc = 0, yc = 0),degx = 70, degy = 20, degz = 0)
#'
#' # without any rotation makes for a very boring view!
#' ggplot() +
#' coord_equal() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_cube(mapping = aes(xc = 0, yc = 0),degx = 0, degy = 0, degz = 0)
#'
#' # removing coord_equal() results in cube being deformed according to scale
#' ggplot() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_cube(mapping = aes(xc = .5, yc = 0),degx = 70, degy = 20, degz = 0)
#'
#' fill and colour set as usual!
#' ggplot() +
#' coord_equal() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_cube(
#'   data = tibble(xc = c(0,.5), yc = c(0,.5), l = 2, fill = c(1,2) %}% as.factor()),
#'   mapping = aes(xc = xc, yc = yc, l = l, fill = fill),
#'   degx = c(70,20), degy = 20, degz = 0,
#'   viewx = NA, viewy = NA
#'   )


geom_cube = function(
  mapping = NULL,
  data = NULL,
  stat = 'cube',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  degx = 70, degy = 20, degz = 0,
  faces = list(c('top','front','right')),
  viewx = NA, viewy = NA,
  respect = TRUE,
  ...
) {
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomCube,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      degx = degx, degy = degy, degz = degz,
      faces = faces, # face_colour_map = face_colour_map,
      viewx = viewx, viewy = viewy, respect = TRUE,
      ...)
  )
}
