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
#'   geom_cuboid_slice(
#'     mapping = aes(
#'       xc = 0, yc = 0,
#'       lx = 1, ly = 1, lz = 1,
#'       sx = .5, sxend = 1,
#'       sy = .2, syend = 1,
#'       sz = 0, szend = 1
#'       ),
#'     viewx = 1, viewy = 1, respect = FALSE,
#'     fill = NA,
#'     colour = 'black'
#'     )

geom_cuboid_slice = function(
  mapping = NULL,
  data = NULL,
  stat = 'cuboid_slice',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  degx = 70, degy = 20, degz = 0,
  # faces = list(c('top','front','right')),
  viewx = NA, viewy = NA,
  respect = TRUE,
  ...
) {
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomCuboidSlice,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      degx = degx, degy = degy, degz = degz,
      # faces = faces,
      viewx = viewx, viewy = viewy, respect = TRUE,
      ...)
  )
}
