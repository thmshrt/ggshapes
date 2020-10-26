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
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(ggplot2)
#' library(grid)
#' library(magrittr)
#' # rotate a cube
#' #    70 degrees anticlockwise about its x axis
#' #    20 degrees anticlockwise about its y axis
#' # and then view a cube's front, right, and top faces
#' # notice that stat does not color the cube
#' # this is what geom_unit_cube is for!
#' ggplot() +
#' coord_equal() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' stat_matrix_slice(mapping = aes(
#'   xc = 1, yc = 0,
#'   lx = 3, ly = 3, lz = 3,
#'   ix = 1, ixend = 1,
#'   iy = 1, iyend = 2,
#'   iz = 1, izend = 3)
#'   ,degx = 70, degy = 20, degz = 0)

stat_sparse_matrix = function(
  mapping = NULL,
  data = NULL,
  geom = 'polygon',
  position = 'identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  # additional rotational parameters
  degx = 0, degy = 0, degz = 0,
  viewx = 1,viewy = 1, respect = TRUE,
  ...
) {
  layer(
    stat = StatMatrixSlice,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      # additional rotational parameters
      degx = degx, degy = degy, degz = degz,
      viewx = viewx, viewy = viewy, respect = TRUE,
      ...
    )
  )
}


