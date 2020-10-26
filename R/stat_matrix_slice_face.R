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
#' stat_cube_matrix(mapping = aes(xc = 0, yc = 0, l = 2),degx = 70, degy = 20, degz = 0)

stat_matrix_slice_face = function(
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
    stat = StatMatrixSliceFace,
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


