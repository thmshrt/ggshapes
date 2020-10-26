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
#' pts_unit_matrix() %}%
#'   center(0,0,0) %}%
#'   rotate(70,20,0) %}%
#'   dplyr::filter(face %in% c('top','front','right')) -}
#'   cube
#
#' pts_unit_axes() %}%
#'   translate(dx = -.5, dy = -.5, dz = -.5) %}%
#'   rotate(70,20,0) %}%
#'   (function(.x) dplyr::bind_cols(.x %}% slice(seq(1,n(),2)), .x %}% slice(seq(2,n(),2)))) %}%
#'   (function(.x) {names(.x) = paste(names(.x),rep(c('','end'),each=6),sep=''); .x}) %}%
#'   dplyr::rename_all((function(name) name %}% stringr::str_remove('\\.\\.\\.[0-9]+'))) -}
#'   axes
#
# ggplot() +
#   coord_equal() +
#   scale_x_continuous(limits = c(-2,2)) +
#   scale_y_continuous(limits = c(-2,2)) +
#   geom_polygon(
#     data = cube,
#     mapping = aes(x = x, y = y, group = face, fill = face),
#     colour = 'black'
#   ) +
#   geom_segment(
#     data = axes,
#     mapping = aes(
#       x = x, xend = xend, y = y, yend = yend
#     ),
#     arrow = arrow(angle = 15,length = unit(.25,'cm')),
#     colour = 'black'
#   )

pts_unit_matrix_axes = function() {
  tribble(
    ~x     , ~y     , ~z     , ~face    , ~side    , ~position,
    -0.1   , -0.1   ,  1.    , 'front'  , 'left'   ,  'start' ,
    -0.1   , -0.1   ,  0.    , 'front'  , 'left'   ,  'end'   ,

    -0.1   ,  1     ,  1.1   , 'top'    , 'left'   ,  'start' ,
    -0.1   ,  0     ,  1.1   , 'top'    , 'left'   ,  'end'   ,

    0     ,  1.1   ,  1.1   , 'top'    , 'top'    ,  'start' ,
    1     ,  1.1   ,  1.1   , 'top'    , 'top'    ,  'end'   ,
  )
}
