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
#' #
#' pts_unit_axis_labels() %}%
#'   translate(dx = -.5, dy = -.5, dz = -.5) %}%
#'   rotate(70,20,0) %}%
#'   dplyr::mutate(label = paste(face,side,position,sep=',')) %}%
#'   dplyr::mutate(angle = ifelse(side =='top',90,0)) -}
#'   labels
#
#' #
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(
#'     data = cube,
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_text(
#'     data = labels,
#'     mapping = aes(
#'       x = x, y = y, label = label
#'     ),
#'     nudge_x = c(rep(-.1,6),rep(0,3)),
#'     nudge_y = c(rep(0,6),rep(+.1,3)),
#'     hjust = c(rep('right',6),rep('left',3)),
#'     angle = labels$angle,
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = labels,
#'     mapping = aes(x = x, y = y)
#'   )

pts_unit_matrix_axis_labels = function() {
  tribble(
    ~x     , ~y     , ~z     , ~face    , ~side    , ~position,
    -0.1   , -0.1   ,  1     , 'front'  , 'left'   ,  'start' ,
    -0.1   , -0.1   ,  0.5   , 'front'  , 'left'   ,  'middle',
    -0.1   , -0.1   ,  0     , 'front'  , 'left'   ,  'end'   ,

    -0.1   ,  1     , 1.1    , 'top'    , 'left'   ,  'start' ,
    -0.1   ,  0.5   , 1.1    , 'top'    , 'left'   ,  'middle',
    -0.1   ,  0     , 1.1    , 'top'    , 'left'   ,  'end'   ,

    0     ,  1.1   , 1.1    , 'top'    , 'top'    ,  'start' ,
    0.5   ,  1.1   , 1.1    , 'top'    , 'top'    ,  'middle',
    1     ,  1.1   , 1.1    , 'top'    , 'top'    ,  'end'   ,
  )
}

