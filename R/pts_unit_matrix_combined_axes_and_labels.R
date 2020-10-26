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
#' pts_unit_combined_axes() %}%
#'   translate(dx = -.5, dy = -.5, dz = -.5) %}%
#'   rotate(70,20,0) %}%
#'   (function(.x) dplyr::bind_cols(.x %}% slice(seq(1,n(),2)), .x %}% slice(seq(2,n(),2)))) %}%
#'   (function(.x) {names(.x) = paste(names(.x),rep(c('','end'),each=6),sep=''); .x}) %}%
#'   dplyr::rename_all((function(name) name %}% stringr::str_remove('\\.\\.\\.[0-9]+'))) -}
#'   axes
#
#' pts_unit_combined_axis_labels() %}%
#'   translate(dx = -.5, dy = -.5, dz = -.5) %}%
#'   rotate(70,20,0) %}%
#'   dplyr::mutate(label = paste(face,side,position,sep=',')) %}%
#'   dplyr::mutate(
#'     labelshort = paste(
#'       stringr::str_sub(face,1,1),
#'       stringr::str_sub(side,1,1),
#'       stringr::str_sub(position,1,1),
#'       sep=',')
#'     ) %}%
#'   dplyr::mutate(angle = ifelse(side =='top',90,0)) -}
#'   labels
#
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(
#'     data = cube,
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_segment(
#'     data = axes,
#'     mapping = aes(
#'       x = x, xend = xend, y = y, yend = yend
#'     ),
#'     arrow = arrow(angle = 15,length = unit(.25,'cm')),
#'     colour = 'black'
#'   ) +
#'   geom_text(
#'     data = labels,
#'     mapping = aes(
#'       x = x, y = y, label = labelshort
#'     ),
#'     angle = labels$angle,
#'     colour = 'black'
#'   )

pts_unit_combined_axes = function() pts_unit_axes()

pts_unit_combined_axis_labels = function() {
  tribble(
    ~x     , ~y     , ~z     , ~face    , ~side    , ~position,
    -0.2   , -0.2   ,  1     , 'front'  , 'left'   ,  'start' ,
    -0.2   , -0.2   ,  0.5   , 'front'  , 'left'   ,  'middle',
    -0.2   , -0.2   ,  0     , 'front'  , 'left'   ,  'end'   ,

    -0.2   ,  1     , 1.2    , 'top'    , 'left'   ,  'start' ,
    -0.2   ,  0.5   , 1.2    , 'top'    , 'left'   ,  'middle',
    -0.2   ,  0     , 1.2    , 'top'    , 'left'   ,  'end'   ,

    0     ,  1.2   , 1.2    , 'top'    , 'top'    ,  'start' ,
    0.5   ,  1.2   , 1.2    , 'top'    , 'top'    ,  'middle',
    1     ,  1.2   , 1.2    , 'top'    , 'top'    ,  'end'   ,
  )
}

