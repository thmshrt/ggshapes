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
#' pts_matrix(1,2,3) %}%
#'   center3(0,0,0,old_xc = 1/2,old_yc = 2/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   dplyr::filter(face %in% c('top','front','right')) -}
#'   matrix
#'
#' pts_matrix_combined_axes(1,2,3) %}%
#'   center3(0,0,0,old_xc = 1/2,old_yc = 2/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   (function(.x) dplyr::bind_cols(.x %}% slice(seq(1,n(),2)), .x %}% slice(seq(2,n(),2)))) %}%
#'   (function(.x) {names(.x) = paste(names(.x),rep(c('','end'),each=6),sep=''); .x}) %}%
#'   dplyr::rename_all((function(name) name %}% stringr::str_remove('\\.\\.\\.[0-9]+'))) -}
#'   axes
#'
#' pts_matrix_combined_labels(1,2,3) %}%
#'   center3(0,0,0,old_xc = 1/2,old_yc = 2/2,old_zc = 3/2) %}%
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
#'
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-4,4)) +
#'   scale_y_continuous(limits = c(-4,4)) +
#'   geom_polygon(
#'     data = matrix,
#'     mapping = aes(x = x, y = y, group = draw_order, fill = face),
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
#'     nudge_x = c(rep(-.5,6),rep(0,3)),
#'     nudge_y = c(rep(0,6),rep(+.5,3)),
#'     angle = labels$angle,
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = labels,
#'     mapping = aes(x = x, y = y)
#'   )

pts_matrix_combined_axes = function(lx,ly,lz) pts_matrix_axes(lx,ly,lz)

pts_matrix_combined_labels = function(lx,ly,lz) {
  tribble(
    ~x     , ~y     , ~z     , ~face    , ~side    , ~position, ~bounding,
    -0.2*lx, -0.2*ly,  1.0*lz, 'front'  , 'left'   ,  'start' ,     FALSE,
    -0.2*lx, -0.2*ly,  0.5*lz, 'front'  , 'left'   ,  'middle',     FALSE,
    -0.2*lx, -0.2*ly,  0.0*lz, 'front'  , 'left'   ,  'end'   ,     FALSE,

    -0.2*lx,  1.0*ly,  1.2*lz, 'top'    , 'left'   ,  'start' ,     FALSE,
    -0.2*lx,  0.5*ly,  1.2*lz, 'top'    , 'left'   ,  'middle',     FALSE,
    -0.2*lx,  0.0*ly,  1.2*lz, 'top'    , 'left'   ,  'end'   ,     FALSE,

     0.0*lx,  1.2*ly,  1.2*lz, 'top'    , 'top'    ,  'start' ,     FALSE,
     0.5*lx,  1.2*ly,  1.2*lz, 'top'    , 'top'    ,  'middle',     FALSE,
     1.0*lx,  1.2*ly,  1.2*lz, 'top'    , 'top'    ,  'end'   ,     FALSE,
  ) %>% dplyr::bind_rows(
    tribble(
      # bounding box points
      ~x, ~y, ~z, ~face   , ~tb     , ~rl     , ~point_order, ~face_order, ~bounding,
      0 ,  0,  0, 'front' , 'bottom', 'left'  ,           NA,          NA,      TRUE,
      0 ,  0, lz, 'front' , 'top'   , 'left'  ,           NA,          NA,      TRUE,
      lx,  0, lz, 'front' , 'top'   , 'right' ,           NA,          NA,      TRUE,
      lx,  0,  0, 'front' , 'bottom', 'right' ,           NA,          NA,      TRUE,
      0 , ly,  0, 'back'  , 'bottom', 'left'  ,           NA,          NA,      TRUE,
      0 , ly, lz, 'back'  , 'top'   , 'left'  ,           NA,          NA,      TRUE,
      lx, ly, lz, 'back'  , 'top'   , 'right' ,           NA,          NA,      TRUE,
      lx, ly,  0, 'back'  , 'bottom', 'right' ,           NA,          NA,      TRUE
    )
  )
}

