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
#' ##
#' pts_matrix_labels(1,2,3) %}%
#'   center3(0,0,0,old_xc = 1/2,old_yc = 2/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   scale_into_view3(use_bounding = TRUE) %}%
#'   dplyr::filter(!bounding) %}%
#'   dplyr::mutate(label = paste(face,side,position,sep=',')) %}%
#'   dplyr::mutate(angle = ifelse(side =='top',90,0)) -}
#'   labels
#'
#' ##
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-7,7)) +
#'   scale_y_continuous(limits = c(-7,7)) +
#'   geom_polygon(
#'     data = matrix,
#'     mapping = aes(x = x, y = y, group = draw_order, fill = face),
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

pts_matrix_labels = function(lx,ly,lz) {
  tribble(
    ~x     , ~y     , ~z     , ~face    , ~side    , ~position, ~bounding,
    -0.1*lx, -0.1*ly,    1*lz, 'front'  , 'left'   ,  'start' ,     FALSE,
    -0.1*lx, -0.1*ly,  0.5*lz, 'front'  , 'left'   ,  'middle',     FALSE,
    -0.1*lx, -0.1*ly,    0*lz, 'front'  , 'left'   ,  'end'   ,     FALSE,

    -0.1*lx,    1*ly,  1.1*lz, 'top'    , 'left'   ,  'start' ,     FALSE,
    -0.1*lx,  0.5*ly,  1.1*lz, 'top'    , 'left'   ,  'middle',     FALSE,
    -0.1*lx,    0*ly,  1.1*lz, 'top'    , 'left'   ,  'end'   ,     FALSE,

       0*lx,  1.1*ly,  1.1*lz, 'top'    , 'top'    ,  'start' ,     FALSE,
     0.5*lx,  1.1*ly,  1.1*lz, 'top'    , 'top'    ,  'middle',     FALSE,
       1*lx,  1.1*ly,  1.1*lz, 'top'    , 'top'    ,  'end'   ,     FALSE,
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


