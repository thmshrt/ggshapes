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
#' pts_matrix_slice(3,3,3,1,1,1,2,2,3) %}%
#'   center3(0,0,0,old_xc = 3/2,old_yc = 3/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   scale_into_view3(mwx = 2,mwy = 2) %}%
#'   dplyr::filter(!bounding) -}
#'   matrix
#
#' pts_matrix_slice_axes(3,3,3,1,1,1,2,2,3) %}%
#'   center3(0,0,0,old_xc = 3/2,old_yc = 3/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   scale_into_view3(mwx = 2,mwy = 2, use_bounding = TRUE) %}%
#'   dplyr::filter(!bounding) %}%
#'   dplyr::select(x,y,z,face,side,position) %}%
#'   tidyr::pivot_wider(names_from = position, values_from  = c(x,y,z)) -}
#'   axes
#
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(
#'     data = matrix,
#'     mapping = aes(x = x, y = y, group = draw_order, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_segment(
#'     data = axes,
#'     mapping = aes(
#'       x = x_start, xend = x_end, y = y_start, yend = y_end
#'     ),
#'     arrow = arrow(angle = 15,length = unit(.25,'cm')),
#'     colour = 'black'
#'   ) +
#'   geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')

pts_matrix_slice_axes = function(lx,ly,lz,ix,ixend,iy,iyend,iz,izend) {
  tribble(
    ~x           , ~y           , ~z           , ~face    , ~side    , ~position, ~bounding,
    -0.1*lx      , -0.1*iyend   ,  1.0*izend   , 'front'  , 'left'   ,  'start' ,     FALSE,
    -0.1*lx      , -0.1*iyend   ,      iz -1   , 'front'  , 'left'   ,  'end'   ,     FALSE,

    -0.1*lx      ,  1.0*iyend   ,  1.1*izend   , 'top'    , 'left'   ,  'start' ,     FALSE,
    -0.1*lx      ,  iy-1        ,  1.1*izend   , 'top'    , 'left'   ,  'end'   ,     FALSE,

     ix-1        ,  1.1*iyend   ,  1.1*izend   , 'top'    , 'top'    ,  'start' ,     FALSE,
     1.0*ixend   ,  1.1*iyend   ,  1.1*izend   , 'top'    , 'top'    ,  'end'   ,     FALSE
  ) %>%
    dplyr::bind_rows(
      tribble(
        # extreme points
        ~x, ~y, ~z, ~face   , ~tb     , ~rl     , ~point_order, ~face_order, ~bounding,
        0,  0,  0, 'front' , 'bottom', 'left'  ,           NA,          NA,    TRUE,
        0,  0, lz, 'front' , 'top'   , 'left'  ,           NA,          NA,    TRUE,
       lx,  0, lz, 'front' , 'top'   , 'right' ,           NA,          NA,    TRUE,
       lx,  0,  0, 'front' , 'bottom', 'right' ,           NA,          NA,    TRUE,
        0, ly,  0, 'back'  , 'bottom', 'left'  ,           NA,          NA,    TRUE,
        0, ly, lz, 'back'  , 'top'   , 'left'  ,           NA,          NA,    TRUE,
       lx, ly, lz, 'back'  , 'top'   , 'right' ,           NA,          NA,    TRUE,
       lx, ly,  0, 'back'  , 'bottom', 'right' ,           NA,          NA,    TRUE
      )
      )
}
