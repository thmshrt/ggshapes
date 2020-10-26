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
#' pts_matrix_slice(3,3,3,1,1,1,2,1,3) %}%
#'   center3(0,0,0,old_xc = 3/2,old_yc = 3/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   dplyr::filter(!bounding) -}
#'   matrix
#
#' ##
#' pts_matrix_slice_labels(3,3,3,1,1,1,2,1,3) %}%
#'   center3(0,0,0,old_xc = 3/2,old_yc = 3/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   scale_into_view3(use_bounding = TRUE) %}%
#'   dplyr::filter(!bounding) %}%
#'   dplyr::mutate(label = paste(face,side,position,sep=',')) %}%
#'   dplyr::mutate(angle = ifelse(side =='top',90,0)) -}
#'   labels
#
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

pts_matrix_slice_combined_axes = function(lx,ly,lz,ix,ixend,iy,iyend,iz,izend) {
  pts_matrix_slice_axes(lx,ly,lz,ix,ixend,iy,iyend,iz,izend)
}

pts_matrix_slice_combined_labels = function(lx,ly,lz,ix,ixend,iy,iyend,iz,izend) {
  tribble(
    ~x                 , ~y                  , ~z                  , ~face    , ~side    , ~position, ~bounding,
    -0.2*lx            , -0.2*iyend          ,  1.0*izend          , 'front'  , 'left'   ,  'start' ,     FALSE,
    -0.2*lx            , -0.2*iyend          ,  iz-1+(izend-iz+1)/2, 'front'  , 'left'   ,  'middle',     FALSE,
    -0.2*lx            , -0.2*iyend          ,  iz-1               , 'front'  , 'left'   ,  'end'   ,     FALSE,

    -0.2*lx            ,  1.0*iyend          ,  1.2*izend          , 'top'    , 'left'   ,  'start' ,     FALSE,
    -0.2*lx            ,  iy-1+(iyend-iy+1)/2,  1.2*izend          , 'top'    , 'left'   ,  'middle',     FALSE,
    -0.2*lx            ,  iy-1               ,  1.2*izend          , 'top'    , 'left'   ,  'end'   ,     FALSE,

    ix-1               ,  1.2*iyend          ,  1.2*izend          , 'top'    , 'top'    ,  'start' ,     FALSE,
    ix-1+(ixend-ix+1)/2,  1.2*iyend          ,  1.2*izend          , 'top'    , 'top'    ,  'middle',     FALSE,
    1.0*ixend          ,  1.2*iyend          ,  1.2*izend          , 'top'    , 'top'    ,  'end'   ,     FALSE
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


