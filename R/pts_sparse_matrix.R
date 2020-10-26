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
#' pts_sparse_matrix(pts$ix,pts$iy,pts$iz,lx = 3, ly = 3, lz = 3) %}%
#'   center3(xc = 0, yc = 0,zc = 0,old_xc = 3/2,old_yc = 3/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   scale_into_view3(mwx = 2, mwy = 2, respect = TRUE) %}%
#'   # remove bounding points before viewing!
#'   dplyr::filter(!bounding) %}%
#'   ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(
#'     mapping = aes(x = x, y = y, group = draw_order, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')

pts_sparse_matrix = function(ix,iy,iz,lx,ly,lz) {

  tidyr::crossing(
    tibble(xi = ix,yi = iy,zi = iz) %>% dplyr::mutate(shape_index = 1:n()),
    tribble(
      ~x, ~y, ~z, ~face   , ~tb     , ~rl     , ~point_order, ~face_order, ~bounding,
       0,  0,  0, 'front' , 'bottom', 'left'  ,            1,           1,    FALSE,
       0,  0,  1, 'front' , 'top'   , 'left'  ,            2,           1,    FALSE,
       1,  0,  1, 'front' , 'top'   , 'right' ,            3,           1,    FALSE,
       1,  0,  0, 'front' , 'bottom', 'right' ,            4,           1,    FALSE,

       0,  0,  1, 'top'   , 'bottom', 'left'  ,            1,           3,    FALSE,
       0,  1,  1, 'top'   , 'top'   , 'left'  ,            2,           3,    FALSE,
       1,  1,  1, 'top'   , 'top'   , 'right' ,            3,           3,    FALSE,
       1,  0,  1, 'top'   , 'bottom', 'right' ,            4,           3,    FALSE,

       1,  0,  0, 'right' , 'bottom', 'left'  ,            1,           2,    FALSE,
       1,  0,  1, 'right' , 'top'   , 'left'  ,            2,           2,    FALSE,
       1,  1,  1, 'right' , 'top'   , 'right' ,            3,           2,    FALSE,
       1,  1,  0, 'right' , 'bottom', 'right' ,            4,           2,    FALSE
    )) %>%
    # update x,y,z
    # dplyr::group_by(shape_index) %>%
    dplyr::mutate(
      x = xi - (1 - x),
      y = yi - (1 - y),
      z = zi - (1 - z)
    ) %>%
    # filter to keep exterior
    # determine which are visible from top
    dplyr::group_by(x,y) %>%
    dplyr::mutate(is_z_max = z == max(z)) %>%
    dplyr::ungroup() %>%
    # determine which are visible from right
    dplyr::group_by(y,z) %>%
    dplyr::mutate(is_x_max = x == max(x)) %>%
    dplyr::ungroup() %>%
    # determine which are visible from front
    dplyr::group_by(x,z) %>%
    dplyr::mutate(is_y_min = y == min(y)) %>%
    dplyr::ungroup() %>%
    # # determine at face level
    # dplyr::group_by(shape_index,face) %>%
    # dplyr::mutate(is_outer = any(is_x_max | is_y_min | is_x_max)) %>%
    # dplyr::ungroup() %>%
    # dplyr::filter(is_outer) %>%
    # artificial draw order
    dplyr::arrange(desc(yi),face_order,xi,desc(zi),point_order) %>%
    dplyr::mutate(draw_order = rep(1:(n()/4),each = 4)) %>%
    # set bounding box
    dplyr::bind_rows(
      tribble(
        ~x, ~y, ~z, ~face   , ~tb     , ~rl     , ~point_order, ~face_order, ~bounding,
        0,  0,  0, 'front' , 'bottom', 'left'  ,           NA,          NA,       TRUE,
        0,  0, lz, 'front' , 'top'   , 'left'  ,           NA,          NA,       TRUE,
       lx,  0, lz, 'front' , 'top'   , 'right' ,           NA,          NA,       TRUE,
       lx,  0,  0, 'front' , 'bottom', 'right' ,           NA,          NA,       TRUE,
        0, ly,  0, 'back'  , 'bottom', 'left'  ,           NA,          NA,       TRUE,
        0, ly, lz, 'back'  , 'top'   , 'left'  ,           NA,          NA,       TRUE,
       lx, ly, lz, 'back'  , 'top'   , 'right' ,           NA,          NA,       TRUE,
       lx, ly,  0, 'back'  , 'bottom', 'right' ,           NA,          NA,       TRUE
      )
      )
}
