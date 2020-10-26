#' Unit matrix with labeled points
#'
#'
#'
#' * `x`     is x coordinate of point
#' * `y`     is y coordinate of point
#' * `z`     is z coordinate of point
#' * `face`  is face of point
#' * `tb`    is tb is whether point is bottom or top, this is relative
#' * `rl`    is rl is whether point is right or left, this is relative
#' * `order` is for grouping in conjuction with `face` to pass to polygon
#'
#' @return           [tibble], with `x`, `y`, `z`, `face`, `tb`, `rl`, `order`
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
#' pts_cube_matrix(l=3) %}%
#'   center3(xc = 0, yc = 0,zc = 0,old_xc = 3/2,old_yc = 3/2,old_zc = 3/2) %}%
#'   rotate(70,20,0) %}%
#'   ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(
#'     mapping = aes(x = x, y = y, group = draw_order, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')
#' pts_cube_matrix(l=10) %}%
#'   center3(xc = 0, yc = 0,zc = 0,old_xc = 10/2,old_yc = 10/2,old_zc = 10/2) %}%
#'   rotate(70,20,0) %}%
#'   ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(
#'     mapping = aes(x = x, y = y, group = draw_order, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')

pts_cube_matrix = function(l) {
  tidyr::crossing(
    tidyr::crossing(ix = 1:l,iy = 1:l,iz = 1:l) %>% dplyr::mutate(shape_index = 1:n()),
    tribble(
      ~x, ~y, ~z, ~face   , ~tb     , ~rl     , ~point_order, ~face_order,
       0,  0,  0, 'front' , 'bottom', 'left'  ,            1,          1,
       0,  0,  1, 'front' , 'top'   , 'left'  ,            2,          1,
       1,  0,  1, 'front' , 'top'   , 'right' ,            3,          1,
       1,  0,  0, 'front' , 'bottom', 'right' ,            4,          1,

       0,  0,  1, 'top'   , 'bottom', 'left'  ,            1,          3,
       0,  1,  1, 'top'   , 'top'   , 'left'  ,            2,          3,
       1,  1,  1, 'top'   , 'top'   , 'right' ,            3,          3,
       1,  0,  1, 'top'   , 'bottom', 'right' ,            4,          3,

       1,  0,  0, 'right' , 'bottom', 'left'  ,            1,          2,
       1,  0,  1, 'right' , 'top'   , 'left'  ,            2,          2,
       1,  1,  1, 'right' , 'top'   , 'right' ,            3,          2,
       1,  1,  0, 'right' , 'bottom', 'right' ,            4,          2
      )
    ) %>%
    # update x,y,z to reflect face
    # dplyr::group_by(shape_index) %>%
    dplyr::mutate(
      x = ix - (1 - x),
      y = iy - (1 - y),
      z = iz - (1 - z)
    ) %>%
    # filter to keep exterior
    dplyr::filter(
        (face == 'right' & x == max(x))
      | (face == 'front' & y == min(y))
      | (face == 'top'   & z == max(z))
    ) %>%
    # artificial draw order
    dplyr::arrange(desc(iy),face_order,ix,desc(iz),point_order) %>%
    dplyr::mutate(draw_order = rep(1:(n()/4),each = 4))
}
