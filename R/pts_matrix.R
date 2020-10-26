#' Unit matrix with labeled points
#'
#' {description placeholder}
#'
#' @param {param}
#'
#'
#' @return           [tibble], with `x`, `y`, `z`, `face`, `tb`, `rl`, `order`
#' * `x`     is x coordinate of point
#' * `y`     is y coordinate of point
#' * `z`     is z coordinate of point
#' * `face`  is face of point
#' * `tb`    is tb is whether point is bottom or top, this is relative
#' * `rl`    is rl is whether point is right or left, this is relative
#' * `order` is for grouping in conjuction with `face` to pass to polygon
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
#' lx = 3
#' ly = 3
#' lz = 3
#' xc = -1
#' yc = 0
#' zc = 0
#' pts_matrix(lx = lx, ly = ly, lz = lz) %>%
#'   dplyr::mutate(lx,ly,lz,xc,yc,zc) %>%
#'   center3(xc = 0, yc = 0, zc = 0,old_xc = lx[1]/2,old_yc = ly[1]/2,old_zc = lz[1]/2) %>%
#'   rotate(70,20,0) %>%
#'   scale_into_view3(mwx = 2, mwy = 2, respect = TRUE) %>%
#'   center3(xc = xc, yc = yc, zc = zc,old_xc = 0,old_yc = 0,old_zc = 0) %>%
#'   ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-4,4)) +
#'   scale_y_continuous(limits = c(-4,4)) +
#'   geom_viewport(data = tibble(), mapping = aes(xc = xc, yc = yc, h = 2, w = 2)) +
#'   geom_polygon(
#'     mapping = aes(x = x, y = y, group = draw_order, fill = face),
#'     colour = 'black'
#'   )

pts_matrix = function(lx,ly,lz) {
  tidyr::crossing(
    tidyr::crossing(ix = 1:lx,iy = 1:ly,iz = 1:lz) %>% dplyr::mutate(shape_index = 1:n()),
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
