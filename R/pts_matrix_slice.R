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
#' pts_matrix_slice(lx = 3, ly = 3, lz = 3,1,1,1,2,1,3) %}%
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
pts_matrix_slice = function(lx,ly,lz,ix,ixend,iy,iyend,iz,izend) {

  tidyr::crossing(
    tidyr::crossing(xi = 1:lx,yi = 1:ly,zi = 1:lz) %>% dplyr::mutate(shape_index = 1:n()),
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
    # update x,y,z to reflect face
    # dplyr::group_by(shape_index) %>%
    dplyr::mutate(
      x = xi - (1 - x),
      y = yi - (1 - y),
      z = zi - (1 - z)
    ) %>%
    # slice matrix
    dplyr::filter(
        (ix <= xi & xi <= ixend)
      & (iy <= yi & yi <= iyend)
      & (iz <= zi & zi <= izend)
    ) %>%
    # filter to keep exterior
    dplyr::filter(
        (face == 'right' & x == max(x))
      | (face == 'front' & y == min(y))
      | (face == 'top'   & z == max(z))
    ) %>%
    # artificial draw order
    dplyr::arrange(desc(yi),face_order,xi,desc(zi),point_order) %>%
    dplyr::mutate(draw_order = rep(1:(n()/4),each = 4)) %>%
    dplyr::bind_rows(
      tribble(
        # bounding points
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
