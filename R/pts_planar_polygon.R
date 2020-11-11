
#BEGIN: description
#' Create pts for a planar polygon
#'
#'
#' @usage
#' pts_planar_polygon(
#'   x = c(0,0,1,1),
#'   y = c(0,1,1,0),
#'   z = 0
#' )
#'
#' @param x   \[numeric\], length >= 1, x coordinates of polygon
#' @param y   \[numeric\], length >= 1, y coordinates of polygon
#' @param z   \[numeric\], length >= 1, z coordinates of polygon
#'
#' @return
#' [tibble] with columns
#' * `x` x coordinate of point
#' * `y` y coordinate of point
#' * `z` z coordinate of point
#' * `face` is face front, back, top, bottom, left or right
#' * `tb` is the point on top, bottom or middle of face
#' * `rl` is the point on left, right or middle of face
#' * `point_order` points draw order
#' * `face_order`  face draw order for default faces
#'
#' @export
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

pts_planar_polygon = function(
  x = c(0,0,1,1),
  y = c(0,1,1,0),
  z = 0
) {
  #BEGIN: setup params

  #END: setup params

  #BEGIN: param checks
  if (!all(c(length(x),length(y),length(z)) >= 1))
    rlang::abort('params x, y, z must satisfy length(v) >= 1')

  if (
    (length(x) == 1 && length(y) == 1) |
    (length(y) == 1 && length(z) == 1) |
    (length(x) == 1 && length(z) == 1))
    rlang::abort('exactly 2 of x, y, z can satisfy length(v) >= 2')
  #END: param checks

  # browser()
  #BEGIN: computation
  sx_ = max(x) - min(x)
  sy_ = max(y) - min(y)
  sz_ = max(z) - min(z)
  at_x_ = mean(x)
  at_y_ = mean(y)
  at_z_ = mean(z)

  tibble(
    x = x, y = y, z = z,
    face = NA, tb = NA, rl = NA,
    point_order = 1:max(length(x),length(y)), bounding_box = FALSE) %>%
    dplyr::bind_rows(
      pts_unit_bounding_box() %>%
        center3(0,0,0) %>%
        scale3(sx_ = !!sx_, sy_ = !!sy_, sz_ = !!sz_) %>%
        center3(at_x_ = !!at_x_, at_y_ = !!at_y_, at_z_ = !!at_z_)
    )
  #END: computation

  #BEGIN: return

  #END: return
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # default case is unit square
#' library(ggplot2)
#' pts_planar_polygon() %>%
#'   rotate3(70,20,keep_bounding = FALSE) -> data
#' ggplot() +
#'   coord_equal() +
#'   xlim(-2,2) +
#'   ylim(-2,2) +
#'   geom_polygon(data = data,
#'                mapping = aes(x = x, y = y),
#'                color = 'black', fill = NA)
#' #END: example
#'
#' #BEGIN: example
#' # {case description}
#' {code placeholder}
#' #END: example
#END: examples
