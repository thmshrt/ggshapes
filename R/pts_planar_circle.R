
#BEGIN: description
#' Pts for planar circle
#'
#'
#'@usage
#' pts_planar_circle(
#'   c1 = 0,
#'   c2 = 0,
#'   p = 0,
#'   r = 1,
#'   c1_var = x,
#'   c2_var = y,
#'   p_var = z,
#'   n = 50
#' )
#'
#' @param c1       \[numeric\], length 1, axis 1 coordinate for circle center
#' @param c2       \[numeric\], length 1, axis 2 coordinate for circle center
#' @param p       \[numeric\], length 1, third axis value for circle
#' @param r        \[numeric\], length 1, radius of circle
#' @param c1_var        \[symbol\], length 1, axis 1 variable for tibble
#' @param c2_var        \[symbol\], length 1, axis 2 variable for tibble
#' @param p_var        \[symbol\], length 1, planar axis variable for tibble
#' @param n        \[numeric\], length 1, points to use to approximate cirlce
#'
#' @return
#' [tibble] default
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

pts_planar_circle = function(
  c1 = 0,
  c2 = 0,
  p = 0,
  r = 1,
  c1_var = x,
  c2_var = y,
  p_var = z,
  n = 50
) {
  #BEGIN: setup params
  c1_var = rlang::ensym(c1_var)
  c2_var = rlang::ensym(c2_var)
  p_var = rlang::ensym(p_var)
  #END: setup params

  #BEGIN: param checks
  if (!all(c(length(c1),length(c2))==1))
    rlang::abort(message = 'params c1, c2 must satisfy length(v) == 1')

  if (!all(c(length(r),length(n))==1))
    rlang::abort(message = 'params r, n must satisfy length(v) == 1')
  #END: param checks

  #BEGIN: computation
  theta = seq(0,2*pi - 2*pi/n,length = n)
  tibble(
    !!c1_var := r * cos(theta) + c1,
    !!c2_var := r * sin(theta) + c2,
    !!p_var := p,
    face = NA, tb = NA, rl = NA,
    point_order = 1:n, face_order = NA, bounding_box = FALSE
  )
  #END: computation

  #BEGIN: return

  #END: return
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # default is circle of radius 1 centered at 0,0
#' library(ggplot2)
#' pts_planar_circle() -> data
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
#' # default is circle of radius 1 centered at 0,0
#' library(ggplot2)
#' pts_planar_circle(r = 2) -> data
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
#' library(ggplot2)
#' pts_planar_circle(c1 = 1, c2 = 1.5, r = .5) -> data
#' ggplot() +
#'   coord_equal() +
#'   xlim(-2,2) +
#'   ylim(-2,2) +
#'   geom_polygon(data = data,
#'                mapping = aes(x = x, y = y),
#'                color = 'black', fill = NA)
#' #END: example
#END: examples
