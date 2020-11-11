
#BEGIN: description
#' Pts for a planar ellipse
#'
#'
#' @usage
#' pts_planar_ellipse(
#'   c1 = 0,
#'   c2 = 0,
#'   p = 0,
#'   c1_var = x,
#'   c2_var = y,
#'   p_var = z,
#'   a = 1,
#'   b = 1,
#'   n = 50
#' )
#'
#' @param c1       \[numeric\], center on axis 1
#' @param c2       \[numeric\], center on axis 2
#' @param p       \[numeric\], value for planar axis
#' @param c1_var   \[symbol\], name of axis 1
#' @param c2_var   \[symbol\], name of axis 2
#' @param p_var   \[symbol\], name of axis p
#' @param a   \[symbol\], expansion in axis 1
#' @param b   \[symbol\], expansion in axis 2
#' @param n   \[symbol\], number of points to approximate
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

pts_planar_ellipse = function(
  c1 = 0,
  c2 = 0,
  p = 0,
  c1_var = x,
  c2_var = y,
  p_var = z,
  a = 1,
  b = 1,
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

  if (!all(c(length(a),length(b),length(n))==1))
    rlang::abort(message = 'params a, b, n must satisfy length(v) == 1')
  #END: param checks

  #BEGIN: computation
  theta = seq(0,2*pi - 2*pi/n,length = n)
  tibble(
    !!c1_var := a * cos(theta) + c1,
    !!c2_var := b * sin(theta) + c2,
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
#' pts_planar_ellipse() -> data
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
#' # a stretches the ellipse in the "horizontal" direction
#' # b stretches the ellipse in the "vertical" direction
#' library(ggplot2)
#' pts_planar_ellipse(a = 1.5, b = 1) -> data
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
