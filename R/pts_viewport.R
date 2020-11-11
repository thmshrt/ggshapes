
#BEGIN: description
#' Points for viewport
#'
#' @usage
#' pts_viewport(
#'   xc_ = 0,
#'   yc_ = 0,
#'   s_ = 1
#' )
#'
#' @param xc_   \[numeric\], length 1, non-negative, viewport x center, default is 0
#' @param yc_   \[numeric\], length 1, non-negative, viewport x center, default is 0
#' @param s_    \[numeric\], length 1, non-negative, amount to scale points by
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
#' @importFrom tibble tribble
#END: description
#BEGIN: code

pts_viewport = function(
  xc_ = 0,
  yc_ = 0,
  s_ = 1
) {

  #BEGIN: setup params
  xc_ = rlang::enexpr(xc_)
  yc_ = rlang::enexpr(yc_)
  s_ = rlang::enexpr(s_)
  #END: setup params

  #BEGIN: params check

  #END: params check

  #BEGIN: computation
  pts_unit_viewport() %>%
    # compute on points
    center3(at_x_ = 0, at_y_ = 0, use_bounding_box = FALSE) %>%
    scale3(sx_ = !!s_, sy_ = !!s_, sz_ = !!s_, use_bounding_box = FALSE) %>%
    center3(at_x_ = !!xc_, at_y_ = !!yc_, use_bounding_box = FALSE) %>%
    # compute on label points
    center3(at_x_ = 0, at_y_ = 0, x_ = label_x, y_ = label_y, use_bounding_box = FALSE) %>%
    scale3(sx_ = !!s_, sy_ = !!s_, sz_ = !!s_, x_ = label_x, y_ = label_y, use_bounding_box = FALSE) %>%
    scale3(sx_ = !!s_, x_ = size, use_bounding_box = FALSE) %>%
    center3(at_x_ = !!xc_, at_y_ = !!yc_, x_ = label_x, y_ = label_y, use_bounding_box = FALSE) %>%
    dplyr::mutate(label = sprintf('%+.3f\n%+.3f',x,y))
  #END: computation
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # default behavior is to a unit viewport centered at (x=0,y=0)
#' pts_viewport() -> data
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_point(
#'     data = data,
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_text(
#'     data = data,
#'     mapping = aes(x = label_x, y = label_y, label = sprintf('x:%+.2f\ny:%+.2f',x,y)),
#'     size = data$size
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # viewport with a new center and scale, scale also affects the size of the
#' # text. future versions of ggshapes will permit this to be optional
#' pts_viewport(xc_ = .5,yc_ = -.5,s_ = .5) -> data
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_point(
#'     data = data,
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_text(
#'     data = data,
#'     mapping = aes(x = label_x, y = label_y, label = sprintf('x:%+.2f\ny:%+.2f',x,y)),
#'     size = data$size
#'   )
#' #END: example
#'
#END: examples
