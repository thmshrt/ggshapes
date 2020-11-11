
#BEGIN: description
#' Scale a pts_tbl into a viewport
#'
#'
#' @usage
#' scale_into_viewport(
#'   tbl,
#'   xc_ = 0, yc_ = 0,
#'   side_ = 1,
#'   x_ = x, y_ = y,
#'   use_bounding_box = TRUE,
#'   keep_bounding = FALSE
#' )
#'
#' @param tbl       \[tibble\], tibble of points
#' @param xc_       \[numeric\], length 1, x center of viewport
#' @param yc_       \[numeric\], length 1, y center of viewport
#' @param side_     \[numeric\], length 1, nonnegative, the width of the side of the viewport
#' @param x_        \[symbol\], length 1, variable name to use as `x` in `tbl`
#' @param y_        \[symbol\], length 1, variable name to use as `y` in `tbl`
#' @param side_     \[numeric\], length 1, nonnegative, the width of the side of the viewport
#' @param use_bounding_box    \[logical\], length 1, default `TRUE`, \cr
#' if `FALSE` rotate3 does not use bounding box points to make computations \cr
#' default behavior is to use bounding box points to make computations
#' @param keep_bounding       \[logical\], length 1, default `TRUE`, \cr
#' if `FALSE` does not rbind the bounding box back after computation \cr
#' default behavior is to use bounding box points to make computations
#'
#' @return           [{type}]
#' * [tibble] with values
#' * `x` x coordinate of viewport
#' * `y` y coordinate of viewport
#' * `z` z coordinate of viewport
#' * `face` is point part of "front", "back", "left", "right", "bottom" or "top" face
#' * `tb` is point on "top", "middle", "bottom" side of face
#' * `rl` is point on "right", "middle", "left" side of face
#'
#' @export
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

scale_into_viewport = function(
  tbl,
  xc_ = 0, yc_ = 0,
  side_ = 1,
  x_ = x, y_ = y,
  use_bounding_box = TRUE,
  keep_bounding = FALSE
) {
  #BEGIN: setup params
  xc_ = rlang::enexpr(xc_)
  yc_ = rlang::enexpr(yc_)
  side_ = rlang::enexpr(side_)
  x_ = rlang::enexpr(x_)
  y_ = rlang::enexpr(y_)
  #END: setup params

  #BEGIN: setup params
  if (!('bounding_box' %in% names(tbl)) && use_bounding_box)
    rlang::abort(message = "if use_bounding_box=TRUE, then 'bounding_box' must be in 'tbl'")

  if (use_bounding_box == FALSE)
    keep_bounding = FALSE

  # if (!all(c(class(xc_),class(yc_),class(side_)) %in% c('double','numeric','integer')))
  #   rlang::abort(message = "params xc_, yc_, side_ must be of type 'double', 'numeric', or 'integer'")

  if (side_ < 0)
    rlang::abort(message = "param side_ must be non-negative")
  #END: param checks

  #BEGIN: computation
  if (use_bounding_box) {
    tbl %>%
      dplyr::filter(bounding_box) %>%
      dplyr::pull(!!x_) %>% range() %>% diff() -> width_x
    tbl %>%
      dplyr::filter(bounding_box) %>%
      dplyr::pull(!!y_) %>% range() %>% diff() -> width_y
  } else{
    tbl %>%
      dplyr::filter(!bounding_box) %>%
      dplyr::pull(!!x_) %>% range() %>% diff() -> width_x
    tbl %>%
      dplyr::filter(!bounding_box) %>%
      dplyr::pull(!!y_) %>% range() %>% diff() -> width_y
  }

  s_ = 1 / max(width_x, width_y)

  tbl %>%
    center3(at_x_ = 0, at_y_ = 0, at_z_ = 0) %>%
    scale3(sx_ = !!s_, sy_ = !!s_, sz_ = !!s_) %>%
    center3(at_x_ = !!xc_, at_y_ = !!yc_, keep_bounding = keep_bounding)
  #END: computation

  #BEGIN: return
  #END: return
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # default bevhavior is to scale into a unit square centered at (x=0,y=0)
#' pts_cuboid(lx_ = 1.5,ly_ = 1,lz_ = .5) %>%
#'   center3(0,-1,0) %>%
#'   rotate3(degx_ = 70, degy = 20) ->
#'   data
#'
#' data %>%
#'   scale_into_viewport() ->
#'   data_scaled
#'
#' data %>%
#'   dplyr::filter(!bounding_box) ->
#'   data
#'
#' pts_viewport() ->
#'   data_viewport
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,+2)) +
#'   ylim(c(-2,+2)) +
#'   # viewport
#'   geom_polygon(
#'     data = data_viewport,
#'     mapping = aes(x = x, y = y, group = face),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_point(
#'     data = data_viewport,
#'     mapping = aes(x = x, y = y, group = face),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_text(
#'     data = data_viewport,
#'     mapping = aes(x = label_x, y = label_y, label = label)
#'   ) +
#'   geom_polygon(
#'     data = data_scaled,
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   )
#' #END: example
#'
#' # visualization of default case
#' #BEGIN: example
#' pts_cuboid(lx_ = 1.5,ly_ = 1,lz_ = .5) %>%
#' center3(0,-1,0) %>%
#'   rotate3(degx_ = 70, degy = 20) ->
#'   data
#'
#' data %>%
#'   scale_into_viewport(xc_ = -1.0, yc_ = -1, keep_bounding = FALSE) ->
#'   data_scaled
#'
#' data %>%
#'   dplyr::filter(!bounding_box) ->
#'   data
#'
#' pts_viewport(xc_ = -1,yc_ = -1) ->
#'   data_viewport
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,+2)) +
#'   ylim(c(-2,+2)) +
#'   # viewport
#'   geom_polygon(
#'     data = data_viewport,
#'     mapping = aes(x = x, y = y, group = face),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_point(
#'     data = data_viewport,
#'     mapping = aes(x = x, y = y, group = face),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_text(
#'     data = data_viewport,
#'     mapping = aes(x = label_x, y = label_y, label = label)
#'   ) +
#'   geom_polygon(
#'     data = data_scaled,
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   )
#' #END: example
#'
#END: examples
