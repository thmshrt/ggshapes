
#BEGIN: description
#' Pts for cuboid axis labels
#'
#' @usage
#' pts_cuboid_axis_label(
#'   lx_ = 1,
#'   ly_ = 1,
#'   lz_ = 1,
#'   expand_ = 1.2,
#'   face_ = 'top',
#'   side_ = 'left',
#'   label.start = "start",
#'   label.middle = "middle",
#'   label.end = "end",
#'   reverse = FALSE
#' )
#'
#' @param lx_   \[numeric\], length 1, length of cuboid in x direction
#' @param ly_   \[numeric\], length 1, length of cuboid in y direction
#' @param lz_   \[numeric\], length 1, length of cuboid in z direction
#' @param expand_   \[numeric\], length 1, nonnegative, proportion to expand axis by
#' @param face_   \[character\], length 1, one of 'top','bottom','left','right','front', or 'back'
#' @param side_   \[character\], length 1, one of 'left','right','top', or 'bottom'
#' @param label.start   \[character\], length 1, default is "start", label for start of axis
#' @param label.middle   \[character\], length 1, default is "middle", label for middle of axis
#' @param label.end   \[character\], length 1, default is "end", label for end of axis
#' @param reverse   \[logical\], length 1, default FALSE, if true axis is from top to bottom
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
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

pts_cuboid_axis_label = function(
  lx_ = 1,
  ly_ = 1,
  lz_ = 1,
  expand_ = 1.2,
  face_ = 'top',
  side_ = 'left',
  label.start = "start",
  label.middle = "middle",
  label.end = "end",
  reverse = FALSE
) {

  # browser()
  #BEGIN: setup params
  lx_ = rlang::enexpr(lx_)
  ly_ = rlang::enexpr(ly_)
  lz_ = rlang::enexpr(lz_)
  expand_ = rlang::enexpr(expand_)
  #END: setup params

  #BEGIN: param checks
  if (!all(c(length(lx_),length(ly_),length(lz_),length(expand_)) == 1))
    rlang::abort(message = "params lx_, ly_, lz_, expand_ must satisfy length(v) == 1")

  if (!all(c(class(lx_),class(ly_),class(lz_),class(expand_)) %in% c('double','numeric','integer')))
    rlang::abort(message = "params lx_, ly_, lz_, expand_ must be of type 'double', 'numeric', or 'integer'")

  if (!all(c(lx_,ly_,lz_,expand_) >= 0))
    rlang::abort(message = "params lx_, ly_, lz_, expand_ must satisfy v >= 0")

  if (!all(c(length(face_),length(side_)) == 1))
    rlang::abort(message = "params face_, side_ must satisfy length(v) == 1")

  if (!(face_ %in% c("top","bottom","left","right","front","back")))
    rlang::abort(message = "params face_ must be one of 'top','bottom','left','right','front','back'")

  if (!(side_ %in% c("left","right","top","bottom")))
    rlang::abort(message = "params face_ must be one of 'left','right','top', or 'bottom'")

  if (!all(c(length(label.start),length(label.middle),length(label.end),length(expand_)) == 1))
    rlang::abort(message = "params label.start, label.middle, label.end must satisfy length(v) == 1")

  if (!all(c(class(label.start),class(label.middle),class(label.end)) %in% c('character')))
    rlang::abort(message = "params label.start, label.middle, label.end must be of type 'character'")
  #END: param checks

  if (face_ %in% c('left','right')) {
    tbl_labels = tibble(
      tb = c('bottom','middle','top'),
      label = c(label.start, label.middle, label.end)
    )
  } else if (face_ %in% c('top','bottom')) {
    tbl_labels = tibble(
      rl = c('left','middle','right'),
      label = c(label.start, label.middle, label.end)
    )
  }

  if (reverse)
    tbl_labels %>% dplyr::mutate(label = rev(label)) -> tbl_labels

  #BEGIN: computation
  if (side_ %in% c("left","right"))
    cond = rlang::expr(rl == !!side_)
  if (side_ %in% c("top","bottom"))
    cond = rlang::expr(tb == !!side_)

  pts_unit_cube(faces = face_) %>%
    center3(0,0,0) -> pts_ret
  if (face_ %in% c("front","back","left","right"))
    if (side_ %in% c("left","right")) {
      pts_ret %>% scale3(sx_ = !!expand_, sy_ = !!expand_, sz_ = 1) -> pts_ret
    } else if (side_ %in% c("top","bottom")) {
      pts_ret %>% scale3(sx_ = !!expand_, sy_ = 1, sz_ = !!expand_) -> pts_ret
    }

  if (face_ %in% c("top","bottom"))
    if (side_ %in% c("left","right")) {
      pts_ret %>% scale3(sx_ = !!expand_, sy_ = 1, sz_ = !!expand_) -> pts_ret
    } else if (side_ %in% c("top","bottom")) {
      pts_ret %>% scale3(sx_ = 1, sy_ = !!expand_, sz_ = !!expand_) -> pts_ret
    }

  pts_ret %>%
    center3(!!lx_/2,!!ly_/2,!!lz_/2) %>%
    dplyr::filter( !!cond | bounding_box) %>%
    dplyr::right_join(tbl_labels)
  #END: computation
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # default is top, left
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black', alpha = 0.5
#'   ) +
#'   geom_text(
#'     data = pts_cuboid_axis_labels() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, label = label, hjust = 'right'),
#'     colour = 'black'
#'   )
#' ##END: example
#'
#' #BEGIN: example
#' # changing to top, top
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_text(
#'     data = pts_cuboid_axis_labels(face_ = 'top',side_ = 'top') %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, label = label, hjust = 'right'),
#'     colour = 'black'
#'   )
#' ##END: example
#'
#' #BEGIN: example
#' # changing to top, top, and reversing
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_text(
#'     data = pts_cuboid_axis_labels(face_ = 'top',side_ = 'top', reverse = TRUE) %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, label = label, hjust = 'right'),
#'     colour = 'black'
#'   )
#' #END: example
#END: examples
