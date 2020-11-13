
#BEGIN: description
#' Pts for a planar points
#'
#'
#' @usage
#' pts_planar_points(
#'   pts1_ = c(0,0,1,1),
#'   pts2_ = c(0,1,0,1),
#'   c1_ = mean(pts1_),
#'   c2_ = mean(pts2_),
#'   l1_ = 1,
#'   l2_ = 1,
#'   p_ = 0,
#'   var1_ = x,
#'   var2_ = y,
#'   varp_ = z
#' )
#'
#' @param pts1_    \[numeric\], length any, pts values axis 1
#' @param pts2_    \[numeric\], length any, pts values on axis 2
#' @param c1_        \[numeric\], length 1, center of points along axis 1, default mean(pts_l1_)
#' @param c2_        \[numeric\], length 1, center of points along axis 2, default mean(pts_l2_)
#' @param l1_        \[numeric\], length 1, length along axis 1, default is 1
#' @param l2_        \[numeric\], length 1, length along axis 2, default is 1
#' @param p_         \[numeric\], length 1, value for planar axis, default is 0
#' @param var1_     \[symbol\], name of axis 1, default is x
#' @param var2_     \[symbol\], name of axis 2, default is y
#' @param varp_      \[symbol\], name of axis p, default is z
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
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

pts_planar_points = function(
  pts1_ = c(0,0,1,1),
  pts2_ = c(0,1,0,1),
  c1_ = mean(pts1_),
  c2_ = mean(pts2_),
  l1_ = 1,
  l2_ = 1,
  p_ = 0,
  var1_ = x,
  var2_ = y,
  varp_ = z
) {

  ##BEGIN: param checks
  if (!(length(pts1_) == length(pts2_)))
    rlang::abort("params pts1_ and pts2_ must of same length")

  if (!all(c(class(pts1_),class(pts2_)) %in% c('numeric','double','integer')))
    rlang::abort("params pts1_, pts2_ must be one of types 'numeric','double','integer'")

  c1_ = rlang::enexpr(c1_) %>% rlang::eval_tidy(data = list(pts1_ = pts1_))
  c2_ = rlang::enexpr(c2_) %>% rlang::eval_tidy(data = list(pts2_ = pts2_))

  if (!all(c(class(l1_),class(l2_),class(p_)) %in% c('numeric','double','integer')))
    rlang::abort("params l1_, l2_, p_, must be one of types 'numeric','double','integer'")

  if (!all(c(length(l1_),length(l2_),length(p_)) == 1))
    rlang::abort("params l1_, l2_, p_, must satisfy length(v) == 1")

  pts_l1_width = pts1_ %>% range() %>% diff()
  pts_l2_width = pts2_ %>% range() %>% diff()
  if (!(pts_l1_width <= l1_ && pts_l2_width <= l2_))
    rlang::abort("diff(range(v)) of pts1_, pts2_ must be <= l1_ and l2_ respectively")

  var1_ = rlang::enexpr(var1_)
  var2_ = rlang::enexpr(var2_)
  varp_ = rlang::enexpr(varp_)

  if (!all(c(length(var1_),length(var2_),length(varp_))== 1))
    rlang::abort("params var1_, var2_, varp_ must satisfy length(v) == 1")
  ##END: param checks

  ##BEGIN: computation
  pts_unit_bounding_box() %>%
    dplyr::filter(face == 'top') %>%
    scale3(sx_ = !!l1_, sy_ = !!l2_, sz_ = 0) %>%
    center3(!!c1_, !!c2_, !!p_) %>%
    dplyr::bind_rows(
      tibble(pts1_,pts2_,p_,bounding_box = FALSE) %>%
        dplyr::rename(!!var1_ := pts1_,
                      !!var2_ := pts2_,
                      !!varp_ := p_) %>%
        center3(!!c1_, !!c2_, !!p_, use_bounding_box = FALSE)
    ) %>%
    dplyr::arrange(bounding_box)
  ##END: computation
}

#END: code
#BEGIN: examples
#' @examples
#' ##BEGIN: example
#' ## default case is unit square
#' library(ggplot2)
#' ggplot() +
#'   xlim(-2,2) +
#'   ylim(-2,2) +
#'   geom_polygon(
#'     data = pts_planar_points() %>% dplyr::filter(bounding_box),
#'     mapping = aes(x = x, y = y, group = face, fill = face)
#'   ) +
#'   geom_point(
#'     data = pts_planar_points() %>% dplyr::filter(!bounding_box),
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   )
#' ##END: example
#'
#' ##BEGIN: example
#' ## c1_, c2_ should be supplied when not using defaults
#' library(ggplot2)
#' set.seed(20200920)
#' pts_planar_points(
#'   pts1_ = runif(20),
#'   pts2_ = runif(20),
#'   c1_ = 0.5,
#'   c2_ = 0.5
#' ) -> data
#'
#' ggplot() +
#'   xlim(-2,2) +
#'   ylim(-2,2) +
#'   geom_polygon(
#'     data = data %>% dplyr::filter(bounding_box),
#'     mapping = aes(x = x, y = y, group = face, fill = face)
#'   ) +
#'   geom_point(
#'     data = data %>% dplyr::filter(!bounding_box),
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   )
#' #' ##END: example
#'
#' ##BEGIN: example
#' ## set the center to anything you like
#' library(ggplot2)
#' set.seed(20200920)
#' pts_planar_points(
#'   pts1_ = runif(20),
#'   pts2_ = runif(20),
#'   c1_ = -0.5,
#'   c2_ = 0.5
#' ) -> data
#'
#' ggplot() +
#'   xlim(-2,2) +
#'   ylim(-2,2) +
#'   geom_polygon(
#'     data = data %>% dplyr::filter(bounding_box),
#'     mapping = aes(x = x, y = y, group = face, fill = face)
#'   ) +
#'   geom_point(
#'     data = data %>% dplyr::filter(!bounding_box),
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   )
#' ##END: example
#'
#' ##BEGIN: example
#' ## set the center to anything you like
#' library(ggplot2)
#' set.seed(20200920)
#' pts_planar_points(
#'   pts1_ = runif(20),
#'   pts2_ = runif(20),
#'   c1_ = -0.5,
#'   c2_ = 0.5
#' ) %>%
#'   rotate3(70,20) -> data
#'
#' ggplot() +
#'   xlim(-2,2) +
#'   ylim(-2,2) +
#'   geom_polygon(
#'     data = data %>% dplyr::filter(bounding_box),
#'     mapping = aes(x = x, y = y, group = face),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_point(
#'     data = data %>% dplyr::filter(!bounding_box),
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   )
#' ##END: example
#END: examples
