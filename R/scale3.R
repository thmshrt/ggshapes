
#BEGIN: description
#' Scale pts in 3D
#'
#' @usage
#' scale3(
#'   tbl,
#'   sx_ = 1, sy_ = 1, sz_ = 1,
#'   x_ = x, y_ = y, z_ = z,
#'   use_bounding_box = TRUE,
#'   keep_bounding = TRUE
#' )
#'
#' @param tbl    \[tibble\], a tibble containing points to be scaled
#' @param sx_   \[numeric\], length 1, default 1, value by which to scale `x` variable in `tbl`,
#' @param sy_   \[numeric\], length 1, default 1, value by which to scale `y` variable in `tbl`
#' @param sz_   \[numeric\], length 1, default 1, value by which to scale `z` variable in `tbl`
#' @param x_     \[expr\], variable name in `tbl` of `x` variable
#' @param y_     \[expr\], variable name in `tbl` of `y` variable
#' @param z_     \[expr\], variable name in `tbl` of `z` variable
#' @param use_bounding_box    \[logical\], length 1, default `TRUE`, \cr
#' if `FALSE` rotate3 does not use bounding box points to make computations \cr
#' default behavior is to use bounding box points to make computations
#' @param keep_bounding       \[logical\], length 1, default `TRUE`, \cr
#' if `FALSE` does not rbind the bounding box back after computation \cr
#' default behavior is to use bounding box points to make computations
#'
#' @return
#' * [tibble] with values
#' * `x` scaled x coordinate
#' * `y` scaled y coordinate
#' * `z` scaled z coordinate
#' * `face` is point part of "front", "back", "left", "right", "bottom" or "top" face
#' * `tb` is point on "top", "middle", "bottom" side of face
#' * `rl` is point on "right", "middle", "left" side of face
#' * `bounding_box` if `TRUE`, point is part of bounding box
#'
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

scale3 = function(
  tbl,
  sx_ = 1, sy_ = 1, sz_ = 1,
  x_ = x, y_ = y, z_ = z,
  use_bounding_box = TRUE,
  keep_bounding = TRUE
) {

  #BEGIN: setup params
  if (!('bounding_box' %in% names(tbl)) && use_bounding_box)
    rlang::abort(message = "if use_bounding_box=TRUE, then 'bounding_box' must be in 'tbl'")

  if (use_bounding_box == FALSE)
    keep_bounding = FALSE

  if (use_bounding_box || !keep_bounding) {
    tbl %>% dplyr::filter(bounding_box) -> tbl_bounding_box
    tbl %>% dplyr::filter(!bounding_box) -> tbl
  }

  sx_ = rlang::enexpr(sx_)
  sy_ = rlang::enexpr(sy_)
  sz_ = rlang::enexpr(sz_)

  x_ = rlang::enexpr(x_)
  y_ = rlang::enexpr(y_)
  z_ = rlang::enexpr(z_)
  #END: setup params

  #BEGIN: param checks
  if (!all(c(class(sx_),class(sy_),class(sz_)) %in% c('integer','double','numeric','name')))
    rlang::abort(message = "params sx_, sy_, sz_ must be of type 'integer','double','numeric', or 'name'")

  # identity case
  if (all(c(class(sx_),class(sy_),class(sz_)) %in% c('integer','double','numeric')))
    if (sx_ == 1 && sy_ == 1 && sz_ == 1) {
      if (keep_bounding) { return(dplyr::bind_rows(tbl,tbl_bounding_box)) }
      else { return(dplyr::bind_rows(tbl)) }
    }

  # membership
  char = as.character(c(x_,y_,z_))
  if (!all(char %in% names(tbl)))
    rlang::abort(message = "params x_, y_, z_ must relate to names in tbl")
  #END: param checks

  #BEGIN: computation
  tbl %>%
    dplyr::mutate(
      !!x_ := !!x_ * !!sx_,
      !!y_ := !!y_ * !!sy_,
      !!z_ := !!z_ * !!sz_,
    ) -> tbl_return

  tbl_bounding_box  %>%
    dplyr::mutate(
      !!x_ := !!x_ * !!sx_,
      !!y_ := !!y_ * !!sy_,
      !!z_ := !!z_ * !!sz_,
    ) -> tbl_bounding_box_return

  dplyr::bind_rows(
    tbl_return,
    if (keep_bounding) { tbl_bounding_box_return }
  )
  #END: computation
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # does nothing
#' pts_unit_cube() %>%
#'   scale3() %>%
#'   all.equal(pts_unit_cube(),.)
#' #END: example
#'
#' #BEGIN: example
#' # passing arguments
#' pts_unit_cube() %>%
#'   scale3(sx_ = 1,sy_ = 2,sz_ = 3) %>%
#'   all.equal(
#'     target = pts_unit_cube() %>% dplyr::mutate(x = x * 1, y = y * 2, z = z * 3),
#'     .
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # using variables
#' sx_ = 1; sy_ = 2; sz_ = 3;
#' pts_unit_cube() %>%
#'   scale3(sx_ = !!sx_,sy_ = !!sy_,sz_ = !!sz_) %>%
#'   all.equal(
#'     target = pts_unit_cube() %>% dplyr::mutate(x = x * 1, y = y * 2, z = z * 3),
#'     .
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # default case produces a unit cube at (x=0.5, y=0.5)
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       scale3() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black', alpha = 0.5
#'   )
#' # # we can overlap polygons to just that these transformations
#' # # are the same
#' # +
#' #   geom_polygon(
#' #     data = pts_unit_cube() %>%
#' #       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#' #     # center3(-1,1, keep_bounding = FALSE),
#' #     mapping = aes(x = x, y = y, group = face, fill = face),
#' #     colour = 'black', alpha = 0.5
#' #   )
#' #END: example
#'
#' #BEGIN: example
#' # scaling without centering results in expansion of shape outward
#' # along the axes
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       # center3(0,0,0) %>%
#'       scale3(sx = 1.5) %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black',
#'     alpha = 0.5
#'   ) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black', alpha = 0.5
#'   )
#' #END: example
#' #'
#' #BEGIN: example
#' # scaling after centering results in expansion of shape outward
#' # from the center point
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       center3(0,0,0) %>%
#'       scale3(sx = 1.5) %>%
#'       center3(0.5,0.5,0.5) %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black',
#'     alpha = 0.5
#'   ) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black', alpha = 0.5
#'   )
#' #END: example
#END: examples
