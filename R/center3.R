
#BEGIN: description
#' Center pts in 3D
#'
#' {description placeholder}
#'
#' @usage
#' center3(
#'   tbl,
#'   at_x_ = NULL, at_y_ = NULL, at_z_ = NULL,
#'   x_ = x, y_ = y, z_ = z,
#'   use_bounding_box = TRUE,
#'   keep_bounding = TRUE
#' )
#'
#' @param tbl     \[tibble\], a tibble containing points to be centered
#' @param at_x_   \[numeric\], length 1, default NULL, \cr
#' value by which to center `x` variable in `tbl` \cr
#' default behavior is to leave center unchanged
#' @param at_y_   \[numeric\], length 1, default NULL, \cr
#' value by which to center `y` variable in `tbl` \cr
#' default behavior is to leave center unchanged
#' @param at_z_   \[numeric\], length 1, default NULL, \cr
#' value by which to center `z` variable in `tbl` \cr
#' default behavior is to leave center unchanged
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
#' * `x` centered x coordinate
#' * `y` centered y coordinate
#' * `z` centered z coordinate
#' * `face` is point part of "front", "back", "left", "right", "bottom" or "top" face
#' * `tb` is point on "top", "middle", "bottom" side of face
#' * `rl` is point on "right", "middle", "left" side of face
#' * `bounding_box` if `TRUE`, point is part of bounding box
#'
#' @export
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

center3 = function(
  tbl,
  at_x_ = NULL, at_y_ = NULL, at_z_ = NULL,
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

  at_x_ = rlang::enexpr(at_x_)
  at_y_ = rlang::enexpr(at_y_)
  at_z_ = rlang::enexpr(at_z_)

  x_ = rlang::enexpr(x_)
  y_ = rlang::enexpr(y_)
  z_ = rlang::enexpr(z_)
  #END: setup params

  #BEGIN: param checks
  # identity case
  if (is.null(at_x_) && is.null(at_y_) && is.null(at_z_)) {
    if (keep_bounding) { return(dplyr::bind_rows(tbl,tbl_bounding_box)) }
    else { return(dplyr::bind_rows(tbl)) }
  }

  # set null center parameters to means
  if (is.null(at_x_)) at_x_ = rlang::expr(mean(!!x_))
  if (is.null(at_y_)) at_y_ = rlang::expr(mean(!!y_))
  if (is.null(at_z_)) at_z_ = rlang::expr(mean(!!z_))

  # membership
  char = as.character(c(x_,y_,z_))
  if (!all(char %in% names(tbl)))
    rlang::abort(message = "params x_, y_, z_ must relate to names in tbl")
  #END: param checks

  # browser()
  #BEGIN: computation
  if (use_bounding_box) {
    # centers are based on bounding box
    tbl_bounding_box %>%
      dplyr::mutate(
        xc = mean(!!x_),
        yc = mean(!!y_),
        zc = mean(!!z_),
        at_x = !!at_x_,
        at_y = !!at_y_,
        at_z = !!at_z_,
      ) -> tbl_bounding_box

    tbl_bounding_box %>%
      dplyr::slice(1) %>%
      dplyr::select(xc,yc,zc,at_x,at_y,at_z) %>%
      dplyr::bind_cols(tbl,.) ->
      tbl
  } else {
    # centers are based on shape
    tbl %>%
      dplyr::mutate(
        xc = mean(!!x_),
        yc = mean(!!y_),
        zc = mean(!!z_),
        at_x = !!at_x_,
        at_y = !!at_y_,
        at_z = !!at_z_,
      ) -> tbl
  }

  tbl %>%
    dplyr::mutate(
      !!x_ := !!x_ - xc + at_x,
      !!y_ := !!y_ - yc + at_y,
      !!z_ := !!z_ - zc + at_z,
    ) %>%
    dplyr::select(-xc,-yc,-zc,-at_x,-at_y,-at_z) -> tbl_return

  if (keep_bounding) {
    tbl_bounding_box %>%
      dplyr::mutate(
        !!x_ := !!x_ - xc + at_x,
        !!y_ := !!y_ - yc + at_y,
        !!z_ := !!z_ - zc + at_z,
      ) %>%
      dplyr::select(-xc,-yc,-zc,-at_x,-at_y,-at_z) -> tbl_bounding_box_return
  }

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
#' # default case, the shape remains centered at its original location
#' pts_unit_cube() %>%
#'   center3() %>%
#'   all.equal(pts_unit_cube(),.)
#' #END: example
#'
#' #BEGIN: example
#' # passing values
#' pts_unit_cube() %>%
#'   center3(1,0,0) %>%
#'   all.equal(
#'     target = pts_unit_cube() %>% dplyr::mutate(x = x + 0.5, y = y - 0.5, z = z - 0.5),
#'     .)
#' #END: example
#'
#' #BEGIN: example
#' # passing variables
#' at_x_ = 1; at_y_ = 0; at_z_ = 0;
#' pts_unit_cube() %>%
#'   center3(!!at_x_,!!at_y_,!!at_z_) %>%
#'   all.equal(
#'     target = pts_unit_cube() %>% dplyr::mutate(x = x + 0.5, y = y - 0.5, z = z - 0.5),
#'     .)
#' #END: example
#'
#' #BEGIN: example
#' # default case, the shape remains centered at its original location
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       center3(keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # a new center at (x=-1.5,y=1)
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       center3(at_x_ = -1.5,at_y_ = 1),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = tibble(),
#'     mapping = aes(x = -1.5, y = 1)
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # a new center at (x=-1.5,y=1)
#' library(ggplot2)
#' foox = -1.5
#' fooy = 1
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'     center3(at_x_ = !!foox,at_y_ = !!fooy),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = tibble(),
#'     mapping = aes(x = -1.5, y = 1)
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # rotation at a point can be done using center3(0,0,0) %>% rotate3 %>% center3
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       center3(0,0,0) %>%
#'       rotate3(70,20) %>%
#'       center3(at_x_ = -1.5,at_y_ = 1,keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = tibble(),
#'     mapping = aes(x = -1, y = 1)
#'   )
#' #END: example
#END: examples
