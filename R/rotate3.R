
#BEGIN: description
#' Rotate pts in 3D
#'
#' @usage
#' rotate3(
#'   tbl,
#'   degx_ = NULL, degy_ = NULL, degz_ = NULL,
#'   x_ = x, y_ = y, z_ = z,
#'   xc_ = mean(x), yc_ = mean(y), zc_ = mean(z),
#'   use_bounding_box = TRUE,
#'   keep_bounding = TRUE
#' )
#'
#' @param tbl    \[tibble\], a tibble containing points to be rotated
#' @param degx_   \[numeric\], length 1, degrees anticlockwise about x axis to rotate shape
#' @param degy_   \[numeric\], length 1, degrees anticlockwise about y axis to rotate shape
#' @param degz_   \[numeric\], length 1, degrees anticlockwise about z axis to rotate shape
#' @param x_     \[expr\], variable name in `tbl` of `x` variable
#' @param y_     \[expr\], variable name in `tbl` of `y` variable
#' @param z_     \[expr\], variable name in `tbl` of `z` variable
#' @param xc_    \[expr\], expression describing `xc_` in terms of variables in `tbl`
#' @param yc_    \[expr\], expression describing `yc_` in terms of variables in `tbl`
#' @param zc_    \[expr\], expression describing `zc_` in terms of variables in `tbl`
#' @param use_bounding_box    \[logical\], length 1, default `TRUE`, \cr
#' if `FALSE` rotate3 does not use bounding box points to make computations \cr
#' default behavior is to use bounding box points to make computations
#' @param keep_bounding       \[logical\], length 1, default `TRUE`, \cr
#' if `FALSE` does not rbind the bounding box back after computation \cr
#' default behavior is to use bounding box points to make computations
#'
#' @return
#' * [tibble] with values
#' * `x` rotated x coordinate
#' * `y` rotated y coordinate
#' * `z` rotated z coordinate
#' * `face` is point part of "front", "back", "left", "right", "bottom" or "top" face
#' * `tb` is point on "top", "middle", "bottom" side of face
#' * `rl` is point on "right", "middle", "left" side of face
#' * `bounding_box` if `TRUE`, point is part of bounding box
#'
#' @export
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

rotate3 = function(
  tbl,
  degx_ = NULL, degy_ = NULL, degz_ = NULL,
  x_ = x, y_ = y, z_ = z,
  xc_ = mean(x), yc_ = mean(y), zc_ = mean(z),
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

  # use <var>_ to permit tidyverse to distinguish between
  # tibble column names and passed parameters
  degx_ = rlang::enexpr(degx_)
  degy_ = rlang::enexpr(degy_)
  degz_ = rlang::enexpr(degz_)

  x_ = rlang::enexpr(x_)
  y_ = rlang::enexpr(y_)
  z_ = rlang::enexpr(z_)

  xc_ = rlang::enexpr(xc_)
  yc_ = rlang::enexpr(yc_)
  zc_ = rlang::enexpr(zc_)

  #END: setup params

  #BEGIN: param checks
  # identity case
  if (is.null(degx_) && is.null(degy_) && is.null(degz_)) {
    if (keep_bounding) { return(dplyr::bind_rows(tbl,tbl_bounding_box)) }
    else { return(dplyr::bind_rows(tbl)) }
  }

  # set null degrees to 0
  if (is.null(degx_)) degx_ = rlang::expr(0)
  if (is.null(degy_)) degy_ = rlang::expr(0)
  if (is.null(degz_)) degz_ = rlang::expr(0)

  # presence of variables in tbl is checked by dplyr
  #END: param checks

  #BEGIN: computation
  # browser()
  if (use_bounding_box) {
    # centers are based on bounding box
    tbl_bounding_box %>%
      dplyr::mutate(
        xc = !!xc_,
        yc = !!yc_,
        zc = !!zc_
      ) -> tbl_bounding_box

    tbl_bounding_box %>%
      dplyr::slice(1) %>%
      dplyr::select(xc,yc,zc) %>%
      dplyr::bind_cols(tbl,.) ->
      tbl
  } else {
    # centers are based on shape
    tbl %>%
      dplyr::mutate(
        xc = !!xc_,
        yc = !!yc_,
        zc = !!zc_
      ) -> tbl
  }

  # compute rotation on each of points and bounding
  tbl %>%
    dplyr::mutate(
      !!x_ := !!x_ - xc,
      !!y_ := !!y_ - yc,
      !!z_ := !!z_ - zc
    ) %>%
    dplyr::select(!!x_,!!y_,!!z_) %>%
    cbind() -> points

  if (keep_bounding) {
    tbl_bounding_box %>%
      dplyr::mutate(
        !!x_ := !!x_ - xc,
        !!y_ := !!y_ - yc,
        !!z_ := !!z_ - zc
      ) %>%
      dplyr::select(!!x_,!!y_,!!z_) %>%
      cbind() -> points_bounding
  }

  # convert degrees to radians
  # form rotation matrices
  radx = pi/180 * rlang::eval_bare(degx_)
  R.x = rbind(
    c(         1,         0,         0),
    c(         0, cos(radx), sin(radx)),
    c(         0,-sin(radx), cos(radx))
  )

  rady = pi/180 * rlang::eval_bare(degy_)
  R.y = rbind(
    c( cos(rady),         0,-sin(rady)),
    c(         0,         1,         0),
    c( sin(rady),         0, cos(rady))
  )

  radz = pi/180 * rlang::eval_bare(degz_)
  R.z = rbind(
    c( cos(radz), sin(radz),         0),
    c(-sin(radz), cos(radz),         0),
    c(         0,         0,         1)
  )

  # transform
  points %>%
    t() %>%
    `%*%`(R.x,.) %>%
    `%*%`(R.y,.) %>%
    `%*%`(R.z,.) %>%
    t() -> rotated_points

  if (keep_bounding) {
    points_bounding %>%
      t() %>%
      `%*%`(R.x,.) %>%
      `%*%`(R.y,.) %>%
      `%*%`(R.z,.) %>%
      t() -> rotated_points_bounding
    }

  #END: computation

  #BEGIN: return
  tbl %>% dplyr::mutate(
    !!x_ := rotated_points[,1] + xc,
    !!y_ := rotated_points[,2] + yc,
    !!z_ := rotated_points[,3] + zc
    ) %>%
    dplyr::select(-xc,-yc,-zc) -> tbl_return

  if (keep_bounding) {
    tbl_bounding_box %>% dplyr::mutate(
      !!x_ := rotated_points_bounding[,1] + xc,
      !!y_ := rotated_points_bounding[,2] + yc,
      !!z_ := rotated_points_bounding[,3] + zc
    ) %>%
      dplyr::select(-xc,-yc,-zc) -> tbl_bounding_box_return
  }

  dplyr::bind_rows(
    tbl_return,
    if (keep_bounding) { tbl_bounding_box_return }
  )
  #END: return
}


#END: code
#BEGIN: examples
#' @examples
#'
#' #BEGIN: example
#' # default case does nothing
#' pts_unit_bounding_box() %>%
#'   rotate3() %>%
#'   all.equal(pts_unit_bounding_box(),.)
#' #END: example
#'
#' #BEGIN: example
#' # passing vars
#' degx_ = 70; degy_ = 20;
#' pts_unit_bounding_box() %>%
#'   rotate3(!!degx_,!!degy_) %>%
#'   all.equal(
#'     pts_unit_bounding_box() %>% rotate3(70,20),
#'     .)
#' #END: example
#'
#' #BEGIN: example
#' # default case using the unit bounding box results in us viewing the top
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_bounding_box() %>% rotate3(),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # deg_ parameters rotates the shape anticlockwise around the specified axis
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       rotate3(degx_ = 70,degy_ = 20,keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = tibble(x = 0.5, y = 0.5, z = 0.5),
#'     mapping = aes(x = x, y = y)
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # by default computation will use a bounding box to perform centering
#' # rotations and scalings. this is a result of ggshapes choosing a "lite"
#' # path resulting in only a partial set of points to represent a shape
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       rotate3(degx_ = 70,degy_ = 20,use_bounding_box = TRUE, keep_bounding = FALSE) %>%
#'       dplyr::mutate(x = x + 0.5),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_label(
#'     data = tibble(),
#'     mapping = aes(x =  1, y = -1, label = '(default)\nuse_bounding_box\n= TRUE'),
#'     colour = 'black'
#'   ) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       rotate3(degx = 70,degy = 20,use_bounding_box = FALSE) %>%
#'       dplyr::mutate(x = x - 1.5),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_label(
#'     data = tibble(),
#'     mapping = aes(x = -1, y = -1, label = 'use_bounding_box\n= FALSE'),
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = tibble(x = 0.5 + c(0.5,-1.5), y = 0.5, z = 0.5),
#'     mapping = aes(x = x, y = y)
#'   )
#' #END: example
#END: examples
