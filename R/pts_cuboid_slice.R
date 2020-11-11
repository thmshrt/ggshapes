
#BEGIN: description
#' Points for a cuboid slice
#'
#' {description placeholder}
#'
#' @usage
#' pts_cuboid_slice(
#'   lx_ = 1, ly_ = 1, lz_ = 1,
#'   x_ = 0, xend_ = 1,
#'   z_ = 0, yend_ = 1,
#'   y_ = 0, zend_ = 1
#' )
#'
#' @param lx_     \[numeric\], length 1, length of cuboid in `x` direction
#' @param ly_     \[numeric\], length 1, length of cuboid in `y` direction
#' @param lz_     \[numeric\], length 1, length of cuboid in `z` direction
#' @param x_      \[numeric\], length 1, >= 0 & <= lx_ & <= xend_, start of slice in `x` direction
#' @param xend_   \[numeric\], length 1, >= x_ & <= lx_, end of slice in `x` direction
#' @param y_      \[numeric\], length 1, >= 0 & <= ly_ & <= yend_, start of slice in `y` direction
#' @param yend_   \[numeric\], length 1, >= y_ & <= ly_, end of slice in `y` direction
#' @param z_      \[numeric\], length 1, >= 0 & <= lz_ & <= zend_, start of slice in `z` direction
#' @param zend_   \[numeric\], length 1, >= z_ & <= lz_, end of slice in `z` direction
#'
#' @return
#' * [tibble] with values
#' * `x` coordinate of slice or bounding box
#' * `y` coordinate of slice or bounding box
#' * `z` coordinate of slice or bounding box
#' * `face` is point part of "front", "back", "left", "right", "bottom" or "top" face
#' * `tb` is point on "top", "middle", "bottom" side of face
#' * `rl` is point on "right", "middle", "left" side of face
#' * `bounding_box` if `TRUE`, point is part of bounding box
#'
#' @export
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

pts_cuboid_slice = function(
  lx_ = 1, ly_ = 1, lz_ = 1,
  x_ = 0, xend_ = 1,
  z_ = 0, yend_ = 1,
  y_ = 0, zend_ = 1
) {
  #BEGIN: setup params
  lx_ = rlang::enexpr(lx_)
  ly_ = rlang::enexpr(ly_)
  lz_ = rlang::enexpr(lz_)

  x_ = rlang::enexpr(x_)
  xend_ = rlang::enexpr(xend_)
  y_ = rlang::enexpr(y_)
  yend_ = rlang::enexpr(yend_)
  z_ = rlang::enexpr(z_)
  zend_ = rlang::enexpr(zend_)
  #END: setup params

  #BEGIN: param checks
  if (!all(c(length(lx_),length(ly_),length(lz_)) == 1))
    rlang::abort(message = "params lx_, ly_, lz_ must satisfy length(v) == 1")

  if (!all(c(class(lx_),class(ly_),class(lz_)) %in% c('double','numeric','integer')))
    rlang::abort(message = "params lx_, ly_, lz_ must be of type 'double', 'numeric', or 'integer'")

  if (!all(c(lx_,ly_,lz_) >= 0))
    rlang::abort(message = "params lx_, ly_, lz_ must satisfy v >= 0")

  if (!all(c(length(x_),length(xend_),length(y_),length(yend_),length(z_),length(zend_)) == 1))
    rlang::abort(message = "params x_, xend_, y_, yend_, z_, z_end_ must satisfy length(v) == 1")

  if (!all(c(class(x_),class(xend_),class(y_),class(yend_),class(z_),class(zend_)) %in% c('double','numeric','integer')))
    rlang::abort(message = "params x_, xend_, y_, yend_, z_, z_end_ must be of type 'double', 'numeric', or 'integer'")

  if (!all(c(x_,xend_,y_,yend_,z_,zend_) >= 0))
    rlang::abort(message = "params x_, xend_, y_, yend_, z_, z_end_ must satisfy v >= 0")

  if (!all(c(x_,xend_) <= lx_))
    rlang::abort(message = "params x_, xend_ must satisfy v <= lx_")

  if (!all(c(y_,yend_) <= ly_))
    rlang::abort(message = "params y_, yend_ must satisfy v <= ly_")

  if (!all(c(z_,zend_) <= lz_))
    rlang::abort(message = "params z_, zend_ must satisfy v <= lz_")
  #END: param checks

  #BEGIN: computation
  pts_unit_cube() %>%
    scale3(
      sx_ = !!xend_ - !!x_,
      sy_ = !!yend_ - !!y_,
      sz_ = !!zend_ - !!z_
      ) %>%
    # translate
    dplyr::mutate(
      x = x + !!x_,
      y = y + !!y_,
      z = z + !!y_,
    ) %>%
    dplyr::filter(!bounding_box) -> out_tbl

  pts_unit_cube() %>%
    scale3(
      sx_ = !!lx_,
      sy_ = !!ly_,
      sz_ = !!lz_
    ) %>%
    dplyr::filter(bounding_box) -> out_tbl_bounding_box

  dplyr::bind_rows(
    out_tbl,
    out_tbl_bounding_box
  )
  #END: computation
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # default params create a unit cube centered at (x=0.5,y=0.5)
#' library(ggplot2)
#' pts_cuboid_slice() %>%
#'   rotate3(70,20,keep_bounding = FALSE) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,+2)) +
#'   ylim(c(-2,+2)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     color = 'black'
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # by keeping the bounding box we can visualize both the shape and
#' # the volume it takes up
#' pts_cuboid_slice(
#'   x_ = 0, xend_ = 0.5,
#'   y_ = 0, yend_ = 0.5,
#'   z_ = 0, zend_ = 0.5
#'   ) %>%
#'   rotate3(70,20) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,+2)) +
#'   ylim(c(-2,+2)) +
#'   geom_polygon(
#'     data = data %>% dplyr::filter(bounding_box),
#'     mapping = aes(x = x, y = y, group = face),
#'     fill = NA, color = 'black'
#'   ) +
#'   geom_polygon(
#'     data = data %>% dplyr::filter(!bounding_box),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     color = 'black'
#'   )
#' #END: example
#END: examples
