
#BEGIN: description
#' Points of faces of cuboid
#'
#' @usage
#' pts_cuboid(
#'   lx_ = 1,
#'   ly_ = 1,
#'   lz_ = 1
#' )
#'
#' @param lx_   \[numeric\], length 1, non-negative, default 1, length of cuboid along x axis
#' @param ly_   \[numeric\], length 1, non-negative, default 1, length of cuboid along y axis
#' @param lz_   \[numeric\], length 1, non-negative, default 1, length of cuboid along z axis
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

pts_cuboid = function(
  lx_ = 1,
  ly_ = 1,
  lz_ = 1
) {
  #BEGIN: setup params
  lx_ = rlang::enexpr(lx_)
  ly_ = rlang::enexpr(ly_)
  lz_ = rlang::enexpr(lz_)

  #END: setup params

  #BEGIN: param checks
  # browser()
  if (!all(c(length(lx_),length(ly_),length(lz_)) == 1))
    rlang::abort(message = "params lx_, ly_, lz_ must satisfy length(v) == 1")

  if (!all(c(class(lx_),class(ly_),class(lz_)) %in% c('double','numeric','integer')))
    rlang::abort(message = "params lx_, ly_, lz_ must be of type 'double', 'numeric', or 'integer'")

  if (!all(c(lx_,ly_,lz_) >= 0))
    rlang::abort(message = "params lx_, ly_, lz_ must satisfy v >= 0")
  #END: param checks

  #BEGIN: computation
  pts_unit_cube() %>% scale3(sx_ = !!lx_, sy_ = !!ly_, sz_ = !!lz_)
  #END: computation
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # pts_cuboid default behavior is to generate a unit cube
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid() %>%
#'       center3(-1,0,0) %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_label(data = tibble(),
#'              mapping = aes(x = -1,y = -1, label = 'pts_cuboid\ndefault behavior')) +
#'   geom_polygon(
#'     data = pts_unit_cube() %>%
#'       center3(1,0,0) %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_label(data = tibble(),
#'              mapping = aes(x = 1,y = -1, label = 'pts_unit_cube'))
#' #END: example
#'
#' #BEGIN: example
#' # scaling in x, y and z directions after centering
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid(lx_ = 3,ly_ = 2, lz_ = 1) %>%
#'       center3(0,0,0) %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_label(data = tibble(),
#'              mapping = aes(x = 0,y = -1, label = 'lx = 3,ly = 2, lz = 1'))
#' #END: example
#'
#' #BEGIN: example
#' # scaling in x, y and z directions without centering
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid(lx_ = 1.5,ly_ = 1, lz_ = .5) %>%
#'       # center3(0,0,0) %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE), # %>%
#'     # center3(-1,1, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_label(data = tibble(),
#'              mapping = aes(x = 0,y = -1, label = 'lx = 1.5,ly = 1, lz = .5'))
#' #END: example
#'
#END: examples
