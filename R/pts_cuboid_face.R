
#BEGIN: description
#' Points of a single face of a cuboid
#'
#' @usage
#' pts_cuboid_face(
#' lx_ = 1,
#' ly_ = 1,
#' lz_ = 1,
#' face_ = 'bottom'
#' )
#'
#' @param lx_   \[numeric\], length 1, non-negative, default 1, length of cuboid along x axis
#' @param ly_   \[numeric\], length 1, non-negative, default 1, length of cuboid along y axis
#' @param lz_   \[numeric\], length 1, non-negative, default 1, length of cuboid along z axis
#' @param face_ \[character\], one of "top","bottom","left","right","front","back"
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

pts_cuboid_face = function(
  lx_ = 1,
  ly_ = 1,
  lz_ = 1,
  face_ = 'bottom'
) {
  # browser()
  #BEGIN: setup params
  lx_ = rlang::enexpr(lx_)
  ly_ = rlang::enexpr(ly_)
  lz_ = rlang::enexpr(lz_)
  #END: setup params

  #BEGIN: param checks
  if (!all(c(length(lx_),length(ly_),length(lz_)) == 1))
    rlang::abort(message = "params lx_, ly_, lz_ must satisfy length(v) == 1")

  if (!all(c(class(lx_),class(ly_),class(lz_)) %in% c('double','numeric','integer')))
    rlang::abort(message = "params lx_, ly_, lz_ must be of type 'double', 'numeric', or 'integer'")

  if (!all(c(lx_,ly_,lz_) >= 0))
    rlang::abort(message = "params lx_, ly_, lz_ must satisfy v >= 0")

  if (!(length(face_) == 1))
    rlang::abort(message = "params face_ must satisfy length(v) == 1")

  if (!all(face_ %in% c("top","bottom","left","right","front","back")))
    rlang::abort(message = "params face_ must be one of 'top','bottom','left','right','front','back")
  #END: param checks

  # browser()
  #BEGIN: computation
  pts_unit_cube(faces = face_) %>%
    scale3(sx_ = !!lx_, sy_ = !!ly_, sz_ = !!lz_)
  #END: computation

  #BEGIN: return
  #END: return
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # default is bottom face
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid_face() %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # multiple faces
#' library(ggplot2)
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = pts_cuboid_face(face_ = 'front') %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_polygon(
#'     data = pts_cuboid_face(face_ = 'top') %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_polygon(
#'     data = pts_cuboid_face(face_ = 'right') %>%
#'       rotate3(degx = 70,degy = 20, keep_bounding = FALSE),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   )
#' #END: example
#END: examples
