
#BEGIN: description
#' Points for a Scalar Matrix
#'
#' @usage
#' pts_scalar(
#'   keep_bounding = TRUE,
#'   faces_ = c('front','top','right')
#' )
#'
#' @param keep_bounding       \[logical\], length 1 , default `TRUE`, \cr
#' if `FALSE` does not rbind the bounding box back after computation \cr
#' default behavior is to use bounding box points to make computations
#' @param faces_ \[character\], one or more of 'front','back','top','bottom','left','right'
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

pts_scalar = function(
  keep_bounding = TRUE,
  faces_ = c('front','top','right')
  ) {
  #BEGIN: setup params
  #END: setup params

  #BEGIN: param checks
  if (length(keep_bounding) != 1)
    rlang::abort(message = "params keep_bounding must satisfy length(v) == 1")

  if (!is.logical(keep_bounding))
    rlang::abort(message = "params keep_bounding must satisfy is.logical(v)")

  if (!all(length(faces_) >= 1,length(faces_) <= 6))
    rlang::abort(message = "params face_ must satisfy length(v) >= 1 & length(v) <= 6")

  if (!all(faces_ %in% c("top","bottom","left","right","front","back")))
    rlang::abort(message = "params face_ must be one of 'top','bottom','left','right','front','back")
  #END: param checks

  #BEGIN: computation
  if (keep_bounding) return(pts_unit_cube(faces = faces_))
  else return(pts_unit_cube(faces = faces_) %>% dplyr::filter(!bounding_box))
  #END: computation

  #BEGIN: return
  #END: return
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # pts_scalar default behavior is to return the pts of a unit cue
#' pts_scalar() %>%
#'   rotate3(70,20,keep_bounding = FALSE) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_point(
#'     data = data,
#'     mapping = aes(x = x, y = y)
#'   )
#' #END: example
#END: examples
