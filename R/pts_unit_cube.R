
#BEGIN: description
#' Annotated Points of a Unit Cube
#'
#' @usage
#' pts_unit_cube(faces = c("front", "top", "right"))
#'
#' @param faces   \[character\], with one or more of "front","top","right","back","bottom","left",
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
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom tibble tribble
#END: description
#BEGIN: code

pts_unit_cube = function(faces = c('front','top','right')) {
  #BEGIN: param checks
  if (length(faces) > 6)
    rlang::abort(message = "params faces must satisfy length(v) <= 6")

  if (!all(faces %in% c('front','back','top','bottom','right','left')))
    rlang::abort(message = "params faces must satisfy v %in% c('front','back','top','bottom','right','left')")
  #END: param checks

  tribble(
    ~x  , ~y  , ~z  , ~face   , ~tb     , ~rl    , ~point_order, ~face_order, ~bounding_box,
    # front face
    0   , 0   , 0   , 'front' , 'bottom', 'left'  ,           1,           6,         FALSE,
    0   , 0   , 0.5 , 'front' , 'middle', 'left'  ,           2,           6,         FALSE,
    0   , 0   , 1   , 'front' , 'top'   , 'left'  ,           3,           6,         FALSE,
    0.5 , 0   , 1   , 'front' , 'top'   , 'middle',           4,           6,         FALSE,
    1   , 0   , 1   , 'front' , 'top'   , 'right' ,           5,           6,         FALSE,
    1   , 0   , 0.5 , 'front' , 'middle', 'right' ,           6,           6,         FALSE,
    1   , 0   , 0   , 'front' , 'bottom', 'right' ,           7,           6,         FALSE,
    0.5 , 0   , 0   , 'front' , 'bottom', 'middle',           8,           6,         FALSE,

    # back face
    0   , 1   , 0   , 'back'  , 'bottom', 'left'  ,           1,           3,         FALSE,
    0   , 1   , 0.5 , 'back'  , 'middle', 'left'  ,           2,           3,         FALSE,
    0   , 1   , 1   , 'back'  , 'top'   , 'left'  ,           3,           3,         FALSE,
    0.5 , 1   , 1   , 'back'  , 'top'   , 'middle',           4,           3,         FALSE,
    1   , 1   , 1   , 'back'  , 'top'   , 'right' ,           5,           3,         FALSE,
    1   , 1   , 0.5 , 'back'  , 'middle', 'right' ,           6,           3,         FALSE,
    1   , 1   , 0   , 'back'  , 'bottom', 'right' ,           7,           3,         FALSE,
    0.5 , 1   , 0   , 'back'  , 'bottom', 'middle',           8,           3,         FALSE,

    # top face
    0   , 0   , 1   , 'top'   , 'bottom', 'left'  ,           1,           4,         FALSE,
    0   , 0.5 , 1   , 'top'   , 'middle', 'left'  ,           2,           4,         FALSE,
    0   , 1   , 1   , 'top'   , 'top'   , 'left'  ,           3,           4,         FALSE,
    0.5 , 1   , 1   , 'top'   , 'top'   , 'middle',           4,           4,         FALSE,
    1   , 1   , 1   , 'top'   , 'top'   , 'right' ,           5,           4,         FALSE,
    1   , 0.5 , 1   , 'top'   , 'middle', 'right' ,           6,           4,         FALSE,
    1   , 0   , 1   , 'top'   , 'bottom', 'right' ,           7,           4,         FALSE,
    0.5 , 0   , 1   , 'top'   , 'bottom', 'middle',           8,           4,         FALSE,

    # bottom face
    0   , 0   , 0   , 'bottom', 'bottom', 'left'  ,           1,           1,         FALSE,
    0   , 0.5 , 0   , 'bottom', 'middle', 'left'  ,           2,           1,         FALSE,
    0   , 1   , 0   , 'bottom', 'top'   , 'left'  ,           3,           1,         FALSE,
    0.5 , 1   , 0   , 'bottom', 'top'   , 'middle',           4,           1,         FALSE,
    1   , 1   , 0   , 'bottom', 'top'   , 'right' ,           5,           1,         FALSE,
    1   , 0.5 , 0   , 'bottom', 'middle', 'right' ,           6,           1,         FALSE,
    1   , 0   , 0   , 'bottom', 'bottom', 'right' ,           7,           1,         FALSE,
    0.5 , 0   , 0   , 'bottom', 'bottom', 'middle',           8,           1,         FALSE,

    # left face
    0   , 0   , 0   , 'left'  , 'bottom', 'left'  ,           1,           2,         FALSE,
    0   , 0   , 0.5 , 'left'  , 'middle', 'left'  ,           2,           2,         FALSE,
    0   , 0   , 1   , 'left'  , 'top'   , 'left'  ,           3,           2,         FALSE,
    0   , 0.5 , 1   , 'left'  , 'top'   , 'middle',           4,           2,         FALSE,
    0   , 1   , 1   , 'left'  , 'top'   , 'right' ,           5,           2,         FALSE,
    0   , 1   , 0.5 , 'left'  , 'middle', 'right' ,           6,           2,         FALSE,
    0   , 1   , 0   , 'left'  , 'bottom', 'right' ,           7,           2,         FALSE,
    0   , 0.5 , 0   , 'left'  , 'bottom', 'middle',           8,           2,         FALSE,

    # right face
    1   , 0   , 0   , 'right' , 'bottom', 'left'  ,           1,           5,         FALSE,
    1   , 0   , 0.5 , 'right' , 'middle', 'left'  ,           2,           5,         FALSE,
    1   , 0   , 1   , 'right' , 'top'   , 'left'  ,           3,           5,         FALSE,
    1   , 0.5 , 1   , 'right' , 'top'   , 'middle',           4,           5,         FALSE,
    1   , 1   , 1   , 'right' , 'top'   , 'right' ,           5,           5,         FALSE,
    1   , 1   , 0.5 , 'right' , 'middle', 'right' ,           6,           5,         FALSE,
    1   , 1   , 0   , 'right' , 'bottom', 'right' ,           7,           5,         FALSE,
    1   , 0.5 , 0   , 'right' , 'bottom', 'middle',           8,           5,         FALSE,
  ) %>%
    dplyr::filter(face %in% faces) %>%
    dplyr::bind_rows(pts_unit_bounding_box())
}

#END: code
#BEGIN: examples
#' @examples
#BEGIN: examples
#'  @examples
#' #BEGIN: example
#' # pts_unit_cube returns a "top", "front", "right" by default in addition
#' # to adding in a bounding box. the bounding box is very important for
#' # reliably computing transformations on the shape, rotation
#' library(ggplot2)
#' pts_unit_cube() %>%
#'   rotate3(70,20,keep_bounding = TRUE) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,+2)) +
#'   ylim(c(-2,+2)) +
#'   geom_polygon(
#'     data = data %>% dplyr::filter(!bounding_box),
#'     mapping = aes(x = x, y = y, group = face, fill = face),
#'     alpha = 0.5
#'   ) +
#'   geom_polygon(
#'     data = data %>% dplyr::filter(bounding_box),
#'     mapping = aes(x = x, y = y, group = face),
#'     color = 'black', fill = NA
#'   )
#' #END: example
#END: examples
