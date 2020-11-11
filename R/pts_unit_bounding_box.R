
#BEGIN: description
#' Points for a unit bounding box
#'
#'
#' @usage
#' pts_unit_bounding_box()
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

pts_unit_bounding_box = function() {
  tribble(
    ~x  , ~y  , ~z  , ~face   , ~tb     , ~rl    , ~point_order, ~face_order, ~bounding_box,
    # front face
    0   , 0   , 0   , 'front' , 'bottom', 'left'  ,           1,           6,          TRUE,
    0   , 0   , 0.5 , 'front' , 'middle', 'left'  ,           2,           6,          TRUE,
    0   , 0   , 1   , 'front' , 'top'   , 'left'  ,           3,           6,          TRUE,
    0.5 , 0   , 1   , 'front' , 'top'   , 'middle',           4,           6,          TRUE,
    1   , 0   , 1   , 'front' , 'top'   , 'right' ,           5,           6,          TRUE,
    1   , 0   , 0.5 , 'front' , 'middle', 'right' ,           6,           6,          TRUE,
    1   , 0   , 0   , 'front' , 'bottom', 'right' ,           7,           6,          TRUE,
    0.5 , 0   , 0   , 'front' , 'bottom', 'middle',           8,           6,          TRUE,

    # back face
    0   , 1   , 0   , 'back'  , 'bottom', 'left'  ,           1,           3,          TRUE,
    0   , 1   , 0.5 , 'back'  , 'middle', 'left'  ,           2,           3,          TRUE,
    0   , 1   , 1   , 'back'  , 'top'   , 'left'  ,           3,           3,          TRUE,
    0.5 , 1   , 1   , 'back'  , 'top'   , 'middle',           4,           3,          TRUE,
    1   , 1   , 1   , 'back'  , 'top'   , 'right' ,           5,           3,          TRUE,
    1   , 1   , 0.5 , 'back'  , 'middle', 'right' ,           6,           3,          TRUE,
    1   , 1   , 0   , 'back'  , 'bottom', 'right' ,           7,           3,          TRUE,
    0.5 , 1   , 0   , 'back'  , 'bottom', 'middle',           8,           3,          TRUE,

    # top face
    0   , 0   , 1   , 'top'   , 'bottom', 'left'  ,           1,           4,          TRUE,
    0   , 0.5 , 1   , 'top'   , 'middle', 'left'  ,           2,           4,          TRUE,
    0   , 1   , 1   , 'top'   , 'top'   , 'left'  ,           3,           4,          TRUE,
    0.5 , 1   , 1   , 'top'   , 'top'   , 'middle',           4,           4,          TRUE,
    1   , 1   , 1   , 'top'   , 'top'   , 'right' ,           5,           4,          TRUE,
    1   , 0.5 , 1   , 'top'   , 'middle', 'right' ,           6,           4,          TRUE,
    1   , 0   , 1   , 'top'   , 'bottom', 'right' ,           7,           4,          TRUE,
    0.5 , 0   , 1   , 'top'   , 'bottom', 'middle',           8,           4,          TRUE,

    # bottom face
    0   , 0   , 0   , 'bottom', 'bottom', 'left'  ,           1,           1,          TRUE,
    0   , 0.5 , 0   , 'bottom', 'middle', 'left'  ,           2,           1,          TRUE,
    0   , 1   , 0   , 'bottom', 'top'   , 'left'  ,           3,           1,          TRUE,
    0.5 , 1   , 0   , 'bottom', 'top'   , 'middle',           4,           1,          TRUE,
    1   , 1   , 0   , 'bottom', 'top'   , 'right' ,           5,           1,          TRUE,
    1   , 0.5 , 0   , 'bottom', 'middle', 'right' ,           6,           1,          TRUE,
    1   , 0   , 0   , 'bottom', 'bottom', 'right' ,           7,           1,          TRUE,
    0.5 , 0   , 0   , 'bottom', 'bottom', 'middle',           8,           1,          TRUE,

    # left face
    0   , 0   , 0   , 'left'  , 'bottom', 'left'  ,           1,           2,          TRUE,
    0   , 0   , 0.5 , 'left'  , 'middle', 'left'  ,           2,           2,          TRUE,
    0   , 0   , 1   , 'left'  , 'top'   , 'left'  ,           3,           2,          TRUE,
    0   , 0.5 , 1   , 'left'  , 'top'   , 'middle',           4,           2,          TRUE,
    0   , 1   , 1   , 'left'  , 'top'   , 'right' ,           5,           2,          TRUE,
    0   , 1   , 0.5 , 'left'  , 'middle', 'right' ,           6,           2,          TRUE,
    0   , 1   , 0   , 'left'  , 'bottom', 'right' ,           7,           2,          TRUE,
    0   , 0.5 , 0   , 'left'  , 'bottom', 'middle',           8,           2,          TRUE,

    # right face
    1   , 0   , 0   , 'right' , 'bottom', 'left'  ,           1,           5,          TRUE,
    1   , 0   , 0.5 , 'right' , 'middle', 'left'  ,           2,           5,          TRUE,
    1   , 0   , 1   , 'right' , 'top'   , 'left'  ,           3,           5,          TRUE,
    1   , 0.5 , 1   , 'right' , 'top'   , 'middle',           4,           5,          TRUE,
    1   , 1   , 1   , 'right' , 'top'   , 'right' ,           5,           5,          TRUE,
    1   , 1   , 0.5 , 'right' , 'middle', 'right' ,           6,           5,          TRUE,
    1   , 1   , 0   , 'right' , 'bottom', 'right' ,           7,           5,          TRUE,
    1   , 0.5 , 0   , 'right' , 'bottom', 'middle',           8,           5,          TRUE,
  )
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # pts_unit_bounding_box returns a 6 sides box, it's not very interesting to
#' # look at
#' library(ggplot2)
#' pts_unit_bounding_box() %>%
#'   rotate3(70,20,keep_bounding = TRUE) -> data
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
#END: examples
