
#BEGIN: description
#' Points for unit viewport
#'
#' @usage
#' pts_unit_viewport()
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
#' @importFrom tibble tribble
#END: description
#BEGIN: code

pts_unit_viewport = function() {
  tribble(
    ~x  , ~y  , ~z  , ~face   , ~tb     , ~rl    , ~point_order, ~face_order, ~bounding_box,
    # top face
    0   , 0   , 1   , 'top'   , 'bottom', 'left'  ,           1,           4,         FALSE,
    0   , 0.5 , 1   , 'top'   , 'middle', 'left'  ,           2,           4,         FALSE,
    0   , 1   , 1   , 'top'   , 'top'   , 'left'  ,           3,           4,         FALSE,
    0.5 , 1   , 1   , 'top'   , 'top'   , 'middle',           4,           4,         FALSE,
    1   , 1   , 1   , 'top'   , 'top'   , 'right' ,           5,           4,         FALSE,
    1   , 0.5 , 1   , 'top'   , 'middle', 'right' ,           6,           4,         FALSE,
    1   , 0   , 1   , 'top'   , 'bottom', 'right' ,           7,           4,         FALSE,
    0.5 , 0   , 1   , 'top'   , 'bottom', 'middle',           8,           4,         FALSE,
  ) %>%
    center3(0,0,0,use_bounding_box = FALSE) %>%
    dplyr::mutate(size = 3.88, label_x = x * 1.5, label_y = y * 1.5) %>%
    dplyr::mutate(label = sprintf('%+.3f\n%+.3f',x,y))
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # pts_unit_viewport default behavior is to create a unit square centered at
#' # (x=0,y=0). this is. a different behavior from shapes since we to center
#' # our shapes in a viewport, but not necessarily start with out shapes centered
#' pts_unit_viewport() -> data
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_point(
#'     data = data,
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   ) +
#'   geom_text(
#'     data = data,
#'     mapping = aes(x = label_x, y = label_y, label = sprintf('x:%+.2f\ny:%+.2f',x,y)),
#'     size = data$size
#'   )
#' #END: example
#END: examples
