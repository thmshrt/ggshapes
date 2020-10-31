#' {title placeholder}
#'
#' {description placeholder}
#'
#' @usage
#' {usage placeholder}
#'
#' @param {param}   \[{type}\], {restrictions}
#'
#' @return           [{type}]
#'
#' @export
#' @import magrittr rlang purrr tibble dplyr ggplot2
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(ggplot2)
#' library(grid)
#' library(magrittr)
#' # identity case
#'
#'
pts_unit_cube = function() {
  tribble(
    ~x, ~y, ~z, ~face   , ~tb     , ~rl    , ~order, ~face_int,
    0,  0,  0, 'front' , 'bottom', 'left'  ,      1,         1,
    0,  0,  1, 'front' , 'top'   , 'left'  ,      2,         1,
    1,  0,  1, 'front' , 'top'   , 'right' ,      3,         1,
    1,  0,  0, 'front' , 'bottom', 'right' ,      4,         1,

    # 0,  1,  0, 'back'  , 'bottom', 'left'  ,      1,         2,
    # 0,  1,  1, 'back'  , 'top'   , 'left'  ,      2,         2,
    # 1,  1,  1, 'back'  , 'top'   , 'right' ,      3,         2,
    # 1,  1,  0, 'back'  , 'bottom', 'right' ,      4,         2,

    0,  0,  1, 'top'   , 'bottom', 'left'  ,      1,         3,
    0,  1,  1, 'top'   , 'top'   , 'left'  ,      2,         3,
    1,  1,  1, 'top'   , 'top'   , 'right' ,      3,         3,
    1,  0,  1, 'top'   , 'bottom', 'right' ,      4,         3,

    # 0,  0,  0, 'bottom', 'bottom', 'left'  ,      1,         4,
    # 0,  1,  0, 'bottom', 'top'   , 'left'  ,      2,         4,
    # 1,  1,  0, 'bottom', 'top'   , 'right' ,      3,         4,
    # 1,  0,  0, 'bottom', 'bottom', 'right' ,      4,         4,

    # 0,  0,  0, 'left'  , 'bottom', 'left'  ,      1,         5,
    # 0,  0,  1, 'left'  , 'top'   , 'left'  ,      2,         5,
    # 0,  1,  1, 'left'  , 'top'   , 'right' ,      3,         5,
    # 0,  1,  0, 'left'  , 'bottom', 'right' ,      4,         5,

    1,  0,  0, 'right' , 'bottom', 'left'  ,      1,         6,
    1,  0,  1, 'right' , 'top'   , 'left'  ,      2,         6,
    1,  1,  1, 'right' , 'top'   , 'right' ,      3,         6,
    1,  1,  0, 'right' , 'bottom', 'right' ,      4,         6
  )
}
