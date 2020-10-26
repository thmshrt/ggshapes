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
#' ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   # polygon
#'   geom_polygon(
#'     data = pts_unit_viewport(),
#'     mapping = aes(x = x, y = y),
#'     fill = NA,
#'     colour = 'black'
#'   ) +
#'   # points
#'   geom_point(
#'     data = pts_unit_viewport(),
#'     mapping = aes(x = x, y = y)
#'   ) +
#'   # labels
#'   geom_text(
#'     data = pts_unit_viewport() %}% dplyr::mutate(angle = ifelse(bt == 'top' | bt == 'bottom',90,0)),
#'     mapping = aes(x = x, y = y, label = paste(bt,rl,sep=','), angle = angle)
#'   )

pts_unit_viewport = function() {
  tribble(
    ~x     , ~y    , ~bt     ,  ~rl    , ~order,
    0      , 0     , 'bottom', 'left'  , 1.    ,
    0      , 0.5   , 'middle', 'left'  , 2.    ,
    0      , 1     , 'top'   , 'left'  , 3.    ,
    0.5    , 1     , 'top'   , 'middle', 4.    ,
    1      , 1     , 'top'   , 'right' , 5.    ,
    1      , 0.5   , 'middle', 'right' , 6.    ,
    1      , 0     , 'bottom', 'right' , 7.    ,
    0.5    , 0     , 'bottom', 'middle', 8.    ,
  )
}
