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
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(ggplot2)
#' library(grid)
#' library(magrittr)
#' # identity case
#'
#' @export
#' @import magrittr rlang purrr tibble dplyr ggplot2
GeomUnitMatrix = ggplot2::ggproto(

  `_class` = 'GeomUnitMatrix',
  `_inherit` = Geom,

  required_aes = c('x','y'),

  draw_key = draw_key_polygon,

  default_aes = aes(
    fill = 'grey',
    colour = 'black',
    size = 0.5,
    linetype = 1,
    alpha = 1
  ),

  draw_group = function(data,panel_params,coord) {

    # transform data to fit into panel coordinate
    data_transformed = coord$transform(data,panel_params)

    # print(data_transformed)

    # polygonGrob takes only 1 parameter
    first_row = data_transformed[1,]

    # return a
    grid::polygonGrob(
      x = data_transformed$x,
      y = data_transformed$y,
      id = data_transformed$face_int,
      default.units = 'native',
      gp = grid::gpar(
        col = first_row$colour,
        fill = scales::alpha(first_row$fill, first_row$alpha),
        lwd = first_row$size * .pt,
        lty = first_row$linetype
      )
    )
  }
)

