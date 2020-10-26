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
GeomMatrixSliceAxes = ggplot2::ggproto(

  `_class` = 'GeomMatrixSliceAxes',
  `_inherit` = Geom,

  required_aes = c('x','xend','y','yend'),

  draw_key = draw_key_polygon,

  default_aes = aes(
    colour = 'black',
    size = 0.5,
    linetype = 1,
    alpha = NA
  ),

  draw_panel = function(data,panel_params,coord,
                        arrow = NULL, arrow.fill = NULL,
                        lineend = 'butt', linejoin = 'round') {

    # transform data to fit into panel coordinate
    data_transformed = coord$transform(data,panel_params)

    # print(data_transformed)

    # polygonGrob takes only 1 parameter

    # return a
    grid::segmentsGrob(
      x0 = data_transformed$x,
      x1 = data_transformed$xend,
      y0 = data_transformed$y,
      y1 = data_transformed$yend,
      default.units = 'native',
      gp = grid::gpar(
        col = scales::alpha(data_transformed$colour, data_transformed$alpha),
        # fill = scales::alpha(data_transformed$fill, data_transformed$alpha),
        lwd = data_transformed$size * .pt,
        lty = data_transformed$linetype,
        lineend = lineend,
        linejoin = linejoin
      ),
      arrow = arrow
    )
  }
)

