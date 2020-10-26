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
GeomPlanarPoint = ggplot2::ggproto(

  `_class` = 'GeomPlanarPoint',
  `_inherit` = Geom,

  required_aes = c('x','y'),

  draw_key = draw_key_polygon,

  default_aes = aes(
    shape = 19,
    colour = 'black',
    size = 1.5,
    fill = NA,
    alpha = NA,
    stroke = 0.5
  ),

  draw_panel = function(data,panel_params,coord) {

    # transform data to fit into panel coordinate
    data_transformed = coord$transform(data,panel_params)

    # return a polygonGrob
    grid::pointsGrob(
      x = data_transformed$x,
      y = data_transformed$y,
      pch = data_transformed$shape,
      size = data_transformed$size,
      default.units = 'native',
      gp = grid::gpar(
        col = scales::alpha(data_transformed$colour, data_transformed$alpha),
        fill = scales::alpha(data_transformed$fill, data_transformed$alpha),
        fontsize = data_transformed$size * .pt + data_transformed$stroke * .stroke / 2,
        lwd = data_transformed$stroke * .stroke / 2
      )
    )

  }
)

