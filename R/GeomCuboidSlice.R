#' {title placeholder} 
#'
#' {description placeholder} 
#' 
#' @usage
#' {usage placeholder}
#'
#' @param {param}   \[{type}\], {restrictions}
#' 
#' {return}
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
GeomCuboidSlice = ggplot2::ggproto(

  `_class` = 'GeomCuboidSlice',
  `_inherit` = Geom,

  required_aes = c('x','y','sx','sy'),

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
    data %>% coord$transform(data = ., panel_params) -> outer_transformed
    data %>%
      dplyr::select(-x,-y) %>%
      dplyr::rename(x = sx, y = sy) %>%
      coord$transform(data = .,panel_params) -> inner_transformed

    # polygonGrob takes only 1 parameter
    first_row_outer = outer_transformed[1,]
    first_row_inner = inner_transformed[1,]

    # return a polygonGrob
    gList(
      # outer polygon
      grid::polygonGrob(
        x = outer_transformed$x,
        y = outer_transformed$y,
        id = outer_transformed$face_int,
        default.units = 'native',
        gp = grid::gpar(
          col = first_row_outer$colour,
          fill = scales::alpha(first_row_outer$fill, first_row_outer$alpha),
          lwd = first_row_outer$size * .pt,
          lty = first_row_outer$linetype
        )
      ),
      # inner polygon
      grid::polygonGrob(
        x = inner_transformed$x,
        y = inner_transformed$y,
        id = inner_transformed$face_int,
        default.units = 'native',
        gp = grid::gpar(
          col = first_row_inner$colour,
          fill = scales::alpha(first_row_inner$fill, first_row_inner$alpha),
          lwd = first_row_inner$size * .pt,
          lty = first_row_inner$linetype
        )
      )
    )
  }
)

