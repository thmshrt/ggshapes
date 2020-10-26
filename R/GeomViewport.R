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
GeomViewport = ggplot2::ggproto(

  `_class` = 'GeomViewport',
  `_inherit` = Geom,

  required_aes = c('xc','yc','w','h'),

  draw_key = draw_key_polygon,

  default_aes = aes(
    polygon.fill = 'grey',
    polygon.colour = 'black',
    polygon.size = 0.5,
    polygon.linetype = 1,
    polygon.alpha = 1,

    point.shape = 19,
    point.colour = 'black',
    point.size = 1.5,
    point.fill = NA,
    point.alpha = NA,
    point.stroke = 0.5,

    text.colour = 'black',
    text.size = 3.88,
    text.angle = 0,
    text.hjust = 0.5,
    text.vjust = 0.5,
    text.alpha = NA,
    text.family = "",
    text.fontface = 1,
    text.lineheight = 1.2
  ),

  draw_group = function(data,panel_params,coord) {

    # transform data to fit into panel coordinate
    data_transformed = coord$transform(data,panel_params)

    data_text = data %>% dplyr::mutate(
        label = paste('(',round(x,2),',',round(y,2),')',sep=''),
        x = x + c(-.25, -.25, -.25,    0, +.25, +.25, +.25,    0),
        y = y + c(-.25,    0, +.25, +.25, +.25,    0, -.25, -.25)
        ) %>%
      coord$transform(.,panel_params)

    # print(data_transformed)

    # polygonGrob takes only 1 parameter
    first_row = data_transformed[1,]

    # return a polygonGrob
    grid::gList(
      grid::polygonGrob(
        x = data_transformed$x,
        y = data_transformed$y,
        default.units = 'native',
        gp = grid::gpar(
          col = first_row$polygon.colour,
          fill = scales::alpha(first_row$polygon.fill, first_row$polygon.alpha),
          lwd = first_row$polygon.size * .pt,
          lty = first_row$polygon.linetype
        )
      ),
      # grid::pointsGrob(
      #   x = data_transformed$x,
      #   y = data_transformed$y,
      #   pch = data_transformed$point.shape,
      #   size = data_transformed$point.size,
      #   default.units = 'native',
      #   gp = grid::gpar(
      #     col = scales::alpha(data_transformed$point.colour, data_transformed$point.alpha),
      #     fill = scales::alpha(data_transformed$point.fill, data_transformed$point.alpha),
      #     fontsize = data_transformed$point.size * .pt + data_transformed$point.stroke * .stroke / 2,
      #     lwd = data_transformed$point.stroke * .stroke / 2
      #   )
      # ),
      grid::textGrob(
        x = data_text$x,
        y = data_text$y,
        label = data_text$label,
        hjust = data_text$text.hjust,
        vjust = data_text$text.vjust,
        default.units = "native",
        rot = data_text$text.angle,
        gp = gpar(
          col = alpha(data_text$text.colour, data_text$text.alpha),
          fontsize = data_text$text.size * .pt, fontfamily = data_text$text.family,
          fontface = data_text$text.fontface, lineheight = data_text$text.lineheight
          )
      )
    )
  }
)

