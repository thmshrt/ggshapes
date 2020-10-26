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
GeomMatrixSliceLabels = ggplot2::ggproto(

  `_class` = 'GeomMatrixLabels',
  `_inherit` = Geom,

  required_aes = c('xc', 'yc',
                   'lx', 'ly', 'lz',
                   'ix', 'ixend', 'iy', 'iyend', 'iz', 'izend'),

  draw_key = draw_key_polygon,

  default_aes = aes(
    colour = 'black',
    size = 3.88,
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    alpha = NA,
    family = '',
    fontface = 1,
    lineheight = 1.2
  ),

  draw_panel = function(data,panel_params,coord,
                        xlabel, xpos,
                        ylabel, ypos,
                        zlabel, zpos,
                        check_overlap = FALSE) {

    # transform data to fit into panel coordinate
    data_transformed = coord$transform(data,panel_params) %>%
      dplyr::mutate(label = '') %>%
      dplyr::mutate(
        # xlabel
        label = replace(label, face == 'top' & side == 'top' & (position %in% xpos), xlabel),
        # ylabel
        label = replace(label, face == 'top' & side == 'left' & (position %in% ypos), ylabel),
        # zlabel
        label = replace(label, face == 'front' & side == 'left' & (position %in% zpos), zlabel)
      )

    # print(data_transformed)

    # polygonGrob takes only 1 parameter

    # return a text grob
    grid::textGrob(
      x = data_transformed$x,
      y = data_transformed$y,
      label = data_transformed$label,
      default.units = 'native',
      hjust = data_transformed$hjust,
      vjust = data_transformed$vjust,
      rot = data_transformed$angle,
      gp = grid::gpar(
        col = scales::alpha(data_transformed$colour, data_transformed$alpha),
        # fill = scales::alpha(data_transformed$fill, data_transformed$alpha),
        fontsize = data_transformed$size * .pt,
        fontfamily = data_transformed$family,
        fontface = data_transformed$fontface,
        lineheight = data_transformed$lineheight
      ),
      check.overlap = check_overlap
    )
  }
)
