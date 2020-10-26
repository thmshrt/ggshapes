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
#'# creating a circle with radius 1.5
#'StatPlanarCircle$compute_panel(
#'  data = tibble(xc = 0, yc = 0,r = 1.5), scales = NULL,
#'  degx = 0, degy = 0, degz = 0,
#'  faces = list(c('front','top','right')),
#'  viewx = NA, viewy = NA, respect = TRUE
#') %}%
#'  ggplot() +
#'  coord_equal() +
#'  scale_x_continuous(limits = c(-2,2)) +
#'  scale_y_continuous(limits = c(-2,2)) +
#'  geom_polygon(mapping = aes(x = x, y = y), fill = NA, colour = 'black') +
#'  geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')


StatPlanarPoint = ggplot2::ggproto(

    `_class` = 'StatPlanarPoint',
    `_inherit` = Stat,

    required_aes = c('x','y','z'),

    compute_panel = function(
      data,scales,
      degx, degy, degz,
      xc, yc, zc,
      viewx, viewy, respect
      ) {

      # -------- BEGIN: checks
      # degx, degy, degz must be integer or numeric, length 1 or length data
      # viewx, viewy must be integer or numeric, length 1 or length data
      # faces must be list of character, length > 0 & length <= 8, values in 'top', 'bottom', ...
      # respect must be logical, length 1 or length data
      # -------- END: checks

      # -------- BEGIN: logic
      data %>%
        center(xc = 0, yc = 0, zc = 0) %>%
        rotate(degx = degx, degy = degy, degz = degz) %>%
        center(xc = xc, yc = yc, zc = zc) %>%
        scale_into_view(mwx = viewx[1], mwy = viewy[1], respect = respect[1])
      # -------- END: logic

    }
  )



