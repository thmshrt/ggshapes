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
#' StatPlanarPointgon$compute_panel(
#'   data = pts_planar_circle() %}% dplyr::mutate(group = 1), scales = NULL,
#'   degx = 0, degy = 0, degz = 0,
#'   xc = 0, yc = 0, zc = 0,
#'   viewx = NA, viewy = NA, respect = TRUE
#' ) %}%
#'   ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(mapping = aes(x = x, y = y), fill = NA, colour = 'black') +
#'   geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')

StatPlanarPointgon = ggplot2::ggproto(

    `_class` = 'StatPlanarPointgon',
    `_inherit` = Stat,

    required_aes = c('x','y','z'),

    compute_group = function(
      data,scales,
      degx, degy, degz,
      xc, yc, zc,
      viewx, viewy, respect
      ) {

      # -------- BEGIN: checks
      # x, y, z are numeric or integer
      # all z values must be the same
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



