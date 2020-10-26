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
#'StatPlanarNgon$compute_panel(
#'  data = tibble(xc = c(-2,0,2),yc = c(-2,0,2), zc = 0, n = c(3,4,5), r = c(1,1,1), group = 1:3),
#'  degx = 0, degy = 0, degz = 0,
#'  viewx = NA, viewy = NA, respect = FALSE
#') %}%
#'  ggplot() +
#'  coord_equal() +
#'  scale_x_continuous(limits = c(-4,4)) +
#'  scale_y_continuous(limits = c(-4,4)) +
#'  geom_polygon(
#'    mapping = aes(
#'      x = x,
#'      y = y,
#'      group = group
#'    ),
#'    fill = NA,
#'    colour = 'black'
#'  )

StatPlanarNgon = ggplot2::ggproto(

    `_class` = 'StatPlanarHexagon',
    `_inherit` = Stat,

    required_aes = c('xc','yc','zc','n','r'),

    compute_panel = function(
      data,scales,
      degx,degy,degz,
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
        dplyr::mutate(group = 1:n(), .before = 1) %>%
        dplyr::mutate(degx,degy,degz,viewx,viewy,respect) %>%
        dplyr::group_by(group) %>%
        dplyr::group_modify(
          ~(function(dotx)
            pts_planar_ngon(n = dotx$n) %>%
              scale(sx = dotx$r[1], sy = dotx$r[1]) %>%
              center(xc = 0, yc = 0, zc = 0) %>%
              rotate(degx = dotx$degx[1], degy = dotx$degy[1], degz = dotx$degz[1]) %>%
              center(xc = dotx$xc[1], yc = dotx$yc[1], zc = 0) %>%
              scale_into_view(mwx = viewx[1], mwy = viewy[1], respect = respect[1])
            )(.x)) %>%
        # remove grouping
        dplyr::ungroup() %>%
        # order
        dplyr::arrange(group,order)
      # -------- END: logic

    }
  )
