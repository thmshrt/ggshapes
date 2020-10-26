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
StatPlanarHexagon = ggplot2::ggproto(

    `_class` = 'StatPlanarHexagon',
    `_inherit` = Stat,

    required_aes = c('xc','yc','zc','r'),

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
      tidyr::crossing(
        data %>%
          dplyr::mutate(group = 1:n(),.before = 1) %>%
          dplyr::mutate(degx,degy,degz,viewx,viewy,respect),
        pts_planar_ngon(n = 6)
      ) %>%
        # center each cube center at xc, yc
        dplyr::group_by(group) %>%
        dplyr::group_modify(
          ~(function(dotx) {
            dotx %>%
              scale(sx = dotx$r[1], sy = dotx$r[1]) %>%
              center(xc = 0, yc = 0, zc = 0) %>%
              rotate(degx = dotx$degx[1], degy = dotx$degy[1], degz = dotx$degz[1]) %>%
              center(xc = dotx$xc[1], yc = dotx$yc[1], zc = 0) %>%
              scale_into_view(mwx = viewx[1], mwy = viewy[1], respect = respect[1])
          })(.x)
        ) %>%
        # remove grouping
        dplyr::ungroup() %>%
        # order
        dplyr::arrange(group,order)
      # -------- END: logic

    }
  )



