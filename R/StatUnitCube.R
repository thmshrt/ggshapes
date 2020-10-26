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
# StatUnitCube$compute_panel(
#   data = tibble(xc = 0, yc = 0), scales = NULL,
#   degx = 70, degy = 20, degz = 0,
#   faces = list(c('front','top','right')),
#   viewx = NA, viewy = NA, respect = TRUE
# ) %}%
# ggplot() +
#   coord_equal() +
#   scale_x_continuous(limits = c(-2,2)) +
#   scale_y_continuous(limits = c(-2,2)) +
#   geom_polygon(mapping = aes(x = x, y = y,group = face, fill = face)) +
#   geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')
#'
#' StatUnitCube$compute_panel(
#'   data = tibble(xc = 0, yc = 0), scales = NULL,
#'   degx = 70, degy = 20, degz = 0,
#'   faces = list(c('front','top','right')),
#'   viewx = 2, viewy = 2, respect = TRUE
#' ) %}%
#'   ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(mapping = aes(x = x, y = y,group = face, fill = face)) +
#'   geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')

StatUnitCube = ggplot2::ggproto(

    `_class` = 'StatUnitCube',
    `_inherit` = Stat,

    required_aes = c('xc','yc'),

    compute_panel = function(
      data,scales,
      degx,degy,degz,
      faces,
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
          dplyr::mutate(degx,degy,degz,faces,viewx,viewy,respect),
        pts_unit_cube(),
      ) %>%
        # center each cube center at xc, yc
        dplyr::group_by(group) %>%
        dplyr::group_modify(
          ~(function(dotx) {
            dotx %>%
              center(xc = 0, yc = 0,zc = 0) %>%
              rotate(degx = dotx$degx[1], degy = dotx$degy[1], degz = dotx$degz[1]) %>%
              center(xc = dotx$xc[1], yc = dotx$yc[1], zc = 0) %>%
              scale_into_view(mwx = viewx[1], mwy = viewy[1], respect = respect[1]) %>%
              dplyr::filter(face %in% faces[1][[1]])
          })(.x)
        ) %>%
        # remove grouping
        dplyr::ungroup() %>%
        # rearrange to get polygon correct
        dplyr::arrange(face,order)
      # -------- END: logic

    }
  )



