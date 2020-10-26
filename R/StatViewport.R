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
#' StatViewport$
#'   compute_group(
#'      data = tibble(
#'        xc = c(0,1),yc = c(0,1),
#'        w = c(.5,1), h = c(.5,1)
#'        )
#'     ) %}%
#'   ggplot() +
#'   coord_equal() +
#'   scale_x_continuous(limits = c(-2,2)) +
#'   scale_y_continuous(limits = c(-2,2)) +
#'   geom_polygon(mapping = aes(x = x, y = y, group = group), fill = NA, colour = 'black') +
#'   geom_point(mapping = aes(x = x, y = y)) +
#'   geom_text(mapping = aes(x = x, y = y, label = paste(rl,bt,sep=',')))

StatViewport = ggplot2::ggproto(

    `_class` = 'StatViewport',
    `_inherit` = Stat,

    required_aes = c('xc','yc','w','h'),

    compute_group = function(data,scales) {

      # -------- BEGIN: checks
      # xc is numeric or integer
      # yc is numeric or integer
      # w is numeric or integer
      # h is numeric or integer
      # -------- END: checks

      # -------- BEGIN: logic
      tidyr::crossing(
        data %>% dplyr::mutate(group = 1:n(),z = 0,.before = 1),
        pts_unit_viewport()
      ) %>%
        # center each cube center at xc, yc
        dplyr::group_by(group) %>%
        dplyr::group_modify(
          ~(function(dotx) {
            dotx %>%
              center(xc = 0, yc = 0,zc = 0) %>%
              scale(sx = dotx$w[1], sy = dotx$h[1], respect = FALSE) %>%
              center(xc = dotx$xc[1], yc = dotx$yc[1], zc = 0)
          })(.x)
        ) %>%
        # remove grouping
        dplyr::ungroup() %>%
        # rearrange to get polygon correct
        dplyr::arrange(group,order)
      # -------- END: logic

    }
  )



