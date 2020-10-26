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
#' # rotate a matrix
#' #    70 degrees anticlockwise about its x axis
#' #    20 degrees anticlockwise about its y axis
#' # and then view a matrix's front, right, and top faces
#'StatMatrixFace$compute_panel(
#'  data = tibble(xc = 0, yc = 0, lx = 1, ly = 2, lz = 3), scales = NULL,
#'  degx = 70, degy = 20, degz = 0,
#'  face = 'top',
#'  viewx = 2, viewy = 2, respect = TRUE
#') %}%
#'  ggplot() +
#'  coord_equal() +
#'  scale_x_continuous(limits = c(-2,2)) +
#'  scale_y_continuous(limits = c(-2,2)) +
#'  geom_polygon(mapping = aes(x = x, y = y,group = draw_order, fill = face), colour = 'black') +
#'  geom_rect(mapping = aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = 'black')

StatMatrixFace = ggplot2::ggproto(

  `_class` = 'StatMatrixFace',
  `_inherit` = Stat,

  required_aes = c('xc','yc','lx','ly','lz'),

  compute_panel = function(
    data,scales,
    degx,degy,degz,
    face,
    param_face = face,
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
      dplyr::mutate(shape_id = 1:n(),.before = 1) %>%
      dplyr::mutate(degx,degy,degz,param_face,viewx,viewy,respect) %>%
      dplyr::group_by(shape_id) %>%
      dplyr::group_modify(
        ~(function(dotx) {
          tidyr::crossing(
            dotx,
            pts_matrix(lx = dotx$lx[1], ly = dotx$ly[1], lz = dotx$lz[1])
          ) %>%
            center3(xc = 0, yc = 0,zc = 0,old_xc = dotx$lx[1]/2,old_yc = dotx$ly[1]/2,old_zc = dotx$lz[1]/2) %>%
            rotate(degx = dotx$degx[1], degy = dotx$degy[1], degz = dotx$degz[1]) %>%
            center3(xc = dotx$xc[1], yc = dotx$yc[1], zc = 0, old_xc = 0, old_yc = 0, old_zc = 0) %>%
            scale_into_view(mwx = dotx$viewx[1], mwy = dotx$viewy[1], respect = dotx$respect[1]) %>%
            dplyr::filter(face == dotx$param_face[1])
        })(.x)
      ) %>%
      # remove grouping
      dplyr::ungroup() %>%
      # ensure draw order is correct
      dplyr::arrange(shape_id,draw_order,point_order)
    # -------- END: logic

  }
)
