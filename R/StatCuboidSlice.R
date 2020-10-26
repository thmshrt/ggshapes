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
#' # rotate a cube
#' #  70 degrees anticlockwise about its x axis
#' #  20 degrees anticlockwise about its y axis
#' # and then view a cube's front, right, and top faces
#'
#'StatCuboidSlice$compute_panel(
#'  data = tibble(
#'    xc = 0, yc = 0,
#'    lx = 1, ly = 1, lz = 1,
#'    sx = .5, sxend = 1, sy = .2, syend = 1, sz = 0, szend = 1
#'    ),
#'  degx = 70, degy = 20, degz = 0,
#'  # faces = list(c('top','front','right')),
#'  viewx = 1, viewy = 1, respect = FALSE
#') %}%
#'  ggplot() +
#'  coord_equal() +
#'  scale_x_continuous(limits = c(-2,2)) +
#'  scale_y_continuous(limits = c(-2,2)) +
#'  geom_polygon(mapping = aes(x = sx, y = sy, group = face), fill = 'grey', colour = 'blue') +
#'  geom_polygon(mapping = aes(x = x, y = y, group = face), fill = NA, colour = 'black')

StatCuboidSlice = ggplot2::ggproto(

    `_class` = 'StatCuboidSlice',
    `_inherit` = Stat,

    required_aes = c('xc','yc','lx','ly','lz','sx','sxend','sy','syend','sz','szend'),

    compute_panel = function(
      data,scales,
      degx,degy,degz,
      # faces,
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
          # dplyr::mutate(degx,degy,degz,faces,viewx,viewy,respect),
          dplyr::mutate(degx,degy,degz,viewx,viewy,respect),
        pts_unit_cube(),
      ) %>%
        # center each cube center at xc, yc
        dplyr::group_by(group) %>%
        dplyr::group_modify(
          ~(function(dotx) {
            dotx %>%
              cuboid_slice(
                sx = dotx$sx[1], sxend = dotx$sxend[1],
                sy = dotx$sy[1], syend = dotx$syend[1],
                sz = dotx$sz[1], szend = dotx$szend[1]
                ) %>%
              # center slice at 0,0,0 of cuboid
              center3(xc = 0, yc = 0, zc = 0, x = sx, y = sy, z = sz, old_xc = mean(x), old_yc = mean(y), old_zc = mean(z)) %>%
              # center cuboid at 0,0,0
              center3(xc = 0,yc = 0,zc = 0) %>%
              # scale slice
              scale3(sx = dotx$lx[1], sy = dotx$ly[1], sz = dotx$lz[1], x = sx, y = sy,z = sz) %>%
              # scale cuboid
              scale3(sx = dotx$lx[1], sy = dotx$ly[1], sz = dotx$lz[1]) %>%
              # rotate slice
              rotate3(degx = dotx$degx[1],degy = dotx$degy[1],degz = dotx$degz[1],x = sx,y = sy,z = sz) %>%
              # rotate cuboid
              rotate3(degx = dotx$degx[1],degy = dotx$degy[1],degz = dotx$degz[1]) %>%
              # recenter cuboid at xc, yc, zc
              center3(xc = dotx$xc[1], yc = dotx$yc[1], zc = 0) %>%
              # recenter slice at xc, yc, zc
              center3(xc = 0, yc = 0, zc = 0, x = sx, y = sy, z = sz, old_xc = mean(x), old_yc = mean(y), old_zc = mean(z)) %>%
              # scale slice into view
              scale_into_view3(mwx = viewx[1], mwy = viewy[1], w_var = x, h_var = y, x = sx, y = sy, respect = respect[1]) %>%
              # scale cuboid into view
              scale_into_view3(mwx = viewx[1], mwy = viewy[1], respect = respect[1]) # %>%
              # # filter for faces
              # dplyr::filter(face %in% faces[1][[1]])
          })(.x)
        ) %>%
        # remove grouping
        dplyr::ungroup() %>%
        # rearrange to get polygon correct
        dplyr::arrange(face,order)
      # -------- END: logic

    }
  )
