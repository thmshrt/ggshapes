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
#'cuboid_slice(
#'  tbl = pts_unit_cube(),
#'  sx = .5, sxend = 1, sy = .2, syend = 1, sz = 0, szend = 1
#') %}%
#'  # center slice at center of cuboid
#'  center3(xc = 0, yc = 0, zc = 0, x = sx, y = sy, z = sz, old_xc = mean(x), old_yc = mean(y), old_zc = mean(z)) %}%
#'  # center cuboid at center of cuboid
#'  center3(xc = 0,yc = 0,zc = 0) %}%
#'  # rotate slice
#'  rotate3(degx = 70,degy = 20,degz = 0,x = sx,y = sy,z = sz) %}%
#'  # rotate cuboid
#'  rotate3(degx = 70,degy = 20,degz = 0) %}%
#'  ggplot() +
#'  coord_equal() +
#'  scale_x_continuous(limits = c(-2,2)) +
#'  scale_y_continuous(limits = c(-2,2)) +
#'  geom_polygon(mapping = aes(x = x, y = y, group = face), fill = NA, colour = 'black') +
#'  geom_polygon(mapping = aes(x = sx, y = sy, group = face), fill = NA, colour = 'blue')

cuboid_slice = function(
  tbl,
  sx, sxend, sy, syend, sz, szend
) {
  param_sx = sx
  param_sxend = sxend
  param_sy = sy
  param_syend = syend
  param_sz = sz
  param_szend = szend

  # identity case
  # sx, sxend, sy, syend, sz, szend are length 1
  # sx, sxend, sy, syend, sz, szend are pairwise related <
  # sx, sxend, sy, syend, sz, szend are all -inf, inf respectively

  # replacement case
  # sx, sxend, sy, syend, sz, szend are length 1
  # sx, sxend, sy, syend, sz, szend are pairwise related <
  # replace -inf, inf with min max

  # BEGIN checks
  # tbl is a tibble or data.frame
  # x,y,z in tbl
  # x,y,z are numeric or integer
  # sx, sxend, sy, syend, sz, szend are length 1
  # sx, sxend, sy, syend, sz, szend are length 1
  # END

  # logic
  # replace min with sx
  # replace max with sxend
  tbl %>%
    dplyr::mutate(sx = 0, sy = 0, sz = 0) %>%
    dplyr::mutate(
      sx = replace(x, x==min(x), param_sx),
      sx = replace(sx, x==max(x), max(x) * param_sxend),

      sy = replace(y, y==min(y), param_sy),
      sy = replace(sy, y==max(y), max(y) * param_syend),

      sz = replace(z, z==min(z), param_sz),
      sz = replace(sz, z==max(z), max(y) * param_szend),
    )
}
