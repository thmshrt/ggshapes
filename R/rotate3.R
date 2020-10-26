#' rotates x,y,z coords by specified degrees
#'
#' Rotates `x`, `y`, `z` coordinates by degrees `degx`, `degy`, `degz` about the
#' origin (x = 0,y = 0,z = 0).
#'
#' @param table      \[data.frame or tibble\], contains named columns `x`, `y`, `z`
#' @param degx       \[numeric\], length 1, degrees to rotate `x` by
#' @param degy       \[numeric\], length 1, degrees to rotate `y` by
#' @param degz       \[numeric\], length 1, degrees to rotate `x` by
#' @return           [tibble], with `x`, `y`, `z` rotated by `degx`, `degy`, `degz`
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
#' #    70 degrees anticlockwise about its x axis
#' #    20 degrees anticlockwise about its y axis
#' # and then view a cube's front, right, and top faces
#' pts_unit_cube() %}%
#' center() %}%
#' rotate(degx = 70, degy = 20, degz = 0) %}%
#' dplyr::filter(face %in% c('front','top','right')) %}%
#' ggplot() +
#' coord_equal() +
#' scale_x_continuous(limits = c(-2,2)) +
#' scale_y_continuous(limits = c(-2,2)) +
#' geom_polygon(mapping = aes(x = x, y = y,group = face, fill = face))
rotate3 = function(
  tbl,
  degx = 0, degy = 0, degz = 0,
  x = x, y = y, z = z
) {
  # params
  x = rlang::enquo(x)
  y = rlang::enquo(y)
  z = rlang::enquo(z)

  # checks
  # how do i check this?

  # logic
  # identity case
  if (degx == 0 && degy == 0 && degz == 0) return(tbl)

  tbl %>% dplyr::select(!!x,!!y,!!z) %>% cbind() -> p

  # convert degrees to radians
  # form rotation matrices
  radx = pi/180 * degx
  R.x = rbind(
    c(         1,         0,         0),
    c(         0, cos(radx), sin(radx)),
    c(         0,-sin(radx), cos(radx))
  )

  rady = pi/180 * degy
  R.y = rbind(
    c( cos(rady),         0,-sin(rady)),
    c(         0,         1,         0),
    c( sin(rady),         0, cos(rady))
  )

  radz = pi/180 * degz
  R.z = rbind(
    c( cos(radz), sin(radz),         0),
    c(-sin(radz), cos(radz),         0),
    c(         0,         0,         1)
  )

  # transform
  p %>% t() %>% `%*%`(R.x,.) %>% `%*%`(R.y,.) %>% `%*%`(R.z,.) %>% t() -> rotp

  # return
  tbl %>% dplyr::mutate(!!x := rotp[,1], !!y := rotp[,2], !!z := rotp[,3])
}

# f = function(tbl,x = x, y =y ) {
#   x = rlang::enquo(x)
#   y = rlang::enquo(y)
#   tbl %}% dplyr::select(!!x, !!y) %}% cbind()
# }
# tbl %}% f()
