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
#'
#'
#' @export
rotate = function(
  tbl,
  degx = 0, degy = 0, degz = 0
) {

  # checks
  # check tbl
  if (!any(class(tbl) %in% c(class(tibble()),class(data.frame()))))
    stop(paste0('param tbl must be one of ',paste(class(tibble()),sep = ',')))
  if (!all(c('x','y','z') %in% names(tbl)))
    stop(paste0('tbl must contain columns x,y,z'))
  if (!all(c(class(tbl$x),class(tbl$y),class(tbl$z)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('columns x,y,z must be of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # check degrees length
  if (!all(length(degx) == 1 && length(degy) == 1 && length(degz) == 1))
    stop(paste0('degx, degy, degz must be length 1'))

  # check degrees type
  if (!all(c(class(degx),class(degy),class(degz)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('param degx,degy,degz must be one of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # logic
  # identity case
  if (degx == 0 && degy == 0 && degz == 0) return(tbl)

  cbind(tbl$x,tbl$y,tbl$z) -> p

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
  tbl %>% dplyr::mutate(x = rotp[,1], y = rotp[,2], z = rotp[,3])
}
