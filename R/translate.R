#' translates x,y,z coords by a specified amount
#'
#' Moves `x`, `y`, `z` coordinates by amounts in `x`, `y`, `z` direction by
#' `dx`, `dy` `dz`. Using column `x` as an example, the new coordinates are
#' computed using \cr
#' \cr
#' \deqn{x_{i} = x_{i} + dx}
#' \cr
#' where \cr
#' * \eqn{x}         is `m x 1` column
#' * \eqn{x_{i}}     is `i`th element in column
#' * \eqn{dx}        is `1 x 1` scalar
#'
#' @param table      \[data.frame or tibble\], contains named columns `x`, `y`, `z`
#' @param dx         \[numeric\], length 1, amount to shift `x` by
#' @param dy         \[numeric\], length 1, amount to shift `y` by
#' @param dz         \[numeric\], length 1, amount to shift `z` by
#' @return           [tibble], with `x`, `y`, `z` shifted by `dx`, `dy`, `dz`
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
#' #TODO
#'
#'

translate = function(
  tbl,
  dx=0,dy=0,dz=0
) {

  # checks
  # check tbl
  if (!any(class(tbl) %in% c(class(tibble()),class(data.frame()))))
    stop(paste0('param tbl must be one of ',paste(class(tibble()),sep = ',')))
  if (!all(c('x','y','z') %in% names(tbl)))
    stop(paste0('tbl must contain columns x,y,z'))
  if (!all(c(class(tbl$x),class(tbl$y),class(tbl$z)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('columns x,y,z must be of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # check scale length
  if (!all(length(dx) == 1 && length(dy) == 1 && length(dz) == 1))
    stop(paste0('dx, dy, dx must be length 1'))

  # check degrees type
  if (!all(c(class(dx),class(dy),class(dz)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('param dx, dy, dz must be one of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # logic
  # identity case
  if (dx == 0 && dy == 0 && dz == 0) return(tbl)

  tbl %>% dplyr::mutate(
    x = x + dx,
    y = y + dy,
    z = z + dz
    )
}
