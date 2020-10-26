#' scales x,y,z coords by a specified amount
#'
#' Scales `x`, `y`, `z` coordinates by amounts `sx`, `sy`, `sz` respectively.
#' Using column `x` as an example, the new coordinates are computed using \cr
#' \cr
#' \deqn{x_{i} = x_{i} * sx}
#' \cr
#' where \cr
#' * \eqn{x}         is `m x 1` column
#' * \eqn{x_{i}}     is `i`th element in column
#' * \eqn{sx}        is `1 x 1` scalar
#'
#' @param table      \[data.frame or tibble\], contains named columns `x`, `y`, `z`
#' @param sx         \[numeric\], length 1, nonnegative, amount to scale `x` by
#' @param sy         \[numeric\], length 1, nonnegative, amount to scale `y` by
#' @param sz         \[numeric\], length 1, nonngeative amount to scale `z` by
#' @param respect    \[logical\], length 1, if true, scaling respect original aspect ratio
#' @return           [tibble], with `x`, `y`, `z` scale by `sx`, `sy`, `sz`
#
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
scale = function(
  tbl,
  sx = 1, sy = 1, sz = 1,
  respect = FALSE
) {

  # -------- BEGIN: checks
  # check tbl
  if (!any(class(tbl) %in% c(class(tibble()),class(data.frame()))))
    stop(paste0('param tbl must be one of ',paste(class(tibble()),sep = ',')))
  if (!all(c('x','y','z') %in% names(tbl)))
    stop(paste0('tbl must contain columns x,y,z'))
  if (!all(c(class(tbl$x),class(tbl$y),class(tbl$z)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('columns x,y,z must be of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # check scale length
  if (!all(length(sx) == 1 && length(sy) == 1 && length(sz) == 1))
    stop(paste0('param sx, sy, sz must be length 1'))

  # check degrees type
  if (!all(c(class(sx),class(sy),class(sz)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('params sx, sy, sz must be one of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # check scale value
  if (!all(sx >= 0 && sy >= 0 && sz >= 0))
    stop(paste0('params sx, sy, sz must be >= 0'))

  # check respect type
  if (!all(c(class(respect)) %in% c(class(logical()))))
    stop(paste0('param respect must be of type',paste(c(class(logical())),sep = ',')))

  # check respect length
  if (!all(length(respect) == 1))
    stop(paste0('param respect must be length 1'))
  # -------- END: checks

  # -------- BEGIN: logic
  # logic
  # identity case
  if (sx == 1 && sy == 1 && sz == 1) return(tbl)

  if (respect) {
    s = sx * sy * sx
    return(
      tbl %>% dplyr::mutate(
        x = x * s,
        y = y * s,
        z = z * s
      )
    )
  } else {
    return(
      tbl %>% dplyr::mutate(
        x = x * sx,
        y = y * sy,
        z = z * sz
      )
    )
  }
  # -------- END: logic
}
