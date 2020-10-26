#' scales x,y coords by to fit into a quasiviewport
#'
#' TODO
#'
#' @param tbl        \[data.frame or tibble\], contains named columns `x`, `y`
#' @param mwx        \[numeric\], length 1, nonnegative, maximum width of `x`
#' @param mwy        \[numeric\], length 1, nonnegative, maximum width of `y`
#' @param respect    \[logical\], length 1, if true (default), scaling respects original aspect ratio
#' @return           [tibble], with `x`, `y`, `z` scale by `sx`, `sy`, `sz`
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
scale_into_view = function(
  tbl,
  mwx = NA, mwy = NA,
  respect = TRUE
) {

  # identity case
  if (all(length(mwx) == 1,length(mwy) == 1))
    if (all(is.na(mwx),is.na(mwy)))
      return(tbl)

  # -------- BEGIN: checks
  # check tbl
  if (!any(class(tbl) %in% c(class(tibble()),class(data.frame()))))
    stop(paste0('param tbl must be one of ',paste(class(tibble()),sep = ',')))
  if (!all(c('x','y') %in% names(tbl)))
    stop(paste0('tbl must contain columns x,y'))
  if (!all(c(class(tbl$x),class(tbl$y)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('columns x,y must be of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # check diff range length
  if (!all(length(mwx) == 1,length(mwy) == 1))
    stop(paste0('param mwx, mwy must be length 1'))

  # check diff range type
  if (!all(c(class(mwx),class(mwy)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('params mwx, mwy must be one of type ',paste(c(class(numeric()),class(integer())),sep = ',')))

  # check diff range value
  if (!all(mwx >= 0, mwy >= 0))
    stop(paste0('params mwx, mwy must be >= 0'))

  # check respect type
  if (!all(c(class(respect)) %in% c(class(logical()))))
    stop(paste0('param respect must be of type',paste(c(class(logical())),sep = ',')))

  # check respect length
  if (!all(length(respect) == 1))
    stop(paste0('param respect must be length 1'))
  # -------- END: checks

  # -------- BEGIN: logic
  # compute vector width = diff(range(x))
  vwx = tbl$x %>% vector_width()
  vwy = tbl$y %>% vector_width()

  if (respect) {
    s = min( mwx / vwx, mwy / vwy )
    return(
      tbl %>% dplyr::mutate(
        x = x * s,
        y = y * s
      )
    )
  } else {
    return(
      tbl %>% dplyr::mutate(
        x = x * 1 / vwx,
        y = y * 1 / vwy
      )
    )
  }
  # -------- END: logic
}
