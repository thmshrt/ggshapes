#' scales x,y coords by to fit into a quasiviewport
#'
#' #TODO
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
#'
#' @note
#'
#' @export
scale_into_view3 = function(
  tbl,
  mwx = NA, mwy = NA,
  w_var = x, h_var = y,
  old_w = NULL, old_h = NULL,
  x = x, y = y,
  respect = TRUE,
  use_bounding = FALSE
) {
  w_var = rlang::enquo(w_var)
  h_var = rlang::enquo(h_var)
  x = rlang::enquo(x)
  y = rlang::enquo(y)

  # identity case
  if (all(length(mwx) == 1,length(mwy) == 1))
    if (all(is.na(mwx),is.na(mwy)))
      return(tbl)

  # -------- BEGIN: checks
  # how to check this?
  # -------- END: checks

  # -------- BEGIN: logic
  # compute vector width = diff(range(x))
  vwx = tbl %>% dplyr::pull(!!w_var) %>% vector_width()
  vwy = tbl %>% dplyr::pull(!!h_var) %>% vector_width()
  if (use_bounding) {
    vwx = tbl %>% dplyr::filter(bounding) %>% dplyr::pull(!!w_var) %>% vector_width()
    vwy = tbl %>% dplyr::filter(bounding) %>% dplyr::pull(!!h_var) %>% vector_width()
  }

  if (!is.null(old_w)) vwx = old_w
  if (!is.null(old_h)) vwy = old_h

  if (respect) {
    s = min( mwx / vwx, mwy / vwy )
    return(
      tbl %>% dplyr::mutate(
        !!x := !!x * s,
        !!y := !!y * s
      )
    )
  } else {
    return(
      tbl %>% dplyr::mutate(
        !!x := !!x * 1 / vwx,
        !!y := !!y * 1 / vwy
      )
    )
  }
  # -------- END: logic
}
