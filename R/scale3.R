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
scale3 = function(
  tbl,
  sx = 1, sy = 1, sz = 1,
  x = x, y = y, z = z,
  respect = FALSE
) {
  x = rlang::enquo(x)
  y = rlang::enquo(x)
  z = rlang::enquo(x)

  # -------- BEGIN: checks
  # check tbl
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
        !!x := !!x * sx,
        !!y := !!y * sy,
        !!z := !!z * sz
      )
    )
  }
  # -------- END: logic
}
