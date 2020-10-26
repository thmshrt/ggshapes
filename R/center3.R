#' Use expressions to center a tbl or data.frame at `xc`, `yc` `zc`
#'
#' Centers a table containing with variables aliased by `x`, `y`, `z`
#' at a point specified by expressions `xc`, `yc` `zc`. Using column
#' `x` as an example, the new coordinates are computed using \cr
#' \cr
#' \deqn{x_{i} = x_{i} - mean(x) + xc}
#' \cr
#' where \cr
#' * \eqn{x}         is `m x 1` column
#' * \eqn{x_{i}}     is `i`th element in column
#' * \eqn{mean(x)}   is \eqn{\frac{1}{\vert x \vert} \sum_i x_i}
#' * \eqn{xc}        is `1 x 1` scalar
#'
#' @usage
#' center3(tbl, xc = NULL, yc = NULL, zc = NULL, old_xc = NULL,
#' old_yc = NULL, old_zc = NULL, x = x, y = y, z = z)
#'
#' @param tbl        \[data.frame or tibble\], contains named columns `x`, `y`, `z`
#' @param xc         \[numeric\], default NULL, length 1, new `x` center
#' @param yc         \[numeric\], default NULL, length 1, new `y` center
#' @param zc         \[numeric\], default NULL, length 1, new `z` center
#' @param old_xc     \[numeric\], default NULL, length 1, old `x` center
#' @param old_yc     \[numeric\], default NULL, length 1, old `y` center
#' @param old_zc     \[numeric\], default NULL, length 1, old `z` center
#' @return           [tibble], with `x`, `y`, `z` centered at `xc`,`yc`,`zc`
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(ggplot2)
#' library(grid)
#' library(magrittr)
#' # default call behavior is returns tibble or data.frame without
#' # modification (identity transformation)
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %>%
#'   center3()
#'
#' # fails if one or more of x,y,z not present
#' # modification (identity transformation)
# tribble(
#   ~x,
#   0,
#   0,
#   1,
#   1,
# ) %>%
#   center3()

#' # centering at 0, 0, 0
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %>%
#'   center3(0,0,0)
#'
#' # centering at 1, 0, 1
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %>%
#'   center3(1,0,1)
#'
#' # centering at 1, 0, 1
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %>%
#'   center3(xc = 1,zc = 1)
#' @export
#' @import rlang purrr tibble dplyr
#'
center3 = function(
  tbl,
  xc = NULL,yc = NULL,zc = NULL,
  old_xc = NULL, old_yc = NULL, old_zc = NULL,
  x = x, y = y, z = z
) {

  x = rlang::expr(x)
  y = rlang::expr(y)
  z = rlang::expr(z)

  # -------- checks
  # TODO

    # check tbl
  if (!any(class(tbl) %in% c(class(tibble()),class(data.frame()))))
    stop(paste0('param tbl must be one of ',paste(class(tibble()), collapse = ', '),sep = ''))

  # check all variables in tbl
  names = purrr::map_chr(c(x,y,z),rlang::as_name)
  if (!all(names %in% names(tbl)))
    stop(paste0('tbl must contain columns ',paste(names,collapse = ', ')))

  # check if all classes are numeric
  # check is performed by dplyr::mutate

  # identity case
  if (all(length(xc) == 0,length(yc) == 0,length(zc) == 0))
    if (all(is.null(xc),is.null(yc),is.null(zc)))
      return(tbl)

  # # check parameter lengths
  # # cannot be check since expression length is always 1

  # # check parameter types
  # # cannot be check since we don't know the expression values until it is evaluated

  # -------- BEGIN: setup parameters
  if (is.null(old_xc)) old_xc = rlang::expr(mean(!!x))
  if (is.null(old_yc)) old_yc = rlang::expr(mean(!!y))
  if (is.null(old_zc)) old_zc = rlang::expr(mean(!!z))

  xshift = rlang::expr(- !!old_xc + !!xc)
  yshift = rlang::expr(- !!old_yc + !!yc)
  zshift = rlang::expr(- !!old_zc + !!zc)
  # -------- BEGIN: end parameters

  # -------- logic
  tbl %>%
    dplyr::mutate(
      !!x := !!x + !!xshift,
      !!y := !!y + !!yshift,
      !!z := !!z + !!zshift
    )
}
