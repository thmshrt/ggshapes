#' Center a tbl or data.frame at `xc`, `yc` `zc`
#'
#' Centers a table containing `x`, `y`, `z` coordinates at a point specified by
#' `xc`, `yc` `zc`. Using column `x` as an example, the new coordinates are
#' computed using \cr
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
#' center(tbl, xc = NULL, yc = NULL, zc = NULL)
#'
#' @param tbl        \[data.frame or tibble\], contains named columns `x`, `y`, `z`
#' @param xc         \[numeric\], default NULL, length 1, new `x` center
#' @param yc         \[numeric\], default NULL, length 1, new `y` center
#' @param zc         \[numeric\], default NULL, length 1, new `z` center
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
#' ) %}%
#'   center()
#'
#' # fails if one or more of x,y,z not present
#' tribble(
#'   ~x,
#'   0,
#'   0,
#'   1,
#'   1,
#' ) %}%
#'   center()
#'
#' # centering at 0, 0, 0
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %}%
#'   center(0,0,0)
#'
#' # centering at 1, 0, 1
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %}%
#'   center(1,0,1)
#'
#' # center only x at 1
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %}%
#'   center(xc = 1)
#'
#' # center multiple by keyword
#' tribble(
#'   ~x, ~y, ~z,
#'   0,  0,  0,
#'   0,  1,  0,
#'   1,  1,  0,
#'   1,  0,  0,
#' ) %}%
#'   center(xc = 1, zc = 1)
#'
#' @export
#' @import tibble dplyr rlang
#'
center = function(
  tbl,
  xc = NULL,yc = NULL,zc = NULL
) {

  # -------- BEGIN: checks

  # check tbl
  if (!any(class(tbl) %in% c(class(tibble()),class(data.frame()))))
    stop(paste0('param tbl must be one of ',paste(class(tibble()), collapse = ', '),sep = ''))

  if (!all(c('x','y','z') %in% names(tbl)))
    stop(paste0('tbl must contain columns x, y, z'))

  if (!all(c(class(tbl$x),class(tbl$y),class(tbl$z)) %in% c(class(numeric()),class(integer()))))
    stop(paste0('columns x, y, z must be of type ',paste(c(class(numeric()),class(integer())),collapse=', '),sep = ''))

  # identity case
  if (all(length(xc) == 0,length(yc) == 0,length(zc) == 0))
    if (all(is.null(xc),is.null(yc),is.null(zc)))
      return(tbl)

  # check parameter lengths
  if (!all(c(length(xc),length(yc), length(zc)) %in% c(0,1)))
    stop(paste0('params xc, yc, zc must be length 1'))

  # check parameter types
  if (!all(c(class(xc),class(yc),class(zc)) %in% c(class(numeric()),class(integer()),class(NULL))))
    stop(paste0('params xc, yc, zc must be one of type ',paste(c(class(numeric()),class(integer()),class(NULL)),collapse = ', ')))

  # -------- END: checks

  # -------- BEGIN: setup parameters
  xshift = rlang::expr(- mean(x) + xc)
  yshift = rlang::expr(- mean(y) + yc)
  zshift = rlang::expr(- mean(z) + zc)

  if (is.null(xc)) xshift = rlang::expr(0)
  if (is.null(yc)) yshift = rlang::expr(0)
  if (is.null(zc)) zshift = rlang::expr(0)
  # -------- BEGIN: end parameters

  # -------- BEGIN: logic
  tbl %>%
    dplyr::mutate(
      x = x + !!xshift,
      y = y + !!yshift,
      z = z + !!zshift
    )
  # -------- END: logic
}
