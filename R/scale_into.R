
#BEGIN: description
#' Scale a vector of values into a range
#'
#' @usage
#' scale_into(x,
#'            new_xmin,
#'            new_xmax,
#'            old_xmin = NULL,
#'            old_xmax = NULL)
#'
#' @param x          \[numeric\], length >= 1,
#' @param new_xmin   \[numeric\], length == 1,
#' @param new_xmax   \[numeric\], length == 1,
#' @param old_xmin   \[numeric\], NULL or length 1
#' @param old_xmax   \[numeric\], NULL or length 1
#'
#' @return           [numeric]
#'
#' @export
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

scale_into = function(
  x,
  new_xmin,
  new_xmax,
  old_xmin = NULL,
  old_xmax = NULL
  ) {

  #BEGIN: setup params

  #END: setup params
  if (!(length(x) >= 1))
    rlang::abort(message = 'param x must satisfy length(v) >= 1')

  if (!all(c(length(new_xmin),length(new_xmax)) == 1))
    rlang::abort(message = 'param new_xmin, new_xmax must satisfy length(v) == 1')

  if (!((length(old_xmin) == 1 && length(old_xmax) == 1) |
        (is.null(old_xmin) && is.null(old_xmax))))
    rlang::abort(message = 'param old_xmin, old_max must satisfy is.null(v) or length(v) == 1')
  #BEGIN: param checks

  #END: param checks

  #BEGIN: begin computation
  # scale into 0,1
  if (is_null(old_xmin)) old_xmin = min(x)
  if (is_null(old_xmax)) old_xmax = max(x)


  old_range = old_xmax - old_xmin

  (x - old_xmin) / old_range *
    # expand into new range
    (new_xmax - new_xmin) +
    # shift to new start
    new_xmin

  #END: end computation
}

#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # scale into range
#' x = 0:10
#' x %>% scale_into(0,1)
#' #END: example
#'
#' #BEGIN: example
#' # scale into range using a different xmin and x max
#' x %>% scale_into(0,1,0,10)
#' #END: example
#'
#END: examples
