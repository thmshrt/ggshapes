
(defun insert-doc-template ()
  (interactive)
  (insert "#' <title placeholder> 
#'
#' <description placeholder> 
#' 
#' @usage
#' <usage placeholder>
#'
#' @param <param>   \\[<type>\\], <restrictions>
#' 
#' @return           [<type>]
#'
#' @export
#' @import magrittr rlang purrr tibble dplyr ggplot2
#' 
#' @examples
#' # identity case
#'
#'
"
	  ))

(defun insert-imports ()
  (interactive)
  (insert "#' @export
#' @import magrittr rlang purrr tibble dplyr ggplot2
"
	  ))







