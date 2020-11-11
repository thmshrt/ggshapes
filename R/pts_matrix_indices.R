
#BEGIN: description
#' {title placeholder}
#'
#' {description placeholder}
#'
#' @usage
#' pts_matrix_indices(
#'   nx_ = 1, ny_ = 1, nz_ = 1,
#'   ix_ = 1,
#'   iy_ = 1,
#'   iz_ = 1
#' )
#'
#' @param nx_    \[integer\], length 1, positive, x dimension of matrix
#' @param ny_    \[integer\], length 1, positive, y dimension of matrix
#' @param nz_    \[integer\], length 1, positive, z dimension of matrix
#' @param ix_    \[integer\], length >= 1, positive, index of x cell
#' @param iy_    \[integer\], length >= 1, positive, index of y cell
#' @param iz_    \[integer\], length >= 1, positive, index of z cell
#'
#' @return
#' [tibble] with columns
#' * `x` x coordinate of scalar
#' * `y` y coordinate of scalar
#' * `z` z coordinate of scalar
#' * `face` is face front, back, top, bottom, left or right
#' * `tb` is the scalar on top, bottom or middle of face
#' * `rl` is the scalar on left, right or middle of face
#' * `point_order` points draw order
#' * `face_order`  face draw order for default faces
#'
#' @export
#' @importFrom magrittr %>%
#END: description
#BEGIN: code

pts_matrix_indices = function(
  nx_ = 1, ny_ = 1, nz_ = 1,
  ix_ = 1,
  iy_ = 1,
  iz_ = 1
) {
  #BEGIN: setup params
  # ix_ = rlang::enexpr(ix_)
  # iy_ = rlang::enexpr(iy_)
  # iz_ = rlang::enexpr(iz_)
  #END: setup params

  # browser()
  #BEGIN: param checks
  if (!all(c(length(nx_),length(ny_),length(nz_)) == 1))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy length(v) == 1")

  if (!all(c(nx_,ny_,nz_) == as.integer(c(nx_,ny_,nz_))))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy v == as.integer(v)")

  if (!all(c(nx_,ny_,nz_) >= 1))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy v >= 1")

  if (!all(c(length(ix_),length(iy_),length(iz_)) >= 1))
    rlang::abort(message = "params ix_, iy_, iz_ must satisfy length(v) >= 1")

  if (!all(c(ix_,iy_,iz_) == as.integer(c(ix_,iy_,iz_))))
    rlang::abort(message = "params ix_, iy_, iz_ must satisfy v == as.integer(v)")

  if (!all(c(ix_) <= nx_))
    rlang::abort(message = "params ix_ must satisfy v <= nx_")

  if (!all(c(iy_) <= ny_))
    rlang::abort(message = "params iy_ must satisfy v <= ny_")

  if (!all(c(iz_) <= nz_))
    rlang::abort(message = "params iz_, must satisfy v <= nz_")
  #END: param checks

  #BEGIN: computation
  pts_unit_bounding_box() %>%
    scale3(!!nx_, !!ny_, !!nz_) ->
    tbl_bounding_box

  #END: computation
  tibble(
    ix = ix_,
    iy = iy_,
    iz = iz_
  ) %>%
    tidyr::crossing(.,pts_scalar(keep_bounding = FALSE)) %>%
    dplyr::mutate(
      x = ix - 1 + x,
      y = ny_ - iy + y,
      z = nz_ - iz + z,
    ) %>%
    dplyr::arrange(desc(iz),ix,iy,face_order,point_order) %>%
    dplyr::mutate(shape_id =
                    paste(iz,ix,iy,face_order,sep=',') %>%
                    factor(levels = unique(.)) %>%
                    as.integer()) %>%
    dplyr::bind_rows(tbl_bounding_box)
  #BEGIN: return

  #END: return
}

#END: code
#BEGIN: examples
#' @examples
#' #' #BEGIN: example
#' # pts_matrix_indices default behavior is a scalar matrix
#' pts_matrix_indices() %>%
#'   center3(0,0,0) %>%
#'   rotate3(70,20,keep_bounding = FALSE) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-4,4)) +
#'   ylim(c(-4,4)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y, group = shape_id, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_text(
#'     data = data %>% dplyr::filter(face == 'right' & tb == 'top' & rl == 'middle'),
#'     mapping = aes(x = x, y = y, label = sprintf('%i,%i,%i',ix,iy,iz))
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # pts_matrix_indices default behavior is a scalar matrix
#' pts_matrix_indices(
#'   nx_ = 3, ny_ = 3, nz = 3,
#'   ix_ = c(1,2,3,1,1),
#'   iy_ = c(1,1,1,2,3),
#'   iz_ = c(1,2,3,2,3)
#' ) %>%
#'   center3(0,0,0) %>%
#'   rotate3(70,20) -> data
#'
#' data %>% dplyr::filter(bounding_box) -> data_bounding
#' data %>% dplyr::filter(!bounding_box) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-4,4)) +
#'   ylim(c(-4,4)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y, group = shape_id, fill = face),
#'     colour = 'black', alpha = 0.7
#'   ) +
#'   geom_polygon(
#'     data = data_bounding %>% dplyr::filter(face == 'bottom'),
#'     mapping = aes(x = x, y = y, group = face),
#'     colour = 'black', fill = NA, alpha = 0.5
#'   ) +
#'   geom_text(
#'     data = data %>% dplyr::filter(face == 'right' & tb == 'top' & rl == 'middle'),
#'     mapping = aes(x = x, y = y, label = sprintf('%i,%i,%i',ix,iy,iz))
#'   )
#' #END: example
#END: examples
