
#BEGIN: description
#' Matrix slice points
#'
#' {description placeholder}
#'
#' @usage
#' pts_matrix_slice(
#'   nx_ = 1, ny_ = 1, nz_ = 1,
#'   ix_ = 1, ixend_ = 1,
#'   iy_ = 1, iyend_ = 1,
#'   iz_ = 1, izend_ = 1
#' )
#'
#' @param nx_    \[integer\], length 1, positive, x dimension of matrix
#' @param ny_    \[integer\], length 1, positive, y dimension of matrix
#' @param nz_    \[integer\], length 1, positive, z dimension of matrix
#' @param ix_    \[integer\], length 1, >= 1 & <= ixend_, 1 indexed start of x slice
#' @param ixend_ \[integer\], length 1, >= ix_ & <= nx_, 1 indexed end of x slice (inclusive)
#' @param iy_    \[integer\], length 1, >= 1 & <= iyend_, 1 indexed start of y slice
#' @param iyend_ \[integer\], length 1, >= iy_ & <= ny_, 1 indexed end of y slice (inclusive)
#' @param iz_    \[integer\], length 1, >= 1 & <= izend_, 1 indexed start of z slice
#' @param izend_ \[integer\], length 1, >= iz_ & <= nz_, 1 indexed end of z slice (inclusive)

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

pts_matrix_slice = function(
  nx_ = 1, ny_ = 1, nz_ = 1,
  ix_ = 1, ixend_ = 1,
  iy_ = 1, iyend_ = 1,
  iz_ = 1, izend_ = 1
) {
  #BEGIN: setup params
  nx_ = rlang::enexpr(nx_)
  ny_ = rlang::enexpr(ny_)
  nz_ = rlang::enexpr(nz_)

  ix_ = rlang::enexpr(ix_)
  ixend_ = rlang::enexpr(ixend_)

  iy_ = rlang::enexpr(iy_)
  iyend_ = rlang::enexpr(iyend_)

  iz_ = rlang::enexpr(iz_)
  izend_ = rlang::enexpr(izend_)
  #END: setup params

  #BEGIN: param checks
  if (!all(c(length(nx_),length(ny_),length(nz_)) == 1))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy length(v) == 1")

  if (!all(c(nx_,ny_,nz_) == as.integer(c(nx_,ny_,nz_))))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy v == as.integer(v)")

  if (!all(c(nx_,ny_,nz_) >= 1))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy v >= 1")


  if (!all(c(length(ix_),length(ixend_),length(iy_),length(iyend_),length(iz_),length(izend_)) == 1))
    rlang::abort(message = "params ix_, ixend_, iy_, iyend_, iz_, iz_end_ must satisfy length(v) == 1")

  if (!all(c(ix_,ixend_,iy_,iyend_,iz_,izend_) == as.integer(c(ix_,ixend_,iy_,iyend_,iz_,izend_))))
    rlang::abort(message = "params ix_, ixend_, iy_, iyend_, iz_, iz_end_ must satisfy v == as.integer(v)")

  if (!all(c(ix_,ixend_,iy_,iyend_,iz_,izend_) >= 1))
    rlang::abort(message = "params ix_, ixend_, iy_, iyend_, iz_, iz_end_ must satisfy v >= 1")


  if (!all(c(ix_,ixend_) <= nx_))
    rlang::abort(message = "params ix_, ixend_ must satisfy v <= nx_")

  if (!all(c(iy_,iyend_) <= ny_))
    rlang::abort(message = "params iy_, iyend_ must satisfy v <= ny_")

  if (!all(c(iz_,izend_) <= nz_))
    rlang::abort(message = "params iz_, izend_ must satisfy v <= nz_")
  #END: param checks

  #BEGIN: computation
  pts_unit_bounding_box() %>%
    scale3(!!nx_, !!ny_, !!nz_) ->
    tbl_bounding_box

  tidyr::crossing(ix = !!ix_:!!ixend_,
                  iy = !!iy_:!!iyend_,
                  iz = !!iz_:!!izend_
  ) %>%
    # remove unseen blocks
    # keep only visible top, right, front blocks
    dplyr::mutate(is_min_z = iz == min(iz),
                  is_max_x = ix == max(ix),
                  is_max_y = iy == max(iy)) %>%
    dplyr::filter(is_max_x | is_max_y | is_min_z ) %>%
    dplyr::select(-is_min_z, -is_max_x, -is_max_y) %>%
    # add in points
    dplyr::mutate(scalar_id=1:n()) %>%
    tidyr::crossing(.,pts_scalar(keep_bounding = FALSE)) %>%
    # dplyr::group_by(scalar_id) %>%
    # dplyr::group_modify(
    #   ~(function(dotx)
    #     bind_cols(dotx,)
    #     )(.x)) %>%
    # dplyr::ungroup() %>%
    # compute x, y, z
    dplyr::mutate(
      x = ix - 1 + x,
      y = !!ny_ - iy + y,
      z = !!nz_ - iz + z,
    ) %>%
    # # remove unseen faces
    # keep only visible top, right, front faces
    dplyr::mutate(top_is_visible = iz == min(iz) & face == 'top',
                  right_is_visible = ix == max(ix) & face == 'right',
                  front_is_visible = iy == max(iy) & face == 'front') %>%
    dplyr::filter(top_is_visible | right_is_visible | front_is_visible) %>%
    dplyr::select(-top_is_visible, -right_is_visible, -front_is_visible) %>%
    # dplyr::select(-is_min_z, -is_max_x, -is_max_y, -keep_face) %>%
    dplyr::arrange(desc(iz),iy,ix,face_order,point_order) %>%
    dplyr::mutate(shape_id =
                    paste(iz,ix,iy,face_order,sep=',') %>%
                    factor(levels = unique(.)) %>%
                    as.integer()) %>%
    dplyr::bind_rows(tbl_bounding_box)
  # keep only outermost faces
  #END: computation
}

#END: code
#BEGIN: examples
#' @examples
#' #' #BEGIN: example
#' # pts_slice_matrix default behavior is a scalar matrix
#' pts_matrix_slice() %>%
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
#' # creating a matrix slice of dimension 3x2x1
#' # notice that it is indexed by standard mathematical notation
#' pts_matrix_slice(3,3,3,1,3,1,2,1,1) %>%
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
#'     colour = 'black'
#'   ) +
#'   geom_polygon(
#'     data = data_bounding,
#'     mapping = aes(x = x, y = y, group = face),
#'     fill = NA, colour = 'black'
#'   )
#' #END: example
#'
#END: examples
