
#BEGIN: description
#' Matrix face points
#'
#'
#' @usage
#' pts_matrix_face(
#'   nx_ = 1,
#'   ny_ = 1,
#'   nz_ = 1,
#'   face_ = 'bottom'
#' )
#'
#' @param nx_    \[integer\], length 1, positive, x dimension of matrix
#' @param ny_    \[integer\], length 1, positive, y dimension of matrix
#' @param nz_    \[integer\], length 1, positive, z dimension of matrix
#' @param face_ \[character\], one of 'front','back','top','bottom','left','right'
#'
#' @return
#' [tibble] with columns
#' * `x` x coordinate of point
#' * `y` y coordinate of point
#' * `z` z coordinate of point
#' * `face` is face front, back, top, bottom, left or right
#' * `tb` is the point on top, bottom or middle of face
#' * `rl` is the point on left, right or middle of face
#' * `point_order` points draw order
#' * `face_order`  face draw order for default faces
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols
#END: description
#BEGIN: code

pts_matrix_face = function(
  nx_ = 1,
  ny_ = 1,
  nz_ = 1,
  face_ = 'bottom'
) {
  #BEGIN: setup params
  nx_ = rlang::enexpr(nx_)
  ny_ = rlang::enexpr(ny_)
  nz_ = rlang::enexpr(nz_)

  #END: setup params

  #BEGIN: param checks
  if (!all(c(length(nx_),length(ny_),length(nz_)) == 1))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy length(v) == 1")

  if (!all(c(nx_,ny_,nz_) == as.integer(c(nx_,ny_,nz_))))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy v == as.integer(v)")

  if (!all(c(nx_,ny_,nz_) >= 1))
    rlang::abort(message = "params nx_, ny_, nz_ must satisfy v >= 1")

  if (!(length(face_) == 1))
    rlang::abort(message = "params face_ must satisfy length(v) == 1")

  if (!(face_ %in% c("top","bottom","left","right","front","back")))
    rlang::abort(message = "params face_ must be one of 'top','bottom','left','right','front','back")
  #END: param checks

  # browser()
  #BEGIN: computation
  pts_unit_bounding_box() %>%
    scale3(!!nx_, !!ny_, !!nz_) ->
    tbl_bounding_box

  # indices
  tidyr::crossing(ix = 1:!!nx_,
                  iy = 1:!!ny_,
                  iz = 1:!!nz_
  ) %>%
    # remove unseen blocks according to face
    dplyr::mutate(
      keep_top    = if(face_ == 'top'){iz == min(iz)} else {FALSE},
      keep_bottom = if(face_ == 'bottom'){iz == max(iz)} else {FALSE},
      keep_left   = if(face_ == 'left'){ix == min(ix)} else {FALSE},
      keep_right  = if(face_ == 'right'){ix == max(ix)} else {FALSE},
      keep_front  = if(face_ == 'front'){iy == max(iz)} else {FALSE},
      keep_back   = if(face_ == 'back'){iy == min(iy)} else {FALSE}) %>%
    dplyr::filter(keep_top | keep_bottom | keep_left | keep_right | keep_front | keep_back) %>%
    dplyr::select_at(vars(-starts_with('keep'))) %>%
    # add in pts_scalar
    dplyr::mutate(scalar_id=1:n()) %>%
    tidyr::crossing(.,pts_scalar(faces_ = face_, keep_bounding = FALSE)) %>%
    # compute x, y, z
    dplyr::mutate(
      x = ix - 1 + x,
      y = !!ny_ - iy + y,
      z = !!nz_ - iz + z,
    ) %>%
    # # remove unseen faces
    # keep only visible top, right, front faces
    dplyr::filter(face == face_) %>%
    # dplyr::select(-is_min_z, -is_max_x, -is_max_y, -keep_face) %>%
    dplyr::arrange(desc(iz),iy,ix,face_order,point_order) %>%
    dplyr::mutate(shape_id = paste(iz,iy,iz,face_order,sep=',')) %>%
    dplyr::bind_rows(tbl_bounding_box)
  # keep only outermost faces
  #END: computation
}


#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # pts_matrix face default behavior is to display the bottom face
#' pts_matrix_face() %>%
#'   center3(0,0,0) %>%
#'   rotate3(70,20,keep_bounding = FALSE) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-4,4)) +
#'   ylim(c(-4,4)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y, group = shape_id),
#'     colour = 'black', fill = 'grey'
#'   )
#' #END: example
#'
#' #BEGIN: example
#' # creating a matrix of dimension 3x2x1, default remains bottom
#' pts_matrix_face(3,2,1) %>%
#'   center3(0,0,0) %>%
#'   rotate3(70,20,keep_bounding = FALSE) -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-4,4)) +
#'   ylim(c(-4,4)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y, group = shape_id),
#'     colour = 'black', fill = 'grey'
#'   )
#' #END: example
#END: examples
