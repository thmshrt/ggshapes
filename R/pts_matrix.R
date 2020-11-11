
#BEGIN: description
#' Matrix points
#'
#' {description placeholder}
#'
#' @usage
#' pts_matrix(
#'   nx_ = 1,
#'   ny_ = 1,
#'   nz_ = 1
#' )
#'
#' @param nx_    \[integer\], length 1, positive, x dimension of matrix
#' @param ny_    \[integer\], length 1, positive, y dimension of matrix
#' @param nz_    \[integer\], length 1, positive, z dimension of matrix
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

pts_matrix = function(
  nx_ = 1,
  ny_ = 1,
  nz_ = 1
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
  #END: param checks

  #BEGIN: computation
  pts_unit_bounding_box() %>%
    scale3(!!nx_, !!ny_, !!nz_) ->
    tbl_bounding_box

  # indices
  tidyr::crossing(ix = 1:!!nx_,
                  iy = 1:!!ny_,
                  iz = 1:!!nz_
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
    dplyr::mutate(shape_id = paste(iz,iy,iz,face_order,sep=',')) %>%
    dplyr::bind_rows(tbl_bounding_box)
    # keep only outermost faces
  #END: computation
}


#END: code
#BEGIN: examples
#' @examples
#' #BEGIN: example
#' # pts_matrix default behavior is a scalar matrix
#' pts_matrix() %>%
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
#' # creating a matrix of dimension 3x2x1
#' pts_matrix(3,2,1) %>%
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
#'     data = data %>% dplyr::filter(face == 'top' & tb == 'bottom' & rl == 'middle'),
#'     mapping = aes(x = x, y = y, label = sprintf('%i,%i,%i',ix,iy,iz))
#'   )
#' #END: example
#'
#' #' #BEGIN: example
#' # indices are in standard mathematical notation orientation
#' # but are 1 indexed to be consistent with R's indexing
#' pts_matrix(3,2,1) %>%
#'   center3(0,0,0,keep_bounding = FALSE) -> data
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
#'     data = data %>% dplyr::filter(face == 'top' & tb == 'bottom' & rl == 'middle'),
#'     mapping = aes(x = x, y = y, label = sprintf('%i,%i,%i',ix,iy,iz))
#'   )
#' #END: example
#'
#' #' #BEGIN: example
#' # drawing larger matrices is a bit slow. we are actively working to speed
#' # this up. please consider using ellipses matrices to very large matrices.
#' # also ellispes matrices represent very large matrices quite nicely
#' pts_matrix(10,10,10) %>%
#'   center3(0,0,0) %>%
#'   rotate3(70,20) %>%
#'   scale_into_viewport() -> data
#'
#' ggplot() +
#'   coord_equal() +
#'   xlim(c(-2,2)) +
#'   ylim(c(-2,2)) +
#'   geom_polygon(
#'     data = data,
#'     mapping = aes(x = x, y = y, group = shape_id, fill = face),
#'     colour = 'black'
#'   ) +
#'   geom_polygon(
#'     data = pts_viewport(),
#'     mapping = aes(x = x, y = y),
#'     fill = NA, colour = 'black'
#'   )
#'
#' #END: example
#END: examples
