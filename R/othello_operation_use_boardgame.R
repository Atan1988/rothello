#' @title generate the default board matrix
#' @name generate_othello_base_M
#' @export
generate_othello_base_M <- function() {
  matrix(
    c(rep(0, 27), 1, -1, rep(0,6), -1, 1, rep(0, 27)),
    ncol = 8
  )
}

#' @title check whether the space is 0 and eligible
#' @name chk_0_sp1
#' @param df board map
#' @param search_color color of the opposite piece
#' @export
chk_0_sp1 <- function(df, search_color) {

  tmp_df <- around_df %>%
    dplyr::inner_join(
      df %>% dplyr::select(row, col, s_val := val),
      by = c('s_row' = 'row', 's_col' = 'col')
    ) %>%
    dplyr::inner_join(
      df %>% dplyr::select(id,  val),
      by = c('id')
    )  %>% dplyr::filter(val == 0) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(sum(ifelse(dist == 1 & s_val == search_color, 1, 0)) >= 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id, x, y) %>%
    dplyr::mutate(same_color_dist = min(ifelse(s_val == (-1 * search_color), dist, 99 ))) %>%
    dplyr::filter(dist < same_color_dist) %>%
    dplyr::mutate(cum_s_val = cumsum(s_val)) %>%
    dplyr::filter(abs(max(cum_s_val)) == max(dist) )

  return(tmp_df)
}

#' @title  make legal move
#' @name mk_a_move
#' @param df board map
#' @param legal_move_df legal move space
#' @param row row of the legal move
#' @param col col of the legal move
#' @param color color of the legal move
#' @export
mk_a_move <- function(df, legal_move_df, row, col, color) {
   tmp_df <- dplyr::bind_rows(
     df %>% dplyr::filter(row == !!row, col == !!col) %>%
         dplyr::mutate(val = color),
     legal_move_df %>% dplyr::filter(row == !!row, col == !!col) %>%
         dplyr::mutate(s_val = s_val * -1) %>%
         dplyr::select(row := s_row, col := s_col, val := s_val) %>%
         dplyr::inner_join(df %>% dplyr::select(id, row, col), by = c('row', 'col'))
   )

   dplyr::bind_rows(
     df %>% dplyr::anti_join(tmp_df, by = 'id'),
     tmp_df
   )  %>% dplyr::arrange(col, row) %>% dplyr::distinct()
}
