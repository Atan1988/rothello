#' @title generate the default board matrix
#' @name generate_othello_base_M
#' @param sz size of the board
#' @param val value of the board
#' @export
generate_othello_base_M <- function(sz, val = NULL) {
  if (is.null(val)) {
    mat <- matrix(
      rep(0, sz^2),
      ncol = sz
    )
    x <- c(sz/2, sz/2 + 1, sz/2 + 1, sz/2)
    y <- c(sz/2, sz/2 + 1, sz/2, sz/2 + 1)
    idx <- (y - 1) * sz + x
    mat[idx] <- c(1, 1, -1, -1)
  } else {
    mat <- matrix(val, ncol = sz)
  }
  mat
}

#' @title check whether the space is 0 and eligible
#' @name chk_0_sp1
#' @param df board map
#' @param search_color color of the opposite piece
#' @export
chk_0_sp1 <- function(df, search_color) {

      tmp_df <- around_df

      tmp_df$val <- with(tmp_df, df$val[(col - 1) * 8 + row])

      tmp_df$s_val <- with(tmp_df, df$val[(s_col - 1) * 8 + s_row])

      tmp_df$chk <- with(tmp_df, ifelse(dist == 1 & s_val == search_color, 1, 0))

      tmp_sums <- tapply(tmp_df$chk, tmp_df$id, sum)
      tmp_df$chk_sums <- with(tmp_df, tmp_sums[id])

      tmp_df <- tmp_df %>% .[.$val == 0 & .$chk_sums >= 1, ]

      tmp_df$same_color_dist <- with(tmp_df, ifelse(s_val == (-1 * search_color), dist, 99 ))

      tmp_df$tmp_idx <- with(tmp_df, paste(id, x, y, sep = '_'))

      same_color_vec <- tapply(tmp_df$same_color_dist, tmp_df$tmp_idx, min)
      tmp_df$same_color <- same_color_vec[tmp_df$tmp_idx]

      tmp_df <- tmp_df %>% .[.$dist < .$same_color & .$same_color != 99, ]

      cum_sum_s_val <- tapply(tmp_df$s_val, tmp_df$tmp_idx, sum)
      max_dist <- tapply(tmp_df$dist, tmp_df$tmp_idx, max)

      tmp_df$cum_s_val <- cum_sum_s_val[tmp_df$tmp_idx]

      tmp_df$max_dist <- max_dist[tmp_df$tmp_idx]

      tmp_df <- tmp_df %>% .[.$max_dist == abs(.$cum_s_val), ]
  #})

  # tmp_df <- around_df %>%
  #   dplyr::inner_join(
  #     df %>% dplyr::select(row, col, s_val := val),
  #     by = c('s_row' = 'row', 's_col' = 'col')
  #   ) %>%
  #   dplyr::inner_join(
  #     df %>% dplyr::select(id,  val),
  #     by = c('id')
  #   )  %>% dplyr::filter(val == 0) %>%
  #   dplyr::group_by(id) %>%
  #   dplyr::filter(sum(ifelse(dist == 1 & s_val == search_color, 1, 0)) >= 1) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(id, x, y) %>%
  #   dplyr::mutate(same_color_dist = min(ifelse(s_val == (-1 * search_color), dist, 99 ))) %>%
  #   dplyr::filter(dist < same_color_dist) %>%
  #   dplyr::mutate(cum_s_val = cumsum(s_val)) %>%
  #   dplyr::filter(abs(max(cum_s_val)) == max(dist) )

  return(tmp_df[c('id', 'dist', 'x', 'y', 'row', 'col', 's_row', 's_col', 'val', 's_val')])
}

#' @title  make legal move
#' @name mk_a_move
#' @param df board map
#' @param legal_move_df legal move space
#' @param id id of the legal move
#' @param color color of the legal move
#' @export
mk_a_move <- function(df, legal_move_df, id, color) {

  #microbenchmark({
      df1 <- df %>% .[.$id == id, ]
      df1$val <- color

      df2 <- legal_move_df[legal_move_df$id == id, ]
      df2$s_val <- df2$s_val * - 1
      df2$row <- df2$s_row
      df2$col <- df2$s_col
      df2$val <- df2$s_val
      df2 <- df2[c('row', 'col', 'val')]
      df2$id <- with(df2, df$id[(col - 1) * 8 + row])

      df3 <- df[!df$id %in% c(df1$id, df2$id), ]
      tmp_df <- rbind(df1, df2, df3)
      tmp_df <- tmp_df[order(tmp_df$id), ]
  #})


  # microbenchmark({
  #   tmp_df <- dplyr::bind_rows(
  #     df %>% dplyr::filter(row == !!row, col == !!col) %>%
  #       dplyr::mutate(val = color),
  #     legal_move_df %>% dplyr::filter(row == !!row, col == !!col) %>%
  #       dplyr::mutate(s_val = s_val * -1) %>%
  #       dplyr::select(row := s_row, col := s_col, val := s_val) %>%
  #       dplyr::inner_join(df %>% dplyr::select(id, row, col), by = c('row', 'col'))
  #   )
  #
  #   dplyr::bind_rows(
  #     df %>% dplyr::anti_join(tmp_df, by = 'id'),
  #     tmp_df
  #   )  %>% dplyr::arrange(col, row) %>% dplyr::distinct()
  # })
  return(tmp_df)
}
