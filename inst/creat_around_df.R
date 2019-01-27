df <- rothello::generate_othello_base()

around_df <- purrr::cross_df(
  list(id = df$id,
       dist = seq(1, 8, 1),
       x = c(-1, 0, 1),
       y = c(-1, 0, 1)
  )
) %>% dplyr::filter(x != 0 | y != 0) %>%
  dplyr::inner_join(
    df %>% dplyr::select(id, row, col)
  ) %>%
  dplyr::mutate(
    s_row = row + y * dist,
    s_col = col + x * dist
  ) %>%
  dplyr::inner_join(
    df %>% select(row, col),
    by = c('s_row' = 'row', 's_col' = 'col')
  ) %>%
  dplyr::arrange(id, x, y, dist)
