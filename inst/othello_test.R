devtools::load_all(".")

board <- rothello::generate_othello_base()

play_run <- play_randomly(board, 1)

play_run1 <- play_randomly1(board, 1)

system.time(
  for (i in 1:60) {
    df <- play_run1()
  }
)
print(df %>% dplyr::select(-id) %>%
        tidyr::spread(col, val))
print(df$val %>% sum())


system.time(
  for (i in 1:60) {
    df <- play_run()
  }
)
print(df %>% tidyr::spread(col, val))
print(df$val %>% sum())


system.time({
  df <- board
  color = 1
  for (i in 1:60) {
    print(i)
    legal_move <- df %>% check_legal_move(color)
    if (nrow(legal_move) == 0) return(df %>% tidyr::spread(col, val))

    move_to_make <- legal_move[sample(1:nrow(legal_move), 1), ]

    df <- make_legal_move(df, move_to_make, color)
    color <- -1 * color
    #print(df %>% tidyr::spread(col, val))
  }
}
)
sum(df$val)

board <- rothello::generate_othello_base_M()

board[1, 1]
