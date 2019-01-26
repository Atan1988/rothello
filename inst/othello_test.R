devtools::load_all(".")

board <- rothello::generate_othello_base()

play_run <- play_randomly(board, 1)

system.time(
  for (i in 1:20) {
    print(play_run())
  }
)

df <- board
color = 1
system.time(
  for (i in 1:200) {
    print(i)
    legal_move <- df %>% check_legal_move(color)
    if (nrow(legal_move) == 0) return(df %>% tidyr::spread(col, val))

    move_to_make <- legal_move[sample(1:nrow(legal_move), 1), ]
    df <- make_legal_move(df, move_to_make, color)
    color <- -1 * color
    #print(df %>% tidyr::spread(col, val))
  }
)
sum(df$val)
