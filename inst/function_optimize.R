library(lineprof)
f <- function() {
  pause(0.1)
  g()
  h()
}
g <- function() {
  pause(0.1)
  h()
}
h <- function() {
  pause(0.1)
}

l <- lineprof(f())
l

l <- lineprof(check_legal_move(df = board, color = 1))
l

l <- lineprof(chk_eight_ds(df = board, row = 4, col = 5))
l

l <- lineprof(chk_one_direction(df, row = 4, col = 5,
                                piece_color = -1, search_color = 1, x = -1, y =0, place = T))
l

l <- lineprof(chk_0_sp1(df, search_color = -1))
l

library(microbenchmark)

microbenchmark::microbenchmark(
  1:nrow(direction_df) %>% purrr::map_df(
    ~chk_valid_move_1d(df, row, col, direction_df$x[.], direction_df$y[.])),
    1:nrow(direction_df) %>% lapply(
          function(x) {chk_valid_move_1d(df, row, col, direction_df$x[x], direction_df$y[x])}
      ) %>% dplyr::bind_rows()
)


microbenchmark(
  chk_one_direction(df, row = 4, col = 5, piece_color = -1, search_color = 1, x =-1, y = 0, place = T),
  chk_one_direction(df, row = 4, col = 5, piece_color = -1, search_color = 0, x =1, y = 0, place = T),
  times = 1000
)

microbenchmark(
  {
    df <- board
    color = 1
    for (i in 1:60) {
      legal_move <- df %>% check_legal_move(color)
      if (nrow(legal_move) == 0) return(df %>% tidyr::spread(col, val))

      move_to_make <- legal_move[sample(1:nrow(legal_move), 1), ]
      df <- make_legal_move(df, move_to_make, color)
      color <- -1 * color
      #print(df %>% tidyr::spread(col, val))
    }
  }

)

color <- 1
df <- rothello::generate_othello_base()
test <- test_func <- function() {

  for (i in 1:60) {
    legal_move_df <- chk_0_sp1(df, search_color = color * -1);
    if (nrow(legal_move_df) > 1) {
      move_to_gos <- legal_move_df %>% dplyr::select(id, row, col) %>% dplyr::distinct()
      next_move <- move_to_gos[sample(1:nrow(move_to_gos), 1), ]
      df <- mk_a_move(df, legal_move_df, row = next_move$row, col = next_move$col, color = color)
    }
    color <- color * -1
    #print(df %>% dplyr::select(-id) %>% tidyr::spread(col, val))
  }
  return(df$val %>% sum())
}

system.time(
  sim_res <- 1:1000 %>% purrr::map_dbl(
    ~test()
  )
)

hist(sim_res)



color <- 1
df <- rothello::generate_othello_base()
system.time({
  for (i in 1:60) {
    legal_move_df <- chk_0_sp1(df, search_color = color * -1);
    if (nrow(legal_move_df) > 1) {
      move_to_gos <- legal_move_df %>% dplyr::select(id, row, col) %>% dplyr::distinct()
      next_move <- move_to_gos[sample(1:nrow(move_to_gos), 1), ]
      df <- mk_a_move(df, legal_move_df, row = next_move$row, col = next_move$col, color = color)
    }
    color <- color * -1
    #print(df %>% dplyr::select(-id) %>% tidyr::spread(col, val))
  }
})

print(df %>% dplyr::select(-id) %>% tidyr::spread(col, val))
print(df$val %>% sum())
