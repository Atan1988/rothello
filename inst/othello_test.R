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

test <- function() {
  play_run1 <- play_randomly1(board, 1)

    for (i in 1:60) {
      df <- play_run1()
    }

  return(sum(df$val))
}

system.time(
  sim_res <- 1:1000 %>% purrr::map_dbl(
    ~test()
  )
)

hist(sim_res)
mean(sim_res)
summary(sim_res)

game <- ini_othello(8)
game %>% mk_move(.$moves[1])
