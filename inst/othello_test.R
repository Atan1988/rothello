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

gregor = function(mat) {
  n = nrow(mat)
  mat.pad = rbind(NA, cbind(NA, mat, NA), NA)
  ind = 2:(n + 1) # row/column indices of the "middle"
  neigh = rbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
                NE = as.vector(mat.pad[ind - 1, ind + 1]),
                E  = as.vector(mat.pad[ind    , ind + 1]),
                SE = as.vector(mat.pad[ind + 1, ind + 1]),
                S  = as.vector(mat.pad[ind + 1, ind    ]),
                SW = as.vector(mat.pad[ind + 1, ind - 1]),
                W  = as.vector(mat.pad[ind    , ind - 1]),
                NW = as.vector(mat.pad[ind - 1, ind - 1]))
  return(neigh)
}

signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

Rcpp::cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')



play_ran <- function(sz  = 8) {
  s <- ini_othello(sz)
  for (i in 1:64) {
    moves <- s$moves
    if (length(moves) == 0) break;
    if (length(moves) == 1) mv <- s$moves else mv <- sample(s$moves, 1)
    s <- mk_move(s, move = mv)
  }
  return(s$df)
}

microbenchmark(
  res <- play_ran()
)
