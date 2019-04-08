dat_list <- readr::read_rds('temp/checkpoint_0.RData.RData')


train_dat <- 1:length(dat_list) %>%
  purrr::map_df(
    function(x) {
      tmp_dat <- dat_list[[x]]

      mats <- unlist(tmp_dat$mat, F)
      pis <- unlist(tmp_dat$pis, F)
      dat_name <- names(mats)

      dat <- tibble::tibble(
        name = dat_name,
        mats = mats,
        pis = pis,
        Vs = tmp_dat$V %>% purrr::map(~rep(., 8)) %>% unlist()
      ) %>%
      dplyr::mutate(eps = x)

      return(dat)
    }
  )


dups <- train_dat %>% dplyr::group_by(name) %>% dplyr::filter(n() > 1) %>%
  dplyr::arrange(name, eps)


g <- ini_othello(8)

play_ran <- function(g) {
  iter <- 1
  while(getGameEnded(g) == 0) {
    iter <- iter + 1
    if (iter > 100) {
      print(g); saveRDS(g, 'inst/prob_games/g.RData'); break
    }
     mvs <- getValidMove(g)
     if (length(mvs) == 0) {
       mv <- 0
     } else if (length(mvs) == 1) {
       mv <- mvs
     } else {
       mv <- sample(mvs, 1)
     }
     g <- getNextState(g, mv)
  }
  return(getGameEnded(g))
}

library(microbenchmark)
microbenchmark(
  play_ran(g), times = 1000
)

board <- g$df; to_move <- g$player_to_move
board

microbenchmark(
zeros <- search_neighbor(self$board, self$CurrPlayer * -1)
)

g <- ini_othello(8)

play_ran <- function(g) {

  for (i in 1:60) {
    mvs <- getValidMove(g)
    if (length(mvs) == 0) {
      g <- getNextState(g, 0)
    } else {
      if (length(mvs) == 1) mv <- mvs else mv <- sample(mvs, 1)
      g <- getNextState(g, mv)
    }
  }
  return(g)
}

microbenchmark(
  play_ran(g)
)

microbenchmark::microbenchmark(
  getGameEnded(game),
  getValidMove(game)
)
