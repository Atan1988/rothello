game1 <- ini_othello(8)

stringRepresentation(game1)

getGameEnded(game1)

getValidMove(game1)

CanonicalForm(game1)

mvs <- getValidMove(game1)

next_state <- getNextState(game1, 21)

microbenchmark::microbenchmark(
  mvs <- getValidMove(game1),
  next_state <- getNextState(game1, 21)
)

s <- ini_othello(8)
play_random <- function(s) {
  for (i in 1:60) {
    mvs <- getValidMove(s)
    if (length(mvs) == 0) {
      s <- getNextState(s, 0)
    } else {
      if (length(mvs) == 1) mv <- mv[1] else mv <- sample(mvs, 1)
      s <-  getNextState(s, mv)
    }
    #print(s$df)
  }
  return(s)
}

microbenchmark::microbenchmark(
  play_random(s)
)
