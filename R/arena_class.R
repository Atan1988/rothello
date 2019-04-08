#' @title arena class
#' @name Arena
#' @description An Arena class where any 2 agents can be pit against each other.
Arena <- R6::R6Class("Arena", list(
 player1 = NULL,
 player2 = NULL,
 game = NULL,
 display = NULL,
 initialize = function(player1, player2, game, display) {
   # Input:
   #   player 1,2: two functions that takes board as input, return action
   # game: Game object
   # display: a function that takes board as input and prints it (e.g.
   #                display in othello/OthelloGame). Is necessary for verbose
   # mode.
   # see othello/OthelloPlayers.py for an example. See pit.py for pitting
   # human players/other baselines with each other.
   self$player1 = player1
   self$player2 = player2
   self$game = game
   self$display = display
 },
 playGame = function(verbose=F){
   # Executes one episode of a game.
   # Returns:
   #   either
   # winner: player who won the game (1 if player1, -1 if player2)
   # or
   # draw result returned from the game that is neither 1, -1, nor 0.
   #players = [self.player2, None, self.player1]
   players <- list(self$player1, self$player2)

   #games <- list()
   prog_bar <- dplyr::progress_estimated(100)
   #for (i in 1:100){
      curPlayer <-  1
      self$game <-  getInitBoard(self$game)
      it <-  0
      while (getGameEnded(self$game)==0) {
         it <- it + 1
         if (verbose) {
            #assert(self.display)
            cat("Turn ", it, "Player ", self$game$player_to_move, '\n')
            self$display(self$game$df)
         }

         if (curPlayer == -1) {
            action <- self$player2(CanonicalForm(self$game))
         } else {
            action <- self$player1(CanonicalForm(self$game))
         }
         if (is.na(action)) print(self$game)

         valids <- getValidMove(CanonicalForm(self$game))
         # print(valids)
         # print(action)

         if (!action %in% valids) {
            #print(action)
            self$game <- getNextState(self$game, 0)
         }

         if(action %in% valids) {
            self$game <- getNextState(self$game, action)
         }
      }
      if (verbose){
         cat("Game over: Turn ", it, "Result ", getGameEnded(self$game), '\n')
         self$display(self$game$df)
      }
      #games[[length(games) + 1]] <- self$game
      #prog_bar$tick()$print()
   #}

   return(getGameEnded(self$game))
 },
 playGames = function(num, verbose=F) {
   # Plays num games in which player1 starts num/2 games and player2 starts
   # num/2 games.
   # Returns:
   #   oneWon: games won by player1
   # twoWon: games won by player2
   # draws:  games won by nobody
   eps_time <- 0 #AverageMeter()
   eps_time_ct <- 0
   # bar = Bar('Arena.playGames', max=num)
   end <-  Sys.time()
   eps <-  0
   maxeps <-  as.integer(num)

   num <- as.integer(num/2)
   oneWon <- 0
   twoWon <- 0
   draws <- 0

   progbar <- dplyr::progress_estimated(2 * num)
   for (i in (1:num)){
     gameResult <- self$playGame(verbose=verbose)
     if (gameResult ==1) {
       oneWon <- oneWon + 1
     } else if (gameResult == -1) {
       twoWon <- twoWon + 1
     } else {
       draws <- draws + 1
     }
     # bookkeeping + plot progress
     progbar$tick()$print()
   }

   players <- list(self$player1, self$player2)
   self$player1 <- players[[2]]
   self$player2 <- players[[1]]


   for (i in (1:num)){
     gameResult <- self$playGame(verbose=verbose)
     if (gameResult == -1) {
       oneWon <- oneWon + 1
     } else if (gameResult == 1) {
       twoWon <- twoWon + 1
     } else {
       draws <- draws + 1
     }
     # bookkeeping + plot progress
     progbar$tick()$print()
   }

   return(c(oneWon = oneWon, twoWon = twoWon, draws = draws))
 }
 )
)


