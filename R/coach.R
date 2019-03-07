#' @title coach class
#' @name coach
coach <- R6::R6Class("coach", list(
    # This class executes the self-play + learning. It uses the functions defined
    # in Game and NeuralNet. args are specified in main.py.
  game = NULL,
  nnet = NULL,
  args = NULL,
  mcts = NULL,
  curPlayer = NULL,
  trainExamplesHistory = NULL,
  skipFirstSelfPlay = NULL,
  initialize = function(game, nnet, args)  {
    self$game <- game
    self$nnet <- nnet
    self$pnet <- self.nnet.__class__(self.game)
    #self$pnet <-  # the competitor network
    self$args <- args
    self$mcts <- MTCSzero$new(self$game, self$nnet, self$args)
    # history of examples from args.numItersForTrainExamplesHistory latest iterations
    self$trainExamplesHistory <-  list()
    # can be overriden in loadTrainExamples()
    self$skipFirstSelfPlay <- FALSE
  },
  executeEpisode = function(){
        # This function executes one episode of self-play, starting with player 1.
        # As the game is played, each turn is added as a training example to
        # trainExamples. The game is played till the game ends. After the game
        # ends, the outcome of the game is used to assign values to each example
        # in trainExamples.
        # It uses a temp=1 if episodeStep < tempThreshold, and thereafter
        # uses temp=0.
        # Returns:
        #     trainExamples: a list of examples of the form (canonicalBoard,pi,v)
        #                    pi is the MCTS informed policy vector, v is +1 if
        #                    the player eventually won the game, else -1.

    trainExamples <- list(mat = list(), pis = list(), curPlayer = list())
    board <-  self$game
    self$curPlayer <- 1
    episodeStep <- 0

    r <- 0
    while (r == 0){
       episodeStep <- episodeStep + 1
       canonicalBoard <- CanonicalForm(board)
       temp <- as.integer(episodeStep < self$args$tempThreshold)

       pi <- self$mcts$getActionProb(canonicalBoard, temp)
       sym <- getSymmetries(canonicalBoard, pi)

       sym$curPlayer <- self$curPlayer
       trainExamples$mat[[length(trainExamples$mat) + 1]] <- sym$mats
       trainExamples$pis[[length(trainExamples$pis) + 1]] <- sym$pis
       trainExamples$curPlayer[[length(trainExamples$curPlayer) + 1]] <- sym$curPlayer

       action <- sample(seq(1, length(pi), 1), 1, prob = pi)
       board <- getNextState(board, action)
       self$curPlayer <- board$player_to_move

       r <-  getGameEnded(board)
    }

    trainExamples$curPlayer <- unlist(trainExamples$curPlayer)
    trainExamples$V <- r * ((-1) ^ (trainExamples$curPlayer != self$curPlayer))

    trainExamples$curPlayer <- NULL
    return(trainExamples)
  },
  learn = function() {
        # Performs numIters iterations with numEps episodes of self-play in each
        # iteration. After every iteration, it retrains neural network with
        # examples in trainExamples (which has a maximium length of maxlenofQueue).
        # It then pits the new neural network against the old one and accepts it
        # only if it wins >= updateThreshold fraction of games.

    for (i in 1:(self$args$numIters + 1)) {
      # bookkeeping
      print(paste0('------ITER ', i, '------'))
      # examples of the iteration
      if (!(self$skipFirstSelfPlay) | i > i){
        iterationTrainExamples <- list()
        for (eps in 1:self$args$numEps) {
          self$mcts = MTCSzero$new(self$game, self$nnet, self$args)
          iterationTrainExamples[[eps]] <-  self$executeEpisode()

          # bookkeeping + plot progress


        }
        self$trainExamplesHistory <- append(self$trainExamplesHistory, iterationTrainExamples)
      }

      if (length(self$trainExamplesHistory) > self$args$numItersForTrainExamplesHistory) {
        cat("len(trainExamplesHistory) =", len(self$trainExamplesHistory),
            " => remove the oldest trainExamples", "\n")
        self$trainExamplesHistory <- self$trainExamplesHistory[-1]
        # backup history to a file
        # NB! the examples were collected using the model from the previous iteration, so (i-1)
      }
      self$saveTrainExamples(i-1)

      # shuffle examlpes before training
      trainExamples <- trainExamples[sample(1:length(self$trainExamplesHistory),
                                            length(self$trainExamplesHistory))]

      # training new network, keeping a copy of the old one
      self$nnet$save_checkpoint(folder=sell$args$checkpoint, filename='temp.pth.tar')
      self$pnet$load_checkpoint(folder=self$args$checkpoint, filename='temp.pth.tar')
      pmcts <- MTCSzero$new(self$game, self$pnet, self$args)
      #
      self$nnet$train(trainExamples)
      nmcts <-  MCTS(self$game, self$nnet, self$args)
      #
      print('PITTING AGAINST PREVIOUS VERSION')
    }

    # if len(self.trainExamplesHistory) > self.args.numItersForTrainExamplesHistory:
    #   print("len(trainExamplesHistory) =", len(self.trainExamplesHistory), " => remove the oldest trainExamples")
    # self.trainExamplesHistory.pop(0)
    # # backup history to a file
    # # NB! the examples were collected using the model from the previous iteration, so (i-1)
    # self.saveTrainExamples(i-1)
  }
 )
)
