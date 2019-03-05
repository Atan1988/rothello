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
  }

)
)
