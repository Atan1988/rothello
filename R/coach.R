#' @title coach class
#' @name coach
coach <- R6::R6Class("coach", list(
    # This class executes the self-play + learning. It uses the functions defined
    # in Game and NeuralNet. args are specified in main.py.
  game = NULL,
  nnet = NULL,
  pnet = NULL,
  args = NULL,
  mcts = NULL,
  curPlayer = NULL,
  trainExamplesHistory = NULL,
  skipFirstSelfPlay = NULL,
  initialize = function(game, nnet, pnet, args)  {
    self$game <- game
    self$nnet <- nnet
    self$pnet <- pnet
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

       if (is.na(pi) %>% sum() > 0) {
         print(pi); action <- rep(0, length(action) )
       } else {
         action <- sample(seq(1, length(pi), 1), 1, prob = pi)
       }
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
        prog <- dplyr::progress_estimated(self$args$numEps)
        for (eps in 1:self$args$numEps) {
          self$mcts = MTCSzero$new(self$game, self$nnet, self$args)
          iterationTrainExamples[[eps]] <-  self$executeEpisode()

          # bookkeeping + plot progress
          prog$tick()$print()
        }
        #self$trainExamplesHistory <- append(self$trainExamplesHistory, iterationTrainExamples)
        self$trainExamplesHistory[[length(self$trainExamplesHistory) + 1]] <- iterationTrainExamples
      }

      if (length(self$trainExamplesHistory) > self$args$numItersForTrainExamplesHistory) {
        cat("len(trainExamplesHistory) =", length(self$trainExamplesHistory),
            " => remove the oldest trainExamples", "\n")
        len_sample <- length(self$trainExamplesHistory)
        self$trainExamplesHistory <-
          self$trainExamplesHistory[(len_sample - self$args$numItersForTrainExamplesHistory + 1):len_sample]
        # backup history to a file
        # NB! the examples were collected using the model from the previous iteration, so (i-1)
      }
      self$saveTrainExamples(i-1)

      # shuffle examlpes before training
      trainExamples <- self$trainExamplesHistory[sample(1:length(self$trainExamplesHistory),
                                            length(self$trainExamplesHistory))]

      # training new network, keeping a copy of the old one
      self$nnet$save_checkpoint(folder=self$args$checkpoint, filename=paste0('temp', '.RData'))
      self$pnet$load_checkpoint(folder=self$args$checkpoint, filename= paste0('temp', '.RData'))
      pmcts <- MTCSzero$new(self$game, self$pnet, self$args)


      ## further training the nnet object and apply MTCS search
      #saveRDS(trainExamples, 'data/trainExamples.RData')
      self$nnet$train(trainExamples)
      nmcts <-  MTCSzero$new(self$game, self$nnet, self$args)
      #
      print('PITTING AGAINST PREVIOUS VERSION')
      arena <- Arena$new(function(x) which.max(pmcts$getActionProb(x, temp=0)),
                     function(x) which.max(nmcts$getActionProb(x, temp=0)), self$game,
                     display = print)

      results <- arena$playGames(self$args$arenaCompare, verbose = F)
      pwins <- results[1]; nwins <- results[2]; draws <- results[3]

      cat('New WINS: ', nwins, "; Prev WINS: ", pwins, '; Draws: ', draws, '\n' )
      cat('NEW/PREV WINS : ', round(nwins / (pwins + nwins) * 100, 1), '% ; DRAWS : ', draws , ' \n')
      if ((pwins + nwins == 0) | ((nwins)/(pwins+nwins)) < self$args$updateThreshold) {
        print('REJECTING NEW MODEL')
        self$nnet$load_checkpoint(folder=self$args$checkpoint, filename='temp.RData')
      } else {
        print('ACCEPTING NEW MODEL')
        self$nnet$save_checkpoint(folder = self$args$checkpoint, filename = self$getCheckpointFile(i))
        self$nnet$save_checkpoint(folder = self$args$checkpoint, filename = 'best.RData')
      }
    }
  },
  getCheckpointFile = function(iteration) {
    return(paste0('checkpoint_', iteration, '.RData'))
  },
  saveTrainExamples = function(iteration){
    folder <- self$args$checkpoint
    if (!dir.exists(folder)) dir.create(folder)

    filename <- file.path(folder, paste0(self$getCheckpointFile(iteration), ".RData"))
    saveRDS(self$trainExamplesHistory, filename)
  },
  loadTrainExamples = function() {
    modelFile <-  load_folder_file
    examplesFile = paste0(modelFile, ".examples")
    if (!file.exists(examplesFile)){
      print(examplesFile)

    } else {
      print("File with trainExamples found. Read it.")
      self$trainExamplesHistory <- readr::read_rds(examplesFile)
      # examples based on the model were already collected (loaded)
      self$skipFirstSelfPlay <-  TRUE
    }
  }
 )
)
