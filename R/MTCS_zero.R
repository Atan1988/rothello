#' @title MTCSzero class
#' @name MTCSzero
MTCSzero <- R6::R6Class("MTCSzero", list(
  game = NULL,
  nnet = NULL,
  args = NULL,
  Qsa = list(),  # stores Q values for s,a (as defined in the paper)
  Nsa = list(),  # stores #times edge s,a was visited
  Ns = list(),   # stores #times board s was visited
  Ps = list(),   # stores initial policy (returned by neural net)

  Es = list(),   # stores game.getGameEnded ended for board s
  Vs = list(),
  initialize = function(game, nnet, args) {
    self$game <- game
    self$nnet <- nnet
    self$args <- args
  },
  print = function(...) {
    # cat("Person: \n")
    # cat("  Name: ", self$name, "\n", sep = "")
    # cat("  Age:  ", self$age, "\n", sep = "")
    # invisible(self)
  },
  ###get action probablities
  getActionProb = function(canonicalBoard, temp=1) {
        # This function performs numMCTSSims simulations of MCTS starting from
        # canonicalBoard.
        # Returns:
        #     probs: a policy vector where the probability of the ith action is
        #            proportional to Nsa[(s,a)]**(1./temp)

    for (i in 1:self$args$numMCTSSims) {
       self$search(canonicalBoard)
    }
    s <- stringRepresentation(canonicalBoard)

    actions <- 1:getActionSize(self$game)
    counts <- lapply(actions, function(x){
        s_a <- paste(s, x, sep = "_")
        if (s_a %in% names(self$Nsa)) return(self$Nsa[[s_a]]) else return(0)
    }) %>% unlist()

    if (temp==0){
      bestA <- which(counts == max(counts))
      probs <- rep(0, length(counts))
      probs[bestA] <- 1
      return(probs)
    }

    counts <- counts ^ (1/temp)
    probs <- counts / sum(counts)
    return(probs)
  },

  ###search for moves
  search = function(canonicalBoard){
    # This function performs one iteration of MCTS. It is recursively called
    # till a leaf node is found. The action chosen at each node is one that
    # has the maximum upper confidence bound as in the paper.
    # Once a leaf node is found, the neural network is called to return an
    # initial policy P and a value v for the state. This value is propogated
    # up the search path. In case the leaf node is a terminal state, the
    # outcome is propogated up the search path. The values of Ns, Nsa, Qsa are
    # updated.
    # NOTE: the return values are the negative of the value of the current
    # state. This is done since v is in [-1,1] and if v is the value of a
    # state for the current player, then its value is -v for the other player.
    # Returns:
    #   v: the negative of the value of the current canonicalBoard

    s <- stringRepresentation(canonicalBoard)

    if (!s %in% names(self$Es)) {
          self$Es[[s]] <- getGameEnded(canonicalBoard)
    }

    if (self$Es[[s]] != 0) {
            #     # terminal node
            return(-self$Es[[s]])
    }


    if (!s %in% names(self$Ps)) {
      # leaf node
      input_dat <- keras::array_reshape(canonicalBoard$df, dim = c(1, 8, 8, 1))
      c(Ps, v) %<-% self$nnet$model$predict(input_dat)
      self$Ps[[s]] <- Ps
      mvs <-  getValidMove(canonicalBoard)
      valids <- rep(0, getActionSize(canonicalBoard)); valids[mvs] <- 1
      self$Ps[[s]] <-  as.vector(self$Ps[[s]]) * valids      # masking invalid moves
      sum_Ps_s <-  sum(self$Ps[[s]])

      if (sum_Ps_s > 0){
        self$Ps[[s]] <- self$Ps[[s]] / sum_Ps_s    # renormalize
      } else {
        # if all valid moves were masked make all valid moves equally probable

        # NB! All valid moves may be masked if either your NNet architecture is insufficient or you've get overfitting or something else.
        # If you have got dozens or hundreds of these messages you should pay attention to your NNet and/or training process.
        print("All valid moves were masked, do workaround.")
          self$Ps[[s]] <- self$Ps[[s]] + valids
          self$Ps[[s]] <-  self$Ps[[s]] / sum(self$Ps[[s]])
      }

      self$Vs[[s]] <- valids
      self$Ns[[s]] <- 0
      return(-v)
    }

    valids <-  self$Vs[[s]]
    cur_best <-  -Inf
    best_act <-  -1

    # pick the action with the highest upper confidence bound
    for (a in 1:getActionSize(self$game)) {
      s_a <- paste(s, a, sep = "_")
      if (valids[a] == 1){
        if (s_a %in% names(self$Qsa)){
          u <- self$Qsa[[s_a]] + self$args$cpuct * self$Ps[[s]][a] * sqrt(self$Ns[[s]]) / (1+self$Nsa[[s_a]])
        } else {
          u <- self$args$cpuct * self$Ps[[s]][a] * sqrt(self$Ns[[s]] + self$args$EPS)     # Q = 0 ?
        }

        if (u > cur_best) {
          cur_best = u
          best_act = a
        }
      }
    }

    a <-  best_act
    s_a <- paste(s, a, sep = "_")
    print(s); print(a)
    next_s <-  getNextState(canonicalBoard, a) %>% CanonicalForm()

    v <- self$search(next_s)

    if (s_a %in% names(self$Qsa)) {
      self$Qsa[[s_a]] = (self$Nsa[[s_a]] * self$Qsa[[s_a]] + v)/(self$Nsa[[s_a]] + 1)
      self$Nsa[[s_a]] <- self$Nsa[[s_a]] + 1
    } else {
      self$Qsa[[s_a]] <- v
      self$Nsa[[s_a]] <-  1
    }

    self$Ns[[s]] <- self$Ns[[s]] + 1
    return(-v)
  }
))
