#' @title UCT with NNet function
#' @name UCTNN
#' @param MT monte carlo Tree
#' @param rootstate state of the root
#' @param NN neural net model
#' @param itermax max iterations
#' @param verbose whether to print results
#' @export
UCTNN <- function(MT = NULL, rootstate, NN, itermax, verbose = FALSE){

}

#' @title initiate MTCS zero
#' @name init_MTCSzero
#' @param game game object, for example result of ini_othello
#' @param nnet prediction neural network
#' @param args arguments, numMCTSSims number of MCTS simulations
#' @export
init_MTCSzero  <- function(game, nnet, args = list(EPS = 1e-8, numMCTSSims = 50)) {
  obj <- list(
   game = game,
   nnet = nnet,
   args = args,
   Qsa = list(),  # stores Q values for s,a (as defined in the paper)
   Nsa = list(),  # stores #times edge s,a was visited
   Ns = list(),   # stores #times board s was visited
   Ps = list(),   # stores initial policy (returned by neural net)

   Es = list(),   # stores game.getGameEnded ended for board s
   Vs = list()    # stores game.getValidMoves for board s
  )

  structure(
    obj
    , class = 'MTCS_zero'
  )
}


#' @title get action probability of MTCSzero
#' @name getActionProb.MTCSzero
#' @param MTCSzero MTCSzero object
#' @param canonicalBoard canonical Board position
#' @export
getActionProb.MTCSzero <- function(MTCSzero, canonicalBoard, temp=1) {
        # This function performs numMCTSSims simulations of MCTS starting from
        # canonicalBoard.
        # Returns:
        #     probs: a policy vector where the probability of the ith action is
        #            proportional to Nsa[(s,a)]**(1./temp)

  for (i in 1:MTCSzero$args$numMCTSSims) {
    MTCSsearch(MTCSzero, canonicalBoard)
  }

  s <-  stringRepresentation(canonicalBoard)
  actions <- 1:(getActionSize(MTCSzero$game))
  counts <-  sapply(actions, function(x) {
    s_a <- paste(s, x, sep = "_")
    if (s_a %in% names(MTCSzero$Nsa)) return(MTCSzero$Nsa[[s_a]]) else 0
  })

  if (temp==0) {
    bestA <-  which(counts == max(counts))
    probs <- rep(0, length(counts))
    probs[bestA]  <- 1
    return(probs)
  }

  counts <- counts ^ (1/temp)
  probs <- counts / sum(counts)
  return(probs)
}

#' @title perform one search of MTCS
#' @name MTCSsearch.MTCSzero
#' @param MTCSzero MTCSzero object
#' @param canonicalBoard canonical Board position
#' @export
MTCSsearch.MTCSzero <- function(MTCSzero, canonicalBoard){
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
        #     v: the negative of the value of the current canonicalBoard

  s <-  stringRepresentation(canonicalBoard)

  if (!s %in% names(MTCSzero$Es)){
    MTCSzero$Es[[s]] <<- getGameEnded(canonicalBoard)
  }
  if (MTCSzero$Es[[s]] !=0) return(-MTCSzero$Es[[s]]) # terminal node

  if (!s %in% names(MTCSzero$Ps)){
    # leaf node
    MTCSzero$Ps[[s]] <<- predict(nnet, canonicalBoard$df)[[1]]
    v <-  predict(nnet, canonicalBoard$df)[[2]]
    valids <- getValidMoves(canonicalBoard)
    MTCSzero$Ps[[s]] <<- MTCSzero$Ps[[s]] * valids      # masking invalid moves
    sum_Ps_s = sum(MTCSzero$Ps[[s]])
    if (sum_Ps_s > 0){
      MTCSzero$Ps[[s]] <<-  MTCSzero$Ps[[s]] / sum_Ps_s    # renormalize
    } else {
      # if all valid moves were masked make all valid moves equally probable

      # NB! All valid moves may be masked if either your NNet architecture is insufficient or you've get overfitting or something else.
      # If you have got dozens or hundreds of these messages you should pay attention to your NNet and/or training process.
      print("All valid moves were masked, do workaround.")
      MTCSzero$Ps[[s]] <<-  MTCSzero$Ps[[s]] + valids
      MTCSzero$Ps[[s]] <<-  MTCSzero$Ps[[s]] / sum(MTCSzero$Ps[[s]])
    }

    MTCSzero$Vs[[s]] <<-  valids
    MTCSzero$Ns[[s]] <<-  0
    return(-v)
  }

  valids <- MTCSzero$Vs[[s]]
  cur_best <- -Inf
  best_act <-  -1

  # pick the action with the highest upper confidence bound
  for (a in 1:getActionSize(MTCSzero$game)){
    if (valids[a]) {
      s_a <- paste0(s, "_", a)
      if (s_a %in% names(MTCSzero$Qsa)) {
        u <- MTCSzero$Qsa[[s_a]] +
              MTCSzero$args$cpuct * MTCSzero$Ps[[s]][a] * sqrt(MTCSzero$Ns[[s]])/(1 + MTCSzero$Nsa[[s_a]])
      } else {
        u <- MTCSzero$args$cpuct * MTCSzero$Ps[[s]][a] * sqrt(MTCSzero$Ns[[s]] + MTCSzero$args$EPS) # Q = 0 ?

        if (u > cur_best) {
          cur_best <-  u
          best_act <-  a
        }
      }
    }
  }


  a <- best_act
  next_s <- getNextState(game, a)
  #next_s, next_player = self.game.getNextState(canonicalBoard, 1, a)
  #next_s = self.game.getCanonicalForm(next_s, next_player)

  v <- MTCSsearch(MTCSzero, next_s)

  if (s_a %in% names(MTCSzero$Qsa)){
    MTCSzero$Qsa[[s_a]] <<- (MTCSzero$Nsa[[s_a]] * MTCSzero$Qsa[[s_a]] + v)/( MTCSzero$Qsa[[s_a]] +1)
    MTCSzero$Nsa[[s_a]] <<- MTCSzero$Nsa[[s_a]] + 1
  } else{
    MTCSzero$Qsa[[s_a]] <<-  v
    MTCSzero$Nsa[[s_a]] <<-  1
  }

  MTCSzero$Ns[[s]] <<- MTCSzero$Ns[[s]] + 1
  return(-v)
}

