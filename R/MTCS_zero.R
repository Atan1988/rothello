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
init_MTCSzero  <- function(game, nnet, args) {
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
#' @name getActionProb
#' @param MTCSzero MTCSzero object
#' @param canonicalBoard canonical Board position
#' @param args arguments
#' @export
getActionProb <- function(MTCSzero, canonicalBoard, temp=1){
  # This function performs numMCTSSims simulations of MCTS starting from
  # canonicalBoard.
  # Returns:
  #   probs: a policy vector where the probability of the ith action is
  # proportional to Nsa[(s,a)]**(1./temp)
}

#' @title perform one search of MTCS
#' @name search_MTCSzero
#' @param MTCSzero MTCSzero object
#' @param canonicalBoard canonical Board position
#' @export
search_MTCSzero <- function(MTCSzero, canonicalBoard){
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

  s <-  othello_stringRepresentation(canonicalBoard)

  if (!s %in% names(MTCSzero$Es)){
    MTCSzero$Es[[s]] <- othello_getGameEnded(canonicalBoard, 1)
  }
  if (MTCSzero$Es[[s]] !=0) return(-MTCSzero$Es[[s]]) # terminal node

}

