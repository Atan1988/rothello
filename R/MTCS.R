#' @title node class definition
#' @name init_node
#' @param MT the tree to initiate node into
#' @param move the move to make
#' @param parent id of parent
#' @param state game state
#' @export
ini_node <- function(MT = NULL, move = NA, parent = NA, state = NULL) {
  #structure(
  node <-  tibble::tibble(
      Id = state$df$val %>% ifelse(. == -1, 2, .) %>% tertodec()
      , move = move # the move that got us to this node - NULL for the root node
      , parentNode = parent # NULL for the root node
      , childNodes = list(c(NULL))
      , wins = 0
      , visits = 0
      , untriedMoves = list(state$moves) # future child nodes
      , playerJustMoved = state$player_to_move * -1# the only part of the state that the Node needs later
    )

   if (is.null(MT)) return(node) else return(dplyr::bind_rows(MT, node))
    #, class = "mtcs_node"
  #)
}

#' @title add child node
#' @name AddChild
#' @param node the current node
#' @param m move
#' @param s game state
#' @export
AddChild <- function(node, m, s) {
  n <-  ini_node(move = m, parent = node$Id, state = s)
  node$untriedMoves[[1]] <- node$untriedMoves[[1]] %>% .[!. %in% m]
  #node$childNodes <- append(node$childNodes, list(n))
  node$childNodes <- list(c(node$childNodes[[1]], s$df$val %>% tertodec()))
  return(node)
}

#' @title update node value
#' @name node_update
#' @param node the current node
#' @param res result of game
#' @export
node_update <- function(node, res) {
  node$wins <- node$wins + res
  node$visits <- node$visits + 1
  return(node)
}


#' @title select child note
#' @name UCTSelectChild
#' @param node a mtcs node
#' @param UCTK constant to control exploration vs exploitation
#' @export
UCTSelectChild <- function(node, UCTK = 1) {
  node$childNodes %>% purrr::map_df(
    function(x) {
       tibble::tibble(
         ID = x$ID, wins = x$wins, visits = x$visits
       )
    }) %>%
    dplyr::mutate(
      sort_val = wins/visits + sqrt(2*log(node$visits)/visits)
    ) %>%
    dplyr::arrange(-sort_val) %>% dplyr::pull(id) %>% .[1]
}

#' @title UCT function
#' @name UCT
#' @param rootstate state of the root
#' @param itermax max iterations
#' @param verbose whether to print results
#' @export
UCT <- function(rootstate, itermax, verbose = False){
  MT <- ini_node(state = rootstate)
  rootnode <- MT %>% dplyr::filter(is.na(parentNode))

  for (i in 1:itermax) {
     node <- rootnode
     state <- rootstate

     # Select
     while (length(node$untriedMoves) == 0 & length(node$childNodes) > 0 ) {
        node <- node %>% UCTSelectChild()
        state <- mk_move(s = state, move = node$move)
     }

     # Expand
     if (length(node$untriedMoves) > 0) { # if we can expand (i.e. state/node is non-terminal)
       m <- sample(node$untriedMoves[[1]], 1)
       state <- mk_move(s = state, move = m)
       node <- AddChild(node, m = m, s = state) # add child and descend tree
       child_ID <- tertodec(s$df$val)
       if (!child_ID %in% MT$Id) MT <- ini_node(MT, move = m, parent = node$Id, state = s)
       MT <- MT %>% dplyr::filter(Id != node$Id)
       MT <- dplyr::bind_rows(MT, node)
     }

     # Rollout - this can often be made orders of magnitude quicker using a state.GetRandomMove() function
     while (length(state$moves) > 0 ) {
        state <- mk_move(s = state, move = sample(state$moves, 1))
     }

     # Backpropagate
     while (!is.na(node$Id)) {
        result <- get_res(state, node$playerJustMoved)
     }

     # while node != None: # backpropagate from the expanded node and work back to the root node
     #   node.Update(state.GetResult(node.playerJustMoved)) # state is terminal. Update node with result from POV of node.playerJustMoved
     # node = node.parentNode
  }
}


#' @title base3 to base 10
#' @name tertodec
#' @param val game vals
#' @export
tertodec <- function(val) {
  val1 <- rev(val)
  (val1 * 3 ^ (seq_len(length(val1)) - 1)) %>% sum()
}
