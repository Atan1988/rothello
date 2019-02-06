#' @title node class definition
#' @name init_node
#' @param MT the tree to initiate node into
#' @param move the move to make
#' @param parent id of parent
#' @param state game state
#' @export
ini_node <- function(MT = NULL, move = NA, parent = NA, parent_move = NA, state = NULL) {
  #structure(
  node <-  tibble::tibble(
      Id = state$df$val %>% ifelse(. == -1, 2, .) %>% tertodec()
      , move = move # the move that got us to this node - NULL for the root node
      , parentNode = parent # NULL for the root node
      , parentmove = parent_move
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
#' @param MT mtcs tree
#' @param node_df a mtcs node
#' @param UCTK constant to control exploration vs exploitation
#' @export
UCTSelectChild <- function(MT, node_df, UCTK = 1) {
  MT %>% dplyr::inner_join(
       node_df %>% dplyr::select(parentNode := Id), by = "parentNode"
    ) %>%
    dplyr::mutate(
      sort_val = wins/visits + sqrt(2*log(node$visits)/visits)
    ) %>%
    dplyr::arrange(-sort_val) %>% dplyr::select(Id, move) %>% .[1, ]
}

#' @title UCT function
#' @name UCT
#' @param rootstate state of the root
#' @param itermax max iterations
#' @param verbose whether to print results
#' @export
UCT <- function(rootstate, itermax, verbose = False){
  MT <- ini_node(state = rootstate)
  rootnode_Id <- MT %>% dplyr::filter(is.na(parentNode)) %>% dplyr::pull(Id)
  rootparent_Id <- NA

  system.time(
    {
      itermax = 4
      for (i in 1:itermax) {
        print(i)
        node_df <- tibble::tibble(
          Id = rootnode_Id,
          move = NA,
        )
        state <- rootstate

        node <- MT %>% dplyr::inner_join(node_df, by = c("Id", "move"))
        #Select
        while (length(node$untriedMoves[[1]]) == 0 & length(node$childNodes[[1]]) > 0 ) {
          print(node$Id)
          node_df <- node_df %>% UCTSelectChild(MT, .)
          node <- MT %>% dplyr::inner_join(node_df, by = c('Id', 'move'))
          state <- mk_move(s = state, move = node$move)
        }

        if (nrow(node_df) > 1) break;
        # Expand
        if (length(node$untriedMoves[[1]]) > 0) { # if we can expand (i.e. state/node is non-terminal)
          if (length(node$untriedMoves[[1]]) == 1) m <- node$untriedMoves[[1]] else {
            m <- sample(node$untriedMoves[[1]], 1)

          }
          print(m)
          state <- mk_move(s = state, move = m)
          node <- AddChild(node, m = m, s = state) # add child and descend tree
          child_ID <- tertodec(s$df$val %>% ifelse(. == -1, 2, .))
          if (!child_ID %in% MT$Id) MT <- ini_node(MT, move = m, parent = node$Id,
                                                   parent_move = node$move, state = state)
          MT <- MT %>% dplyr::anti_join(node_df, by = c("Id", "move"))
          MT <- dplyr::bind_rows(MT, node)
        }

        # Rollout - this can often be made orders of magnitude quicker using a state.GetRandomMove() function
        while (length(state$moves) > 0 ) {
          state <- mk_move(s = state, move = sample(state$moves, 1))
        }

        # Backpropagate
        while (nrow(node_df) == 1) {
          node <- MT %>% dplyr::inner_join(node_df, by = c("Id", "move"))
          result <- get_results(state, node$playerJustMoved)
          node <- node_update(node, res = result)
          MT <- dplyr::bind_rows(
            MT %>% dplyr::anti_join(node_df, by = c("Id", "move")),
            node
          )
          node_df <- MT %>% dplyr::select(Id, move) %>%
            dplyr::inner_join(node %>%
                                dplyr::select(Id := parentNode, move := parentmove),
                              by = c("Id", "move"))
        }

        # while node != None: # backpropagate from the expanded node and work back to the root node
        #   node.Update(state.GetResult(node.playerJustMoved)) # state is terminal. Update node with result from POV of node.playerJustMoved
        # node = node.parentNode
      }
    }
  )

  MT
}


#' @title base3 to base 10
#' @name tertodec
#' @param val game vals
#' @export
tertodec <- function(val) {
  val1 <- rev(val)
  (val1 * 3 ^ (seq_len(length(val1)) - 1)) %>% sum()
}


toBoard <- function(state,some.board){
  # some.board is just used for dimensions
  id <- convertToTrinary(state,some.board)
  id[which(id==2)] <- NA
  board <- matrix(data=id,nrow=nrow(some.board),ncol=ncol(some.board))
  return(board)
}

#' @title base10 to base 3
#' @name dectoter
#' @param number game Id
#' @param sz board size
#' @export
dectoter <- function(number, sz = 8) {
  n <- sz ^ 2
  num <- c()
  temp <- number
  for (ii in 1:n){
    num[ii] <- floor(temp / 3^(n-ii))
    temp <- temp %% 3^(n-ii)
  }
  return(num)
}

#' @title base10 Id to Board
#' @name to_board
#' @param number game Id
#' @param sz board size
#' @export
to_board <- function(number, sz) {
 vals <- dectoter(number, sz)

 board <- generate_othello_base(sz)
 board$val <- vals

 board %>% dplyr::select(-id) %>% tidyr::spread(col, val)
}
