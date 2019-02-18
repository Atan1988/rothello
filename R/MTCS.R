#' @title node class definition
#' @name init_node
#' @param MT the tree to initiate node into
#' @param move the move to make
#' @param parent id of parent
#' @param state game state
#' @export
ini_node <- function(MT, move = 0, parent = 0, parent_move = 0, state = NULL) {
  #structure(
  Id <- tertodec(ifelse(state$df == -1, 2, state$df))

  MTCS_value <- data.frame(
      Id = Id, move = move, parentId = parent, parentmove = parent_move, wins = 0, visits = 0
  )

  ###newly initated node shouldn't have child nodes
  if (parent_move != 0 | parent != 0) {
    parent_child <- data.frame(
      Id = parent, move = parent_move, childId = !!Id, childmove = !!move
    )
  } else {
    parent_child <- data.frame(
      Id = parent, move = parent_move, childId = Id, childmove = move
    )[0, ]
  }

  if (length(state$moves) > 0) {
    untried_moves <- data.frame(
      Id = Id, move = move, untried_move = state$moves
    )
  } else {
    untried_moves <- data.frame(
      Id = Id, move = move, untried_move = NA
    )[0, ]
  }

  if (DBI::dbExistsTable(MT, 'MTCS_value')) {
    node_exists <- (MT %>% dplyr::tbl('MTCS_value') %>%
                      dplyr::filter(Id == !!Id,  move == !!move) %>%
                      dplyr::tally() %>% dplyr::pull(n)) > 0
  } else {
    node_exists <- FALSE
  }

  if (!node_exists) {
    DBI::dbWriteTable(MT, 'MTCS_value', MTCS_value, append = TRUE)
    DBI::dbWriteTable(MT, 'parent_child', parent_child, append = TRUE)
    DBI::dbWriteTable(MT, 'untried_moves', untried_moves, append = TRUE)
    #DBI::dbWriteTable(MT, 'state_df', state_df, append = TRUE)
  }
}


#' @title node class definition
#' @name init_node1
#' @param MT the tree to initiate node into
#' @param move the move to make
#' @param parent id of parent
#' @param state game state
#' @export
ini_node1 <- function(MT = NULL, move = 0, parent = 0, parent_move = 0, state = NULL) {
  #structure(
  Id <- tertodec(ifelse(state$df == -1, 2, state$df))

  MTCS_value <- data.frame(
    Id = Id, move = move, parentId = parent, parentmove = parent_move, wins = 0, visits = 0
  )

  ###newly initated node shouldn't have child nodes
  if (parent_move != 0 | parent != 0) {
    parent_child <- data.frame(
      Id = parent, move = parent_move, childId = Id, childmove = move
    )
  } else {
    parent_child <- data.frame(
      Id = parent, move = parent_move, childId = Id, childmove = move
    )[0, ]
  }

  if (length(state$moves) > 0) {
    untried_moves <- data.frame(
      Id = Id, move = move, untried_move = state$moves
    )
  } else {
    untried_moves <- data.frame(
      Id = Id, move = move, untried_move = NA
    )[0, ]
  }

  if (!is.null(MT$MTCS_value)) {
    node_exists <- (MT$MTCS_value %>%
                      dplyr::filter(Id == !!Id,  move == !!move) %>%
                      nrow()) > 0
  } else {
    node_exists <- FALSE
  }

  if (!node_exists) {
    MTCS_value1 <- rbind(MT$MTCS_value, MTCS_value);
    parent_child1 <- rbind(MT$parent_child, parent_child)
    untried_moves1 <- rbind(MT$untried_moves, untried_moves)
  }

  list(
    MTCS_value = MTCS_value1,
    parent_child = parent_child1,
    untried_moves = untried_moves1
  )
}

#' @title node class definition
#' @name init_node
#' @param MTCS_value MTCS_value table
#' @param Id Id of the state
#' @param move the move to make
#' @export
node_exist <- function(MTCS_value, Id, move) {
  (MTCS_value %>%
     dplyr::filter(Id == !!Id,  move == !!move) %>%
     dplyr::tally() %>% dplyr::pull(n)) > 0
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
  node$childNodes <- list(c(node$childNodes[[1]], s$df %>% tertodec()))
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
#' @param MTCS_value MTCS_Value table
#' @param node_Id node Id
#' @param nodemove node move
#' @param UCTK constant to control exploration vs exploitation
#' @export
UCTSelectChild <- function(MTCS_value, node_Id, nodemove, UCTK = 1) {
  node_visits <-  MTCS_value %>% dplyr::filter(
    Id == !!node_Id, move == !!nodemove
  ) %>% dplyr::pull(visits)

  MTCS_value %>% dplyr::filter(
       parentId == !!node_Id, parentmove == !!nodemove
    ) %>%
    dplyr::mutate(
      sort_val = wins/visits + sqrt(2*log(node_visits)/visits)
    ) %>%
    dplyr::arrange(-sort_val)  %>% dplyr::collect() %>% .[1, ]
}

#' @title select child note
#' @name UCTSelectChild1
#' @param MTCS_value MTCS_Value table
#' @param node_Id node Id
#' @param nodemove node move
#' @param UCTK constant to control exploration vs exploitation
#' @export
UCTSelectChild1 <- function(MTCS_value, node_Id, nodemove, UCTK = 1) {
  node_visits <-  MTCS_value %>% dplyr::filter(
    Id == !!node_Id, move == !!nodemove
  ) %>% dplyr::pull(visits)

  MTCS_value %>% dplyr::filter(
    parentId == !!node_Id, parentmove == !!nodemove
  ) %>%
    dplyr::mutate(
      sort_val = ifelse(visits == 0, 1e7, wins/visits + sqrt(2*log(node_visits)/visits))
    ) %>%
    dplyr::arrange(desc(sort_val)) %>% .[1, ]
}

#' @title UCT function
#' @name UCT
#' @param rootstate state of the root
#' @param itermax max iterations
#' @param verbose whether to print results
#' @export
UCT <- function(rootstate, itermax, verbose = FALSE){
  #MT <- ini_node(state = rootstate)
    options("scipen"=100)
    MT <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    ini_node(MT, state = rootstate)
    MTCS_value <- MT %>% dplyr::tbl('MTCS_value')
    parent_child <- MT %>% dplyr::tbl('parent_child')
    untried_moves <- MT %>% dplyr::tbl('untried_moves')
    #state_df <- MT %>% dplyr::tbl("state_df")
    rootnode_Id <- rootstate$df %>% ifelse(. == -1, 2, .) %>% tertodec()
    rootmove <- 0

  #system.time({
      prog_bar <- dplyr::progress_estimated(itermax)
      for (i in 1:itermax) {
        #print(i)

        node_Id <- rootnode_Id
        nodemove <- rootmove
        state <- rootstate
        node_playerJustMoved <- rootstate$player_to_move * -1

        node_untriedMoves <- untried_moves %>%
          dplyr::filter(Id == !!node_Id, move == !!nodemove) %>% dplyr::pull(untried_move)
        node_childNodes <- parent_child %>%
          dplyr::filter(Id == !!node_Id, move == !!nodemove)

        #Select
        while (length(node_untriedMoves) == 0 &
               dplyr::pull(node_childNodes %>% dplyr::tally(), n) > 0 ) {
          #print(node_Id); print(nodemove)
          node_df <- UCTSelectChild(MTCS_value, node_Id, nodemove)
          node_Id <- node_df$Id; nodemove <- node_df$move
          node_playerJustMoved <- node_playerJustMoved * -1
          state <- mk_move(s = state, move = nodemove)
          node_untriedMoves <- untried_moves %>%
            dplyr::filter(Id == !!node_Id, move == !!nodemove) %>% dplyr::pull(untried_move)
          node_childNodes <- parent_child %>%
            dplyr::filter(Id == !!node_Id, move == !!nodemove)
        }
        #print(node_Id)

        #if (node_Id == 9223372036854775807) break;
        # Expand
        if (length(node_untriedMoves) > 0) { # if we can expand (i.e. state/node is non-terminal)
          if (length(node_untriedMoves) == 1) m <- node_untriedMoves[1] else {
            m <- sample(node_untriedMoves, 1)
          }

          state <- mk_move(s = state, move = m)
          #node <- AddChild(node, m = m, s = state) # add child and descend tree
          child_ID <- tertodec(ifelse(state$df == -1, 2, state$df))
          child_move <- m
          if (!node_exist(MTCS_value, Id = child_ID, move = child_move)) {
              ini_node(MT, move = child_move, parent = node_Id, parent_move = nodemove,
                       state = state)
          }
          ##remove untried moves
          query <- glue::glue_sql(paste0("DELETE FROM untried_moves WHERE Id = ", node_Id,
                         " and move = ", nodemove, " and untried_move = ", m), .con = MT)
          res <- DBI::dbSendQuery(MT, query)
          res %>% DBI::dbClearResult()
        }

        # Rollout - this can often be made orders of magnitude quicker using a state.GetRandomMove() function
        while (length(state$moves) > 0 ) {
          if (length(state$moves) == 1) m <- state$moves[1] else m <- sample(state$moves, 1)
          state <- mk_move(s = state, move = m)
        }

        # Backpropagate
        while ((!is.null(node_Id) & !is.null(nodemove)) & (node_Id != 0 | nodemove != 0)) {
          result <- get_results(state, node_playerJustMoved)

          ##update results to the node
          query <- glue::glue_sql(
            paste0("UPDATE MTCS_value SET wins = wins + ", result, ",
                            visits = visits + 1 WHERE Id =", node_Id,
                   " and move = ", nodemove), .con = MT)
          res <- DBI::dbSendQuery(MT, query)
          res %>% DBI::dbClearResult()

          ### set node_Id to parent, and nodemove to parentmove
          node_df <- MTCS_value %>%
            dplyr::filter(Id == !!node_Id, move == !!nodemove) %>% dplyr::collect()
          node_Id <- node_df$parentId[1]; nodemove <- node_df$parentmove[1]
          node_playerJustMoved <- node_playerJustMoved * -1
          if (is.null(node_Id)) node_Id <- 0
          if (is.null(nodemove)) nodemove <- 0
        }
        prog_bar$tick()$print()
      }
  #})

  #MTCS_value %>% dplyr::collect() %>% View()
  m_sel <- MTCS_value %>%
    dplyr::filter(parentId == rootnode_Id, parentmove == rootmove) %>%
    dplyr::arrange(desc(visits)) %>% dplyr::collect() %>% .[1, ]
  #return(MT)
  return(m_sel$move[1])
  #parent_child
  #untried_moves
}

#' @title UCT function
#' @name UCT1
#' @param MT monte carlo Tree
#' @param rootstate state of the root
#' @param itermax max iterations
#' @param verbose whether to print results
#' @export
UCT1 <- function(MT = NULL, rootstate, itermax, verbose = FALSE){
  #MT <- ini_node(state = rootstate)
  #
  options("scipen"=100)
  if (is.null(MT)) MT <- ini_node1(state = rootstate)
  rootnode_Id <- rootstate$df %>% ifelse(. == -1, 2, .) %>% tertodec()
  rootmove <- 0
  #
  #system.time({
  start <- Sys.time()
  prog_bar <- dplyr::progress_estimated(itermax)
  for (i in 1:itermax) {
    #print(i)
    #microbenchmark({
    node_Id <- rootnode_Id
    nodemove <- rootmove
    state <- rootstate
    node_playerJustMoved <- rootstate$player_to_move * -1

    node_untriedMoves <- MT$untried_moves %>%
      dplyr::filter(Id == !!node_Id, move == !!nodemove) %>% dplyr::pull(untried_move)
    node_childNodes <- MT$parent_child %>%
      dplyr::filter(Id == !!node_Id, move == !!nodemove)
    #})
    #Select
    while (length(node_untriedMoves) == 0 &
           dplyr::pull(node_childNodes %>% dplyr::tally(), n) > 0 ) {
      #print(node_Id); print(nodemove)
      node_df <- UCTSelectChild1(MT$MTCS_value, node_Id, nodemove)
      node_Id <- node_df$Id; nodemove <- node_df$move
      node_playerJustMoved <- node_playerJustMoved * -1
      state <- mk_move(s = state, move = nodemove)
      node_untriedMoves <- MT$untried_moves %>%
        dplyr::filter(Id == !!node_Id, move == !!nodemove) %>% dplyr::pull(untried_move)
      node_childNodes <- MT$parent_child %>%
        dplyr::filter(Id == !!node_Id, move == !!nodemove)
    }
    #print(node_Id)

    #if (node_Id == 9223372036854775807) break;
    # Expand
    if (length(node_untriedMoves) > 0) { # if we can expand (i.e. state/node is non-terminal)
      if (length(node_untriedMoves) == 1) m <- node_untriedMoves[1] else {
        m <- sample(node_untriedMoves, 1)
      }

      state <- mk_move(s = state, move = m)
      #node <- AddChild(node, m = m, s = state) # add child and descend tree
      child_ID <- tertodec(ifelse(state$df == -1, 2, state$df))
      child_move <- m
      if (!node_exist(MT$MTCS_value, Id = child_ID, move = child_move)) {
        MT <- ini_node1(MT, move = child_move, parent = node_Id, parent_move = nodemove,
                 state = state)
      }
      ##remove untried moves
      MT$untried_moves <- MT$untried_moves %>%
        dplyr::filter(!(Id == node_Id & move == nodemove & untried_move == m))
    }

    # Rollout - this can often be made orders of magnitude quicker using a state.GetRandomMove() function
    while (length(state$moves) > 0 ) {
      if (length(state$moves) == 1) m <- state$moves[1] else m <- sample(state$moves, 1)
      state <- mk_move(s = state, move = m)
    }

    # Backpropagate
    while ((!is.null(node_Id) & !is.null(nodemove)) & (node_Id != 0 | nodemove != 0)) {
      result <- get_results(state, node_playerJustMoved)

      ##update results to the node
      MT$MTCS_value <- MT$MTCS_value %>%
        dplyr::mutate(
          wins = ifelse(Id == node_Id & move == nodemove, wins + result, wins),
          visits = ifelse(Id == node_Id & move == nodemove, visits + 1, visits)
        )

      ### set node_Id to parent, and nodemove to parentmove
      node_df <- MT$MTCS_value %>%
        dplyr::filter(Id == !!node_Id, move == !!nodemove)
      node_Id <- node_df$parentId[1]; nodemove <- node_df$parentmove[1]
      node_playerJustMoved <- node_playerJustMoved * -1
      if (is.null(node_Id)) node_Id <- 0
      if (is.null(nodemove)) nodemove <- 0
    }
    prog_bar$tick()$print()
  }
  #})
  print(Sys.time() - start)
  #MTCS_value %>% dplyr::collect() %>% View()
  m_sel <- MT$MTCS_value %>%
    dplyr::filter(parentId == rootnode_Id, parentmove == rootmove) %>%
    dplyr::arrange(desc(visits)) %>% dplyr::collect() %>% .[1, ]
  #return(MT)
  return(list(m_sel$move[1], MT))
  #parent_child
  #untried_moves
}


#' @title UCT play game function
#' @name UCT_playgame
#' @export
UCT_playgame <- function(player1_depth = 100, player2_depth = 25) {
  # def UCTPlayGame():
  #   """ Play a sample game between two UCT players where each player gets a different number
  #       of UCT iterations (= simulations = tree nodes).
  #   """
  # # state = OthelloState(4) # uncomment to play Othello on a square board of the given size
  # # state = OXOState() # uncomment to play OXO
  state <- ini_othello(8) # uncomment to play Nim with the given number of starting chips
  consecutive_no_mv_track <- 0
  moves <- NULL
  while ( consecutive_no_mv_track < 2) { #length(state$moves) > 0
    print(tibble::as_tibble(state$df))
    cat('Current Player is ', state$player_to_move, '\n')

    if (length(state$moves) > 0 ) {
      if (state$player_to_move == -1) {
        m = UCT(rootstate = state, itermax = player2_depth, verbose = F)
      } else {
        m = UCT(rootstate = state, itermax = player1_depth, verbose = F)
      }
      cat('\n Best Move: ', m, "\n")
      moves <- c(moves, m)
    }

    state <- mk_move(s = state, move = m)
    if (length(state$moves) > 0) consecutive_no_mv_track <-  0
       else consecutive_no_mv_track <- consecutive_no_mv_track + 1
  }
  result1 <- get_results(state, 1)
  result2 <- get_results(state, -1)

  if (result1 == 1) cat('Player ', 1, ' wins!')
  else if (result1 == 1) cat('Player ', 2, ' wins!')
  else cat('Noby Wins')

  return(list(moves = moves, result = sum(state$df) ))
}

#' @title UCT play game function
#' @name UCT_playgame1
#' @export
UCT_playgame1 <- function(player1_depth = 100, player2_depth = 25) {
  # def UCTPlayGame():
  #   """ Play a sample game between two UCT players where each player gets a different number
  #       of UCT iterations (= simulations = tree nodes).
  #   """
  # # state = OthelloState(4) # uncomment to play Othello on a square board of the given size
  # # state = OXOState() # uncomment to play OXO
  state <- ini_othello(8) # uncomment to play Nim with the given number of starting chips
  consecutive_no_mv_track <- 0
  moves <- NULL
  while ( consecutive_no_mv_track < 2) { #length(state$moves) > 0
    print(tibble::as_tibble(state$df))
    cat('Current Player is ', state$player_to_move, '\n')

    MT1 <- NULL
    MT2 <- NULL
    if (length(state$moves) > 0 ) {
      if (state$player_to_move == -1) {
        UCT_res1 <- UCT1(MT = MT1, rootstate = state, itermax = player2_depth, verbose = F)
        m  <- UCT_res1[[1]]
        #MT1 <- UCT_res1[[2]]
      } else {
        UCT_res2 <- UCT1(MT = MT2, rootstate = state, itermax = player1_depth, verbose = F)
        m  <- UCT_res2[[1]]
        #MT2 <- UCT_res2[[2]]
      }
      cat('\n Best Move: ', m, "\n")
      moves <- c(moves, m)
    }

    state <- mk_move(s = state, move = m)
    if (length(state$moves) > 0) consecutive_no_mv_track <-  0
    else consecutive_no_mv_track <- consecutive_no_mv_track + 1
  }
  result1 <- get_results(state, 1)
  result2 <- get_results(state, -1)

  if (result1 == 1) cat('Player ', 1, ' wins!')
  else if (result2 == 1) cat('Player ', 2, ' wins!')
  else cat('Noby Wins')

  return(list(moves = moves, result = sum(state$df) ))
}



#' @title base3 to base 10
#' @name tertodec
#' @param val game vals
#' @export
tertodec <- function(val) {
  val1 <- rev(val)
  (val1 * 3 ^ (seq_len(length(val1)) - 1)) %>% sum()
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
