#' @title othello state class
#' @name ini_othello
#' @param sz size of the board
#' @param val board values
#' @export
ini_othello <- function(sz, val = NULL, player = 1) {
  game_board <- generate_othello_base_M(sz, val)
  #move_df <- get_othello_mv(game_board, search_player = player * -1 )
  #if (length(move_df[[1]]) == 0) pass <- pass + 1 else pass <- 0

  structure(
    list(
      df = game_board
      #, moves = move_df[[1]]
      #, flip_df = move_df[[2]]
      , player_to_move = player
      #, pass_ct = 0
      #, terminal = 0
    )
    , class = 'othello'
  )
}

#' @title othello get initial board
#' @name getInitBoard.othello
#' @param game othello game object
#' @export
getInitBoard.othello <- function(game) {
 sz <-  length(game$df)^0.5
 game$df <- generate_othello_base_M(sz, NULL)
 game$player_to_move <- 1
 return(game)
}

#' @title get canonical form of the othello class
#' @name CanonicalForm.othello
#' @param game othello object
#' @export
CanonicalForm.othello <- function(game) {
  df <- game$df * game$player_to_move
  game$df <- df
  game$player_to_move <- 1
  return(game)
}

#' @title get Symmetries of the othello class
#' @name getSymmetries.othello
#' @param game othello object
#' @export
getSymmetries.othello <- function(game, pi) {

  pi_mat <- matrix(pi, nrow = sqrt(length(pi)))
  mat <- game$df
  #8 symmtries.
  mats <- append(
    list(mat
         , mat %>% Thermimage::flip.matrix()
         , mat %>% Thermimage::mirror.matrix()
         , mat %>% t()
         , mat %>% pracma::rot90(2) %>% t()
    ),
    seq(1, 3, 1) %>% purrr::map(~pracma::rot90(mat, .))
  )

  pi_mats <- append(
    list(pi_mat
         , pi_mat %>% Thermimage::flip.matrix()
         , pi_mat %>% Thermimage::mirror.matrix()
         , pi_mat %>% t()
         , pi_mat %>% pracma::rot90(2) %>% t()
    ),
    seq(1, 3, 1) %>% purrr::map(~pracma::rot90(pi_mat, .))
  )

  Ss <- mats %>% purrr::map_chr(function(x) ifelse(x == -1, 2, x) %>% paste(collapse = ""))
  names(mats) <- Ss
  names(pi_mats) <- Ss

  return(list(mats = mats, pis = pi_mats))
}

#' @title othello string representation
#' @name stringRepresentation.othello
#' @param game othello object
#' @export
stringRepresentation.othello <- function(game){
  ifelse(game$df == -1, 2, game$df) %>% paste(collapse = "")
}

#' @title othello check whether game has ended
#' @name getGameEnded.othello
#' @param game othello object
#' @export
getGameEnded.othello <- function(game) {
  # return 0 if not ended, 1 if player 1 won, -1 if player 1 lost
  # player = 1

  game_pass <- game
  game_pass$player_to_move <- game$player_to_move * -1

  if (length(getValidMove(game)) > 0) return(0)

  if (length(getValidMove(game_pass)) > 0) return(0)

  # small value for draw
  game_sum <- sum(game$df)
  if (game_sum == 0) return(0.001)
  if (sign(game_sum) == sign(game$player_to_move)) return(1) else return(-1)
}

#' @title  othello get moves using matrix
#' @name getValidMove.othello
#' @param game othello object
#' @export
getValidMove.othello <- function(game){
  get_othello_mv(game$df, search_player = game$player_to_move * -1)[[1]]
}

#' @title  othello get action size
#' @name getActionSize.othello
#' @param game othello object
#' @export
getActionSize.othello <- function(game){
  length(game$df)
}

#' @title  othello get board size
#' @name getBoardSize.othello
#' @param game othello object
#' @export
getBoardSize.othello <- function(game){
  c(board_x = sqrt(length(game$df)), board_y =  sqrt(length(game$df)))
}

#' @title  othello make moves using matrix
#' @name getNextState.othello
#' @param game othello object
#' @param move move to make
#' @export
getNextState.othello <- function(game, move){

  if (is.null(move) | length(move) == 0 | move == -1) {
    game$player_to_move <- game$player_to_move * -1
    return(game)
  }

  sz <- nrow(game$df)
  mvs_list <- search_neighbor(game$df, search_player = game$player_to_move * -1)

  mvs <- mvs_list$mvs
  dir_mat <- mvs_list$dir_mat
  #if (length(mvs) == 0) return(list(NULL, NULL))
  mv_df <- valid_mv_chk(mv = move, df = game$df, sz, dir_mat, search_player = game$player_to_move * -1)

  game$df[c(move, mv_df$flip)] <- game$player_to_move
  game$player_to_move <- game$player_to_move * -1
  return(game)
}

chk_valid <- function(df, valid_dir, mv_row, mv_col, sz, search_player) {
  dir <- dir_list[[valid_dir]]
  dir_rows <- mv_row + seq(1, 8, 1) * dir[1]
  dir_cols <- mv_col + seq(1, 8, 1) * dir[2]
  dir_flt <- which(dir_rows >= 1 & dir_rows <= 8 & dir_cols >= 1 & dir_cols <= 8)
  dir_rows <- dir_rows[dir_flt]
  dir_cols <- dir_cols[dir_flt]

  dir_idx <- ((dir_cols - 1) * sz + dir_rows) #%>% .[.<= sz^2]
  dir_val <- df[dir_idx]#%>% .[!is.na(.)]
  oppo_loc <- which(dir_val == (search_player * -1) )[1]
  if (is.na(oppo_loc) | oppo_loc <= 1) return(list(res = F, filp = NULL))
  res <- sum(dir_val[1:(oppo_loc - 1)]) == ((oppo_loc - 1) * (search_player))
  if (res) {
    return(
      list(
        res = T,
        flip = dir_idx[1:(oppo_loc - 1)]
      )
    )
  }
  return(list(res = F, flip = NULL))
}

valid_mv_chk <- function(mv, df, sz, dir_mat, search_player) {
  mv_col <- (mv - 0.01) %/% sz + 1
  mv_row <- mv - (mv_col - 1) * sz
  valid_dirs <- as.vector(which(dir_mat[mv, ] == 1))
  valid_res <- lapply(valid_dirs, chk_valid, df= df, mv_row = mv_row,
                      mv_col = mv_col, sz = sz, search_player)

  res <- sum(sapply(valid_res, function(x) x$res)) > 0
  flip <- unlist(lapply(valid_res, function(x) x$flip))
  return(list(res = res, flip = flip))
}

#' @title  othello get moves using matrix
#' @name get_othello_mv
#' @param df board state
#' @param search_player opponents color
#' @export
get_othello_mv <- function(df, search_player = -1){
  sz <- nrow(df)
  mvs_list <- search_neighbor(df, search_player = search_player)

  mvs <- mvs_list$mvs
  if (length(mvs) == 0) return(list(NULL, NULL))
  dir_mat <- mvs_list$dir_mat

 valid_mvs_chk <- lapply(mvs, valid_mv_chk, df = df, sz = sz, dir_mat = dir_mat,
                         search_player = search_player)
 valid_idx <- sapply(valid_mvs_chk, function(x) x$res)

 ok_mvs <- mvs[valid_idx]
 mv_flips <- lapply(valid_mvs_chk[valid_idx ], function(x) x$flip)
 return(list(ok_mvs, mv_flips))
}

#' @title  search moves using matrix
#' @name search_neighbor
#' @param mat board matrix
#' @param search_player opponents color
#' @export
search_neighbor <-  function(mat, search_player = -1) {
#microbenchmark({
  n <-  nrow(mat)
  ncol  <- ncol(mat)
  val <- as.vector(mat)
  val <- ifelse(val == search_player, 1, 0)
  mat1 <- matrix(val, nrow = n, ncol = ncol)

  mat.pad <-  rbind(0, cbind(0, mat1, 0), 0)
  ind <- 2:(n + 1) # row/column indices of the "middle"

  N <-  as.vector(mat.pad[ind - 1, ind    ] )
  NE <-  as.vector(mat.pad[ind - 1, ind + 1] )
  E <-  as.vector(mat.pad[ind    , ind + 1] )
  SE <-  as.vector(mat.pad[ind + 1, ind + 1] )
  S  <- as.vector(mat.pad[ind + 1, ind    ])
  SW  <-  as.vector(mat.pad[ind + 1, ind - 1] )
  W   <-  as.vector(mat.pad[ind    , ind - 1] )
  NW  <-  as.vector(mat.pad[ind - 1, ind - 1] )
#})


  surr_sum <- N + NE + E + SE + S + SW + W + NW
  mvs <- which(surr_sum > 0)
  mvs_0 <- which(mat == 0)
  mvs <- intersect(mvs, mvs_0 )
  dir_mat <- cbind(N, NE, E, SE, S, SW, W, NW)
  return(list(mvs = mvs,
              dir_mat = dir_mat))
  #return(surr_sum)
}

#' @title make a move to change state
#' @name mk_move
#' @param s game state
#' @param move move to make
#' @export
mk_move <- function(s, move) {
  if (length(s$moves) > 0 & !is.na(move)) {
    flip <- s$flip_df[[which(s$moves == move)]]
    player_color <- s$player_to_move
    new_df <- s$df
    new_df[c(move, flip)] <- player_color
    pass_ct <- 0
  } else {
    new_df <- s$df
    pass_ct <- s$pass_ct + 1
  }
  new_player <- s$player_to_move * -1
  new_move_df <- get_othello_mv(new_df, search_player = new_player * -1 )
  if (pass_ct >= 2 | length(s$df[s$df == 0])) terminal <-  1 else terminal <-  0

  structure(
    list(
      df = new_df
      , moves = new_move_df[[1]]
      , flip_df = new_move_df[[2]]
      , player_to_move = new_player
      , pass_ct = pass_ct
      , terminal = terminal
    )
    , class = 'othello_state'
  )
}

#' @title get result of game
#' @name get_results
#' @param s game state
#' @param player player just moved
#' @export
get_results <- function(s, player) {
  game_sum <- sum(s$df)
  res <- ifelse(sign(game_sum) == sign(player), 1, 0)
  return(res)
}

