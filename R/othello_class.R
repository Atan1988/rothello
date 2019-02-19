#' @title othello state class
#' @name ini_othello
#' @param sz size of the board
#' @param val board values
#' @export
ini_othello <- function(sz, val = NULL, player = 1) {
  game_board <- generate_othello_base_M(sz, val)
  #if (!is.null(game_board)) game_board$val <- val
  move_df <- get_othello_mv(game_board, search_player = player * -1 )
  #if (length(move_df[[1]]) == 0) pass <- pass + 1 else pass <- 0

  structure(
    list(
      df = game_board
      , moves = move_df[[1]]
      , flip_df = move_df[[2]]
      , player_to_move = player
      , pass_ct = 0
      , terminal = 0
    )
    , class = 'othello_state'
  )
}

#' @title get canonical form of the othello class
#' @name othello_CanonicalForm
#' @param game othello object
#' @export
othello_CanonicalForm <- function(game) {
  game$df * game$player_to_move
}

#' @title get Symmetries of the othello class
#' @name othello_Symmetries
#' @param game othello object
#' @export
othello_Symmetries <- function(game) {

}

#' @title othello string representation
#' @name othello_stringRepresentation
#' @param game othello object
#' @export
othello_stringRepresentation <- function(game){

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

chk_valid <- function(valid_dir, mv_row, mv_col) {
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

valid_mv_chk <- function(mv) {
  mv_col <- (mv - 0.01) %/% sz + 1
  mv_row <- mv - (mv_col - 1) * sz
  valid_dirs <- as.vector(which(dir_mat[mv, ] == 1))
  valid_res <- lapply(valid_dirs, chk_valid, mv_row = mv_row, mv_col = mv_col)

  res <- sum(sapply(valid_res, function(x) x$res)) > 0
  flip <- unlist(lapply(valid_res, function(x) x$flip))
  return(list(res = res, flip = flip))
}

 valid_mvs_chk <- lapply(mvs, valid_mv_chk)
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

#' @title summary method for othello game state
#' @name summary.othello_state
#' @param x  anything to be summaried
#' @export
summary.othello_state <- function(x) {
   return(x$df %>% dplyr::select(-id) %>% tidyr::spread(col, val))
}
