#' @title othello state class
#' @name ini_othello
#' @param sz size of the board
#' @export
#' @exportClass
ini_othello <- function(sz, player = 1) {
  game_board <- generate_othello_base(sz)
  move_df <- chk_0_sp1(game_board, search_color = player * -1)

  structure(
    list(
      df = game_board
      , moves = move_df$id %>% unique()
      , flip_df = move_df
      , player_to_move = player
    )
    , class = 'othello_state'
  )
}

#' @title make a move to change state
#' @name mk_move
#' @param s game state
#' @param move move to make
#' @export
mk_move <- function(s, move) {
  new_df <- mk_a_move(s$df, s$flip_df, id = move, color = s$player_to_move)
  new_player <- s$player_to_move * -1
  new_flip_df <- chk_0_sp1(new_df, search_color = new_player * -1)

  structure(
    list(
      df = new_df
      , moves = new_flip_df$id %>% unique()
      , flip_df = new_flip_df
      , player_to_move = new_player
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
  game_sum <- sum(s$df$val)
  res <- ifelse(sign(game_sum) == sign(player), 1, 0)
  return(res)
}

#' @title summary method for othello game state
#' @name summary.othello_state
#' @param x  anything to be summaried
#' @exportClass
summary.othello_state <- function(x) {
   return(x$df %>% dplyr::select(-id) %>% tidyr::spread(col, val))
}
