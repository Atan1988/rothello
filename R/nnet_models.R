#' @title create neural net for othello
#' @name ini_nnet_othello
#' @param game othello game state
#' @param args nnet arguments
#' @export
ini_nnet_othello <- function(game, args){
  obj <- list()

  # game params
    boardsz_vec <- getBoardSize(game)
    obj$board_x <- boardsz_vec[1]; obj$board_y <- boardsz_vec[2]
    obj$action_size <- getActionSize(game)
    obj$args <- args
  #
  # # Renaming functions
  # Relu = tf.nn.relu
  # Tanh = tf.nn.tanh
  # BatchNormalization = tf.layers.batch_normalization
  # Dropout = tf.layers.dropout
  # Dense = tf.layers.dense
}
