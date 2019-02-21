#' @title create neural net for othello
#' @name ini_nnet_othello
#' @param game othello game state
#' @param args nnet arguments
#' @export
ini_nnet_othello <- function(game, args){
  obj <- list()

  # game params
    c(obj$board_x, obj$board_y) %<-% getBoardSize(game)
    obj$action_size <- getActionSize(game)
    obj$args <- args
  #
  # # Renaming functions
  # Relu = tf.nn.relu
  # Tanh = tf.nn.tanh
  # BatchNormalization = tf.layers.batch_normalization
  # Dropout = tf.layers.dropout
  # Dense = tf.layers.dense

    main_input <- keras::layer_input(shape = c( obj$board_x,  obj$board_y, 1),
                                     dtype = 'float32', name = 'main_input')

    main_out <- main_input %>%
      keras::layer_dense(units = 64, activation = keras::activation_relu,
                         name = 'main_out')

    probs_out <- main_out %>%
      #output layer
      keras::layer_dense(obj$action_size, activation = keras::activation_softmax,
                         name = 'probs_out')

    v_out <- main_out %>%
      #output layer
      keras::layer_dense(1, activation = keras::activation_tanh, name = 'v_out')

    model <- keras::keras_model(
      inputs = c(main_input),
      outputs = c(probs_out, v_out)
    )

    obj$model <- model
    structure(
      obj, class = 'nnet_othello'
    )
}
