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

    model <- keras::keras_model_sequential() %>%
      keras::layer_conv_2d()

    # Neural Net
    # self.graph = tf.Graph()
    # with self.graph.as_default():
    #   self.input_boards = tf.placeholder(tf.float32, shape=[None, self.board_x, self.board_y])    # s: batch_size x board_x x board_y
    # self.dropout = tf.placeholder(tf.float32)
    # self.isTraining = tf.placeholder(tf.bool, name="is_training")
    #
    # x_image = tf.reshape(self.input_boards, [-1, self.board_x, self.board_y, 1])                    # batch_size  x board_x x board_y x 1
    # h_conv1 = Relu(BatchNormalization(self.conv2d(x_image, args.num_channels, 'same'), axis=3, training=self.isTraining))     # batch_size  x board_x x board_y x num_channels
    # h_conv2 = Relu(BatchNormalization(self.conv2d(h_conv1, args.num_channels, 'same'), axis=3, training=self.isTraining))     # batch_size  x board_x x board_y x num_channels
    # h_conv3 = Relu(BatchNormalization(self.conv2d(h_conv2, args.num_channels, 'valid'), axis=3, training=self.isTraining))    # batch_size  x (board_x-2) x (board_y-2) x num_channels
    # h_conv4 = Relu(BatchNormalization(self.conv2d(h_conv3, args.num_channels, 'valid'), axis=3, training=self.isTraining))    # batch_size  x (board_x-4) x (board_y-4) x num_channels
    # h_conv4_flat = tf.reshape(h_conv4, [-1, args.num_channels*(self.board_x-4)*(self.board_y-4)])
    # s_fc1 = Dropout(Relu(BatchNormalization(Dense(h_conv4_flat, 1024, use_bias=False), axis=1, training=self.isTraining)), rate=self.dropout) # batch_size x 1024
    # s_fc2 = Dropout(Relu(BatchNormalization(Dense(s_fc1, 512, use_bias=False), axis=1, training=self.isTraining)), rate=self.dropout)         # batch_size x 512
    # self.pi = Dense(s_fc2, self.action_size)                                                        # batch_size x self.action_size
    # self.prob = tf.nn.softmax(self.pi)
    # self.v = Tanh(Dense(s_fc2, 1))                                                               # batch_size x 1
    #
    # self.calculate_loss()
}
