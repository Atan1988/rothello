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

#' @title nnet class
#' @name nnet
nnet <- R6::R6Class("nnet", list(
 initialize = function(game, args) {
     # game params
     self$board_x, self$board_y = game.getBoardSize()
     self.action_size = game.getActionSize()
     self.args = args

     # Neural Net
     self.input_boards = Input(shape=(self.board_x, self.board_y))    # s: batch_size x board_x x board_y

     x_image = Reshape((self.board_x, self.board_y, 1))(self.input_boards)                # batch_size  x board_x x board_y x 1
     h_conv1 = Activation('relu')(BatchNormalization(axis=3)(Conv2D(args.num_channels, 3, padding='same', use_bias=False)(x_image)))         # batch_size  x board_x x board_y x num_channels
     h_conv2 = Activation('relu')(BatchNormalization(axis=3)(Conv2D(args.num_channels, 3, padding='same', use_bias=False)(h_conv1)))         # batch_size  x board_x x board_y x num_channels
     h_conv3 = Activation('relu')(BatchNormalization(axis=3)(Conv2D(args.num_channels, 3, padding='valid', use_bias=False)(h_conv2)))        # batch_size  x (board_x-2) x (board_y-2) x num_channels
     h_conv4 = Activation('relu')(BatchNormalization(axis=3)(Conv2D(args.num_channels, 3, padding='valid', use_bias=False)(h_conv3)))        # batch_size  x (board_x-4) x (board_y-4) x num_channels
     h_conv4_flat = Flatten()(h_conv4)
     s_fc1 = Dropout(args.dropout)(Activation('relu')(BatchNormalization(axis=1)(Dense(1024, use_bias=False)(h_conv4_flat))))  # batch_size x 1024
     s_fc2 = Dropout(args.dropout)(Activation('relu')(BatchNormalization(axis=1)(Dense(512, use_bias=False)(s_fc1))))          # batch_size x 1024
     self.pi = Dense(self.action_size, activation='softmax', name='pi')(s_fc2)   # batch_size x self.action_size
     self.v = Dense(1, activation='tanh', name='v')(s_fc2)                    # batch_size x 1

     self.model = Model(inputs=self.input_boards, outputs=[self.pi, self.v])
     self.model.compile(loss=['categorical_crossentropy','mean_squared_error'], optimizer=Adam(args.lr))
 }
)
)
