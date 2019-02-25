#' @title nnet class
#' @name nnet
nnet <- R6::R6Class("nnet", list(
 board_x = NULL,
 board_y = NULL,
 model = NULL,
 model_pi = NULL,
 model_v = NULL,
 action_size = NULL,
 args = NULL,
 initialize = function(game, args) {
     # # game params
     board_size <- getBoardSize(game)
     self$board_x <- board_size[1]; self$board_y <- board_size[2]
     self$action_size <- getActionSize(game)
     self$args <- args


     main_input <- keras::layer_input(shape = c(self$board_x,  self$board_y),
                                      dtype = 'float32', name = 'main_input')

     main_out <- main_input %>%
       keras::layer_flatten() %>%
       keras::layer_dense(units = 64, activation = keras::activation_relu,
                          name = 'main_out')

     probs_out <- main_out %>%
       #output layer
       keras::layer_dense(self$action_size, activation = keras::activation_softmax,
                          name = 'probs_out')

     v_out <- main_out %>%
       #output layer
       keras::layer_dense(1, activation = keras::activation_tanh, name = 'v_out')

     model <- keras::keras_model(
       inputs = c(main_input),
       outputs = c(probs_out, v_out)
     )

     # model_v <- keras::keras_model(
     #   inputs = c(main_input),
     #   outputs = c(v_out)
     # )

     model %>% keras::compile(
       optimizer = 'adam',
       loss = 'binary_crossentropy',
       metrics = list('accuracy')
     )

     # model_v %>% keras::compile(
     #   optimizer = 'adam',
     #   loss = 'mean_absolute_error',
     #   metrics = list('mae')
     # )

     self$model <- model
     #self$model_v <- model_v
 }
)
)
