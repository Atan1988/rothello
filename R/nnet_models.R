#' @title nnet class
#' @name nnetclass
nnetclass <- R6::R6Class("nnetclass", list(
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


     main_input <- keras::layer_input(shape = c(self$board_x,  self$board_y, 1),
                                      dtype = 'float32', name = 'main_input')

     main_out <- main_input %>%
       ##conv1
       keras::layer_conv_2d(filters = args$num_channels,
                            kernel_size = args$kernel_size, padding = 'same', use_bias = FALSE) %>%
       keras::layer_batch_normalization(axis=3) %>%
       keras::layer_activation(activation = 'relu') %>%
       ##conv2
       keras::layer_conv_2d(filters = args$num_channels,
                            kernel_size = args$kernel_size, padding = 'same', use_bias = FALSE) %>%
       keras::layer_batch_normalization(axis=3) %>%
       keras::layer_activation(activation = 'relu') %>%
       ##conv3
       keras::layer_conv_2d(filters = args$num_channels,
                            kernel_size = args$kernel_size, padding = 'valid', use_bias = FALSE) %>%
       keras::layer_batch_normalization(axis=3) %>%
       keras::layer_activation(activation = 'relu') %>%
       ##conv4
       keras::layer_conv_2d(filters = args$num_channels,
                            kernel_size = args$kernel_size, padding = 'valid', use_bias = FALSE) %>%
       keras::layer_batch_normalization(axis=3) %>%
       keras::layer_activation(activation = 'relu') %>%
       ##flatten layers
       keras::layer_flatten() %>%
       ##dense layer
       keras::layer_dense(1024, use_bias = FALSE) %>%
       keras::layer_batch_normalization(axis=1) %>%
       keras::layer_activation(activation = 'relu') %>%
       keras::layer_dropout(args$dropout) %>%
       ###dense layer 2
       keras::layer_dense(512, use_bias = FALSE) %>%
       keras::layer_batch_normalization(axis=1) %>%
       keras::layer_activation(activation = 'relu') %>%
       keras::layer_dropout(args$dropout)

     probs_out <- main_out %>%
       #output layer
       keras::layer_dense(self$action_size + 1, activation = keras::activation_softmax,
                          name = 'probs_out')

     v_out <- main_out %>%
       #output layer
       keras::layer_dense(1, activation = keras::activation_tanh, name = 'v_out')

     model <- keras::keras_model(
       inputs = c(main_input),
       outputs = c(probs_out, v_out)
     )

     model %>% keras::compile(
       optimizer = keras::optimizer_adam(args$lr),
       loss = c('categorical_crossentropy','mean_squared_error'),
       metrics = list('accuracy')
     )

     self$model <- model

 }
)
)

#' @title nnet wrapper
#' @name nnetwrapper
nnetwrapper <- R6::R6Class("nnetwrapper", list(
        nnet = NULL,
        board_x = NULL,
        board_y = NULL,
        action_size = NULL,
        args = NULL,
        game = NULL,
        initialize = function(game, args){
           self$game <- game
           self$args <- args
           self$nnet <- nnetclass$new(game, args)
           size <- getBoardSize(game)
           self$board_x <- size['board_x']; self$board_y <- size['board_y']
           self$action_size <- getActionSize(game)
         },
        train = function(examples) {
          # examples: list of examples, each example is of form (board, pi, v)
           input_boards <- examples %>% purrr::map(~unlist(.$mat, F))
           target_pis <- examples %>% purrr::map(~unlist(.$pis, F))
           target_vs <- examples %>% purrr::map(~unlist(.$V, F))

           input_boards <- unlist(input_boards, F); names(input_boards) <- NULL
           target_pis <- unlist(target_pis, F); names(target_pis) <- NULL
           target_vs <- target_vs %>% purrr::map(~rep(., 8)) %>% unlist()

           input_boards <- keras::array_reshape(input_boards,
                                dim = c(length(input_boards), self$board_x, self$board_y, 1))
           target_pis <- target_pis %>% purrr::map(~as.vector(.))
           target_pis <- keras::array_reshape(target_pis, dim = c(length(target_pis), self$action_size))

           self$nnet$model %>% keras::fit(x = input_boards,
                               y = list(probs_out = target_pis, v_out = target_vs),
                               batch_size = self$args$batch_size, epochs = self$args$epochs)
        },
        predict = function(board) {
            #board: np array with board
            input_dat <- keras::array_reshape(board, dim = c(1, self$board_x, self$board_y, 1))
            c(Ps, v) %<-% self$nnet$model$predict(input_dat)

            return(list(Ps, v))
        },
        save_checkpoint = function(folder='checkpoint', filename='checkpoint.pth.tar'){
            filepath <- file.path(folder, filename)
            if (!dir.exists(folder)) {
               cat("Checkpoint Directory does not exist! Making directory {}", folder, '\n')
               dir.create(folder)
            } else {
               cat("Checkpoint Directory exists! \n")
            }
            self$nnet$model$save_weights(filepath)
        },
        load_checkpoint = function(folder='checkpoint', filename='checkpoint.pth.tar'){
            filepath <- file.path(folder, filename)
            if (!file.exists(filepath)) {
               cat("No model in path ", filepath, '\n')
               return(NULL)
            }
            self$nnet$model$load_weights(filepath)
        }
  )
)
