nnet_train <- function(x, examples) UseMethod("nnet_train")

nnet_predict <- function(x, board) UseMethod("nnet_predict")

nnet_save_checkpoint <- function(x, folder, filename) UseMethod("nnet_save_checkpoint")

nnet_load_checkpoint <- function(x, folder, filename) UseMethod("nnet_load_checkpoint")
