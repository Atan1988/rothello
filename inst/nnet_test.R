game <- ini_othello(8)

can_game <- CanonicalForm(game)
can_game2 <- CanonicalForm(getNextState(game, 21))

games <- list(can_game$df, can_game2$df)

dat_input <- keras::array_reshape(games, dim = c(2, 8, 8, 1))

args <- list(EPS = 1e-8, num_channels = 32,
             kernel_size = c(3, 3), dropout = 0.2)
nnet <- nnetclass$new(game, args = args)
c(pis, vs) %<-% nnet$model$predict(dat_input)

mtcs <- MTCSzero$new(game, nnet , mtcs_args)
mtcs$getActionProb(canonicalBoard)
