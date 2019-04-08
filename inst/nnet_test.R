game <- ini_othello(8)

can_game <- CanonicalForm(game)
can_game2 <- CanonicalForm(getNextState(game, 21))

games <- list(can_game$df, can_game2$df)
can_game <- CanonicalForm(game)
games <- list(can_game$df)

dat_input <- keras::array_reshape(games, dim = c(1, 8, 8, 1))

args <- list(EPS = 1e-8, num_channels = 32,
             kernel_size = c(3, 3), dropout = 0.2)
nnet <- nnetwrapper$new(game, args = nnet_args)
c(pis, vs) %<-% nnet$predict(dat_input)

c(pis, vs) %<-% nnet$predict(game %>% CanonicalForm() %>% .$df)

mtcs <- MTCSzero$new(game, nnet, main_args)
mtcs$getActionProb(can_game)
