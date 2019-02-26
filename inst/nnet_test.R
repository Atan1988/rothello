can_game <- CanonicalForm(game)
can_game2 <- CanonicalForm(getNextState(game, 21))

games <- list(can_game$df, can_game2$df)

dat_input <- keras::array_reshape(games, dim = c(2, 8, 8, 1))

kkk$model
keras::predict_proba(kkk$model_pi, dat_input)

args <- list(EPS = 1e-8, num_channels = 32,
             kernel_size = c(3, 3), dropout = 0.2)
nnet <- nnet$new(game, args = args)
c(pis, vs) %<-% nnet$model$predict(dat_input)

