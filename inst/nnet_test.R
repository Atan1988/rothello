can_game <- CanonicalForm(game)
can_game2 <- CanonicalForm(getNextState(game, 21))

games <- list(can_game$df, can_game2$df)

dat_input <- keras::array_reshape(games, dim = c(2, 8, 8))

kkk$model
keras::predict_proba(kkk$model_pi, dat_input)


kkk <- nnet$new(game, args = list(EPS = 1e-8))
c(pis, vs) %<-% kkk$model$predict(dat_input)
