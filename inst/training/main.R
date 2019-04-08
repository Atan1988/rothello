main_args<-  list(
  'numIters'= 1,
  'numEps'= 10,
  'tempThreshold'= 15,
  'updateThreshold'= 0.6,
  'maxlenOfQueue'= 200000,
  'numMCTSSims'= 40,
  'arenaCompare'= 20,
  'cpuct'= 1,
  'EPS' = 1e-8,

  'checkpoint'= 'temp/',
  'load_model'= FALSE,
  'load_folder_file'= 'dev/models/best.RData',
  'numItersForTrainExamplesHistory'= 20
)


nnet_args <- list(
  batch_size = 64
  , epochs = 10
  , EPS = 1e-8
  , num_channels = 32
  , kernel_size = c(3, 3)
  , dropout = 0.3
  , lr = 0.01
)


g <- ini_othello(8)
nnet_obj <- nnetwrapper$new(g, args = nnet_args)
pnet_obj <- nnetwrapper$new(g, args = nnet_args)

if (main_args$load_model) nnet_obj$load_checkpoint(main_args$load_folder_file)

c <-  coach$new(g, nnet_obj, pnet_obj, args = main_args)
if (main_args$load_model) print("Load trainExamples from file")

#c.loadTrainExamples()
c$learn()

nnet_obj <- nnetwrapper$new(g, args = nnet_args)
pnet_obj <- nnetwrapper$new(g, args = nnet_args)
#pnet_obj$nnet$model$load_weights('temp/temp3.RData')
#nnet_obj$nnet$model$load_weights('temp/best.RData')

#c$nnet$load_checkpoint('temp', 'temp3.RData')
#c$pnet$load_checkpoint('temp', 'temp1.RData')

nmcts <- MTCSzero$new(g, nnet_obj, main_args)
pmcts <- MTCSzero$new(g, pnet_obj, main_args)

player1 <- function(x) which.max(pmcts$getActionProb(x, temp=0))
player2 <- function(x) which.max(nmcts$getActionProb(x, temp=0))
game <- g

arena <- Arena$new(function(x) which.max(pmcts$getActionProb(x, temp=0)),
                   function(x) which.max(nmcts$getActionProb(x, temp=0)), g,
                   display = print)

start <- Sys.time()
results <- arena$playGames(100, verbose = F)
print(Sys.time() - start)

print(results)
which.max(pmcts$getActionProb(g, temp=0))
which.max(nmcts$getActionProb(g, temp=0))
