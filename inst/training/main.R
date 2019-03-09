mtcs_args<-  list(
  'numIters'= 1000,
  'numEps'= 100,
  'tempThreshold'= 15,
  'updateThreshold'= 0.6,
  'maxlenOfQueue'= 200000,
  'numMCTSSims'= 25,
  'arenaCompare'= 40,
  'cpuct'= 1,
  'EPS' = 1e-8,

  'checkpoint'= './temp/',
  'load_model'= FALSE,
  'load_folder_file'= '/dev/models/best.pth.tar',
  'numItersForTrainExamplesHistory'= 20
)

mtcs_args <- list(
  cpuct = 1,
  EPS = 1e-8
)

nnet_args <- list(
  batch_size = 32
  , epochs = 10
  , EPS = 1e-8
  , num_channels = 32
  , kernel_size = c(3, 3)
  , dropout = 0.2
)
