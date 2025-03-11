TESTONLY <- FALSE # only run a limited time test version
SEED <- 1003 # seed for setting everything up

REPL <- 10 # number of replications for each job
CORES <- 3 # number of used cores for parallelization
PROBNAM <- "linear"

N <- 100 # number of samples
TAU <- c(0, 0.5) # assumed effect of W on Y

current <- as.integer(Sys.time()) # current time, basis for saved files
REGISTRY <- paste("registry", current, sep = "_") # registry folder name

