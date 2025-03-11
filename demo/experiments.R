############# Experiments 
#----
# 0) Setup
#----
source("def.R")
set.seed(SEED)
setups <- expand.grid(repl = seq(1, REPL), N = N, tau = TAU)
setups$seed <- round(runif(nrow(setups)) * 1e8)
pkgs <- c("batchtools", "tram")

#----
# 1) Load helper functions & libraries
#----
rpkgs <- sapply(pkgs, require, character.only = TRUE)
if (!all(rpkgs))
  sapply(pkgs[!rpkgs], install.packages)
rpkgs <- sapply(pkgs, require, character.only = TRUE)
if (!all(rpkgs))
  stop("could not attach all required packages")

generate_data <- function(N = 100L, tau = 0, seed = NULL) {
  # N: number of observations in data set
  # tau: effect size of treatment
  # seed: seed to generate data
  
  ### set and re-set seed
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  } 
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  
  ### sample data
  N <- round(N/2)
  W <- gl(2, N)
  X <- rnorm(length(W))
  Y <- tau * as.numeric(W == "2") + 0.5 * X
  d <- data.frame(y = Y, w = W, x = X)
  return(d)
}

fun_lm <- function(instance, ...) {
  m <- lm(y ~ w + x, data = instance)
  coef(m)[["w2"]]
}

fun_Lm <- function(instance, ...) {
  m <- Lm(y ~ w + x, data = instance)
  coef(m, as.lm = TRUE)[["w2"]]
}

methods <- list("lm_wx" = fun_lm,
                "Lm_wx" = fun_Lm)

#-----
# 2) Create study environment (TEST/NO TEST)
# Result: experimental registry
#-----
# Create registry and results directories
if (!dir.exists("registry"))
  dir.create("registry")
if (!dir.exists("results"))
  dir.create("results")
reg <- makeExperimentRegistry(file.dir = file.path("registry", REGISTRY), 
                              packages = pkgs, seed = SEED)

# Specifications
reg$default.resources <- list(
  ntasks = 1L,
  ncpus = 1L,
  nodes = 1L,
  clusters = "serial")
reg$cluster.functions <- makeClusterFunctionsMulticore(CORES)

#----
# 3) Add problem = dataset
#----
fun <- function(job, N, tau, seed, ...) {
  d <- generate_data(N = N, tau = tau, seed = seed)
  return(d)
}

addProblem(PROBNAM, fun = fun, reg = reg)
prob.designs <- list()
prob.designs[[PROBNAM]] <- setups

#-----
# 4) Add algorithms
#-----
algo.designs <- list()
for (method in names(methods)) {
  addAlgorithm(method, fun = methods[[method]], reg = reg)
  algo.designs[[method]] <- data.frame()
}

#----
# 5) add experiments
#----
addExperiments(prob.designs, algo.designs, reg = reg)

#----
# 6) check setup
#----
# check what has been created
summarizeExperiments(reg = reg)
reg <- unwrap(getJobPars(reg = reg))


if (TESTONLY) {
  testJob(1L)
  testJob(nrow(reg))
} else {
  
  #-----
  # 7) submit jobs
  #----
  submitJobs(21:40)
  waitForJobs()
  
  #-----
  # 8) save results
  #----
  res <- ijoin(
    getJobPars(),
    reduceResultsDataTable(fun = function(x) as.list(x))
  )
  res <- unwrap(res, sep = ".")
  
  RESNAME <- paste0("results/res_", current, ".rds")
  saveRDS(res, file = RESNAME)
}
