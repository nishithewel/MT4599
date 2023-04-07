library(parallel)
library(foreach)
library(doParallel)
source("models.R")

#loads all the imputed dfs
dfs <- lapply(1:5,function(x) loader(x))
num_cores <- detectCores() - 1


dfs <- lapply(dfs, function(complete_df) complete_df %>%
           mutate(across(where(is.numeric), scale)))
# scale the numeric columns in the df
# complete_df <- complete_df %>%
#     mutate(across(where(is.numeric), scale))
# creates matrices for glmnets on 3 cases
xmats <- lapply(dfs, function(df) create_linear_predictor(df))

y <- dfs[[1]]$y
# models_out <- mclapply(xmats, 
#                       function(df){
#                           # print("t")
#                           fit.models.par(df, y)
#                       }, mc.cores = 5)
# 
# # models_out <- lapply(xmats, function(df) fit.models.par(df,y)
# # )
# 
# # m <- mclapply(1:10000, function(x){sqrt(x)},mc.cores = 5)


# windows implementation


# numCores <- 4
registerDoParallel(num_cores)
# loads packages for clusters
packs <- c(
    "MASS", "Matrix", 
    "glmnet", "caret", "spikeslab", "pROC"
)


model_output <- foreach(
    i = xmats, # change
    .packages = packs
    # .export = ls(globalenv())
    #  c("sim.norm", "fit.models")
) %dopar% {
    fit.models.par(i,y)
}

stopImplicitCluster()

saveRDS(models_out,"data/models_par.Rdata")
