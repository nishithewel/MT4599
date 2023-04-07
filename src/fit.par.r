fit.models.par <- function(complete_df) {
    complete_df <- loader()

    # scale the numeric columns in the df
    complete_df <- complete_df %>%
        mutate(across(where(is.numeric), scale))
    # creates matrices for glmnets on 3 cases
    xmats <- create_linear_predictor(complete_df)
    col_names <- lapply(xmats, colnames)
    y <- complete_df$y

    # store lasso models in a list
    lasso_models <- list()
    # store tree models in a list
    tree_models <- list()
    # store spike models in a list
    spike_models <- list()



    # turn xmat colnames to numbers for each xmat in xmats
    for (i in seq_along(xmats)) {
        colnames(xmats[[i]]) <- seq(ncol(xmats[[i]]))
    }

    xmats <- lapply(
        xmats,
        function(x) x[, apply(x, 2, function(x) var(x) > 0.01 | var(x) == Inf)]
    )

    for (i in seq_along(xmats)) {
        # fit the lasso model
        cvlasso <- fit.lasso(y = y, x = xmats[[i]])
        lasso_models[[i]] <- cvlasso

        # turn xmat colnames to numbers
        # colnames(xmats[[i]]) <- seq(ncol(xmats[[i]]))

        # fit rff model

        rrf_mod <- fit.rrf(y = y, x = xmats[[i]])
        tree_models[[i]] <- rrf_mod

        spike_mod <- fit.ss(y = y, x = xmats[[3]])
        spike_models <- append(spike_models, spike_mod)
    }

    models <- list(
        lasso = lasso_models,
        tree = tree_models,
        spike = spike_models
    )
    return(models)
}

# N_iter <- 4
# n = 100
# num_preds <- c(500, 1000, 10000)
# q <- 6

numCores <- 4
registerDoParallel(numCores - 1)
# loads packages for clusters
packs <- c(
    "MASS", "Matrix", "arm",
    "glmnet", "caret", "spikeslab", "pROC"
)


model_output <- foreach(
    i = dfs, # change
    .packages = packs
    # .export = ls(globalenv())
    #  c("sim.norm", "fit.models")
) %dopar% {
    fit.models(df)
}

stopImplicitCluster()
