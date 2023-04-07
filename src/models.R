library(glmnet) ###
library(tidyverse)
library(readxl)
# library(spikeSlabGAM)
library(snow)
library(caret)
library(spikeslab)
library(pROC)
source("helper.R")
source("sparsePC.R")







main <- function() {
    # load data
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

    #
    # load(".RData")

    models <- list(
        lasso = lasso_models,
        tree = tree_models,
        spike = spike_models
    )

    # extract selected features from model in models
    for (j in seq_along(models)) {
        for (i in seq_along(models[[j]])) {
            # extract selected features from model in models
            selected_features <- models[[j]][[i]][["selected_features"]]
            # print 1st 5 selected features
            print(selected_features[1:10])

            # map selected features to their names
            selected_features_names <- col_names[[i]][selected_features]
            print(selected_features_names[1:10])
            models[[j]][[i]][["selected_features_names"]] <- selected_features_names
            # save selected features to file
            # saveRDS(selected_features, paste0("data/selected_features_", model, "_", i, ".Rda"))
        }
    }
    models[[1]][[1]]


    # find the intersection of the selected features names for each model
    for (model_type in seq_along(models)) {
        # for a given xmat, find the names choosen by each model
        select_feature_names_list <- lapply()
    }

    selected_feat_by_model <- lapply(names(models), function(x) models[[x]][[3]][["selected_features_names"]])
    # find intersection of selected_fet_by_model
    intersected_features <- Reduce(intersect, selected_feat_by_model)
    intersected_features
    # no intersection exists



    # extract aucs of models into a dataframe
    results <- data.frame(
        case = numeric(),
        model = character(),
        auc = numeric(),
        num_features = numeric()
    )

    for (j in seq_along(models)) {
        for (i in seq_along(models[[j]])) {
            # extract auc from model in models
            auc <- models[[j]][[i]][["auc"]]

            # get length of selected features
            num_features <- length(models[[j]][[i]][["selected_features"]])

            # accuracy <- models[[j]][[i]][["accuracy"]]
            # print auc
            # print(auc)
            # append auc to results
            results <- rbind(
                results,
                data.frame(
                    case = i,
                    model = names(models)[j],
                    auc = auc,
                    num_features = num_features
                )
            )
        }
    }
    # order results by case
    results <- results[order(results$case), ]
    # write results to file
    write.csv(results, "data/results.csv")
}





loader <- function() {
    # load dataset from rds
    load("data.Rda")


    # for (df in imputed_dfs){}

    # keep only complete cases
    df <- imputed_dfs[[1]]

    complete_df <- df[complete.cases(df), ]
    # complete_df <- cbind(y, complete_df)

    complete_df <- complete_df %>%
        # sorts out cols with constant vals
        dplyr::select(where(function(x) length(unique(x)) > 1)) %>%
        # 1 if y is not 0, 0 if not
        mutate(
            y = as.factor(ifelse(y != 0, 1, 0))
        )
    return(complete_df)
}




fit.lasso <- function(y, xmatrix, plot = FALSE) {
    # fits lasso to given
    lasso <- glmnet(
        x = xmatrix,
        y = as.factor(y),
        family = "binomial"
    )


    cvlasso <- cv.glmnet(xmatrix,
        as.factor(y),
        family = "binomial",
        # type.measure = "class"
        type.measure = "auc",
        alpha = 1, # this indicates the lasso part
        nfolds = 5,
        # becasume we scaled earlier
        standardize = T,
        # should set up the lambda values,
        keep = TRUE
    )






    idmin <- match(cvlasso$lambda.min, cvlasso$lambda)
    # # ROC curves
    # rocs <- roc.glmnet(cvlasso$fit.preval, newy = y)
    # best <- idmin
    # plot(rocs[[best]], type = "l")
    # invisible(sapply(rocs, lines, col = "grey"))
    # lines(rocs[[best]], lwd = 2, col = "red")
    # title("ROC for LASSO model")

    auc <- cvlasso[["cvm"]][idmin]

    # get coefficients
    sparse_coef <- coef(cvlasso, s = "lambda.min")
    # view non-zero coefs
    coefs <- sparse_coef[sparse_coef[, 1] != 0, ]

    selected_features <- readr::parse_number(names(coefs)[-1])

    if (plot == T) {

    }

    return(
        list(
            cvlasso = cvlasso,
            auc = auc,
            selected_features = selected_features
        )
    )
}


lasso_diagnostics <- function(cvlasso, plot = FALSE) {
    # get auc
    idmin <- match(cvlasso$lambda.min, cvlasso$lambda)
    auc <- cvlasso[["cvm"]][idmin]

    # get confusion matrix
    cnf <- confusion.glmnet(cvlasso, newx = xmatrix, newy = y)
    #


    return(list(
        auc = auc,
        cnf = cnf
    ))
}

# lasso_diagnostics(lasso_models[[1]])


fit.rrf <- function(y, x, optimise = F) {
    fitControl <- trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 1,
        # returnResamp="all",
        classProbs = TRUE,
        summaryFunction = twoClassSummary,
        # verboseIter = TRUE,
        allowParallel = TRUE
    )
    levels(y) <- c("no_cont", "cont")
    # add grid to conduct cv over

    if (optimise == T) {
        # grid =
    }

    grid <- data.frame(
        mtry = round(sqrt(ncol(x))),
        coefReg = 0.8
    )

    RRF_fit <- train(x, y,
        method = "RRFglobal",
        trControl = fitControl,
        tuneGrid = grid,
        ## This last option is actually one
        ## for gbm() that passes through,
        metrintic = "ROC",
        importance = T
    )

    selected_features_index <- RRF_fit[["finalModel"]][["feaSet"]] # returns numbers
    selected_features <- RRF_fit[["finalModel"]][["xNames"]][selected_features_index] # returns names


    return(list(
        model = RRF_fit,
        selected_features = selected_features,

        # get auc from the model
        auc = RRF_fit[["results"]][["ROC"]]
    ))
}

# unit test for fit.rff
test_fit.rss <- function() {
    # load data
    df <- loader()
    y <- df$y
    x <- df[, -1]

    # fit model
    rrf <- fit.rrf(y, x)

    # check if auc is a number
    expect_true(is.numeric(rrf$auc))

    # check if selected features is a vector
    expect_true(is.vector(rrf$selected_features))

    # check if model is a list
    expect_true(is.list(rrf$model))

    # check if auc is between 0 and 1
    expect_true(rrf$auc >= 0 & rrf$auc <= 1)

    # check if auc is a number
    expect_true(is.numeric(rrf$auc))

    # check if selected features is a vector
    expect_true(is.vector(rrf$selected_features))

    # check if model is a list
    expect_true(is.list(rrf$model))

    # check if auc is between 0 and 1
    expect_true(rrf$auc >= 0 & rrf$auc <= 1)
}


fit.fixedss <- function(y, x, print = FALSE) {
    # check if y is factor using stopifnot
    stopifnot(is.factor(y))

    ss <- fixedSparsePC(
        x = x,
        y = y,
        n.rep = 1,
        # testing
        verbose = print
    )
    # sparsePC.spikeslab()
    # sparsePC(x = x_ss, y = y, n.rep = 3, verbose = T)
    # confusion matrix
    # cnf <- confusionMatrix(yhat.gnet, y)
    # cnf
    # yhat.gnet <- spikeslab::predict(obj, x = x)$yhat.gnet

    # get the selected features

    selected_features <- ss[["gene.signature"]]

    rf <- ss$rf.object

    # yhat.gnet = predict(rf, x = x)

    # calculated roc using rf
    rf.roc <- roc(
        response = y,
        predictor = rf$votes[, 2]
    )
    # plot(rf.roc)
    rf.auc <- auc(rf.roc)
    # get value of auc



    # return the model and the selected features and auc
    return(list(
        model = ss,
        selected_features = selected_features,
        auc = rf.auc[1]
    ))
}
fit.ss <- function(y, x, print = FALSE) {
    # wrapper for spikeslab

    # check if y is factor using stopifnot
    stopifnot(is.factor(y))

    ss <- spikeslab::sparsePC(
        x = x,
        y = y,
        n.rep = 1,
        # testing
        verbose = print,
        parallel = T
    )
    # sparsePC.spikeslab()
    # sparsePC(x = x_ss, y = y, n.rep = 3, verbose = T)
    # confusion matrix
    # cnf <- confusionMatrix(yhat.gnet, y)
    # cnf
    # yhat.gnet <- spikeslab::predict(obj, x = x)$yhat.gnet

    # get the selected features

    selected_features <- ss[["gene.signature"]]

    rf <- ss$rf.object

    # yhat.gnet = predict(rf, x = x)

    # calculated roc using rf
    rf.roc <- roc(
        response = y,
        predictor = rf$votes[, 2]
    )
    # plot(rf.roc)
    rf.auc <- auc(rf.roc)
    # get value of auc



    # return the model and the selected features and auc
    return(list(
        model = ss,
        selected_features = selected_features,
        auc = rf.auc[1]
    ))
}

# unit test for fit.ss
test_fit.ss <- function() {
    n <- 100
    p <- 100
    # create x matrix with normal samples
    x <- matrix(rnorm(n * p), n, p)
    # create y vector with binomial samples
    y <- as.factor(rbinom(n, 1, 0.5))

    # fit the model
    ss <- fit.ss(y, x)


    # check if auc from ss is not null
    stopifnot(!is.null(ss$auc))
    # check if selected features is not null
    stopifnot(!is.null(ss$selected_features))
}

# test_fit.ss()


fit.ssgam <- function(y, x) {
    # ssgam <- spikeSlabGAM(
    #     formula = y ~ .,
    #     data = cbind(y, as.data.frame(x)),
    #     specials = c("lin", "fct"), # limits the model to linear and factor terms
    #     family = "binomial",
    # )
}
