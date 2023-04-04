library(glmnet) ###
library(tidyverse)
library(readxl)
library(spikeSlabGAM)
library(caret)
library(spikeslab)
library(pROC)

main <- function() {
    # load data
    complete_df <- loader()


   
    # store lasso models in a list
    lasso_models <- list()
    # store tree models in a list
    tree_models <- list()
    # store spike models in a list
    spike_models <- list()

    # creates matrices for glmnets on 3 cases
    xmats <- create_linear_predictor(complete_df)
    y <- complete_df$y

    for (i in 1:length(xmats)) {
        # fit the lasso model
        cvlasso <- fit.lasso(y = y, x = xmats[[i]])
        lasso_models[[i]] <- cvlasso

        # fit the tree model
        tree_mod <- fit.tree(y = y, x = xmats[[i]])
        tree_models[[i]] <- tree_mod

        # fit the spike model, tuning might be required
        spike_mod <- fit.spike(y = y, x = xmats[[i]])
        spike_models[[i]] <- spike_mod
    

    }

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
    # cvlasso$lambda.min


    # par(mfrow = c(1, 2))
    # plot(lasso, xvar = "lambda")

    # abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)

    # plot(cvlasso)

    # abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)
    # abline(v = log(cvlasso$lambda.1se), lwd = 4, lty = 2)






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
        verboseIter = TRUE,
        allowParallel = TRUE
    )
    levels(y) <- c("no_cont", "cont")
    # add grid to conduct cv over

    if (optimise == T) {
        # grid =
    }

    grid <- data.frame(
        mtry = round(sqrt(230)),
        coefReg = 0.8
    )

    RRF_fit <- train(x, y,
        method = "RRFglobal",
        trControl = fitControl,
        tuneGrid = grid,
        ## This last option is actually one
        ## for gbm() that passes through,
        metric = "ROC",
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


fit.ss <- function(y, x, print = FALSE) {
    # check if y is factor using stopifnot
    stopifnot(is.factor(y))

    ss <- sparsePC.spikeslab(
        x = x,
        y = y,
        n.rep = 3,
        # testing
        verbose = print
    )

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


# library(selectiveInference)
#
# lambda <- cvlasso$lambda.min
#
# n <- nrow(xmatrix)
#
# beta <- coef(cvlasso, s = lambda / n)
#
# # run inference
# # maybe we drop the categorical variables here?
# fixedLassoInf(xmatrix,
#     as.numeric(y),
#     family = "binomial",
#     beta = beta,
#     lambda = lambda
# )
# # Error in checkargs.xy(x, y) : x cannot have duplicate columns



# #
#
# mcmc <- list(
#     nChains = 4, chainLength = 1000, burnin = 500,
#     thin = 5
# )
# # test_data <- complete_data[, !sapply(complete_data, function(x) length(levels(x)) == 2)]
#
# lin_pred <- generate_interaction_formula(complete_df %>% select(-y), num_interaction = 1000)
#
# f <- paste("y ~",lin_pred)
#
#
#
# xmatrix <- model.matrix(y ~ .^2,
#                         data = complete_df)[, -1]
#
# lin_pred <- paste(colnames(xmatrix), sep = " ", collapse = " + ")
# f <- paste("y ~",lin_pred)
#
#
# # f <- "y ~ TR.SDG7AffordableCleanEnergy1 + TR.SDG8DecentWorkEconomicGrowth1 + TR.SDG9IndustryInnovationInfrastructure1 + TR.SDG10ReducedInequality1 + TR.SDG11SustainableCitiesCommunities1 + TR.SDG12ResponsibleConsumptionProduction1 + TR.SDG13ClimateAction1 + TR.SDG14LifeBelowWater1 + TR.SDG15LifeonLand1 + TR.SDG16PeaceJusticeStrongInstitutions1 + TR.SDG17PartnershipsAchieveGoal1 + TR.SDG7AffordableCleanEnergy1:TR.SDG8DecentWorkEconomicGrowth1 + TR.SDG7AffordableCleanEnergy1:TR.SDG9IndustryInnovationInfrastructure1 + TR.SDG7AffordableCleanEnergy1:TR.SDG10ReducedInequality1"
#
# xmatrix <- l[["x_pillar_interaction"]]
#
# #for no interactions case# works now
# # lin_pred <- paste(colnames(complete_df %>% select(-y)), sep = " ", collapse = " + ")
#
# interactions <- create_within_interactions(complete_df %>% select(-y), field_df, limit = 500)
#
# # lin_pred <- paste(interactions,
# #                   sep = " ", collapse = " + "
# #                   )
# lin_pred <- interactions
#
# f2 <- paste("y ~", lin_pred )
# # f2
#

#
