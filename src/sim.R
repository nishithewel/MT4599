# create the simulation

library(MASS)
#
library(tidyr)
library(arm)
# do a rep call
library(glmnet)

source("models.R")


# tru_beta <- sapply(rnorm(p), function(x) ifelse(abs(x) < 1.96, 0, x))
# tru_beta



# y <-   x %*% tru_beta + matrix(rnorm(n), nrow = n)

# needs to be binary output
# needs to have covarianve matrix



sim.norm <- function(n, p, q, corrBlockSize = 50) {
    # covariance matrix

    sigma <- matrix(
        rep(0.7, corrBlockSize * corrBlockSize),
        corrBlockSize
    )
    diag(sigma) <- 1

    samples <- mvrnorm(
        n = n,
        mu = rep(0, p),
        Sigma =
            bdiag(
                rep(list(sigma), p / corrBlockSize)
            )
    )
    # append to samples with interactions

    # columns to be used to create interactions
    interaction_locs <-
        tru_beta <- rep(0, p)

    # this seems arb

    locs <- seq(1:q) * 50 + 1


    # coeff <- (1/sqrt(3)) * seq(-2.5,2.5 ,length.out = q)
    coeff <- (1 / sqrt(3)) * c(
        -2.5, -1.5, -0.5,
        0.5, 1.5, 2.5
    )

    tru_beta[locs] <- coeff

    # add the tru beta coeffs for interactions at the end




    prob <- invlogit(
        samples %*% as.matrix(tru_beta)
        # include interaction
        # + samples[some_index]*inter_beta
    )

    # y <- ifelse(p > 0.5, 1, 0)
    y <- rbinom(n = n, size = 1, prob = prob)
    y <- as.factor(y)
    return(list(
        x = samples,
        y = y,
        tru_features = locs
    ))
}



coeff_confusion <- function(tru_coeffs, active_coeffs) {
    # given true
    v <- active_coeffs %in% tru_coeffs
    u <- tru_coeffs %in% active_coeffs

    TP <- sum(v)

    FP <- sum(!v)
    # FP <- sum(!u)
    # tru effects incorrectly excluded from
    # the final active set.
    FN <- sum(!u)

    return(list(
        FP = FP,
        FN = FN
    ))
}

test_coeff_confusion <- function() {
    tru <- 1:8
    lasso <- 2:10

    return(coeff_confusion(tru, lasso))
}

test_coeff_confusion()


extract_rrf <- function(results) {
    auc <- results$auc


    # get the mean and sd of the FP and FN
    # within results
    cnfs <- results$confs
    FP <- sapply(cnfs, function(x) x$FP)
    FN <- sapply(cnfs, function(x) x$FN)
    # return auc, and FP and FN
    return(list(
        auc = auc,
        FP = FP,
        FN = FN
    ))
}



calc_summary <- function(results) {
    auc <- results$auc


    # get the mean and sd of the FP and FN
    # within results
    cnfs <- results$confs
    FP <- sapply(cnfs, function(x) x$FP)
    FN <- sapply(cnfs, function(x) x$FN)

    auc_mean <- mean(auc)
    auc_sd <- sd(auc)

    FP_mean <- mean(FP)
    FP_sd <- sd(FP)

    FN_mean <- mean(FN)
    FN_sd <- sd(FN)

    return(list(
        auc_mean = auc_mean,
        auc_sd = auc_sd,
        FP_mean = FP_mean,
        FP_sd = FP_sd,
        FN_mean = FN_mean,
        FN_sd = FN_sd
    ))
}



main <- function() {
    # run the simuluator
    N_iter <- 2 # 5 # iterations

    # p <- c(500,1000,10000)
    p <- 1000
    n <- 100
    set.seed(1000)

    # diagnostics

    # create a data frame to store the results
    df <- data.frame(
        iter = integer(),
        FP = integer(),
        FN = integer(),
        auc = double(),
        model = character()
    )



    for (i in seq(1:N_iter)) {
        # generate samples
        sim <- sim.norm(n, p, 6)
        # store confusion matrices
        # cnfs

        cvlasso <- fit.lasso(
            y = sim$y,
            x = sim$x
        )


        cnfs_lasso <- coeff_confusion(
            sim$tru_features,
            cvlasso$selected_features
        )

        df <- df %>% add_row(
            iter = i, FP = cnfs_lasso$FP,
            FN = cnfs_lasso$FN,
            auc = cvlasso$auc, model = "lasso"
        )
        # log <- append(log, list(
        #     iter = i, FP = cnfs_lasso$FP,
        #     FN = cnfs_lasso$FN,
        #     auc = cvlasso$auc, model = "lasso"
        # ))

        # fit rrf

        # adjust input to rrf
        # add colnames
        dimnames(sim$x) <- list(NULL, paste0(1:ncol(sim$x)))

        rrf <- fit.rrf(
            y = sim$y,
            x = sim$x
        )
        cnfs_rrf <- coeff_confusion(
            sim$tru_features,
            rrf$selected_features
        )
        df <- df %>% add_row(
            iter = i, FP = cnfs_rrf$FP,
            FN = cnfs_rrf$FN,
            auc = rrf$auc, model = "rrf"
        )
        # log <- append(log, list(
        #     iter = i, FP = cnfs_lasso$FP,
        #     FN = cnfs_lasso$FN,
        #     auc = cvlasso$auc, model = "rrf"
        # ))

        # fit ssgam
    }
    return(df)
}

res <- main()
# _i,res <- main()


#
#
# cvlasso <- cv.glmnet(sim$x,
#                      as.factor(sim$y),
#                      family = "binomial",
#                      # type.measure = "class"
#                      type.measure = "auc",
#
#                      alpha = 1, #this indicates the lasso part
#                      nfolds = 5,
#                      #becasume we scaled earlier
#                      standardize = F,
#                      #should set up the lambda values,
#                      keep = TRUE
# )
# lambda <- cvlasso$lambda.min
#
# idmin = match(cvlasso$lambda.min, cvlasso$lambda)
# cnf <- confusion.glmnet(cvlasso, newx = sim$x, newy = sim$y) #[[idmin]]
# print(cnf)
#
# plot(cvlasso)
#
# beta <- coef(cvlasso, s = "lambda.min")
# #view non-zero coefs
# beta[beta[,1]!=0,]
#
# # In contrast, glmnet multiplies the first
# # term by a factor of 1/n. So after running glmnet, to
# # extract the beta corresponding to a value lambda, you need to
# beta = coef(cvlasso, s=lambda/n)
#
# library(selectiveInference)
#
# fixedLassoInf(sim$x,
#               as.numeric(sim$y),
#               family = "binomial",
#               beta = beta,
#               lambda = lambda)
#
# #TODO
#
#
# library(spikeSlabGAM)
# lp <- paste(unlist(colnames(x_no_missing)),sep = " ", collapse = " + ")
#
# f1 <- paste("y ~", lp)
#
# f1
#
#
# mcmc <- list(nChains = 8, chainLength = 1000, burnin = 500,
#              + thin = 5)
#
# m <- spikeSlabGAM(formula = formula( y ~ .),
#                   data = cb,
#                   family = "binomial",
#                   mcmc = mcmc)
#
# summary(m)
