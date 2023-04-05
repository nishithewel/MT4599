# create the simulation

library(MASS)
#
library(tidyr)
# library(arm)
library(boot)
# do a rep call
library(glmnet)
library(foreach)
library(doParallel)
source("models.R")




# needs to be binary output
# needs to have covarianve matrix



sim.norm <- function(n, p, q, corrBlockSize = 50) {
    # covariance matrix

    #check if p is at least 1000 using stopif
    stopifnot(p >= 500)


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
    # interaction_locs <-
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

    mat_mul <- samples %*% as.matrix(tru_beta)


    prob <- sapply(
        mat_mul,
        function(x) exp(x) / (1 + exp(x))
    )

    # prob2 <- boot::inv.logit(
    #     samples %*% as.matrix(tru_beta)
    #     # include interaction
    #     # + samples[some_index]*inter_beta
    # )
    # stopifnot(all.equal(prob, prob2))
    # y <- ifelse(p > 0.5, 1, 0)
    y <- rbinom(n = n, size = 1, prob = prob)
    y <- as.factor(y)
    return(list(
        x = samples,
        y = y,
        tru_features = locs
    ))
}

# sim.norm(10, 1000, 6)

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
    FP <- cnfs$FP
    FN <- cnfs$FN
    # return auc, and FP and F
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

fit.models <- function(sim) {
    l <- list()

    cvlasso <- fit.lasso(
        y = sim$y,
        x = sim$x
    )


    cnfs_lasso <- coeff_confusion(
        sim$tru_features,
        cvlasso$selected_features
    )

    l$lasso <- list(
        auc = cvlasso$auc,
        confs = cnfs_lasso,
        model = "lasso"
    )

    dimnames(sim$x) <- list(NULL, paste0(1:ncol(sim$x)))

    rrf <- fit.rrf(
        y = sim$y,
        x = sim$x
    )
    cnfs_rrf <- coeff_confusion(
        sim$tru_features,
        rrf$selected_features
    )

    l$rrf <- list(
        auc = rrf$auc,
        confs = cnfs_rrf,
        model = "rrf"
    )

    # fit ss model
    ss <- fit.ss(
        y = sim$y,
        x = sim$x
    )
    cnfs_ss <- coeff_confusion(
        sim$tru_features,
        ss$selected_features
    )

    l$ss <- list(
        auc = ss$auc,
        confs = cnfs_ss,
        model = "ss"
    )
    return(l)
}

analyse_sim_df <- function(df) {

}


modelout_to_df <- function(model_output) {
    df <- data.frame(
        iter = integer(),
        FP = integer(),
        FN = integer(),
        auc = double(),
        model = character()
    )
    N_iter <- length(model_output)
    for (i in seq(1:N_iter)) {
        for (j in seq_along(model_output[[i]])) {
            # extract the results
            res <- extract_rrf(model_output[[i]][[j]])
            # store the results in a data frame
            df <- rbind(df, data.frame(
                iter = i,
                FP = res$FP,
                FN = res$FN,
                auc = res$auc,
                model = names(model_output[[i]])[j]
            ))
        }
    }
    return(df)
}

# run_sim_parallel <- funct

run_sim_loop <- function() {
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

        # fit ss model
        ss <- fit.ss(
            y = sim$y,
            x = sim$x
        )
        cnfs_ss <- coeff_confusion(
            sim$tru_features,
            ss$selected_features
        )
        df <- df %>% add_row(
            iter = i, FP = cnfs_ss$FP,
            FN = cnfs_ss$FN,
            auc = ss$auc, model = "ss"
        )
    }
    return(df)
}

#vectorise run_sim_loop


run_sim_parallel <- function(
    n = 100, p = 1000, q = 6,
    N_iter = 5, parallel = F) {
    # returns df of sim results
    # run the simuluator
    N_iter <- 2 # 5 # iterations

    N_iter <- 500
    n <- 100
    # num_preds <- c(500, 1000, 10000)
    q <- 6



    sims <- list()
    for (p in c(500, 1000)) {
        sims[[p]] <- lapply(seq(1:N_iter), function(x) {
            sim.norm(n, p, q)
        })
        print(paste("done with", p))
    }
    # sims[[10000]] <- lapply(seq(1:(N_iter/5)), function(x) {
    #     sim.norm(n, p = 10000, q)
    # })
    # print("done with 10000")



    registerDoParallel(numCores - 1) # use multicore, set to the number of our cores

    # loads packages for clusters
    packs <- c("MASS", "Matrix", "arm", "glmnet", "caret", "spikeslab", "pROC")

    try(
        model_output <- foreach(
            i = sims,
            .packages = packs
            # .export = ls(globalenv())
            #  c("sim.norm", "fit.models")
        ) %dopar% {
            # sim <- sim.norm(n = 100, p = 1000, q = 6)
            fit.models(i)
            # length(i)
        },
        silent = TRUE
    )


    stopImplicitCluster()

    df <- modelout_to_df(model_output)

    df$p <- c(rep(500, 500), rep(1000, 500), rep(10000, 100))
    # save df to file as rda
    saveRDS(df, "df.rda")

    # group by model and summarise to get mean and standard deviation
    summarydf <- df %>%
        group_by(p, model) %>%
        summarise(
            FP_mean = mean(FP), FP_sd = sd(FP),
            FN_mean = mean(FN), FN_sd = sd(FN),
            auc_mean = mean(auc), auc_sd = sd(auc)
        )
    # write to csv
    write.csv(, "data/summarydf.csv")


    return(df)
}

# main section but run in global env because of issues with foreach
# main <- function() {



# set.seed(1000)

# # diagnostics

# # call sim.norm n_iter times vectorized
# # store the results in a list

# N_iter <- 500
# n = 100
# # num_preds <- c(500, 1000, 10000)
# q <- 6

# numCores <- 4

# sims <- list()
# for (p in c(500, 1000)) {
#     sims[[p]] <- lapply(seq(1:N_iter), function(x) {
#         sim.norm(n, p, q)
#     })
#     print(paste("done with", p))
# }
# # sims[[10000]] <- lapply(seq(1:(N_iter/5)), function(x) {
# #     sim.norm(n, p = 10000, q)
# # })
# # print("done with 10000")



# registerDoParallel(numCores - 1) # use multicore, set to the number of our cores

# # loads packages for clusters
# packs <- c("MASS", "Matrix", "arm", "glmnet", "caret", "spikeslab", "pROC")

# try(
#     model_output <- foreach(
#         i = sims,
#         .packages = packs
#         # .export = ls(globalenv())
#         #  c("sim.norm", "fit.models")
#     ) %dopar% {
#         # sim <- sim.norm(n = 100, p = 1000, q = 6)
#         fit.models(i)
#         # length(i)
#     },
#     silent = TRUE
# )


# stopImplicitCluster()

# df <- modelout_to_df(model_output)

# df$p  <- c(rep(500, 500), rep(1000, 500), rep(10000, 100))
# # save df to file as rda
# saveRDS(df, "df.rda")

# # group by model and summarise to get mean and standard deviation
# summarydf <- df %>%
#     group_by(p ,model) %>%
#     summarise(
#         FP_mean = mean(FP), FP_sd = sd(FP),
#         FN_mean = mean(FN), FN_sd = sd(FN),
#         auc_mean = mean(auc), auc_sd = sd(auc)
#     )
# # write to csv
# write.csv(<-, "data/summarydf.csv")
