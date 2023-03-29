library(glmnet) ###
library(tidyverse)


# load dataset from rds
load("data.Rda")

# keep only complete cases
df <- dfs[[1]]

complete_df <- df[complete.cases(df), ]
complete_df <- cbind(y, complete_df)

complete_df <- complete_df %>%
    # 1 if y is not 0, 0 if not
    mutate(y = ifelse(y != 0, 1, 0))

# write a function to extractr "TR.Policy" from "reported_TR.Policy"
grepl("TR.+", colnames(complete_df))



xmatrix <- model.matrix(y ~ .^2, data = complete_df)[, -1]




# xmatrix <- model.matrix(y, data = complete_data)[,-1]

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
cvlasso$lambda.min


par(mfrow = c(1, 2))
plot(lasso, xvar = "lambda")

abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)

plot(cvlasso)

abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)


summary(cvlasso)

# cvlasso$lambda.min

outcome <- coef(cvlasso, s = "lambda.min")
# view non-zero coefs
outcome[outcome[, 1] != 0, ]



cnf <- confusion.glmnet(cvlasso, newx = xmatrix, newy = y)
print(cnf)




idmin <- match(cvlasso$lambda.min, cvlasso$lambda)
# ROC curves
rocs <- roc.glmnet(cvlasso$fit.preval, newy = y)
best <- idmin
plot(rocs[[best]], type = "l")
invisible(sapply(rocs, lines, col = "grey"))
lines(rocs[[best]], lwd = 2, col = "red")
title("ROC for LASSO model")

auc <- cvlasso[["cvm"]][idmin]
auc

library(selectiveInference)
# inference step
# estimate sigm
# estimateSigma(xmatrix, )
# get teh beta
lambda <- cvlasso$lambda.min

n <- nrow(xmatrix)

beta <- coef(cvlasso, s = lambda / n)

# run inference
# maybe we drop the categorical variables here?
fixedLassoInf(xmatrix,
    as.numeric(y),
    family = "binomial",
    beta = beta,
    lambda = lambda
)
# Error in checkargs.xy(x, y) : x cannot have duplicate columns


library(spikeSlabGAM)
# lp <- paste(unlist(colnames(x_no_missing)),sep = " ", collapse = " + ")
#
# f1 <- paste("y ~", lp)
#
# f1

view(f)
mcmc <- list(
    nChains = 4, chainLength = 1000, burnin = 500,
    thin = 5
)
test_data <- complete_data[, !sapply(complete_data, function(x) length(levels(x)) == 2)]

m <- spikeSlabGAM(
    formula = fct(y) ~ TR.PolicyChildLabor *
        TR.EmbryonicStemCellResearch + TR.PolicyChildLabor * TR.RetailingResponsibility +
        TR.PolicyChildLabor * TR.Alcohol + TR.PolicyChildLabor *
            TR.Gambling + TR.PolicyChildLabor * TR.Tobacco + TR.PolicyChildLabor *
            TR.Armaments + TR.PolicyChildLabor * TR.Pornography + TR.PolicyChildLabor *
            TR.Contraceptives + TR.PolicyChildLabor * TR.ObesityRisk +
        TR.PolicyChildLabor * TR.ProductRecall + TR.PolicyChildLabor *
            TR.TargetsDiversityOpportunity + TR.PolicyChildLabor * TR.WomenEmployees +
        TR.PolicyChildLabor * TR.WomenManagers + TR.PolicyChildLabor *
            TR.FlexibleWorkingHours + TR.PolicyChildLabor * TR.DayCareServices +
        TR.PolicyChildLabor * TR.HIVAIDSProgram + TR.PolicyChildLabor *
            TR.PolicySkillsTraining + TR.PolicyChildLabor * TR.PolicyCareerDevelopment +
        TR.PolicyChildLabor * TR.InternalPromotion + TR.PolicyChildLabor *
            TR.MgtTraining,
    data = test_data,
    family = "binomial",
    mcmc = mcmc
)

summary(m)
