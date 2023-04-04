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
