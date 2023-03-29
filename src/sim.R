#create the simulation

library(MASS)
#
library(tidyr)
library(arm)
#do a rep call


library(glmnet)


# tru_beta <- sapply(rnorm(p), function(x) ifelse(abs(x) < 1.96, 0, x))
# tru_beta 



# y <-   x %*% tru_beta + matrix(rnorm(n), nrow = n)

#needs to be binary output
#needs to have covarianve matrix



sim.norm <- function(n, p, q, corrBlockSize = 50){
    #covariance matrix 
    
    sigma <- matrix(rep(0.7, corrBlockSize*corrBlockSize),
                    corrBlockSize)
    diag(sigma) <- 1
    
    samples <- mvrnorm(n = n,
                       mu = rep(0,p),
                       Sigma = 
                           bdiag(
                               
                               rep(list(sigma), p/corrBlockSize))
    )
    
    
    # p <-   invlogit( samples %*% as.matrix(tru_beta)  
    #                  + matrix(rnorm(n), nrow = n))
    # 
    # # ifelse(p > 0.5, 1, 0)
    # y <- rbinom(n = n, size = 1, prob = p)
    # 
    # q = 6
    tru_beta <- rep(0, p)
    
    #this seems arb
    
    locs <-  seq(1:q)*50 + 1
    
    
    # coeff <- (1/sqrt(3)) * seq(-2.5,2.5 ,length.out = q)
    coeff <- (1/sqrt(3)) * c(-2.5, -1.5, -0.5,
                            0.5, 1.5, 2.5)
    
    tru_beta[locs] <- coeff
    
    
    prob <-   invlogit(
        samples %*% as.matrix(tru_beta) 
                     # + matrix(rnorm(n), nrow = n)
                     )
    
    # y <- ifelse(p > 0.5, 1, 0)
    y <- rbinom(n = n, size = 1, prob = prob)
    
    return(list(x = samples, y = y,
                tru_features = locs
                ))
} 



coeff_confusion <- function(tru_coeffs, active_coeffs){
    #given true 
    v <- active_coeffs %in% tru_coeffs
    u <- tru_coeffs %in% active_coeffs
    
    TP <- sum(v)
    
    FP <- sum(!v)
    # FP <- sum(!u)
    # tru effects incorrectly excluded from
    # the final active set.
    FN <- sum(!u)
    
    return(list(FP = FP,
                
               FN = FN))
    
}

test_coeff_confusion <- function(){
     tru <- 1:8
     lasso <- 2:10
     
     return(coeff_confusion(tru, lasso))
     
}

test_coeff_confusion()

main <- function(){
    #run the simuluator 
    N_iter <- 5 #iterations
    
    p <- 1000
    n <- 100
    set.seed(1000)
    
    #diagnostics
    cnfs <- vector(mode = "list", length = N_iter)
    auc <- rep(NA, N_iter)
    
    
    for (i in seq(1:N_iter)){
        #generate samples
        sim <- sim.norm(n, p , 6)
        #store confusion matrices
        cnfs
        
        #fit lasso
        
        cvlasso <- cv.glmnet(sim$x,
                             as.factor(sim$y),
                             family = "binomial",
                             # type.measure = "class"
                             type.measure = "auc",
                             
                             alpha = 1, #this indicates the lasso part
                             nfolds = 5,
                             #becasume we scaled earlier
                             standardize = F,
                             #should set up the lambda values,
                             keep = TRUE
        )
        
        beta <- coef(cvlasso, s = "lambda.min") 
        #we only care about non-zero
        beta <- beta[beta[,1]!=0,]
        
        #get lasso metrics
        
        #get the a lasso selected cols
        beta_cols <- readr::parse_number(names(beta)[-1])
        
        cnfs[[i]] <-  coeff_confusion(sim$tru_features, 
                                     beta_cols)
        
        #get the cross val error for run with smallest lambda
        
        idmin = match(cvlasso$lambda.min, cvlasso$lambda)
        auc[i] <- cvlasso[["cvm"]][idmin]
        
    }
    return(list(
        confs = cnfs, 
        auc = auc
    ))
}

_i,res <- main()


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
