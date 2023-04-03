
x <- xmats[["x_no_interaction"]]


RRF_fit <- fit.rrf(x, y)
summary(RRF_fit)

ggplot(RRF_fit)
varImp(RRF_fit)

pred <- predict(RRF_fit, newdata = x)

confusionMatrix(data = pred, y)
# Choose the features and classes
# data(PimaIndiansDiabetes2)
# x <- PimaIndiansDiabetes2[c("age","glucose","insulin","mass","pedigree","pregnant","pressure","triceps")]
# y <- PimaIndiansDiabetes2$diabetes




# grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
# x = xmats[["x_no_interaction"]]
# y = complete_df$y
#
# mdl<- train(x=x,y=y,tuneGrid=grid,trControl=fitControl,method="C5.0",verbose=FALSE)
#
# mdl
#
# # visualize the resample distributions
# xyplot(mdl,type = c("g", "p", "smooth"))
