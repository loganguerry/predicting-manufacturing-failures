# rm(list = ls())

## potentially useful packages
library(readr)
library(dplyr)
library(randomForest)
library(rpart)
library(caret)
library(xgboost)
library(splines2)
# library(ParallelForest)
library(mlbench)
library(gbm)

# GBM model
# assign data and check 
mydata <- train
head(mydata)

# get model using gbm
gbm.model <- gbm(fail~.,
                 data = mydata, 
                 shrinkage = 0.01, 
                 distribution = 'bernoulli', #used for classification (fail or not fail)
                 cv.folds=4, 
                 n.trees=10000,
                 verbose=F)

#checking for optimal number of iterations
best.iter = gbm.perf(gbm.model, method="cv")
best.iter
summary(gbm.model)

#Visualize the model 
plot.gbm(gbm.model, 1, best.iter) # change number of trees to ~2,200?
plot.gbm(gbm.model, 2, best.iter) 
plot.gbm(gbm.model, 3, best.iter)

#train the model 
mydata <- train
set.seed(710)
fitControl = trainControl(method="cv", number=4, returnResamp = "all")

model2 = train(fail~., 
               data=mydata,
               method="gbm",
               distribution="bernoulli", 
               trControl=fitControl, 
               verbose=F, 
               tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))

model2

# model performance checking via CONFUSION MATRIX 
#entries are percentual average cell counts across resamples 
CM<- confusionMatrix(model2)
CM

# model predictions
mPred <- predict(model2, mydata, na.action = na.pass)
postResample(mPred, mydata$fail)

# confusion matrix and stats 
confusionMatrix(mPred, mydata$fail)