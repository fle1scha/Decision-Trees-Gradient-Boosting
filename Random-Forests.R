### STA4026S - Analytics - CA1
### FLSANT005 - Antony Fleischer

library(caret)
library(randomForest)
library(gbm)
library(ranger)
library(pdp)

setwd("~/Desktop/Analytics/CA1")
parkinsons <- read.csv("Q2dat.csv", header = TRUE)
attach(parkinsons)

### Question 2
## a)
#default bagged tree
set.seed(13)
parkinsons.bag <-randomForest(total_UPDRS ~ ., data = parkinsons, mtry = ncol(parkinsons) - 1, ntree = 500, 
                              importance = TRUE, na.action = na.exclude, do.trace = 50)
parkinsons.bag
save(parkinsons.bag, file = 'parkinsons.bag.Rdata')
##default random forest
set.seed(13)
parkinsons.rf <- randomForest(total_UPDRS ~ ., data = parkinsons, ntree = 500, 
                              importance = TRUE, na.action = na.exclude, do.trace = 50)
parkinsons.rf
save(parkinsons.rf, file = 'parkinsons.rf.Rdata')

#OOB errors vs number of trees
par(mfrow = c(1,1))
plot(parkinsons.bag$mse, type = 'l', xlab = 'Number of trees', ylab = 'OOB MSE', 
     col = 'blue', lwd = 2, ylim = c(0, max(parkinsons.rf$mse)))
lines(parkinsons.rf$mse, col = 'darkgreen', lwd = 2)
legend('bottomleft', legend = c('Bagging', 'Random Forest'), 
       col = c('blue', 'darkgreen'), lwd = 2, lty = c('solid', 'solid'))

parkinsons.bag.mse <- c(parkinsons.bag$mse[100], parkinsons.bag$mse[200], parkinsons.bag$mse[250], parkinsons.bag$mse[300], parkinsons.bag$mse[400], parkinsons.bag$mse[500])
parkinsons.bag.mse #values of OOB MSE for selected bagged tree sizes.
parkinsons.rf.mse <- c(parkinsons.rf$mse[100], parkinsons.rf$mse[200], parkinsons.rf$mse[250], parkinsons.rf$mse[300], parkinsons.rf$mse[400], parkinsons.rf$mse[500])
parkinsons.rf.mse #values of OOB MSE for selected random forest tree sizes. 

#grid search for optimal hyperparameters
set.seed(13)
rf_grid <- expand.grid(mtry = 2:ncol(parkinsons)-1,
                       splitrule = 'variance',
                       min.node.size = c(1, 5, 10)) #Default for regression is 5. Controls tree size.

set.seed(13)
ctrl <- trainControl(method = 'oob', verboseIter = T)
rf_gridsearch <- train(total_UPDRS ~ ., 
                       data = parkinsons,
                       method = 'ranger',
                       num.trees = 250,
                       importance = 'impurity',
                       oob.error = TRUE,
                       verbose = F,
                       trControl = ctrl,
                       tuneGrid = rf_grid)
rf_gridsearch
rf_gridsearch$finalModel #This model will be used

save(rf_gridsearch, file = 'rf_gridsearch_250.Rdata')
load('rf_gridsearch_250.Rdata')


## b)
set.seed(13)
gbm_parkinsons <- gbm(total_UPDRS ~ ., data = parkinsons, 
                     distribution = 'gaussian',
                     n.trees = 5000,
                     bag.fraction = 1, 
                     cv.folds = 5,
                     n.cores = 3,
                     verbose = F)
gbm_parkinsons
save(gbm_parkinsons, file = 'gbm_parkinsons.Rdata')
load('gbm_parkinsons.Rdata')

#CV Errors per tree
best_B <- gbm.perf(gbm_parkinsons, method = 'cv') 

set.seed(13)
ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(1000, 1500, 2000, 3000, 5000),
                        interaction.depth = c(1, 2, 3),
                        shrinkage = c(0.1, 0.01, 0.001),
                        n.minobsinnode = 1)
gbm_gridsearch <- train(total_UPDRS ~ ., data = parkinsons, 
                        method = 'gbm', 
                        distribution = 'gaussian', 
                        trControl = ctrl, 
                        verbose = F,
                        tuneGrid = gbm_grid)
gbm_gridsearch

save(gbm_gridsearch, file = 'gbm_gridsearch.Rdata')
load('gbm_gridsearch.Rdata')

## c)

load('gbm_gridsearch.Rdata')
load('rf_gridsearch_250.Rdata')

rf_gridsearch$finalModel
#plot(varImp(rf_gridsearch))
#plot(rf_gridsearch$finalModel$variable.importance, main = 'Parkinsons variable importance for rf model',
     #ylab = 'Relative influence', xlab = 'Parkinsons data column index', type = 'p')
#colnames <- c(names(parkinsons[1:2]), names(parkinsons[4:19]))
#colnames

summary(gbm_gridsearch, n.trees = 3000, main = 'Parkisons variable importance for gbm model')


plot(gbm_gridsearch$finalModel, i.var = 1, lwd = 2, col = "blue", main = "Partial Dependence Plot for gbm Age", 
  ylab = 'total_UPDRS')
plot(gbm_gridsearch$finalModel, i.var = 17, lwd = 2, col = "blue", main = "Partial Dependence Plot for gbm DFA", 
     ylab = 'total_UPDRS')
plot(gbm_gridsearch$finalModel, i.var = 2, lwd = 2, col = "blue", main = "Partial Dependence Plot for gbm Sex", 
     ylab = 'total_UPDRS')

#plot partial dependence for random forest
sort(rf_gridsearch$finalModel$variable.importance, decreasing = TRUE)

partial_age <- partial(rf_gridsearch$finalModel, pred.var = 'age', train = parkinsons)
plotPartial(partial_age, main = "Partial Dependence Plot for rf Age",  ylab = 'total_UPDRS', lwd = 2)
partial_sex <- partial(rf_gridsearch$finalModel, pred.var = 'sex', train = parkinsons)
plotPartial(partial_sex, main = "Partial Dependence Plot for rf Sex",  ylab = 'total_UPDRS', lwd = 2)
partial_DFA <- partial(rf_gridsearch$finalModel, pred.var = 'DFA', train = parkinsons)
plotPartial(partial_DFA, main = "Partial Dependence Plot for rf DFA",  ylab = 'total_UPDRS', lwd = 2)

## d)
optimal_rf <- rf_gridsearch$finalModel
optimal_gbm <- gbm_gridsearch$finalModel

rf_rmse <- round(sqrt(optimal_rf$prediction.error), 3)
rf_R2 <- round(optimal_rf$r.squared, 3)
gbm_gridsearch
gbm_rmse <- 4.003864
gbm_R2 <- 0.8602104

gbm_rmse
gbm_R2
plot(total_UPDRS, optimal_rf$predictions,  main = 'Observed vs Predicted UPDRS Scores', xlab = 'Observed UPDRS',
     ylab = 'Random Forest UPDRS Predictions',)
abline(0, 1, col = 'red')
residuals <- optimal_rf$predictions - total_UPDRS

plot(scale(residuals), main = "Out-of-Sample Errors", ylab = 'Scaled errors')
abline(0, 0, col = 'blue')

hist(total_UPDRS)
mean(total_UPDRS)

#RMSE for observed and a model that would predict the average
sqrt(mse(total_UPDRS, mean(total_UPDRS)))

## e)
parkinsons.test <- read.csv('Q2testing.csv')
rf_pred <- predict(optimal_rf, data = parkinsons.test)
rf_pred$predictions

write.table(rf_pred$predictions,"flsant005.csv", row.names = FALSE, col.names = FALSE)
