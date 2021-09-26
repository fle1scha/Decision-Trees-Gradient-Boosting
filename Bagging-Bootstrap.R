### STA4026S - Analytics - CA1
### FLSANT005 - Antony Fleischer

library(tree)
library(caret)
setwd("~/Desktop/Analytics/CA1")
mealie <- read.csv("Q1dat.csv", header = TRUE)
attach(mealie)
str(mealie)

### Question 1
#a)

set.seed(13) 
index <- sample(1:nrow(mealie), 0.8*nrow(mealie)) #get 80% sample indices.
train <- mealie[index,]
test <- mealie[-index,]

set.seed(13) 
tree.mealie <- tree(quality ~ ., data = train) #tree with default splitting and stopping criteria.
summary(tree.mealie)
plot(tree.mealie)
text(tree.mealie, pretty=0, cex=0.8)

#b)
set.seed(13) 
tree.mealie.relaxed <- tree(quality~ ., data = train,control = tree.control(nrow(train), minsize = 2, mindev = 0))
summary(tree.mealie.relaxed)
plot(tree.mealie.relaxed)
text(tree.mealie.relaxed, pretty=0, cex=0.5)

#c)
set.seed(13) 
cv.mealie<- cv.tree(tree.mealie.relaxed, FUN=prune.misclass, K = 10)
size <- cv.mealie$size
cv.mealie$size
cv.mealie$dev
plot(size, cv.mealie$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.mealie$k[1] <- 0
alpha <- round(cv.mealie$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.mealie$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
T = cv.mealie$size[which.min(cv.mealie$dev)]
T
pr.mealie <- prune.misclass(tree.mealie.relaxed, best = 24)
plot(pr.mealie)
text(pr.mealie, pretty = 0, cex = 0.5)
summary(pr.mealie)

#d)
set.seed(13)
yhat_default <- predict(tree.mealie, test, type = 'class')
(c_mat <- table(yhat_default, test$quality))
accuracy_default <- sum(diag(c_mat))/nrow(test)*100 #Classification accuracy for default tree
misclass_default <- c("Test Misclassifcation Rate - Default:",round(1 - sum(diag(c_mat))/nrow(test), 3))

set.seed(13)
yhat_homo <- predict(tree.mealie.relaxed, test, type = 'class')
(c_mat <- table(yhat_homo, test$quality))
accuracy_homo <- sum(diag(c_mat))/nrow(test)*100 #Classification accuracy for homogenous tree
misclass_homo <- c("Test Misclassifcation Rate - Homogenous:", round(1 - sum(diag(c_mat))/nrow(test), 3))

set.seed(13)
yhat_prune<- predict(pr.mealie, test, type = 'class')
(c_mat <- table(yhat_prune, test$quality))
accuracy_prune <- sum(diag(c_mat))/nrow(test)*100 #Classification accuracy for homogenous tree
misclass_prune <- c("Test Misclassifcation Rate - Prune:", round(1 - sum(diag(c_mat))/nrow(test), 3))

#Print testing misclassification results
misclass_default
misclass_homo 
misclass_prune

summary(tree.mealie)
summary(tree.mealie.relaxed)
summary(pr.mealie)

#e)
mealie$pests<-ifelse(mealie$pests=="yes",1,0)
mealie$location <- longitude * count
plot(quality, longitude*count)

set.seed(13)
#mealie$quality <- unclass(mealie$quality)
#summary(mealie)
plot(quality, longitude) #mealies at lower longitudes are better quality
plot(quality, -latitude) #mealies at higher latitudes are better quality
plot(quality, height) #height doesn't seem to have much of an effect on quality
plot(quality, pests) #mealies with pests are higher quality
plot(quality, count) #low count mealies are higher quality
plot(quality, longitude*latitude*count) #a plant that is further north and east is on average of higher quality

plot(pests, longitude) 
plot(pests, latitude) #there are more pests at higher latitudes
plot(pests, height)
plot(pests, count) #pests seem to cause lower count per plant

plot(longitude, latitude)
plot(longitude, height)
plot(longitude, count)

plot(latitude, height)
plot(latitude, count)

plot(height, count)

mealie.cor <- data.frame(mealie)
mealie.cor$quality <- as.numeric(mealie.cor$quality)
mealie.cor$pests <- as.numeric(mealie.cor$pests)
cor(mealie.cor)
summary(longitude)
props_data <- table(quality)
props_data <- prop.table(props_data) * 100
props_data
#plot(quality, longitude * latitude) 
mealie$location <- longitude * latitude
#plot(quality, mealie$location)


