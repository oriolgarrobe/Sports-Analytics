### load data
load("dev/anal.Rda")
summary(anal)


### model data

# library
library(caret)

# Set logical values to numeric in order to avoid issues
data <- anal
data$goal <- as.factor(data$goal)
data$id <- NULL
data$strong_foot <- as.numeric(data$strong_foot)
data$shot.first_time <- as.numeric(data$shot.first_time)
data$under_pressure <- as.numeric(data$under_pressure)
data$home <- as.numeric(data$home)
data$pressure_block <- as.numeric(data$pressure_block)
data$gk_obstacle <- as.numeric(data$gk_obstacle)

### divide data into training - validation - test sets (while balancing the 2 classes = stratified sampling)
prop.table(table(data$goal)) # 16.241% goal
train_index = caret::createDataPartition(data$goal, p = 0.7, list = F) # 70% training
prop.table(table(data$goal[train_index])) # 16.333% goal

train = data[train_index,]
test = data[-train_index,]



####### RANDOM FOREST AND ADABOOST #######

# Libraries
library(mboost)
library(randomForest)


# Fit the random forest model
n.trees <- seq(from = 10, to = 100, by = 10)
me.random <- rep(0,10)
for (e in n.trees) {
  random <- randomForest(formula = goal~., data = train, ntree=e)
  random.pred <- predict(random, newdata = test, type = "class")
  cm.random <- table(random.pred, test$goal)
  me.random[which(e==n.trees)] <- (cm.random[2,1]+cm.random[1,2])/(nrow(test))
}

save(me.random, file="dev/merandom.Rda")

# Fit the Adaboost model
n.trees <- seq(from = 10, to = 100, by = 10)
me.ada <- rep(0,10)
for (i in n.trees) {
  ada <- blackboost(formula = goal~., data = train, family = AdaExp(), control = boost_control(mstop = i))
  ada.pred <- predict(ada, newdata = test, type = "class")
  cm.ada <- table(ada.pred, test$goal)
  me.ada[which(i==n.trees)] <- (cm.ada[2,1]+cm.ada[1,2])/(nrow(test))
}
  
save(me.ada, file="dev/meada.Rda")

# Plot Adaboost and Random forest error rates
plot(n.trees, me.ada, ylim = c(0,0.30), pch = 16, col = "red",
     xlab = "Number of trees", ylab = "Error rates",
     main = "Adaboost Vs. Random forest")
legend("topright",c("Adaboost","R. Forest"),fill=c("red","blue"))
points(n.trees, me.random, pch = 16, col = "blue")

# Final models

# Best model Random Forest with 40 trees
best_random_forest <- randomForest(formula = goal~., data = train, ntree=40)
predicted_random_forest <- predict(best_random_forest, newdata = test, type = "class")
cm_random_forest <- table(test$goal, predicted_random_forest)

# Best model AdaBoost with 30 trees
best_adaboost <- blackboost(formula = goal~., data = train, family = AdaExp(), control = boost_control(mstop = 30))
predicted_adaboost <- predict(best_adaboost, newdata = test, type = "class")
cm_adaboost <- table(test$goal, predicted_adaboost)

# Computing Precision / Recall / F1-Score

# Random forest
recall_random_forest = cm_random_forest[2,2] / (cm_random_forest[2,2] + cm_random_forest[2,1])
recall_random_forest
precision_random_forest = cm_random_forest[2,2] / (cm_random_forest[2,2] + cm_random_forest[1,2])
precision_random_forest
f1_random_forest = (2 * recall_random_forest * precision_random_forest) / (recall_random_forest + precision_random_forest)
f1_random_forest

# AdaBoost
recall_adaboost = cm_adaboost[2,2] / (cm_adaboost[2,2] + cm_adaboost[2,1])
recall_adaboost
precision_adaboost = cm_adaboost[2,2] / (cm_adaboost[2,2] + cm_adaboost[1,2])
precision_adaboost
f1_adaboost = (2 * recall_adaboost * precision_adaboost) / (recall_adaboost + precision_adaboost)
f1_adaboost



##########################################


####### K-Nearest Neighbours ######################

# library
library(kknn)

# Fit the model

# I want to do some CV in order to choose the best number of neighbours
#a1 <- kknn(formula = as.factor(Spam)~., train = train, test = test, k = 30)
#m.train.a1 <- table(a1$fitted.values, train$Spam)
#m.test.a1 <- table(predict(a1), test$Spam)


# Source: https://stats.stackexchange.com/questions/318968/knn-and-k-folding-in-r


library(e1071)
trControl <- trainControl(method  = "cv",
                          number  = 5)

fit <- train(goal~.,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = data) # SHould I use train data here?????

predicted_knn <- predict(fit, newdata = test, type = "raw")
cm_knn <- table(test$goal, predicted_knn)

recall_knn = cm_knn[2,2] / (cm_knn[2,2] + cm_knn[2,1])
recall_knn
precision_knn = cm_knn[2,2] / (cm_knn[2,2] + cm_knn[1,2])
precision_knn
f1_knn = (2 * recall_knn * precision_knn) / (recall_knn + precision_knn)
f1_knn

##########################################
