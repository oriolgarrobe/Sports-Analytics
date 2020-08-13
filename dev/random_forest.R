### load data
load("dev/anal.Rda")
summary(anal)




####### RANDOM FOREST AND ADABOOST #######

# Libraries
library(mboost)
library(randomForest)

# Data modelling
data <- anal
data$goal <- as.factor(data$goal)
data$id <- NULL

#Divide data
n=dim(data)[1]

set.seed(12345)
id=sample(1:n, floor(n*(2/3)))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*(1/3)))
test=data[id2,]

# Fit the Adaboost model
n.trees <- seq(from = 10, to = 100, by = 10)
me.ada <- rep(0,10)
for (i in n.trees) {
  ada <- blackboost(formula = goal~., data = train, family = AdaExp(), control = boost_control(mstop = i))
  ada.pred <- predict(ada, newdata = test, type = "class")
  cm.ada <- table(ada.pred, test$goal)
  me.ada[which(i==n.trees)] <- (cm.ada[2,1]+cm.ada[1,2])/(nrow(test))
}

# Fit the random forest model
me.random <- rep(0,10)
for (e in n.trees) {
  random <- randomForest(formula = goal~., data = train, ntree=e)
  random.pred <- predict(random, newdata = test, type = "class")
  cm.random <- table(random.pred, test$goal)
  me.random[which(e==n.trees)] <- (cm.random[2,1]+cm.random[1,2])/(nrow(test))
}

# Plot Adaboost and Random forest error rates
plot(n.trees, me.ada, ylim = c(0,0.30), pch = 16, col = "red",
     xlab = "Number of trees", ylab = "Error rates",
     main = "Adaboost Vs. Random forest")
legend("topright",c("Adaboost","R. Forest"),fill=c("red","blue"))
points(n.trees, me.random, pch = 16, col = "blue")


##########################################
