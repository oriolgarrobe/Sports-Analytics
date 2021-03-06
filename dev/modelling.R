###################################################################################################################
################ SCRIPT FOR BUILDING THE PROBABILISTIC CLASSIFIER MODEL FOR SHOT ANALYSIS #########################
###################################################################################################################

### load libraries
library(caret)
library(tidyverse)


### load data
load("dev/anal.Rda")
load("dev/shots.Rda")
summary(anal)


### divide data into training - validation - test sets (while balancing the 2 classes = stratified sampling)
prop.table(table(anal$goal)) # 16.241% goal
train_index = caret::createDataPartition(anal$goal, p = 0.7, list = F) # 70% training
prop.table(table(anal$goal[train_index])) # 16.333% goal

train = anal[train_index,]
test = anal[-train_index,]

### modelling
# possible models: Naive Bayes (our features are NOT independent!!!)
#                  Logistic Regression
#                  k-NN
#                  Random Forest


########################################### LOGISTIC REGRESSION ###########################################
model <- glm(goal ~ .,family=binomial(link='logit'), data = subset(train, select = c(2:23)))
coeff = as.data.frame(summary(model)$coeff)
colnames(coeff) = c('Coefficient', 'a', 'b', 'c')
coeff$Feature = rownames(coeff)
coeff <- coeff %>% mutate(Significant = ifelse(c<0.05, TRUE, FALSE)) %>% filter(Significant) %>% select(Feature, Coefficient)
coeff = coeff[2:9,]

### save coeff dataframe for future use
save(coeff,file="dev/coeff.Rda")

fitted <- predict(model,newdata = subset(test, select = c(2:23)),type='response')


# find out which threshold value is best
thresholds = matrix(nrow=5, ncol = 9) # rows = (accuracy, recall, precision, f1)
thresholds[1,] = seq(0.1,0.9,by = 0.1)

for (i in 1:9) {
  t = thresholds[1,i]
  fitted_bool <- ifelse(fitted > t,1,0)
  
  cm_fitted = table(test$goal, fitted_bool)
  
  thresholds[2,i] = 1-mean(fitted_bool != test$goal)
  thresholds[3,i] = cm_fitted[2,2] / (cm_fitted[2,2] + cm_fitted[2,1])
  thresholds[4,i] = cm_fitted[2,2] / (cm_fitted[2,2] + cm_fitted[1,2])
  thresholds[5,i] = (2 * thresholds[3,i] * thresholds[4,i]) / (thresholds[3,i] + thresholds[4,i])
}

thresholds <- t(thresholds)

thresholds <- as.data.frame(thresholds)
colnames(thresholds) <- c('Threshold', 'Accuracy', 'Recall', 'Precision', 'F1 Score')
thresholds[,2:5] <- round(thresholds[,2:5],4)


fitted_bool <- ifelse(fitted > 0.5,1,0)
misClasificError <- mean(fitted_bool != test$goal)
print(paste('Accuracy',1-misClasificError))
cm_fitted = table(test$goal, fitted_bool)
cm_fitted
recall_fitted = cm_fitted[2,2] / (cm_fitted[2,2] + cm_fitted[2,1])
recall_fitted
precision_fitted = cm_fitted[2,2] / (cm_fitted[2,2] + cm_fitted[1,2])
precision_fitted
f1_fitted = (2 * recall_fitted * precision_fitted) / (recall_fitted + precision_fitted)
f1_fitted

# compare probabilities to statsbomb_xg
# calculate confusion matrix based on statsbomb_xg

statsbomb_xg <- shots %>% select(id, shot.statsbomb_xg)
test$pred_bool <- fitted_bool
test$pred <- fitted
test <- test %>% left_join(statsbomb_xg, by = 'id')
test$shot.statsbomb_xg_bool = ifelse(test$shot.statsbomb_xg > 0.5,1,0)

misClasificError <- mean(test$shot.statsbomb_xg_bool != test$goal)
print(paste('Accuracy',1-misClasificError))

cm_statsbomb = table(test$goal, test$shot.statsbomb_xg_bool)
cm_statsbomb
recall_statsbomb = cm_statsbomb[2,2] / (cm_statsbomb[2,2] + cm_statsbomb[2,1])
recall_statsbomb
precision_statsbomb = cm_statsbomb[2,2] / (cm_statsbomb[2,2] + cm_statsbomb[1,2])
precision_statsbomb
f1_statsbomb = (2 * recall_statsbomb * precision_statsbomb) / (recall_statsbomb + precision_statsbomb)
f1_statsbomb

## on baseline, statsbomb's accuracy is 86.36%, which is higher than logreg (85.89%)
## on baseline, statsbomb's recall is 21.79%, which is lower than logreg (23.72%)
## on baseline, statsbomb's F1 is 34.09%, which is lower than logreg (35.24%)



### create predictions for all shots
fitted_full <- predict(model,newdata = subset(anal, select = c(2:23)),type='response')
fitted_full_bool <- ifelse(fitted_full > 0.5,1,0)

anal$pred_bool <- fitted_full_bool
anal$pred <- fitted_full
anal <- anal %>% left_join(statsbomb_xg, by = 'id')
anal$shot.statsbomb_xg_bool = ifelse(anal$shot.statsbomb_xg > 0.5,1,0)

misClasificError <- mean(anal$pred_bool != anal$goal)
print(paste('Accuracy',1-misClasificError))

### save anal dataframe for future use
save(anal,file="dev/anal.Rda")

### related papers

# logistic regression on all events (not just shots)
# https://science.vu.nl/en/Images/werkstuk-mackay_tcm296-849981.pdf

# Shot xG is only a part of the analysis, but they use far fewer features (we can compare results to them)
# https://www.imperial.ac.uk/media/imperial-college/faculty-of-engineering/computing/public/1718-ug-projects/Corentin-Herbinet-Using-Machine-Learning-techniques-to-predict-the-outcome-of-profressional-football-matches.pdf 

# Full Bayesian approach with priors and stuff
# https://www.researchgate.net/publication/281763177_A_Bayesian_Approach_to_Predicting_Football_Match_Outcomes_Considering_Time_Effect_Weight




### sources
# https://machinelearningmastery.com/probability-metrics-for-imbalanced-classification/















