###################################################################################################################
################ SCRIPT FOR BUILDING THE PROBABILISTIC CLASSIFIER MODEL FOR SHOT ANALYSIS #########################
###################################################################################################################

### load data
load("dev/anal.Rda")
summary(anal)


### divide data into training - validation - test sets (while balancing the 2 classes)


### modelling
# probable models: Naive Bayes (our features are NOT independent!!)
#                  Logistic Regression
#                  k-NN
#                  Random Forest



### related papers

# logistic regression on all events (not just shots)
# https://science.vu.nl/en/Images/werkstuk-mackay_tcm296-849981.pdf

# Shot xG is only a part of the analysis, but they use far fewer features (we can compare results to them)
# https://www.imperial.ac.uk/media/imperial-college/faculty-of-engineering/computing/public/1718-ug-projects/Corentin-Herbinet-Using-Machine-Learning-techniques-to-predict-the-outcome-of-profressional-football-matches.pdf 

# Full Bayesian approach with priors and stuff
# https://www.researchgate.net/publication/281763177_A_Bayesian_Approach_to_Predicting_Football_Match_Outcomes_Considering_Time_Effect_Weight




### sources
# https://machinelearningmastery.com/probability-metrics-for-imbalanced-classification/