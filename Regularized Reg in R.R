###S12: Modern Approaches to Regression Problems: Part 2------------------------
##Required libraries-----------------------------
library("glmnet")        #Lasso and ELastic-Net Regularized GLM
library("rpart")         #Classification and Regression trees
library("rpart.plot")    #Plot decision trees
library("tree")          #same as rpart
library("randomForest")  #Random forests for classification and regression
library("gbm")

##Read Data from File----------------------------
#Hitters data set
# Major League Baseball Data from the 1986 and 1987 seasons

data1 <- read.csv("CS_04.csv", header = T)

##Data inspection--------------------------------
dim(data1)
str(data1)

names(data1)

#AtBat: Number of times at bat in 1986
#Hits:  Number of hits in 1986
#HmRun: Number of home runs in 1986
#Runs:  Number of runs in 1986
#RBI:   Number of runs batted in in 1986
#Walks: Number of walks in 1986
#Years: Number of years in the major leagues
#CAtBat: Number of times at bat during his career
#CHits:  Number of hits during his career
#CHmRun: Number of home runs during his career
#CRuns:  Number of runs during his career
#CRBI:   Number of runs batted in during his career
#CWalks: Number of walks during his career
#League: A factor with levels A and N indicating player's league at the end of 1986
#Division: A factor with levels E and W indicating player's division at the end of 1986
#PutOuts:  Number of put outs in 1986
#Assists:  Number of assists in 1986
#Errors:   Number of errors in 1986
#Salary:   1987 annual salary on opening day in thousands of dollars
#NewLeague: A factor with levels A and N indicating player's league at the beginning of 1987

#Convert categorical variables to factor
data1$League   <- factor(data1$League)
data1$Division <- factor(data1$Division)
data1$NewLeague<- factor(data1$NewLeague)

#Remove records with MVs
data2 <- data1[-which(is.na(data1$Salary)),]

#Remove players' name
data2 <- data2[,-1]

summary(data2)
#Divide Data-set into Train and Test--------------
set.seed(1234)
train_cases <- sample(1:nrow(data2), nrow(data2) * 0.8)
train <- data2[train_cases,]
test  <- data2[- train_cases,]
##Building Prediction Model----------------------
#save from previous session:
load("model_comparison.csv")
View(models_comp)

#Model 5: Ridge Regression------------------------
#Regularization
#Ridge Regression:
#The goal is to optimize:
#   RSS + lambda * Sum( beta_i ^ 2)
#   lambda => 0,  a tuning parameter

#we use model.matrix because glmnet() can only take numerical inputs.
x <- model.matrix(log(Salary) ~ + ., data = train)[, -1] #remove intercept
y <- log(train$Salary)

lambda_ridgereg <- 10 ^ seq(10, -2, length = 100) # a range of small to large numbers
lambda_ridgereg

#Apply ridge regression
#if alpha=0 ridge regression will be applied and and if alpha = 1 lasso regression. 
m5_ridge <- glmnet(x, y, alpha = 0, lambda = lambda_ridgereg)
dim(coef(m5_ridge)) #  19 variables plus one intercept and 100 iteration

plot(m5_ridge, xvar = "lambda")
m5_ridge$lambda [50] # inspect lambda 50 as a sample
coef(m5_ridge)[, 50]

#Cross validation to choose the best lambda
set.seed(1234)
ridge_cv    <- cv.glmnet(x, y, alpha = 0)

#The mean cross validated error for 100 iteration
ridge_cv$cvm
# estimation of the standard error of cvm
ridge_cv$cvsd
#value of lambda that gives minimum error
ridge_cv$lambda.min
best_lambda <- ridge_cv$lambda.min
best_lambda

#Coef. of regression w/ best_lambda
ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(ridge_model)
#Test the Model----------------------------------
#Model: m5_ridge
#Prediction
#Cretae model matrix for test
#if we want a model with intercept we should construct both train and test sets without removing intercept
x_test <- model.matrix(log(Salary) ~ + ., data = test)[, -1]#remove intercept
pred_m5_ridge <- predict(m5_ridge, s = best_lambda, newx = x_test)
pred_m5_ridge
pred_m5_ridge <- exp(pred_m5_ridge)
pred_m5_ridge
#Absolute error mean, median, sd, max, min-------
abs_err_m5_ridge <- abs(pred_m5_ridge - test$Salary)
mean(abs_err_m5_ridge)
median(abs_err_m5_ridge)
sd(abs_err_m5_ridge)
range(abs_err_m5_ridge)

models_comp <- rbind(models_comp, "RidgeReg" = c(mean(abs_err_m5_ridge),
                                                 median(abs_err_m5_ridge),
                                                 sd(abs_err_m5_ridge),
                                                 range(abs_err_m5_ridge)))
View(models_comp)
#Actual VS predicted
plot(test$Salary, pred_m5_ridge, xlab = "Actual", ylab = "prediction")
abline(a = 0, b = 1, col= "red", lwd = 2)

#Why Does Ridge Regression Improve Over Least Squares?
#because of additional 'regularization' parameter.

#Model 6: Lasso Regression------------------------
#Regularization
#Lasso Regression:
#The goal is to optimize:
#   RSS + lambda * Sum(abs(beta_i))
#   lambda => 0,  a tuning parameter

#Apply lasso regression
m6_lasso <- glmnet(x, y, alpha = 1, lambda = lambda_ridgereg)
dim(coef(m6_lasso))

plot(m6_lasso, xvar = "lambda")
m6_lasso$lambda [90]
coef(m6_lasso)[, 90]

#Cross validation to choose the best model
set.seed(1234)
lasso_cv    <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
best_lambda <- lasso_cv $lambda.min
best_lambda

#Coef. of regression w/ best_lambda
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso_model)
#The mean cross validated error for 100 iteration
lasso_cv$cvm
# estimation of the standard error of cvm
lasso_cv$cvsd
#value of lambda that gives minimum error
lasso_cv$lambda.min
best_lambda_lasso <- lasso_cv$lambda.min
best_lambda_lasso

#Test the Model----------------------------------
#Model: m6_lasso
#Prediction
#Cretae model matrix for test
pred_m6_lasso <- predict(m6_lasso, s = best_lambda_lasso, newx = x_test)
pred_m6_lasso <- exp(pred_m6_lasso)
pred_m6_lasso
#Absolute error mean, median, sd, max, min-------
abs_err_m6_lasso <- abs(pred_m6_lasso - test$Salary)
mean(abs_err_m6_lasso)
median(abs_err_m6_lasso)
sd(abs_err_m6_lasso)
range(abs_err_m6_lasso)

models_comp <- rbind(models_comp, "LassoReg" = c(mean(abs_err_m6_lasso),
                                                 median(abs_err_m6_lasso),
                                                 sd(abs_err_m6_lasso),
                                                 range(abs_err_m6_lasso)))
View(models_comp)

#Actual VS predicted
plot(test$Salary, pred_m6_lasso, xlab = "Actual", ylab = "prediction")
abline(a = 0, b = 1, col= "red", lwd = 2)

#Model 7: Decision Trees--------------------------
m7_tree_1 <- tree(log(Salary)~ Years + Hits + League, data = train)
summary(m7_tree_1)
#or
m7_tree_1_rpart <- rpart(log(Salary)~ Years + Hits + League, data = train, cp = 0.1, maxdepth = 3) #cp=complexity of model
summary(m7_tree_1_rpart) 
#root = mean(log(train$Salary)) and mean of years = mean(log(train$Salary[train$Years < 4.5]))

#Plot the tree
#league is not included because it was not recognized as an important variable
plot(m7_tree_1)
text(m7_tree_1)
title("Model: m7_tree_1")
#or
prp(m7_tree_1_rpart)

#change complexity of Tree model by changing cp
m7_tree_2_rpart <- rpart(log(Salary)~ Years + Hits + League, data = train, cp = 0.001, maxdepth = 3)
#Plot the tree
plot(m7_tree_2_rpart)
plotcp(m7_tree_2_rpart)
m7_tree_2_rpart$cptable
m7_tree_2_rpart$cptable[which.min(m7_tree_2_rpart$cptable[ , "xerror"])] #xerror column in cptble

#Prune the tree
pruned_m7_tree_1 <- prune.tree(m7_tree_1, best = 3)
#Plot the pruned tree
plot(pruned_m7_tree_1)
text(pruned_m7_tree_1)
title("This is a pruned tree for m7_tree_1")

#or
pruned_m7_tree_2_rpart <- prune.rpart(m7_tree_2_rpart,
                                      cp = m7_tree_2_rpart$cptable[which.min(m7_tree_2_rpart$cptable[ , "xerror"])])
#Plot the pruned tree
prp(pruned_m7_tree_2_rpart) #one node added in comparison with previous model

#Decision tree model using All variables
m7_tree_2 <- tree(formula = log(Salary) ~ ., data = train)
summary(m7_tree_2)
#or
m7_tree_3_rpart <- rpart(log(Salary) ~ ., data = train, cp = 0.0001, maxdepth = 20) #a large cp in order to have a complex tree to prune later
#Plot the tree
plot(m7_tree_2)
text(m7_tree_2)
title("Model: m7_tree_2")
#or 
prp(m7_tree_3_rpart)

#pruned tree
pruned_m7_tree_3_rpart <- prune.rpart(m7_tree_3_rpart,
                                      cp = m7_tree_3_rpart$cptable[which.min(m7_tree_3_rpart$cptable[ , "xerror"])])

#Plot the pruned tree
prp(pruned_m7_tree_3_rpart)
#prediction
pred_m7_rpart <- predict(pruned_m7_tree_3_rpart, newdata = test)
pred_m7_rpart <- exp(pred_m7_rpart)
pred_m7_rpart 
#Absolute error mean, median, sd, max, min-------
abs_err_pred_m7_rpart <- abs(pred_m7_rpart - test$Salary)
mean(abs_err_pred_m7_rpart)
median(abs_err_pred_m7_rpart)
sd(abs_err_pred_m7_rpart)
range(abs_err_m7_tree)

#Select the best size of tree in tree library via cross validation
set.seed(1234)
tree_cv <- cv.tree(m7_tree_2)

plot(tree_cv$size, tree_cv$dev, type = "b")
tree_cv
which.min(tree_cv$dev)

pruned_m7_tree_2 <- prune.tree(m7_tree_2, best = 4)
pred_m7 <- predict(pruned_m7_tree_2, newdata = test)
pred_m7 <- exp(pred_m7)
pred_m7
#Absolute error mean, median, sd, max, min-------
abs_err_m7_tree <- abs(pred_m7 - test$Salary)
mean(abs_err_m7_tree)
median(abs_err_m7_tree)
sd(abs_err_m7_tree)
range(abs_err_m7_tree)

#adding prediction of rpart because of better MSE
models_comp <- rbind(models_comp, "DecisionTree" = c(mean(abs_err_pred_m7_rpart),
                                                median(abs_err_pred_m7_rpart),
                                                sd(abs_err_pred_m7_rpart),
                                                range(abs_err_m7_tree)))
View(models_comp)
#Actual VS predicted
plot(test$Salary, pred_m7_rpart, xlab = "Actual", ylab = "prediction")
abline(a = 0, b = 1, col= "red", lwd = 2)

#Model 8: Bagging--------------------------------
set.seed(123)
# mtry=number of considered variables, ntree= 500 by defualt
m8_bagging <- randomForest(log(Salary) ~ ., mtry = ncol(train) - 1, ntree = 500, data = train)
m8_bagging

pred_m8_bagging <- predict(m8_bagging, newdata = test)
pred_m8_bagging <- exp(pred_m8_bagging)
pred_m8_bagging
#Absolute error mean, median, sd, max, min-------
abs_err_m8_bagging <- abs(pred_m8_bagging - test$Salary)
mean(abs_err_m8_bagging)
median(abs_err_m8_bagging)
sd(abs_err_m8_bagging)
range(abs_err_m8_bagging)

models_comp <- rbind(models_comp, "Bagging" = c(mean(abs_err_m8_bagging),
                                                median(abs_err_m8_bagging),
                                                sd(abs_err_m8_bagging),
                                                range(abs_err_m8_bagging)))
View(models_comp)
plot(test$Salary, pred_m8_bagging, xlab = "Actual", ylab = "prediction")
abline(a = 0, b = 1, col= "red", lwd = 2)

#Model 9: Random Forrest-------------------------
set.seed(123)
m9_rf <- randomForest(log(Salary) ~ ., data = train, ntree= 500, importance = TRUE)
m9_rf
#mtry(number of variables in random forest)= By default Random forest gets (number of all variables/3) as number of variables in model
importance(m9_rf)
varImpPlot(m9_rf)
#%IncMSE: is based upon the mean decrease of accuracy in predictions on the out of bag samples(samples not selected in bootstrap to estimate error) when a given variable is excluded from the model. 
#IncNodePurity: is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees. (in this model CAtBat and cHits)
#    The node impurity is measured by the training RSS.

#Cross vlidation for feature selection
#Increasing the productivity of the model by changing mtry
#step corresponds to the (integer) number of features to remove at each iteration. 
#If within (0.0, 1.0), then step corresponds to the percentage (rounded down) of features to remove at each iteration.
#Note that the last iteration may remove fewer than step features in order to reach min_features_to_select.
#in this case step 1 is 18 and mtry (1,sqrt(19)), step 2 is round(0.75*19) = 14 and mtry (1,sqrt(14)), ....
#The rfcv function allows to perform recursive feature selection (backward) with cross-validation.
#when recursive is true in each step it measures importance(%IncMSE) and  least important ones will be excluded e.g.in step 2, 19-14 = 5
#when it is false, it only measures importance(%IncMSE) in the first step.
m9_rf_cv <- rfcv(train[, -c(19)], log(train$Salary), cv.fold = 10,step = 0.75,
                 mtry = function(p) max(1, floor(sqrt(p))), recursive = FALSE)
class(m9_rf_cv)
str(m9_rf_cv)

#vector of index of variables used at each step
m9_rf_cv$n.var
#corresponding vector of MSE at each step
m9_rf_cv$error.cv
which.min(m9_rf_cv$error.cv)

#remove 8 variables based on importance
sort(importance(m9_rf)[,1])

#syntax of regression formula based on important features from random forest cross validation
reg_formula <- as.formula(log(Salary) ~ Runs+CHmRun+Walks+Hits+Years+CWalks+AtBat+CRBI
                                        +CRuns+CHits+CAtBat)
reg_formula
#mtry
floor(sqrt(11))

set.seed(123)
m9_rf_2 <- randomForest(reg_formula, data = train, mtry= 3, ntree= 500)
m9_rf_2
#prediction
pred_m9_rf_2 <- predict(m9_rf_2, newdata = test)
pred_m9_rf_2 <- exp(pred_m9_rf_2)
pred_m9_rf_2
#Absolute error mean, median, sd, max, min-------
abs_err_m9_rf_2 <- abs(pred_m9_rf_2 - test$Salary)
mean(abs_err_m9_rf_2)
median(abs_err_m9_rf_2)
sd(abs_err_m9_rf_2)
range(abs_err_m9_rf_2)

models_comp <- rbind(models_comp, "RandomForest" = c(mean(abs_err_m9_rf_2),
                                                   median(abs_err_m9_rf_2),
                                                   sd(abs_err_m9_rf_2),
                                                   range(abs_err_m9_rf_2)))
View(models_comp)

#Actual VS Predicted
plot(test$Salary, pred_m9_rf_2, xlab = "Actual", ylab = "prediction")
abline(a = 0, b = 1, col= "red", lwd = 2)

#save the results
save(models_comp, file = "model_comparison2.csv")
load("model_comparison2.csv")
View(models_comp)
###Assignment------------------------------------
#Try to predict the number of applications received
#  using the other variables in the
#  college.csv data set.

# (a) Do a thorough exploratory analysis on data set
# (b) Split the data set into a training set and a test set.
# (c) Fit a linear model using least squares on the training set, and
#     report the test error obtained.
#     (c-1) select predictors based on t-test results
#     (c-2) select predictors based on step-wise method and k-fold cross validation
# (d) Fit a ridge regression model on the training set, with ?? chosen
#     by cross-validation. Report the test error obtained.
# (e) Fit a lasso model on the training set, with ?? chosen by cross-validation.
#     Report the test error obtained.
# (f) Fit a regression tree to the training set. Report the test error obtained.
# (g) Use the bagging approach in order to analyze this data. Report the test error obtained.
# (h) Use random forests to analyze the data set. Report the test error obtained.
# (j) Compare the test errors across the all models. Which one is better?
###End of Code###--------------------------------
