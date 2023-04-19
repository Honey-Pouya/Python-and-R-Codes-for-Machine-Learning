###S13: Boosting Methods in Regression Problems
##Required libraries-----------------------------
library("gbm")
library("xgboost")
library("ggplot2")

#Gradient Descent--------------------------------
#Example 1---------------------------------------

#Objective function f(x) = x ^ 5 - 5 * x ^ 3 + 3 * x + 1
f2 <- function(x) x ^ 5 - 5 * x ^ 3 + 3 * x + 1
curve(f2, -2, 2, n = 200, col = 4); grid()

#Gradient of f2
g2 <- function(x) 5 * x ^ 4 - 15 * x ^ 2 + 3

#Random initialization of x
set.seed(123)
x <- runif(1, -2, 2)

#Step size multiplier as learning rate.
gamma <- 0.005

#the Number of iterations
iter <- 400

#A vector to contain all x's for all steps
all_x <- numeric(length = iter)
all_x

#Gradient descent implementation

for(i in seq_len(iter)){
  x <- x - gamma * g2(x)
  all_x[i] <- x
}

all_x

points(all_x[iter], f2(all_x[iter]), pch = 20, col = 2)
all_x[iter]

#Find the other solution by changing initialization point
set.seed(123)
x <- runif(1, 0, 2)

#Example 2:Calculate Regression Coefficients--------
#Create a synthetic dataset
set.seed(1234)
x <- rnorm(500, mean = 5, sd = 2)
y <- 2 * x + rnorm(500, mean = 0, sd = 2.5)
plot(x, y)

m <- lm(y ~ x)
abline(m, col = "red")
coef(m) #  y = 2x

# we use QR factorization method as R doesn't have gradient descent method
#Gradient Descent implementation
fun_gd <- function(x, y, gamma, conv_threshold, max_iter) {
  n     <- length(x)
  beta0 <- runif(1, 0, 1)
  beta1 <- runif(1, 0, 1)
  yhat  <- beta1 * x + beta0
  MSE   <- c(sum((y - yhat) ^ 2) / n)
  converged  <- FALSE
  iter <- 0
  while(converged == FALSE) {
    ## Implement the gradient descent algorithm
    beta0  <- beta0 - gamma * ((2 / n) * (sum(yhat - y)))
    beta1  <- beta1 - gamma * ((2 / n) * (sum((yhat - y) * x)))
    yhat <- beta1 * x + beta0
    MSE_new <- sum((y - yhat) ^ 2) / n
    MSE <- c(MSE, MSE_new)
    if(MSE[length(MSE)-1] - MSE[length(MSE)] <= conv_threshold) {
      converged <- TRUE
      return(paste("Intercept:", beta0, "Slope:", beta1, "iteration:" , iter))
    }
    iter <- iter + 1
    if(iter > max_iter) { 
      converged <- TRUE
      return(paste("Intercept:", beta0, "Slope:", beta1, "iteration:" , iter))
    }
  }
}

fun_gd(x, y, 0.01, 0.01, 50)
fun_gd(x, y, 0.01, 0.001, 500)
fun_gd(x, y, 0.01,  0.000001, 50000) # better number of iteration as it is closer to real slope
m$coefficients # comparison with lm

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
load("model_comp_S12.R")
View(models_comp)

#Model 10: Generalized Boosted Regression Models(GBM Regression)------------------------
set.seed(123)
#train GBM model 
#the distribution of loss function For most classification problems will be either bernoulli or
#adaboost,For continuous outcomes the choices are gaussian (for minimizing squared error), laplace (for
#minimizing absolute error), and quantile regression (for estimating percentiles of the conditional distribution of the outcome).

m10_gbm_1 <- gbm(formula = log(Salary) ~ .,
                  distribution = "gaussian", #for regression problems
                  data = train,
                  n.trees = 10000, #the total number of trees to fit
                  interaction.depth = 1, #1: stump, the maximum depth of each tree 
                  shrinkage = 0.001, #learning rate
                  cv.folds = 5, #Number of cross-validation folds to perform
                  n.cores = NULL, #will use all cores by default
                  verbose = FALSE)  #printing outcomes in console environment

#get MSE and compute RMSE
min(m10_gbm_1$cv.error) #MSE
sqrt(min(m10_gbm_1$cv.error)) #RMSE

#plot loss function as a result of n trees added to the ensemble
gbm.perf(m10_gbm_1, method = "cv")
#return the standard number of iteration, green color for validation and black for train, in fact
#errors in unseen data for validation are more than errors in seen train data

#Use different parameters
set.seed(123)
m10_gbm_2 <- gbm(formula = log(Salary) ~ .,
                 distribution = "gaussian",
                 data = train,
                 n.trees = 5000,
                 interaction.depth = 3,
                 shrinkage = 0.1,
                 cv.folds = 5,
                 n.cores = NULL, #will use all cores by default
                 verbose = FALSE)  

#get MSE and compute RMSE
sqrt(min(m10_gbm_2$cv.error))

#plot loss function as a result of n trees added to the ensemble
gbm.perf(m10_gbm_2, method = "cv")

#Tuning
#Create hyper-parameter grid
par_grid <- expand.grid(shrinkage = c(0.01, 0.1, 0.3),  #learning rate
                        interaction_depth = c(1, 3, 5), #the maximum depth of each tree
                        n_minobsinnode = c(5, 10, 15),  #the minimum number of observations in the terminal nodes of the trees
                        bag_fraction = c(0.65, 0.8, 1)  #stochastic gradient :bag.fraction < 1
)
View(par_grid)
nrow(par_grid) # 3(number of states for each parameter) power 4 (number of parameters) = 81

#Grid search 
for(i in 1:nrow(par_grid)) {
  set.seed(123)
  
  # train model
  gbm_tune <- gbm(formula = log(Salary) ~ .,
                  distribution = "gaussian",
                  data = train,
                  n.trees = 5000,
                  interaction.depth = par_grid$interaction_depth[i],
                  shrinkage = par_grid$shrinkage[i],
                  n.minobsinnode = par_grid$n_minobsinnode[i],
                  bag.fraction = par_grid$bag_fraction[i],
                  train.fraction = 0.8,
                  cv.folds = 0, #using train validation instead of CV
                  n.cores = NULL, #will use all cores by default
                  verbose = FALSE)  
  #add min training error and trees to grid
  par_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  par_grid$min_RMSE[i]    <- sqrt(min(gbm_tune$valid.error))
}

head(par_grid)
View(par_grid)
par_grid$min_RMSE[which.min(par_grid$min_RMSE)]

#Modify hyper-parameter grid from 5 number of the least min RMSE in grid table
par_grid <- expand.grid(shrinkage = c(0.1, 0.2, 0.3),
                        interaction_depth = c(1, 3, 5),
                        n_minobsinnode = c(5, 10, 15),
                        bag_fraction = c(0.45, 0.55, 0.65)  #stochastic gradient :bag.fraction < 1
)

#Grid search 
for(i in 1:nrow(par_grid)) {
  set.seed(123)
  
  # train model
  gbm_tune <- gbm(formula = log(Salary) ~ .,
                  distribution = "gaussian",
                  data = train,
                  n.trees = 500, # better number based on grid table info(less than 3000)
                  interaction.depth = par_grid$interaction_depth[i],
                  shrinkage = par_grid$shrinkage[i],
                  n.minobsinnode = par_grid$n_minobsinnode[i],
                  bag.fraction = par_grid$bag_fraction[i],
                  train.fraction = 0.8,
                  cv.folds = 0,
                  n.cores = NULL, #will use all cores by default
                  verbose = FALSE)  
  #add min training error and trees to grid
  par_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  par_grid$min_RMSE[i]    <- sqrt(min(gbm_tune$valid.error))
}

head(par_grid)
View(par_grid)
par_grid$min_RMSE[which.min(par_grid$min_RMSE)]
par_grid[42,]

#Final Model
m10_gbm_3 <- gbm(formula = log(Salary) ~ .,
                distribution = "gaussian",
                data = train,
                n.trees = 500,
                interaction.depth = 3,
                shrinkage = 0.3,
                n.minobsinnode = 10,
                bag.fraction = 0.55,
                train.fraction = 0.8,
                cv.folds = 0,
                n.cores = NULL, #will use all cores by default
)  

summary(m10_gbm_3)
#Relative importance:
#the variables with the largest average decrease in MSE are considered

#Test the Model----------------------------------
#Model: m10_gbm_3
#Prediction
pred_m10_gbm <- predict(m10_gbm_3, n.trees = 500, newdata = test)
pred_m10_gbm <- exp(pred_m10_gbm)
pred_m10_gbm
#Absolute error mean, median, sd, max, min-------
abs_err_m10_gbm <- abs(pred_m10_gbm - test$Salary)
mean(abs_err_m10_gbm)
median(abs_err_m10_gbm)
sd(abs_err_m10_gbm)
range(abs_err_m10_gbm)

models_comp <- rbind(models_comp, "GBoostingReg" = c(mean(abs_err_m10_gbm),
                                                 median(abs_err_m10_gbm),
                                                 sd(abs_err_m10_gbm),
                                                 range(abs_err_m10_gbm)))
View(models_comp)
#Actual via Predicted
plot(test$Salary, pred_m10_gbm, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

#Model 11: XGBoost Regression------------------------
x <- model.matrix(log(Salary) ~ + ., data = train)[, -1] #remove intercept
y <- log(train$Salary)

set.seed(123)
m11_xgb_1 <- xgboost(data = x, 
                   label = y,
                   eta = 0.1,            #learning rate
                   lambda = 0,           #regularization term
                   max_depth = 8,        #tree depth 
                   nround = 1000,        #max number of boosting iterations
                   subsample = 0.65,     #percent of training data to sample for each tree
                   objective = "reg:squarederror",  # for regression models not for classification
                   verbose = 0           # silent instead of producing lots of logging output
                   ) 


#train RMSE as this model doesn't have CV
m11_xgb_1$evaluation_log
#plot error vs number trees
ggplot(m11_xgb_1$evaluation_log) +
  geom_line(aes(iter, train_rmse), color = "red") 

#Tuning(train/validation using gboost)
#train and validation sets
set.seed(123)
train_cases <- sample(1:nrow(train), nrow(train)*0.8)
#train data set
train_xgboost <- train[train_cases,]
dim(train_xgboost)

#model matrix
xtrain <- model.matrix(log(Salary) ~ + ., data = train_xgboost)[, -1] #remove intercept
ytrain <- log(train_xgboost$Salary)
  
#validation data set  
validation_xgboost <- train[-train_cases,]
dim(validation_xgboost)  
xvalidation <- model.matrix(log(Salary) ~ + ., data = validation_xgboost)[, -1] #remove intercept
yvalidation <- log(validation_xgboost$Salary)

#create hyper-parameter grid 
par_grid <- expand.grid(eta = c(0.01, 0.05, 0.1, 0.3),
                        labbda = c(0, 1, 2, 5),
                        max_depth = c(1, 3, 5, 7),
                        subsample = c(0.65, 0.8, 1), 
                        colsample_bytree = c(0.8, 0.9, 1))

dim(par_grid)

#grid search
for(i in 1:nrow(par_grid )) {
  set.seed(123)
  
  #train model
  xgb_tune <- xgboost(
    data =  xtrain,
    label = ytrain,
    eta = par_grid$eta[i],
    max_depth = par_grid$max_depth[i],
    subsample = par_grid$subsample[i],
    colsample_bytree = par_grid$colsample_bytree[i],
    nrounds = 1000,
    objective = "reg:squarederror",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # prediction on validation data set
  pred_xgb_validation <- predict(xgb_tune, xvalidation)
  rmse <- sqrt(mean((yvalidation-pred_xgb_validation)^2))
  
  # add validation error
  par_grid$RMSE[i] <- rmse
}


View(par_grid)

#final model
set.seed(123)
m11_xgb_2 <- xgboost(data = x, 
                   label = y,
                   eta = 0.5, #learning rate
                   max_depth = 3, #tree depth 
                   lambda = 0,
                   nround = 1000,
                   colsample_bytree = 1,
                   subsample = 0.8, #percent of training data to sample for each tree
                   objective = "reg:squarederror",  # for regression models
                   verbose = 0               # silent
)

#Test the Model----------------------------------
#Model: m11_xgb_2
x_test <- model.matrix(log(Salary) ~ + ., data = test)[, -1]#remove intercept
pred_m11_xgb <- predict(m11_xgb_2, x_test)
pred_m11_xgb <- exp(pred_m11_xgb)
pred_m11_xgb

#Absolute error mean, median, sd, max, min-------
abs_err_m11_xgb <- abs(pred_m11_xgb - test$Salary)
mean(abs_err_m11_xgb)
median(abs_err_m11_xgb)
sd(abs_err_m11_xgb)
range(abs_err_m11_xgb)


models_comp <- rbind(models_comp, "XGBReg" = c(mean(abs_err_m11_xgb),
                                                   median(abs_err_m11_xgb),
                                                   sd(abs_err_m11_xgb),
                                                   range(abs_err_m11_xgb)))
View(models_comp)

##Actual via Predicted
plot(test$Salary, pred_m11_xgb, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

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
# (j) Use GB and XGB Rgression to analyze the data set. Report the test error obtained.
# (k) Compare the test errors across the all models. Which one is better?
###End of Code###--------------------------------
