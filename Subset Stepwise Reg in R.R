###S11: Modern Approaches to Linear Regression: Part 1------------------------
##Required libraries-----------------------------
library("moments")   #skewness, kurtosis and related tests
library("MASS")      #BOx-cox transformations for Linear models
library("leaps")     #Regression Subset Selection
library("glmnet")    #Visualization of Correlation Matrix

##Read Data from File----------------------------
#Hitters data set
# Major League Baseball Data from the 1986 and 1987 seasons

data1 <- read.csv("CS_04.csv", header = T)

##Understanding the Business Question------------
#Sports Analytics
#Moneyball (film)
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

summary(data1)
#Convert categorical variables to factor
data1$League   <- factor(data1$League)
data1$Division <- factor(data1$Division)
data1$NewLeague<- factor(data1$NewLeague)

summary(data1)
head(data1)

#Dealing with Missing Values, in this case salary is just with 59 MVS 
#Analysis of MVs should be done
#It was decided to Remove records with MVs
data2 <- data1[-which(is.na(data1$Salary)),]

#Remove players' name
data2 <- data2[,-1]

head(data2)
dim(data2)
sum(is.na(data2))
summary(data2)

#Continuous variables distribution
par(mar = c(2, 2, 2, 2)) # margins
par(mfrow = c(3, 6))  # 3 rows and 6 columns
for (i in c(1:13, 16:19)) {
  hist(data2[,i], xlab = "", main = paste("Hist. of", names(data2)[i]))
}

par(mfrow = c(1, 1))

#Normal distribution of data is not a requirement for regression.However,
#this might affects the end result.

#hist of salary is skewed because there were few players who earned a lot of
#money. it is also applicable for other variables like assists and putouts

boxplot(data2$Salary, main = "Salary Dist.")

#identify outliers
tukey_ul <- quantile(data2$Salary, probs = 0.75) +1.5*IQR(data2$Salary)
tukey_ul
sum(data2$Salary > tukey_ul)
sum(data2$Salary > tukey_ul)/nrow(data2) # 4% of data2 are outliers

#Correlation Analysis
cor_table <- round(cor(data2[, c(19, 1:13, 16:18)]), 2)
View(cor_table)

corrplot::corrplot(cor_table)

par(mfrow = c(4, 4))  # 4 rows and 4 columns
for (i in c(1:13, 16:18)) {
  plot(data2[,i], data2$Salary, xlab = "", main = paste("Salary vs.", names(data2)[i]))
}

par(mar = c(3, 3, 3, 3))
par(mfrow = c(1, 1))

#The high correlation is shown between variables which is not unacceptable as
#we except a good player with high salary also has a good performance in
#all other variables. However, it can affect the prediction

#Categorical variables
table(data2$League)
tapply(data2$Salary, list(data2$League), mean)

table(data2$Division) # thoughtful difference
tapply(data2$Salary, list(data2$Division), mean)

table(data2$NewLeague)
tapply(data2$Salary, list(data2$NewLeague), mean)

#Divide Dataset into Train and Test--------------check mean and median of variables after division
set.seed(1234)
train_cases <- sample(1:nrow(data2), nrow(data2) * 0.8)
train <- data2[train_cases,]
test  <- data2[- train_cases,]

dim(train)
summary(train)
dim(test)
summary(test)

#train dataset without outliers
trimmed_train <- train[-which(train$Salary > tukey_ul), ]
dim(trimmed_train)
summary(trimmed_train$Salary)

##Building Prediction Model----------------------
#Model1 : Traditional Linear Regression----------
m1 <- lm(Salary ~ ., data = train)
summary(m1)

m1_1 <- lm(Salary ~ AtBat + Hits + Walks + Division + PutOuts, data = train)
summary(m1_1)

#Check Assumptions of Regression
#Normality of residuals
hist(m1_1$residuals, probability = TRUE, breaks = 25)
lines(density(m1_1$residuals), col = "red")

#QQ-plot
qqnorm(m1_1$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m1_1$residuals, col = "red")

#Test for Skewness and Kurtosis
#Good for sample size > 25
#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m1_1$residuals)

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m1_1$residuals)

#Note: Residuals are not Normally Distributed!

#Diagnostic Plots
plot(m1_1)


#Check multicollinearity
car :: vif(m1)
car :: vif(m1_1)

#Conclusion: severe violation of regression assumption
#Bad model!
#high multicollinearity and a small number of significant variables require us 
#to use another approach. If we could be able of running a combination of regressions
#based on another criteria than r-squared(as it increases with the number of predictors),
#so we can have a better decision by considering the estimation of results, for example
#one method is using cross fold variation

#Test the Model----------------------------------
#Model: m5_ridge
#Prediction
pred_m1_1 <- predict(m1_1, test)

#Absolute error mean, median, sd, max, min-------
abs_err_m1_1 <- abs(pred_m1_1 - test$Salary)
mean(abs_err_m1_1)
median(abs_err_m1_1)
sd(abs_err_m1_1)
range(abs_err_m1_1)

#histogram and boxplot
hist(abs_err_m1_1, breaks = 15)
boxplot(abs_err_m1_1)

#Actual VS. Predicted
plot(test$Salary, pred_m1_1, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)


#Model2 : Box-Cox Transformation-----------------
#to remove heteroscedasticity(because of skewed salary),
#one method is to normalize the residuals of model
box_results <- boxcox(Salary ~ ., data = train, lambda = seq(-5, 5, 0.1))               
box_results <- data.frame(box_results$x, box_results$y)            # Create a data frame with the results
lambda <- box_results[which(box_results$box_results.y == max(box_results$box_results.y)), 1]
lambda

#log transformation
train$boxcox_Salary <- log(train$Salary)

m2 <- lm(boxcox_Salary ~ ., data = train[, -which(names(train) == "Salary")])
summary(m2)

m2_1 <- lm(boxcox_Salary ~ AtBat + Hits + Walks, data = train)
summary(m2_1)

#Check Assumptions of Regression
plot(m2_1)
car :: vif(m2_1)

#Two Problems with Model2:
#   Multicollinearity
#   Not use of so many variables

#Model3: Using Best Subset Selection Methods----------
#Best-subset selection aims to find a small subset of predictors, so that 
#the resulting linear model is expected to have the most desirable prediction accuracy.
#Algorithm:
# 1- Let M0 denote the null model , which contains no predictors. 
# 2- For k = 1, 2,...p:
# (a) Fit all Cr(p, k) models that contain exactly k predictors.
# (b) Pick the best among these models, and call it Mk.
#       The best is defined as having the largest R-squared.
#3- Select a single best model from among M0, ..., Mp
#   using cross-validated prediction error, Cp, BIC, or adjusted R
#Best Subset Selection---------------------------
train2 <- train[, -which(names(train) == "Salary")]
m3_bst <- regsubsets (boxcox_Salary ~ ., nvmax = 19, data = train2, method = "exhaustive")
summary(m3_bst)

#Model Selection
#R2 always increases when the number of variables increases.
summary(m3_bst)$rsq

#AdjR2 = 1 - [(1 - R2)(1 - n)/(n - d - 1)]
# n: the number of samples 
# d: the number of predictors
#AdjR2 takes only those independent variables with some significance and penalizes
#you for adding features that are not significant for predicting the dependent variable.
dev.off() #to clear the current window 
plot(summary(m3_bst)$adjr2, xaxt= "n", xlab = "# of Variables", 
     ylab = "AdjR2", type = "b", xlim = c(1, 19)); grid(); 
axis(1, at = 1:19,labels = 1:19)
points(which.max(summary(m3_bst)$adjr2), 
       summary(m3_bst)$adjr2[which.max(summary(m3_bst)$adjr2)],
       col = "red", cex = 2, pch = 20)

#Cp = 1/n * (RSS + 2 * d * sigma_hat ^ 2) --------- much less cp would be ideal
# n: the number of samples 
# RSS: Residual Sum of Squares
# d: the number of predictors
# sigma_hat: estimate of the variance of the error (estimated on a model containing all predictors) 

plot(summary(m3_bst)$cp, xlab = "# of Variables", 
     ylab = "Cp", type = "l", xlim = c(1, 19))
axis(1, at = 1:19,labels = 1:19)
points(which.min(summary(m3_bst)$cp), 
       summary(m3_bst)$cp[which.min(summary(m3_bst)$cp)],
       col = "red", cex = 2, pch = 20)

#BIC (Bayesian Information Criterion ) =  -2 * LogLikelihood  + log(n) * d  ---- more penaltiesthan cp
# n: the number of samples 
# RSS: Residual Sum of Squares
# d: the number of predictors
# sigma_hat: estimate of the variance of the error 

plot(summary(m3_bst)$bic, xlab = "# of Variables", 
     ylab = "BIC", type = "l", xlim = c(1, 19))
axis(1, at = 1:19,labels = 1:19)
points(which.min(summary(m3_bst)$bic), 
       summary(m3_bst)$bic[which.min(summary(m3_bst)$bic)],
       col = "red", cex = 2, pch = 20)

# a model with higher number of variables would be preferable
coef(m3_bst, 11)
# as leap library doesn't use prediction function by default, we temporally use lm with 
#result variables for prediction, later we use design.matrix method to solve this problem
m3_bst_11vs <- lm(boxcox_Salary ~ AtBat + Hits + Walks + Years +
                            CAtBat + CHmRun + CRBI + CWalks + League + 
                            Division + PutOuts, data = train2)
summary(m3_bst_11vs)

#Test the Model----------------------------------
#Model: m3_bst_11vs
#Prediction
pred_m3_bst <- predict(m3_bst_11vs, test)
pred_m3_bst <- exp(pred_m3_bst)

#Absolute error mean, median, sd, max, min-------
abs_err_m3_bst <- abs(pred_m3_bst - test$Salary)
mean(abs_err_m3_bst)
median(abs_err_m3_bst)
sd(abs_err_m3_bst)
range(abs_err_m3_bst)

#histogram and boxplot
hist(abs_err_m3_bst, breaks = 15)
boxplot(abs_err_m3_bst)

#Actual VS Prediction---- not good performance for the highest-paid players
plot(test$Salary, pred_m3_bst, xlab = "Actual", ylab = "prediction")
abline(a = 0, b = 1, col= "red", lwd = 2)

#Forward and Backward Stepwise Selection---------less calculations compared to subset
#Forward Selection--- adds one independent variable at a time to the model
#Algorithm:
# 1- Let M0 denote the null model , which contains no predictors. 
# 2- For k = 0, 2,...p - 1:
# (a) Consider all p ??? k models that augment the predictors in Mk 
#       with one additional predictor.
# (b) Pick the best among these models, and call it Mk+1.
#       The best is defined as having the largest R-squared.
#3- Select a single best model from among M0, ..., Mp
#   using cross-validated prediction error, Cp, BIC, or adjusted R

m3_fwd <- regsubsets (boxcox_Salary ~ . , nvmax = 19, data = train2, method = "forward")

summary(m3_fwd)

which.max(summary(m3_fwd)$adjr2)
which.min(summary(m3_fwd)$cp)
which.min(summary(m3_fwd)$bic)

#backward Selection
#Algorithm:
# 1- Let Mp denote the full model , which contains all predictors. 
# 2- For k = p, p - 1,..., 1:
# (a) Consider all k models that contain all but one of the predictors
#       in  Mk, for a total of k ??? 1 predictors.
# (b) Pick the best among these models, and call it Mk-1.
#       The best is defined as having the largest R-squared.
#3- Select a single best model from among M0, ..., Mp
#   using cross-validated prediction error, Cp, BIC, or adjusted R

m3_bwd <- regsubsets (boxcox_Salary ~ ., nvmax = 19, data = train2, method = "backward")

summary(m3_bwd)

which.max(summary(m3_bwd)$adjr2)
which.min(summary(m3_bwd)$cp)
which.min(summary(m3_bwd)$bic)

# making a comparison of included variables in each model
coef(m3_bst, 11)# CHmRun, CRBI
coef(m3_fwd, 11)# HmRun, CHits
coef(m3_bwd, 11)# same as bst

#Inspect the performance of test set in different models (Do it yourself)

#Model4:Using K-fold Cross-Validation Approach to evaluate model performance---
k <- 10
set.seed(123)
folds <- sample(1:k, nrow(train), rep = TRUE)
cv_errors <- matrix(NA, k, 19, dimnames = list(NULL , paste(1:19)))

#Create prediction function for regsubsets()
#Design matrices in R--- example
d <-data.frame( z = 91:95, x = factor(c(1, 1, 3, 2, 1)), y = 11:15)
d
summary(d)
model.matrix(~ x, data = d)
model.matrix(~ y, data = d)
model.matrix(z ~ ., data = d)

#Prediction function
predict_regsubsets <- function(object, newdata, id) {
                              reg_formula <- as.formula(object$call[[2]]) # object is one of regression models like m3_bst for example: salary~.ass.formula is to transform text syntax to function
                              mat    <- model.matrix(reg_formula, newdata)
                              coef_i <- coef(object, id = id)
                              mat[, names(coef_i)] %*% coef_i #variables in intercepts
}

for(i in 1:k){
      best_fit <- regsubsets(boxcox_Salary ~ ., data = train2[folds != i,], nvmax = 19)
      for(j in 1:19){
        pred <- predict_regsubsets(best_fit , newdata = train2[folds == i,], id = j) #id= number of variables in regression model
        cv_errors[i, j] <- mean((train2$boxcox_Salary[folds == i] - pred) ^ 2) #using RMSE factor for comparison
    }
}

View(cv_errors)
mean_cv_erros <- apply(cv_errors, 2, mean)
mean_cv_erros 
plot(mean_cv_erros, type = "b")
which.min(mean_cv_erros)

m4 <- regsubsets(boxcox_Salary ~ ., data = train2, nvmax = 19)
m4_coef <- coef(m4, 8)

#Test the Model----------------------------------
#Model: m4
#Prediction
test$boxcox_Salary <- log(test$Salary)
test2 <- test[, -which(names(test) == "Salary")]
head(test2)
model_mat <- model.matrix(boxcox_Salary ~ + ., data = test)
pred_m4   <- as.vector(model_mat[,names(m4_coef)]%*% m4_coef)
pred_m4   <- exp(pred_m4)

#Absolute error mean, median, sd, max, min-------
abs_err_m4 <- abs(pred_m4 - test$Salary)
mean(abs_err_m4)
median(abs_err_m4)
sd(abs_err_m4)
range(abs_err_m4)

#histogram and boxplot
hist(abs_err_m4, breaks = 15)
boxplot(abs_err_m4)

#Comparisions of Models--------------------------
df <- data.frame("SimpleLinearReg" = abs_err_m1_1, 
                 "BestSub" = abs_err_m3_bst, 
                 "BestSub.CV" = abs_err_m4)
models_comp <- data.frame("Mean of AbsErrors"   = apply(df, 2, mean),
                          "Median of AbsErrors" = apply(df, 2, median),
                          "SD of AbsErrors"  = apply(df, 2, sd),
                          "Min of AbsErrors" = apply(df, 2, min),
                          "Max of AbsErrors" = apply(df, 2, max))
rownames(models_comp) <- c("SimpleLinearReg", "BestSub", "BestSub.CV")                        
View(models_comp)
save(models_comp, file = "model_comparison.csv")
load("model_comparison.csv")
View(models_comp)

#Boxplot of absolute errors
boxplot(df, main = "Abs. Errors Dist. of Models")

###End of Code###--------------------------------


