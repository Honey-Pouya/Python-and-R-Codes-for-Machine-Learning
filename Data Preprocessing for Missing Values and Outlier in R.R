###S07: Data Preprocessing in R - Part 2
###Set Work Directory----------------------------
setwd("C:/Users/FarzadM/Desktop")
getwd()

###Required Libraries----------------------------
library("moments")
library("MASS")
library("mice")


##Step 4: Data Manipulation----------------------
#Transforming Skewed Data------------------------
#For right-skewed data: square root, cube root, and log
#For left-skewed data:  square root (constant - x), cube root (constant - x), and log (constant - x)
#Box-Cox transformation

#Create a sample data

set.seed(1234)
x <- rgamma(1000, shape = 1, rate = 1)

#Test of Normality
#Histogram
hist(x, probability = T, breaks = 15)
lines(density(x), col = "red")

#QQ-plot
qqnorm(x, main = "QQ Plot", pch = 20)
qqline(x, col = "red")

#Test for Skewness and Kurtosis
#Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(x)

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(x)

#Log Transformation------------------------------
log_x <- log(x)

#Histogram
hist(log_x, probability = T, breaks = 15)
lines(density(log_x), col = "red")

#QQ-plot
qqnorm(log_x, main = "QQ Plot", pch = 20)
qqline(log_x, col = "red")

#Test for Skewness and Kurtosis
#Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(log_x)

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(log_x)

#Box-Cox Transformation--------------------------
#transformed_x = (x ^ lambda - 1) /lambda  if lambda <> 0
#transformed_x =  log(x)                   if lambda = 0
#https://en.wikipedia.org/wiki/Power_transform
box_results <- boxcox(x ~ 1, lambda = seq(-5, 5, 0.1))               
class(box_results)
box_results

box_results <- data.frame(box_results$x, box_results$y)            # Create a data frame with the results

lambda <- box_results[which(box_results$box_results.y == max(box_results$box_results.y)), 1]

boxcox_x <- (x ^ lambda - 1) / lambda

#Histogram
hist(boxcox_x, probability = T, breaks = 15)
lines(density(boxcox_x), col = "red")

#QQ-plot
qqnorm(boxcox_x, main = "QQ Plot", pch = 20)
qqline(boxcox_x, col = "red")

#Test for Skewness and Kurtosis
#Good for sample size > 25

#Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(boxcox_x)

#Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(boxcox_x)

##Step 5: Dealing with Missing Values------------
#MV is just a value for attribute that was not introduced or was lost in
#             the recording process.

#Problems w/ MVs:
#1: reduction of the sample size available for analysis
#2: bias resulting from differences between missing and complete data

#A Four-Step Process for Identifying Missing Data and Applying Remedies

#STEP 1: Determine the type of MVs
#Know the cause

#Ignorable MVs
#     Specific design of the data collection process
#     Censored data

#STEP 2:  Determine the Extent of MVs
#How much MVs are OK? ---> can generally be ignored
#     under 5% - 10% of observations
#     random
#     sufficient data for the selected analysis technique

#STEP 3:  Diagnose the Randomness of the MVs Processes
#     missing at random (MAR) if the missing values of Y depend on X, but not on Y
#     missing completely at random (MCAR)

#STEP 4:  Select the Imputation Method
#Imputation is the process of estimating the missing value
#               based on valid values of other variables and/or cases in the sample.

#MCAR:
#     Imputation Using Only Valid Data
#               Complete Case Approach
#               Using All-Available Data
#     Imputation by Using Replacement Values
#               Using Known Replacement Values
#                   Hot/Cold Deck Imputation
#                   Case Substitution
#               Calculating Replacement Values
#                   Mean/Median Substitution
#                   Regression Imputation
#MAR:
#     Modeling-based Approaches
#               Expectation-Maximization(EM) Algorithm
#               https://www.sicotests.com/psyarticle.asp?id=267

##Example: Correlation Analysis with Missing Data-------------------
###Read Data from File
data <- read.csv("CS_01_02.csv", header = TRUE)
dim(data)
#STEP 1: Know the cause
#STEP 2:  Determine the Extent of MVs
summary(data)
View(data)

data[data == "."] <- NA
#convert columns 2 to 10 to numeric
for (i in 2:10){
  data[, i] <- as.numeric(data[, i])
}

#convert columns 11 to 15 to factor
for (i in 11:15){
  data[, i] <- as.factor(data[, i])
}

summary(data)

#Summary of MVs for Sample
mv_summary_1 <- data.frame("variable_names" = colnames(data))
mv_summary_1$mvs_freq <- apply(data, 2, function(x) sum(is.na(x)))
mv_summary_1$mvs_percent <- round(mv_summary_1$mvs_freq / nrow(data), 3) * 100
View(mv_summary_1)

#Summary of Cases
mv_summary_2 <- as.data.frame(table(apply(data, 1, function(x) sum(is.na(x)))))
colnames(mv_summary_2) <- c("mvs_per_case", "mvs_freq")
mv_summary_2$mvs_percent <- round(mv_summary_2$mvs_freq / nrow(data), 3) * 100
View(mv_summary_2)

#Patterns of Missing Data by Case 
data_w_mvs <- data[apply(data, 1, function(x) any(is.na(x))), ]
View(data_w_mvs)
data_w_mvs$mvs_count <- apply(data_w_mvs, 1, function(x) sum(is.na(x)))

#Decision: remove V1 and cases with 50% mvs
#                         210, 214, 233, 245, 261, 263

data_1 <- data[- which(data$ID %in% c(210, 214, 233, 245, 261, 263)), ]
data_1 <- data_1[, -2]
dim(data_1)
View(data_1)
#repeat step 2 on data_1

#STEP 3: Diagnosis the Randomness of the Missing Data Process
#Assessing the Randomness of Missing Data Through Group Comparisons 
#       of Observations with Missing Versus Valid Data

#Groups Formed by Missing Data on V2

#V3: t-test
mean(data_1$V3[is.na(data_1$V2)], na.rm = TRUE)
mean(data_1$V3[- is.na(data_1$V2)], na.rm = TRUE)

sum(!is.na((data_1$V3[is.na(data_1$V2)])))
sum(!is.na((data_1$V3[- is.na(data_1$V2)])))

t.test(data_1$V3[is.na(data_1$V2)],
       data_1$V3[- is.na(data_1$V2)],
       alternative = "two.sided")

#t-test significance 
#Number of cases (valid data) 
#Number of cases (missing data) 
#Mean of cases (valid data) 
#Mean cases (missing data) 

#V10:
data_1$ifV2mv <- ifelse(is.na(data_1$V2), 1, 0)
table(data_1$V10, data_1$ifV2mv)

#Chi-Square Test
#H0: MVs in V2 is independent of V10
#H1: MVs in V2 is related to V10
#If p-value < 0.05 reject H0
chisq.test(table(data_1$V10, data_1$ifV2mv))

#Little's test for MCAR (Little 1988) 
#Required Library: BaylorEdPsych

#A complete analysis indicates that although significant differences 
#     can be found due to the missing data on one variable (V2)
#     the effects are limited to only this variable, making it of marginal concern.

#STEP 4:  Select the Imputation Method
#Method 1: Complete Case Approach
data_1_comp <- data_1[apply(data_1, 1, function(x) any(is.na(x))) == F, ] 
View(data_1_comp)

#mean
mean_imput_comp <- data.frame(apply(data_1_comp[2:9], 2, mean))
colnames(mean_imput_comp) <- "all_case"
mean_imput_comp 

#sd
sd_imput_comp   <- data.frame(apply(data_1_comp[2:9], 2, sd))
colnames(sd_imput_comp) <- "all_case"
sd_imput_comp 

#Method 2: Mean Substitution
data_1_mean_sub <- data_1

for (i in 2 : 9){
  data_1_mean_sub[is.na(data_1_mean_sub[ ,i]),i] <- mean(data_1_mean_sub[ ,i], na.rm = T)
}

#mean
mean_imput_comp$mean_sub <- apply(data_1_mean_sub[2:9], 2, mean)
mean_imput_comp 

#sd
sd_imput_comp$mean_sub   <- apply(data_1_mean_sub[2:9], 2, sd)
sd_imput_comp 

#Method 3: Regression
data_1_reg <- data_1
imp_res <- mice(data_1_reg[, 2:9], method = "norm.predict", m = 5)
data_1_reg[, 2:9] <- complete(imp_res)              

#mean
mean_imput_comp$reg <- apply(data_1_reg[2:9], 2, mean)
mean_imput_comp 

#sd
sd_imput_comp$reg   <- apply(data_1_reg[2:9], 2, sd)
sd_imput_comp 

#STEP 5: Correlation Analysis
#Method 1: Complete Case Approach
View(cor(data_1_comp[2:9]))

#Method 2: Mean Substitution
View(cor(data_1_mean_sub[2:9]))

#Method 3: Regression
View(cor(data_1_reg[2:9]))

#Summary
#   The missing data process is MCAR
#   Imputation is the most logical course of action
#   Imputed correlations differ across techniques


##Step 6: Dealing with Outliers------------------

#Problems w/ Outliers:
#1: can have a marked effect on any type of empirical analysis
#2: how representative the outlier is of the population

#source of their uniqueness:
#   procedural error
#   extraordinary event
#   extraordinary observations
#   unique in combination of values across the variables

#Outlier detection-------------------------------
#Problem of Masking------------------------------

x <- c(2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 1000)
y <- c(2, 2, 3, 3, 3, 4, 4, 4, 10000, 100000)

#Classic method for outlier detection
#(x - mean)/ sd > 3

(x - mean(x))/ sd(x) > 3
(y - mean(y))/ sd(y) > 3

#Univariate Detection----------------------------

#Tukey Method

# x > q(0.75) + 1.5 * IQR(x)
# x < q(0.75) - 1.5 * IQR(x)
boxplot(mtcars$hp)

boxplot(x)
boxplot(y)

#MAD-Median Rule
#abs(x - median)/ MAD > 2.5
#Leys et al. (2013): Detecting Outliers - Journal of Experimental Social Psychology

#median absolute deviation (MAD) = 1.4826 * abs(x - median(x))
abs(x - median(x))/mad(x) > 2.5
abs(y - median(y))/mad(y) > 2.5

#Multivariate Detection--------------------------
#Mahalanobis D2
#https://en.wikipedia.org/wiki/Mahalanobis_distance

set.seed(123)
n <-100
#covariance matrix
sigma <-matrix(c(1.0, 0.8 , 0.5 , 0.2 ,
                 0.8, 1.0 , 0.05, 0.05,
                 0.5, 0.05, 1.0 , 0.05,
                 0.2, 0.05, 0.05, 1.0), ncol = 4)
#multivariate normal distribution
x <-mvrnorm(n, sigma, mu = c(10, 10, 10, 10))
x <- data.frame(x)
head(x)
summary(x)

#creat some outliers
outliers <- c(12, 25, 37, 79, 88)
x[outliers, ] <- x[outliers, ] + 
                  c(3.5 * sd(x[ ,1]),
                    3.5 * sd(x[ ,2]),
                    3.5 * sd(x[ ,3]),
                    3.5 * sd(x[ ,4]))

summary(x)
boxplot(x)

mah_d2 <- mahalanobis(x, colMeans(x), sigma)
which(mah_d2 > 10) #consult with related literature for cutoff point

#Treating the outliers
# Imputation
# Capping

###Assignment------------------------------------
#Q1: Complete step 2 from missing value example
#Q2: Complete step 3 from missing value example

###End of Code###--------------------------------