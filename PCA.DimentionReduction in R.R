####S19: PCA-------------------------------------
###Set work directory----------------------------
setwd("C:/Users/FarzadM/Desktop")
###Required Libraries----------------------------
library("ggplot2")    #Create Elegant Data Visualizations 
library("corrplot")   #for Visualization of a Correlation Matrix 
library("pls")        #for Principal Component Regression
###Case 1: Perceptual Map------------------------
###Business Understanding------------------------
###Read Data from File---------------------------
data <- read.csv("CS_09.csv", header = T)
dim(data)
head(data)

#perform  Brand has strong performance
#leader   Brand is a leader in the ???eld
#latest   Brand has the latest products
#fun      Brand is fun
#serious  Brand is serious
#bargain  Brand products are a bargain
#value    Brand products are a good value
#trendy   Brand is trendy
#rebuy    I would buy from Brand again
###Data Inspection-------------------------------
summary(data)
data$brand <- as.factor(data$brand)
str(data)

###Data Preparation------------------------------
#Scale data
data_sc <- data
data_sc[, 1:9] <- scale(data[, 1:9])
summary(data_sc)
###Data Analysis---------------------------------
corr_table <- round(cor(data_sc[, 1:9]), 2)
view(corr_table)
corrplot:: corrplot(corr_table, order = "hclust")

##Descriptive Analysis on Brands-----------------
brand_mean <- aggregate(data_sc[, 1:9], list(data_sc$brand), mean)
View(brand_mean)
#brand (a) has high fun and (b) is really serious and ....

#use brand for the row names
rownames(brand_mean) <- brand_mean[, 1] 
brand_mean <- brand_mean[, -1] 
brand_mean

#prepare data for heatmap in ggplot2
brand_mean_df <- data.frame(brand = rep(rownames(brand_mean), 9),
                            var   = rep(names(brand_mean), each = 10),
                            mean_value  = unlist(brand_mean))

rownames(brand_mean_df) <- NULL
brand_mean_df

#Plot heat map
hm_plot <- ggplot(data = brand_mean_df, aes(x = brand, y = var, fill = mean_value)) +
              geom_tile() +
              scale_fill_gradient(low = "yellow", high = "dark green") 
hm_plot
#e.g.brand(c) is leader and serious in the market

##PCA Analysis-----------------------------------
pca_res <- prcomp(data_sc[, 1:9])
summary(pca_res)
#PC1 explains 33% of total variance. 76% of total variance can be explained by 4 first PCAs.

#plot the results
plot(summary(pca_res)$importance[2,], 
     xaxt = 'n', 
     yaxt = 'n', 
     ylab = 'Proportion of Variance',
     xlab = '# of Components')
axis(1, at = 1 : 9, labels = 1 : 9)
axis(2, at = seq(0, 0.35, 0.05), labels = seq(0, 0.35, 0.05))
#

#A bi-plot of the first two principal components
biplot(pca_res)

#four regions in biplot: 
#   leadership (serious, leader, and perform in the upper right), 
#   value (rebuy,value, and bargain), 
#   trendiness (trendy and latest), and 
#   fun 

##Perceptual Map for Brands----------------------
#PCA using aggregated ratings by brand
brand_mean_pca <- prcomp(brand_mean, scale = TRUE)
summary(brand_mean_pca)

biplot(brand_mean_pca, main = "Brand Positioning", cex = c(1.5, 1))

#What does the map tell us? 
#Suppose that you are the brand manager for brand e.
#What does the map tell you? 
#What should you do about the position of your brand e? 
#   two possible solutions(blue and red ocean):
#           follow others
#           fill a gap between PCAs: value leader

#Cautions with Perceptual Maps:
#   The relationships are strictly relative to the product category and the brands
#         and adjectives that are tested
#   Assess the sensitivity of PCA by running it on several subsets of data   
#   The strength of a brand on a single adjective cannot be read directly from the chart
hm_plot
# b vs. c vs. d on latest

###Case 2: Principal Component Regression--------
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
cat_var <- c("League", "Division", "NewLeague")
data1[, cat_var] <- lapply(data1[, cat_var], factor)

summary(data1)
head(data1)

#Dealing w/ MVs
#Analysis of MVs should be done
#Remove records with MVs
data2 <- data1[-which(is.na(data1$Salary)), ]

#Remove players' name
data2 <- data2[,-1]

#Calculate Log Salary
data2$Log_Salary <- log(data2$Salary)

head(data2)
dim(data2)
sum(is.na(data2))
summary(data2)

#Divide Dataset into Train and Test--------------
set.seed(1234)
train_cases <- sample(1:nrow(data2), nrow(data2) * 0.8)
train <- data2[train_cases,]
test  <- data2[- train_cases,]

dim(train)
summary(train)
dim(test)
summary(test)

#Train dataset w/o outliers
boxplot(data2$Salary, main = "Salary Dist.")

#Identify outliers 
tukey_ul <- quantile(data2$Salary, probs = 0.75) + 1.5 * IQR(data2$Salary)
tukey_ul 
sum(data2$Salary > tukey_ul) / dim(data2)[1]
# 4% of total data

trimmed_train <- train[- which(train$Salary > tukey_ul), ]
dim(trimmed_train)
summary(trimmed_train$Salary)

##Building Prediction Model----------------------
set.seed(1234)
pcr_1 <- pcr(Log_Salary ~. - Salary, data = train, scale = TRUE, validation = "CV")
summary(pcr_1)
pcr_1$coefficients
pcr_1$scores
head(train)

score_mat <- as.matrix(pcr_1$scores)
cor(score_mat[, c(1, 2, 3)])
#low correlation 

#Choose the Best # of Components
validationplot(pcr_1, val.type = "RMSEP") #cross validation
pls_RMSEP <- RMSEP(pcr_1, estimate = "CV")
pls_RMSEP
min_comp <- which.min(pls_RMSEP$val) - 1 #without intercept
min_comp
points(min_comp, min(pls_RMSEP$val), pch = 1, col = "red", cex = 1.5)

#Test the Model----------------------------------
#Model: pcr
#Prediction
pred_pcr <- predict(pcr_1, test, ncomp = 8)
pred_pcr
pred_pcr <- exp(pred_pcr)
pred_pcr
#Absolute error mean, median, sd, max, min-------
abs_err_pcr <- abs(pred_pcr - test$Salary)
mean(abs_err_pcr)
median(abs_err_pcr)
sd(abs_err_pcr)
range(abs_err_pcr)

#histogram and boxplot
boxplot(abs_err_pcr)

#Actual vs. Predicted
plot(test$Salary, pred_pcr, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

#trimmed_train
set.seed(123)
pcr_2 <- pcr(Log_Salary ~. - Salary, data = trimmed_train, scale = TRUE, validation = "CV")
#Choose the Best # of Components
validationplot(pcr_2, val.type = "RMSEP")
pls_RMSEP <- RMSEP(pcr_2, estimate = "CV")
pls_RMSEP
min_comp <- which.min(pls_RMSEP$val) - 1
min_comp
points(min_comp, min(pls_RMSEP$val), pch = 1, col = "red", cex = 1.5)

#Test the Model----------------------------------
#Model: pcr
#Prediction
pred_pcr_2 <- predict(pcr_2, test, ncomp = 8)
pred_pcr_2
pred_pcr_2 <- exp(pred_pcr_2)
pred_pcr_2
#Absolute error mean, median, sd, max, min-------
abs_err_pcr_2 <- abs(pred_pcr_2 - test$Salary)
mean(abs_err_pcr_2)
median(abs_err_pcr_2)
sd(abs_err_pcr_2)
range(abs_err_pcr_2)

#Actual vs. Predicted
plot(test$Salary, pred_pcr_2, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
###End of the Code-------------------------------
