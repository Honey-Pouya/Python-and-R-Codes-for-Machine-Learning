###S15: MLE & Logistic Regression----------------
##Logistic Regression Example--------------------
##Required libraries-----------------------------
#No library required
##Business Understanding-------------------------
# Goal: to analyzing the effect of bundling strategy
##Read data from file----------------------------
data <- read.csv("CS_06.csv", header = TRUE)
names(data)
dim(data)
head(data)

##Data inspection--------------------------------
str(data)

data$Channel     <- factor(data$Channel, levels = c("Mail", "InPerson", "Email"))
data$IFBundle    <- factor(data$IFBundle, levels = c("NoBundle", "Bundle"))
data$IFUseCoupon <- factor(data$IFUseCoupon, levels = c("No", "Yes"))
summary(data)

table(data$IFUseCoupon, data$Channel)
table(data$IFUseCoupon, data$IFBundle)
table(data$Channel, data$IFBundle)
table(data$Channel, data$IFBundle, data$IFUseCoupon)

##Cross Tabulation Analysis----------------------
cross_tab <- table(data$IFBundle, data$IFUseCoupon)
cross_tab

margin.table(cross_tab, 1) #over rows
margin.table(cross_tab, 2) #over columns

prop.table(cross_tab)
prop.table(cross_tab, 1) #over rows
prop.table(cross_tab, 2) #over columns

#Chi-Square Test
#H0: IFUseCoupon is independent of IFBundle
#H1: IFUseCoupon is related to IFBundle
#If p-value < 0.05 reject H0
chisq.test(cross_tab)

##General Linear Model (GLM)---------------------
##model1-----------------------------------------
m1 <- glm(IFUseCoupon ~ IFBundle, data = data, family = "binomial")
summary(m1)

#Overall fit-------------------------------------
#deviance statistic
#H0: the model is not better than chance at predicting the outcome

#The null deviance tells us how well the response variable can be predicted by 
#a model with only an intercept term(without any predictors)
#The residual deviance shows how well the response is predicted by the model 
#when the predictors are included.

#calculating Chi Square P-Value
modelChi1 <- m1$null.deviance - m1$deviance
Chidf1    <- m1$df.null - m1$df.residual
Chisq_prob1 <- 1 - pchisq(modelChi1, Chidf1)
Chisq_prob1

#AIC (Akaike Information Criterion)
#AIC: 4349.4

#Assessing the contribution of predictors--------
#Wald test

#Interpretation of coefficient-------------------
#log(odds) = beta0 + beta1 * x
#bet1 = 0.38879
exp(0.38879) #odds
#the bundle increases the purchase likelihood by 47.5 %
exp(confint(m1)) #Confidence Intervals for Model Parameters
# the tolerance is between 28 to 69 percent that a person will purchase in bundle mode

##model2-----------------------------------------
m2 <- glm(IFUseCoupon ~ IFBundle + Channel, data = data, family = "binomial")
summary(m2)
# residual deviance: 3490 ----having better lower deviance

# in contrast to previous prediction,-0.56022 negative intercept for IFBundleBundle says that
#bundle promotion reduces the chance of purchasing
#why negative contribution of the promotion bundle?
exp(coef(m2))
exp(confint(m2))

##Simpson's paradox because of interaction effects
# https://analica.ir/simpson-paradox/
#Simpson's Paradox is a statistical phenomenon where an association between two variables
#in a population emerges, disappears or reverses when the population is divided
#into sub populations(at least 3 variables).

#interaction effect example:interaction between smoking and inhaling asbestos fibers
#both increase the risk of cancer but exposure to asbestos multiply the risk in smokers
# here, the joint effect of inhaling asbestos and smoking is high

##model3 : considering Interaction effect--------------------
m3 <- glm(IFUseCoupon ~ IFBundle + Channel + IFBundle : Channel, data = data, family = "binomial")
summary(m3) # lower deviance

#good reason to continue the promotion campaign by email
#but its success there does not necessarily imply success of in-person offers
#or through a regular mail campaign

#Overall fit-------------------------------------
#deviance statistic
#H0: the model is not better than chance at predicting the outcome
#p-value < 0.05 reject H0

modelChi3 <- m3$null.deviance - m3$deviance
Chidf3    <- m3$df.null - m3$df.residual
Chisq_prob3 <- 1 - pchisq(modelChi3, Chidf3)
Chisq_prob3

#AIC (Akaike Information Criterion)
#AIC_m1: 4349.4
#AIC_m3: 3406 better model

#Wald test
###End of the Code-------------------------------


