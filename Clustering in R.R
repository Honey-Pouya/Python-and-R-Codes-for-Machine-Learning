####S18: RFM Clustering -------------------------
###Set work directory----------------------------
setwd("C:/Users/FarzadM/Desktop")
###Required libraries----------------------------
library("ggplot2")    #for data visualizations
library("cluster")    #for cluster analysis - CLARA
library("factoextra") #for visualizing the results of multivariate data analyses
install.packages("factoextra")
###Read data from file---------------------------
data <- read.csv("CS_08.csv", header = TRUE)

###Data inspection-------------------------------
dim(data)
head(data)
tail(data)
summary(data)
sum(is.na(data$total_purchase))
hist(data$total_purchase, breaks = 50)

#daily demand
data$date <- as.Date(data$created_ts, "%m/%d/%Y")
head(data)
class(data$date)
daily_demand <- table(data$date)
head(daily_demand)
plot(daily_demand, type = "l")

###Create RFM dataset----------------------------
##Frequency--------------------------------------
#how often the same individual visits your site in a given time period.
customer_f <- as.data.frame(aggregate(data$order_id,
                                      list(data$customer_id), length))
colnames(customer_f) <- c("customer_id","freq")
head(customer_f)
length(customer_f$customer_id)

hist(customer_f$freq, breaks = 50)
summary(customer_f$freq)

##Recency----------------------------------------
#how long it's been since your visitors last came to your site.
tail(data)
r_date <- as.Date("07/23/2019", format = "%m/%d/%Y")
customer_r <- as.data.frame(aggregate(data$date,
                            list(data$customer_id), max))

colnames(customer_r) <- c("customer_id","last_date")
head(customer_r)
customer_r$recency <- as.numeric(r_date - customer_r$last_date)
head(customer_r)

hist(customer_r$recency, breaks = 50)
summary(customer_r$recency)

##Monetary---------------------------------------
#how much the customer spends.
customer_m <- as.data.frame(aggregate(data$total_purchase,
                            list(data$customer_id), sum))

colnames(customer_m) <- c("customer_id","monetary")
head(customer_m)
hist(customer_m$monetary, breaks = 50)
summary(customer_m$monetary)

##RFM dataframe for Customers--------------------
df <- merge(customer_f, customer_r, "customer_id")
head(df)
rfm_customer <- merge(df, customer_m,"customer_id")
head(rfm_customer)
rfm_customer <-  rfm_customer[,-3] #remove last_date
head(rfm_customer)

#Assign customer ids to row names
rownames(rfm_customer) <- rfm_customer$customer_id
head(rfm_customer)
###Clustering------------------------------------
plot(rfm_customer$freq, rfm_customer$recency)
plot(rfm_customer$freq, rfm_customer$monetary)
cor(rfm_customer$freq, rfm_customer$monetary)# strong positive correlation

hist(rfm_customer$freq, breaks = 50)
hist(log10(rfm_customer$freq), breaks = 50)

hist(rfm_customer$recency, breaks = 50)
hist(log10(rfm_customer$recency), breaks = 50)#still high skewness

#preparing features for clustering 
rfm_customer_2 <- rfm_customer [ ,c('freq' ,'recency')]
head(rfm_customer_2)

#Scale features
rfm_customer_2 <- scale(rfm_customer_2)#scale function converts the data frame to a matrix
head(rfm_customer_2)
summary(rfm_customer_2)
class(rfm_customer_2)

hist(rfm_customer_2[,1], breaks = 50)#still high skewness
hist(rfm_customer_2[,2], breaks = 50)#still high skewness

##Apply kmeans on Data---------------------------
#First try
set.seed(123)
seg_km1 <- kmeans(rfm_customer_2, centers = 5)#center = number of clusters

#Results
seg_km1$cluster
table(seg_km1$cluster)
km_res1 <- as.data.frame(seg_km1$cluster)
km_res1$customer_id <- rownames(km_res1)
colnames(km_res1) <- c("cluster","customer_id")
head(km_res1)  

rfm_customer$km1 <- km_res1[match(km_res1$customer_id, rfm_customer$customer_id),'cluster']
head(rfm_customer)
sum(is.na(rfm_customer$km1))  

head(rfm_customer)
aggregate(rfm_customer[,c(2:4)], list(rfm_customer$km1), mean)# mean of columns in each clusters
table(rfm_customer$km1)

#plot clusters
ggplot(data = rfm_customer, aes( x = freq, y = recency, color = factor(km1)))+
  geom_point() +
  ggtitle("kmeans - iter_1")

#Second try
set.seed(11234)
seg_km2 <- kmeans(rfm_customer_2, centers = 5)

#Results
table(seg_km2$cluster)
table(seg_km1$cluster)
km_res2 <- as.data.frame(seg_km2$cluster)
km_res2$customer_id <- rownames(km_res2)
colnames(km_res2) <- c("cluster","customer_id")

rfm_customer$km2 <- km_res2[match(km_res2$customer_id, rfm_customer$customer_id),'cluster']
sum(is.na(rfm_customer$km2))

head(rfm_customer)
aggregate(rfm_customer[,c(2:4)], list(rfm_customer$km2), mean)
table(rfm_customer$km2)

#plot clusters
ggplot(data = rfm_customer, aes(x = freq, y = recency, color = factor(km2)))+
  geom_point() +
  ggtitle("kmeans - iter_2") 

##Apply CLARA on Data----------------------------

#First try
set.seed(1234)
seg_cl1 <- clara(rfm_customer_2, k = 5, samples = 10000, pamLike = TRUE)

#Results
table(seg_cl1$cluster)
cl_res1 <- as.data.frame(seg_cl1$cluster)
cl_res1$customer_id <- rownames(cl_res1)
colnames(cl_res1) <- c("cluster","customer_id")

rfm_customer$cl1 <- cl_res1[match(cl_res1$customer_id, rfm_customer$customer_id),'cluster']
sum(is.na(rfm_customer$cl1))

head(rfm_customer)
aggregate(rfm_customer[,c(2:4)], list(rfm_customer$cl1), mean)
table(rfm_customer$cl1)

#plot clusters
ggplot(data = rfm_customer, aes(x = freq, y = recency, color = factor(cl1)))+
  geom_point() +
  ggtitle("clara - iter_1") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#C3D7A4", "#52854C"))

#Second try
set.seed(12345678)
seg_cl2 <- clara(rfm_customer_2, k = 5, samples = 5000, pamLike = TRUE)

#Results
table(seg_cl2$cluster)
table(seg_cl1$cluster)
cl_res2 <- as.data.frame(seg_cl2$cluster)
cl_res2$customer_id <- rownames(cl_res2)
colnames(cl_res2) <- c("cluster","customer_id")

rfm_customer$cl2 <- cl_res2[match(cl_res2$customer_id, rfm_customer$customer_id),'cluster']
sum(is.na(rfm_customer$cl2))

head(rfm_customer)
aggregate(rfm_customer[,c(2:4)], list(rfm_customer$cl2), mean)
table(rfm_customer$cl2)

#plot clusters
ggplot(data = rfm_customer, aes(x = freq, y = recency, color = factor(cl2)))+
  geom_point() +
  ggtitle("clara - iter_2") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#C3D7A4", "#52854C"))

##Optimal number of clusters---------------------  
#Elbow method

rfm_customer_2_sample <- rfm_customer_2[sample(1:nrow(rfm_customer_2), 5000), ]
plot_elbow_cl <- fviz_nbclust(rfm_customer_2_sample, clara, method = "wss") 
plot_elbow_cl

plot_elbow_cl <- plot_elbow_cl +
                 geom_vline(xintercept = 5, linetype = 2) +
                 labs(subtitle = "Elbow method_Clara")
plot_elbow_cl
plot_elbow_cl$data
#total w/ SS for k = 5:  2172.9988

#Silhouette method
plot_silhouette_cl <- fviz_nbclust(rfm_customer_2_sample, clara, method = "silhouette") +
                      labs(subtitle = "Silhouette method_Clara")

plot_silhouette_cl
plot_silhouette_cl$data
#Average silhouette for k = 5: 0.3994754
###Hierarchical K-Means Clustering---------------
set.seed(1234)
seg_hk1 <- factoextra::hkmeans(rfm_customer_2, k = 5)

#Results
table(seg_hk1$cluster)
hk_res1 <- as.data.frame(seg_hk1$cluster)
hk_res1$customer_id <- rownames(hk_res1)
colnames(hk_res1) <- c("cluster","customer_id")

rfm_customer$hk1 <- hk_res1[match(hk_res1$customer_id, rfm_customer$customer_id),'cluster']
sum(is.na(rfm_customer$hk1))

head(rfm_customer)
aggregate(rfm_customer[,c(2:4)], list(rfm_customer$hk1), mean)
table(rfm_customer$hk1)

#plot clusters
ggplot(data = rfm_customer, aes(x = freq, y = recency, color = factor(hk1)))+
  geom_point() +
  ggtitle("hkmeans - iter_1") + 
  scale_color_manual(values = c("#00AFBB", "#C3D7A4", "#E7B800", "#FC4E07", "#52854C"))


#Elbow method
plot_elbow_hk <- fviz_nbclust(rfm_customer_2_sample, hkmeans, method = "wss") +
                 labs(subtitle = "Elbow method_hkmeans")
plot_elbow_hk
plot_elbow_hk$data
#total w/ SS for k = 5: 1505.4725

#Silhouette method
plot_silhouette_hk <- fviz_nbclust(rfm_customer_2_sample, hkmeans, method = "silhouette") +
                      labs(subtitle = "Silhouette method_hkmeans")

plot_silhouette_hk
plot_silhouette_hk$data
#Average silhouette for k = 5: 0.5024371

###End of the Code-------------------------------
