###S08: Introduction to ggplot2--------------------------------------
###Set Work Directory------------------------------------------------
setwd("C:/Users/FarzadM/Desktop")
getwd()
###Required Libraries------------------------------------------------
#ggplot2: For creating elegant and complex plots
#install.packages("ggplot2") 
library("ggplot2")
#https://ggplot2.tidyverse.org/reference/index.html
#http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

###Dataset-----------------------------------------------------------
mtcars
#mtcars description: https://rpubs.com/neros/61800

data <- mtcars
View(data)
dim(data)

###Data Visualization with ggplot2-----------------------------------
#ggplot2 in 3 easy steps:

#step 1: aesthetic: what you want to graph
#step 2: geom: how you want to graph it
#step 3: options: optional titles, themes, etc.

#aesthetic + a geom or two + other optional elements 

#mpg vs wt
ggplot()
ggplot(data, aes(x = wt, y = mpg))

##Scatter Plot-------------------------------------------------------
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point()

#Scatter Plot based on a Third Variable
ggplot(data, aes(x = wt, y = mpg, color = gear)) +
  geom_point()
class(data$gear)
ggplot(data, aes(x = wt, y = mpg, color = factor(gear))) +
  geom_point()

#Change color of points
ggplot(data, aes(x = wt, y = mpg, color = "blue")) +
  geom_point()

ggplot(data, aes(x = wt, y = mpg)) +
  geom_point(color = "blue")

#Change Size and Shape of Points
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point(color = "blue", size = 2.5, shape = 2)

#Scatter Plot based on a Third Variable
ggplot(data, aes(x = wt, y = mpg, shape = factor(gear))) +
  geom_point()

ggplot(data, aes(x = wt, y = mpg, shape = factor(gear), color = factor(gear))) +
  geom_point()

#Annotation: Adding Text with geom_text()
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_text(label = rownames(data), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T)

data$carname <- rownames(data)
p1 <- ggplot(data, aes(x = wt, y = mpg)) +
            geom_point() +
            geom_text(data = data[data$carname %in% c("Mazda RX4", "Merc 230", "Fiat 128"),], aes(label = carname), 
                      nudge_x = 0.25, nudge_y = 0.25)

#Add one text label only
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_label(
    label = "Sample Lable!", 
    x = 4.5,
    y = 25,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    color = "black",
    fill = "green")

#Different Types of geoms (Geometric Objects)------------------------
#geom_point for drawing individual points (e.g., a scatter plot)
#geom_line for drawing lines (e.g., for a line charts)
#geom_smooth for drawing smoothed lines (e.g., for simple trends or approximations)
#geom_bar for drawing bars (e.g., for bar charts)
#geom_histogram for drawing binned values (e.g. a histogram)
#geom_polygon for drawing arbitrary shapes
#geom_map for drawing polygons in the shape of a map! 

##Add Kernel Smoother------------------------------------------------
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red")

#LOESS (locally estimated scatterplot smoothing)

#Kernel Smoother for each Group
ggplot(data, aes(x = wt, y = mpg, color = factor(gear))) +
  geom_point() +
  geom_smooth(se = FALSE)

#Kernel Smoother for All Groups
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(gear))) +
  geom_smooth(se = TRUE)

##Plot Barchart------------------------------------------------------
ggplot(data, aes(x = factor(gear))) +
  geom_bar()

#Percentage
ggplot(data , aes(x = factor(gear), y = stat(prop * 100), group = 1)) +
  geom_bar() +
  ylab("Percentage")
dim(data)

#Stacked Bar Chart
ggplot(data, aes(x = factor(gear), fill = factor(vs))) +
  geom_bar()

ggplot(data, aes(x = factor(gear), fill = factor(vs))) +
  geom_bar(position = "dodge")

ggplot(data, aes(x = factor(gear), fill = factor(vs))) +
  geom_bar(position = "fill")

#Sorted Bar Chart
ggplot(data, aes(x = rownames(data), y = mpg)) +
  geom_bar(stat = "identity")

ggplot(data, aes(x = reorder(rownames(data), mpg), y = mpg)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data, aes(x = reorder(rownames(data), mpg), y = mpg)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90))

##Plot Histogram-----------------------------------------------------
ggplot(data, aes(x = disp)) +
  geom_histogram() 

ggplot(data, aes(x = mpg)) +
  geom_histogram(fill = "#52be80", bins = 5)
#color codes: https://htmlcolorcodes.com/

##Bubble Chart-------------------------------------------------------
ggplot(data, aes(x = wt, y = mpg)) + 
  geom_point(aes(color = factor(cyl), size = disp), alpha = 0.5) + 
  scale_size(range = c(0.5, 10))  # Adjust the range of points size

##Line Plot with Multiple Groups-------------------------------------
tapply(data$mpg, data$gear, mean)
df_1 <- tapply(data$mpg, list(data$vs,data$gear), mean)
class(df_1)
vshape <- rep(c(0,1),each = 3 )
gear <- rep(c(3,4,5), 2)
df_2 <- data.frame(vshape, gear, as.vector(t(df_1))) 
df_2
colnames(df_2)[3] <- "average_mpg"
View(df_2)

ggplot(df_2, aes(x = factor(gear), y = average_mpg, group = factor(vshape), color = factor(vshape))) +
  geom_line(aes(linetype = factor(vshape))) +
  geom_point(aes(shape = factor(vshape)))

##Density Function---------------------------------------------------
#Create a Dataset
set.seed(1234)
wdata <-  data.frame(
  sex = factor(rep(c("F", "M"), each = 200)),
  weight = c(rnorm(200, 65, 5), rnorm(200, 80, 8)))

head(wdata, 4)
tail(wdata,4)

#Histogram for All Data in One Plot
ggplot(wdata, aes(x = weight)) +
  geom_histogram()

ggplot(wdata, aes(x = weight, fill = sex)) +
  geom_histogram(bins = 40, alpha = 0.5, position = "identity") #position = "dodge"

#Histogram for All Data in Two Plots
ggplot(wdata, aes(x = weight)) +
  geom_histogram(bins = 25) +
  facet_grid(sex ~ .)

#Density Function for All with Mean Vertical Line
ggplot(wdata, aes(x = weight)) +
  geom_density(fill = "gold") +
  geom_vline(aes(xintercept = mean(weight)), linetype = "dashed") +
  geom_text(aes(label = paste0("Ave = ", round(mean(weight),2)), x = mean(weight) + 10, y = 0.035))

#Density Function for Two Groups
ggplot(wdata, aes(x = weight, color = sex)) +
  geom_density()

#Add Mean to Density Function
group_mean <- as.data.frame(tapply(wdata$weight,wdata$sex, mean))
dim(group_mean)
colnames(group_mean) <- "average"
group_mean$sex <- c("F","M")
group_mean

theme_set(theme_classic())

ggplot(wdata, aes(x = weight, color = sex)) +
  geom_density(aes(fill = sex), alpha = 0.4) +
  geom_vline(data = group_mean, aes(xintercept = average, color = sex), linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +
  scale_fill_manual(values = c("#868686FF", "#EFC000FF"))

theme_set(theme_grey())

##Box Plot-----------------------------------------------------------
ggplot(wdata, aes(y = weight)) + 
  geom_boxplot()

ggplot(wdata, aes(x = sex , y = weight)) + 
  geom_boxplot() +
  stat_summary (fun.y = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red")

##Facets-------------------------------------------------------------
ggplot(data) + 
  geom_point(aes(x = wt, y = mpg)) + 
  facet_wrap(~ factor(gear))

##Other Optional Elements--------------------------------------------
#Scatter Plot based on a Third Variable
ggplot(data = mtcars, aes(x = wt, y = mpg, shape = factor(gear))) +
  geom_point(size = 2.5) +
  ggtitle("MPG vs. Weight \n Grouped by # of Gears") +
  labs(shape = " # of Gears") + 
  xlab("Weight") +
  scale_y_continuous(name = "Miles per Gallon", breaks = seq(0, 40, 10), limits = c(0, 40)) +
  theme(plot.title = element_text(hjust = 0.5, color = "dark blue", size = 18, face="bold.italic"),
        axis.line.x = element_line(size = 0.5, color = "red"),
        axis.line.y = element_line(size = 0.5, color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 18),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 12),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(size = 1.5,  linetype = "solid", color = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = "dashed",color = "white")) 
###Case Study--------------------------------------------------------
#read data from file
data <- read.csv("CS_02.csv", header = TRUE)
dim(data)
head(data)
View(data)
summary(data[, c("Gender", "Revenue")])

#data preparation
data$Revenue <- as.numeric(gsub("[$]","", data$Revenue))
data$Gender <- factor(data$Gender)
summary(data[, c("Gender", "Revenue")])

#create required dataframes for the plot
#total revenue by gender
city_gender_rev_1 <- as.data.frame(tapply(data$Revenue, list(data$City, data$Gender), sum)) 
city_gender_rev_1

#order data based on total revenue
city_gender_rev_1$total_rev <- city_gender_rev_1$M + city_gender_rev_1$F
city_gender_rev_1 <- city_gender_rev_1[order(city_gender_rev_1$total_rev), ]
city_gender_rev_1$city <- rownames(city_gender_rev_1)
rownames(city_gender_rev_1) <- NULL
city_gender_rev_1$city <- factor(city_gender_rev_1$city, levels = reorder(city_gender_rev_1$city, city_gender_rev_1$total_rev))

#change city_gender_rev_1 to be suitable for working w/ ggplot2
c1 <- city_gender_rev_1[, c("city", "F")]
colnames(c1)[2] <- "revenue"
c1$gender <- "F"
c2 <- city_gender_rev_1[, c("city", "M")]
colnames(c2)[2] <- "revenue"
c2$gender <- "M"
city_gender_rev_2 <- rbind(c1, c2)

#Plot without a story to tell!
ggplot(city_gender_rev_2, aes(city, revenue, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()

#Plot with a story to tell!
#Initial plot 
ggplot(city_gender_rev_2, aes(x = revenue, y = city)) +
        geom_point(aes(color = gender)) +
        geom_line(aes(group = city))

#prepare dataframe for labels and highlights
#highlight locations experience a 20% or greater difference in revenue generated by males versus females
#revenue difference (%)
city_gender_rev_1$diff_percent <- round((pmax(city_gender_rev_1$M, city_gender_rev_1$F) / pmin(city_gender_rev_1$M, city_gender_rev_1$F) - 1) * 100, 0)
city_gender_rev_1

#which gender creates the highest revenue
city_gender_rev_1$gender_max <- ifelse(city_gender_rev_1$F > city_gender_rev_1$M, "F", "M")

#cities with 20% or higher gap in revenue
big_diff_cities <- city_gender_rev_1$city[city_gender_rev_1$diff_percent >= 20]

#dataframe for creating highlights and labels
highlight <- city_gender_rev_2[city_gender_rev_2$city %in% big_diff_cities, ]
highlight$diff_percent <- city_gender_rev_1[match(highlight$city, city_gender_rev_1$city), "diff_percent"]
highlight$gender_max   <- city_gender_rev_1[match(highlight$city, city_gender_rev_1$city), "gender_max"]
plot_labels <- highlight[highlight$gender == highlight$gender_max,]

#Second plot 
plot <- ggplot(city_gender_rev_2, aes(revenue, city)) +
                geom_line(aes(group = city), alpha = 0.3) +
                geom_point(aes(color = gender), size = 1.5, alpha = 0.3) +
                geom_line(data = highlight, aes(group = city)) +
                geom_point(data = highlight, aes(color = gender), size = 2) +
                geom_text(data = plot_labels, aes(color = gender_max, label = paste0("+", diff_percent, "%")), size = 3, hjust = - 0.5)

plot

#Final plot
plot <- plot + scale_color_discrete(labels = c("Female", "Male")) +
                scale_x_continuous(labels = scales::dollar, expand = c(0.02, 0), 
                                   limits = c(0, 10500),
                                   breaks = seq(0, 10000, by = 2500)) +
                scale_y_discrete(expand = c(.02, 0)) +
                labs(title = "Total Revenue by City and Gender",
                     subtitle = "Out of 23 cities, eight locations experience a 20% or greater difference \nin revenue generated by males versus females. Hidalgo experiences the \ngreatest difference with females generating 86% more revenue than males.") +
                theme_minimal() +
                theme(axis.title = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.title = element_blank(),
                      legend.justification = c(0, 1), 
                      legend.position = c(.1, 1.075),
                      legend.background = element_blank(),
                      legend.direction = "horizontal",
                      plot.title = element_text(size = 20, margin = margin(b = 10)),
                      plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
                      plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))

plot
###Assignment--------------------------------------------------------
#Q1: Go to the link below and download data:
#       https://www.pewresearch.org/wp-content/uploads/sites/3/2016/05/Middle-Class-U.S.-Metro-Areas-5-12-16-Supplementary-Tables.xlsx
#    Try to replicate the income_plot.png
#    Review the paper for elegant plots:
#       https://www.pewsocialtrends.org/2016/05/11/americas-shrinking-middle-class-a-close-look-at-changes-within-metropolitan-areas/

#Q2: Download EconomistData.csv from Google Drive.
#    Try to replicate Economist1.png
#    Find the anwser here:
#       https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

####End of the Code####