### Lab 8 ####
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")

#### Problem 15-23 ####
# Complete parts a and c only

# Read in Edelaar and Benkman 2006 file
PineCones <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))

# 15-a.####
# a. What do we label this type of comparison?
# This type of comparison is a linear regression, because we want to know how much variation in pine cone mass  is 
# is explained by the presence of squirrels on an island.

# 15-c.####
# c. Using these data, carry out a test of the differences among the means of all three groups.


#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

# Full Analysis 

# Read in Darnell and Munguia (2011) file
Crabs <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor() ))
# Remove N/A from data
Crabs <- na.omit(Crabs) 

# Does the data meet the assumptions of ANOVA? 
# The assumptions of ANOVA are:
# 1.  The observations are random and independent from each other
# 2.	The experimental errors of the data must be normally distributed 
# 3.	Treatments must have homoscedasticity

# The observations are random and independent of each other, because each sample is not
# influenced by the measurements of other samples.

# Look at data to check assumptions 2 and 3
head(Carbs)
summary(Carbs)

# To determine if the data is normally distrubted by generating a histogram, boxplot, and q-q plot for bodyTemperature
# Generate histogram
ggplot(Crabs) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.1)+
  facet_wrap(~crabType)


# Generate box plot
ggplot(Crabs) +
  geom_boxplot(aes(x = crabType, y = bodyTemperature))+
  stat_summary(aes(x = crabType, y = bodyTemperature), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(Crabs)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

# Female crab Anaylsis: The female crab histogram shows a normal distrubtion with a smaller number outlier on the left
# side. In the boxplot the whisker lengths are equal, the median is in the center of the IQR box,
# and the mean is only slightly below the median. The only outlier is past the lower whisker and is the same outlier
# visible in the histogram. The boxplot is overall symmetrical. In the q-q plot we see that the female crab data points
# fall along a trendline with the exception of the lower outlier. This data is adequately normal.

# Intact male Analysis: Within the histogram there appears to be a left skew of the data caused by a high outlier, 
# as well as, high values toward the right side of the distribution. The boxplot shows a high outlier above the upper
# whisker. The median of the box plot is also shifted upward in the boxplot indicating left skew, and the upper whisker is
# shorter than the lower one. In the q-q plot we see that the points  fall along a trendline. 

# Male major removed: The histogram appears to have a mostly normal distribution shape with a higher and lower outlier.
# In the boxplot, the median is in the center of the IQR box, and the median and mean overlap. There is one lower and 
# upper outlier visible in the plot, and the lower whisker is shorter than the upper indicating a slight right skew.
# In the q-q plot we see that the points fall along a trendline. 

# Male minor removed: The histrogram shows a left skew with no major outliers present. Within the boxplot we see the 
# median overlaps with the mean. However, the median is low in the IQR box and the upper wisker is shorter than the 
# lower indicating the left skew. In the q-q plot we see that the points fall along a trendline.  

# Sspecify an equation (y~x) and the data

model01 <- lm(bodyTemperature ~ crabType, data = Crabs)


summ_bodyTemperature <- Crabs %>%
  group_by(crabType) %>% 
  summarise(mean_bodyTemperature = mean(bodyTemperature),
            sd_bodyTemperature = sd(bodyTemperature),
            n_bodyTemperature = n())
ratio1 <-(max(summ_bodyTemperature$sd_bodyTemperature))/(min(summ_bodyTemperature$sd_bodyTemperature))
# Ratio below 3 at 1.17, the bodyTemperature data does have homoscedasticity and meets the assumption of equal variances.

# Use function autoplot to get residuals in q-q predicte plot.

autoplot(model01)

# Use the function anova() to answer our first research question: Does mean rate of heat gain differs among crab groups?

anova(model01)
# We found that mean heat gain was significantly different among crab groups.
# (One-way ANOVA: F= 20.312;df= 3,80; P= 6.997e-10)


tukey <- glht(model03, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)

