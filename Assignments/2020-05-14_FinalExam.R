### Final Exam ####
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

### Question 16 ####
# To investigate whether subcutaneous fat quantity affects insulation in humans, Sloan and Keating measured the rate of 
# heat loss by boys spent swimming in water at 20.3˚C.  Heat loss was measured as change in body temperature per minute 
# spent swimming.  Subcutaneous fat quantity was estimated by an index of body “leanness” on each boy as the reciprocal 
# of the skin-fold thickness adjusted for total skin surface area and body mass.  To be clear, the researchers were 
# testing explicitly for a causal relationship between leanness and heat loss.

# Read in insolation data file

Insulation <- read_csv("datasets/demos/insulation.csv.")
head(Insulation)

### (a.) ####
# Identify the variable(s) and whether they are continuous or categorical.  For any categorical variables, 
# name the factor and identify the number of levels.  

# Both variables in this data set are numerical and continous. 

### (b.) ####
# Identify the statistical null hypothesis.

# The statistically null hypothesis is that the correlation coefficent will be p=0, showing to association between the 
# two vairables. 

### (c.) ####
# Do the data meet the assumptions of the default test?  Provide evidence using R.

# The assumptions of correlations are:
# 1. Each measurement has a bivariate normal distribution, which is hard to casually determine. 
# This can be found by determining if the relationship between response variable (Y) and predictor variable (X) is
# linear, if the cloud of points collected is circular or elliptical, and wether the frequency distributions of the two
# variables is normal. 

ggplot(data = Insulation) +
  geom_point(mapping = aes(x = leanness, y = heat_loss),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "Leanness", y = "Heat Loss")
# These points are roughly elliptical and there is a linear relationship 

# Generate histogram, boxplot, q-q plot for leanness (X)

ggplot(Insulation)+
  geom_histogram(aes(leanness), binwidth = .4)
# Anaylsis: The histogram is not normally distributed, there appears to be a left skew with a high low number outlier 
# as well.

ggplot(Insulation) +
  geom_boxplot(aes(x = leanness, y = ""))+
  stat_summary(aes(x = leanness, y = ""), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)
# Analysis: The median is low in the IQR box and the lower whisker is shorter than the upper indicating slight right
# skew. No outliers are visible on the boxplot, and the mean is abover the median.   

ggplot(Insulation)+
  geom_qq(aes(sample = leanness))
# Analysis: Points follow a trendline.

# Leanness Analysis: The histogram is not normally distributed, there appears to be a left skew with a high low number outlier 
# as well. The median is low in the IQR box and the lower whisker is shorter than the upper indicating slight right
# skew. No outliers are visible on the boxplot, and the mean is abover the median.Points follow a trendline. 
# Overall, I would say the data of this variable is not skewed enough to warrent a Spearman's rank correlation. But, 
# data transnformation may help 

# Generate histogram, boxplot, q-q plot for heat loss (X)

ggplot(Insulation)+
  geom_histogram(aes(heat_loss), binwidth = .008)
# Anaylsis: The histogram is not normally distributed, there appears to be a right skew with a high number outlier 
# as well.

ggplot(Insulation) +
  geom_boxplot(aes(x = "", y = heat_loss))+
  stat_summary(aes(x = "", y = heat_loss), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)
# Analysis: The median is low in the IQR box, but the whisker lengths are even. No outliers are visible on the 
# boxplot, and the mean is abover the median.  

ggplot(Insulation)+
  geom_qq(aes(sample = heat_loss))
# Analysis: Most points follow a general trendline.

# Heat loss Analysis: The histogram is not normally distributed, there appears to be a right skew with a high number outlier 
# as well. The median is low in the IQR box, but the whisker lengths are even. No outliers are visible on the 
# boxplot, and the mean is above the median. Most points follow a general trendline. I would recommend this data also 
# be transformed to see if increased normal distribution could occur in the histogram. 

### (d.) ####
# What is the appropriate statistical test?  Is a transformation warranted?

# The appropriate test could be a correlation, but the data should be transformed to adhere to the bivariate normal 
# distribution assumption first. 

# Transform variables.

Insulation <- Insulation %>%
  mutate(Log_leanness = log(leanness))
Insulation <- Insulation %>%
  mutate(Log_heat_loss = log(heat_loss))

# Re-evalutate for normal distribution by generating histograms, box plots, and q-q plot. 

ggplot(Insulation)+
  geom_histogram(aes(Log_leanness), binwidth = .08)
# Anaylsis: The histogram is still not normally distributed, there appears to be a left skew with a high low number outlier 
# as well.

ggplot(Insulation) +
  geom_boxplot(aes(x = Log_leanness, y = ""))+
  stat_summary(aes(x = Log_leanness, y = ""), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)
# Analysis: The median is center in the IQR box and the whisker length is now even.
# No outliers are visible on the boxplot, and the mean is abover the median but much closer now.   

ggplot(Insulation)+
  geom_qq(aes(sample = Log_leanness))
# Analysis: Points follow a trendline.

ggplot(Insulation)+
  geom_histogram(aes(Log_heat_loss), binwidth = .1)
# Anaylsis: The histogram is still not normally distributed, there appears to be a sharp left skew.

ggplot(Insulation) +
  geom_boxplot(aes(x = Log_heat_loss, y = ""))+
  stat_summary(aes(x = Log_heat_loss, y = ""), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)
# Analysis: The median is center in the IQR box. But, the lower the whisker length is now drastically longer than the 
# upper indication a severe left skew. No outliers are visible on the boxplot, and the mean is lower than the median 
# now.    

ggplot(Insulation)+
  geom_qq(aes(sample = Log_heat_loss))
# Analysis: Points follow a trendline much more closely now than before.

# The tranformation did not make the dataset normal enough to adhere to the correlation assumption of bivariate normal 
# distribution. So, we will proceed with Spearman's rank correlation using the original data

### (e.) ####

# We will now perform a non-parametric Spearman's rank correlation on the insulation data. 

#  P-value in the output is not exact.But, can still interpret the results!

InsluationSpear <-cor.test(~ leanness + heat_loss, data = Insulation,
                      method = "spearman")
InsluationSpear
# here is a significant positive association between events increased index of body leanness and rate of heat loss.
# while swimming in water at 20.3˚C
#(Spearman's rank corrrelation: s=21.072, rho=0.9263215, p-1.51e-05)

