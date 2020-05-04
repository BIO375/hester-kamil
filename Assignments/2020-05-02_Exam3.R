### Exam 3 ####
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

### Question 7 ####
Doctor <- tribble(
~Doctorvisits,   ~Offspring,
7,               7,
4,               7,
3,               5,
6,               4,
4,               2,
1,               1,
)

# Generate histogram, boxplot, q-q plot for Doctorvisits (X)

ggplot(Doctor)+
  geom_histogram(aes(Doctorvisits), binwidth = .9)
# Anaylsis: The histogram is normally distributed 

ggplot(Doctor) +
  geom_boxplot(aes(x = Doctorvisits, y = ""))+
  stat_summary(aes(x = Doctorvisits, y = ""), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)
# Analysis: The median is low in the IQR box and the lower whisker is longer than the upper indicating slight left 
# skew

ggplot(Doctor)+
  geom_qq(aes(sample = Doctorvisits))
# Analysis: Points follow a trendline.

# Generate histogram, boxplot, q-q plot for offspring (Y)

ggplot(Doctor)+
  geom_histogram(aes(Offspring), binwidth = .9)
# Anaylsis: Slight left skew.

ggplot(Doctor) +
  geom_boxplot(aes(x = "", y = Offspring))+
  stat_summary(aes(x = "", y = Offspring), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)
# Analysis: The median is in the center of the IQR box and the lower whisker is longer than the upper indicating 
# slight left skew. The mean was slightly below the median

ggplot(Doctor)+
  geom_qq(aes(sample = Offspring))
# Analysis: Points follow a trendline with the exception of one outlier.

### R Section of the Exam ####

# For the following problem identify (i) the null hypothesis or hypotheses 
# (ii) the predictor/response variables and whether they are categorical or continuous
# (iii) the statistical test you would use, (iv) test the assumptions of the statistical test using R
# (v) perform the correct test in R, and (vi) report the results. 

# Read in data file
Caffeine <- read_csv("datasets/demos/caffeine.csv.")

# (i) ####

# Ho (1): There is no statistically significant difference in mean caffeine metabolism rate between men 
# and women without elevated progesterone.

# Ho (2):There is no statistically significant difference in mean caffeine metabolism rate between women with elevated 
# progesterone and women without elevated progesterone. 

# (ii) ####
# Response = Numerical, continuous: caffeine half-life
# Predictor = categorical: gender and progesterone level (female only)

# (iii) ####
# I would use the statistical test One-Way ANOVA because three means are being compared instead of two, so a 
# two-sample t-test would in inapproriate. 

# (iv) ####
# Does the data meet the assumptions of ANOVA? 
# The assumptions of ANOVA are:
# 1.  The observations are random and independent from each other
# 2.	The experimental errors of the data must be normally distributed 
# 3.	Treatments must have homoscedasticity/equal variance among groups

# The observations are random and independent of each other, because each sample is not
# influenced by the measurements of other samples.

# Look at data to check assumptions 2 and 3
head(Caffeine)
summary(Caffeine)

# To determine if the data is normally distrubted by generating a histogram, boxplot, and q-q plot for bodyTemperature
# Generate histogram
ggplot(Caffeine) +
  geom_histogram(aes(half_life), binwidth = 0.7)+
  facet_wrap(~group)


# Generate box plot
ggplot(Caffeine) +
  geom_boxplot(aes(x = group, y = half_life))+
  stat_summary(aes(x = group, y = half_life), 
               fun=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(Caffeine)+
  geom_qq(aes(sample = half_life, color = group))

# High progesterone level:The histogram shows a normal distrubtion with a smaller number outlier on the 
# leftside. The median is low in the IQR box and the mean is over the median. The lower whisker is shorter than the 
# upper whisker indicating the slight right skew. There is also a lower number outlier. In the q-q plot all the points
# fall along a line with the exception of the outlier. 

# Male: The histogram shows a slight right skew causes by an increased frequency in values on the left side of the 
# distribution. In the boxplot we see a high outlier as well as a longer upper whisker than lower. The median is a 
# little high in the IQR box and the mean is above the median. In the q-q plot most points fall along a trendline. 

# Normal progesterone level:The histogram is normally distributed. In the boxplot the whisker lengths are even with no 
# outliers. The median is a little shifted up in the IQR box and the mean is below the median. In the q-q plot most
# points fall along a trendline. 

# Overall, since the skewed groups in the data male and high progesterone are both skewed in the right direction it 
# is reasonable to assume we have a normal enough distribution to proceed with a One-way ANOVA without transformation. 
# The data does not seem skewed enough to warrent a Kruskal-Wallace test. 


# Sspecify an equation (y~x) and the data

model01 <- lm(half_life ~ group, data = Caffeine)


summ_half_life <- Caffeine %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_bhalf_life = n())
ratio1 <-(max(summ_half_life$sd_half_life))/(min(summ_half_life$sd_half_life))
# Ratio below 3 at 1.69, the half life data does have homoscedasticity and meets the assumption of equal variances.

# Use function autoplot to get residuals in q-q predicte plot.

autoplot(model01)
# Residuals did NOT increase as predicted/fitted values increased. Meets the assumption

# (v) ####
# Use the function anova() to answer our first research question: Does mean  caffeine metabolism rate differ among 
# groups?

anova(model01)
# Signficant difference between at aleast two means
# The mean caffeine metabolism rate varied significantly among different genders and progestrone levels
# (One-way ANOVA: F 2,28= 13.866;P= 6.528e-05)

model <- aov(half_life ~ group, Caffeine)
TukeyHSD(model)

# (Vi) ####
# The mean caffeine metabolism rate was significantly different between males and women with elevated progestrone.
# We reject the the first null hypothesis
# (TukeyHSD: P= 0.0000731)

# The mean caffeine metabolism rate was significantly different between women with elevated progestrone and women
# with normal levels. We reject the the second null hypothesis
# (TukeyHSD: P= 0.0010854)


