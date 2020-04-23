#### Lab 7: 1-way ANOVA #### 
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

### General workflow Reminder ####

# The general workflow as you do analyses in R should be as follows:
#   Step 1.  Plot your data (boxplots, histograms, Q-Q plots)
#   Step 2.  Use the function lm() to fit a model, specifying equation & data
#     e.g., y ~ x, data = data
#   Step 3.  Check assumptions again, using residuals plot
#   Step 4.  If assumptions are met, use the functions anova() and summary() 
#     to interpret statistical results.  If assumptions are not met, try 
#     data transformation and/or a non-parametric or robust version of the test

### Question 1 ####

# 1-1.
# Why would it be inappropriate to treat the three gas chromatograph readings from the same 1L mason jar as 
# independent observations?  What have the authors done to avoid violating the assumption of independence?

# Read in Jaffe.csv file 
Jaffe <- read_csv("datasets/demos/Jaffe.csv")

# 1-5.
# Do the data meet the remaining assumptions of ANOVA? How can you tell?  What should you do? 
# A complete answer will address assumptions for aldrin concentration and HCB concentration and give specific details.

# The remaining assumptions of ANOVA other that observations be random and independent are:
# 1.	The experimental errors of the data must be normally distributed 
# 2.	Treatments must have Homoscedasticity

# Look at the data
head(Jaffe)
summary(Jaffe)

# To determine if the data is normally distrubted by generating a histogram, boxplot, and q-q plot for Aldrin

ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 0.5)+
  facet_wrap(~Depth)


# Generate box plot
ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = Aldrin))+
  stat_summary(aes(x = Depth, y = Aldrin), 
               fun=mean, 
               colour="blue", 
               fill = "red",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

# Anaylsis: Of the three histograms for bottom, middepth, and suface depth the most normally distributed 
# histogram is the middepth, the bottom histogram appears to have a slight right skew, while the surface histogram has 
# a clear left skew. The bottom depth boxplot had a high outlier above the upper whisker, its mean was above the median with even wisker length.
# The middepth boxplot has the mean right on the median, but a slightly longer lower whisker than upper. The surface
# boxplot is the most skewd with an a median very close to the upper edge of the IQR, the mean is right below the median.
# however, this boxplot has even wisker length. THe q-q plot shows that for middepth and surface all points fall along a trendline. 
# the bottom points follow a trendline for the most part but have a high outlier. 

# To determine if the data is normally distrubted by generating a histogram, boxplot, and q-q plot for HCB

ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 0.45)+
  facet_wrap(~Depth)


# Generate box plot
ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = HCB))+
  stat_summary(aes(x = Depth, y = HCB), 
               fun=mean, 
               colour="blue", 
               fill = "red",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

# Anaylsis: The three histograms have realtively normal distribution. 
# The bottom depth boxplot has a smaller upper whisker length than bottom, its mean was above the low median within the IQR box.
# The middepth boxplot has the mean below the median, but a longer lower whisker than upper. The surface
# boxplot has mean close to the median (slightly above). The lower whisker is longer then the upper.  
# THe q-q plot shows that for surface depth all points fall along a trendline. This is not the case for the 
# bottom or middepth points. 

# Sspecify an equation (y~x) and the data

model01 <- lm(Aldrin ~ Depth, data = Jaffe)

model02 <- lm(HCB ~ Depth, data = Jaffe)

summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())
ratio1 <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))
# Ratio above 3 at 3.8, the Aldrin data does not have Homoscedasticity

# Can log transform the data to see if possible improvement emerge 

Jaffe <-Jaffe %>%
  mutate(logAldrin = log(Aldrin))

# Generate a histogram, boxplot, and q-q plot for HCB

ggplot(Jaffe) +
  geom_histogram(aes(logAldrin), binwidth = 0.25)+
  facet_wrap(~Depth)


# Generate box plot
ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = logAldrin))+
  stat_summary(aes(x = Depth, y = logAldrin), 
               fun=mean, 
               colour="blue", 
               fill = "red",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(Jaffe)+
  geom_qq(aes(sample = logAldrin, color = Depth))

# New Anaylsis: All dianostic plots much more normalized and less extreme skew found.

model03 <- lm(logAldrin~Depth, data = Jaffe)

summ_logAldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_logAldrin = mean(logAldrin),
            sd_logAldrin = sd(logAldrin),
            n_logAldrin = n())
ratio2 <-(max(summ_logAldrin$sd_logAldrin))/(min(summ_logAldrin$sd_logAldrin))
# Ratio below 3 at 2.09, the Aldrin data does have homoscedasticity

summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio3 <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))
# Ratio is 1.75, below 3, the HCB data does have homoscedasticity

# The function autoplot gives you a residuals by predicte plot, which is 
# called "Residuals vs. Fitted" here.  It also gives you a Q-Q plot of the RESIDUALS.

autoplot(model01)
autoplot(model02)
autoplot(model03)

# Use the function anova() to answer our first research question: is there an effect
# of depth on Aldrin and HCB concentration?

anova(model01)
anova(model02)
anova(model03)

# To address the second question, which depths are different from the control, we
# can choose among approaches.

# Start with a summary of the model results
summary(model02)
summary(model03)

model02 <- aov(HCB ~ Depth, Jaffe)
TukeyHSD(model02)

model03 <- aov(logAldrin ~ Depth, Jaffe)
TukeyHSD(model03)


