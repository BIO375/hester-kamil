### Exam 2 R section ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# ### Install and load packages ####
# # The following commands will install these packages if they are not already installed, 
# # and then load them!
# 
# # if(!require(Rtools)){install.packages("Rtools")}
# # if(!require(Rmisc)){install.packages("Rmisc")}
# if(!require(DescTools)){install.packages("DescTools")}
# # if(!require(boot)){install.packages("boot")}
# # if(!require(rcompanion)){install.packages("rcompanion")}
# # if(!require(summarytools)){install.packages("summarytools")}
# 
# # Open tidyverse
# install.packages("tidyverse", dependencies = TRUE)
# library(tidyverse)
# tidyverse_update()
library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(DescTools)

### Question 17####

Bakers <- read_csv("datasets/demos/baker.csv")

# 1. Identify the variable or variables. For example,the variables in the diatom experiment are: response = diversity,
# predictor = zinc level.
# Reponse = Numerical, continous: streptococcus antibody concentrations
# Predictor = categorical: vaccination status. (before or after vaccination)

# 2. What is the statistical null hypothesis?  If you cannot find the μ symbol, you can just write mu.

# H0: The mean concentration of streptococcus antibody within healthy volunteers after vaccination is the same as the 
# mean concentration of streptococcus antibody before vaccination. 

# 3. What is the default appropriate statistical test (if all assumptions were met)?

# The default appropriate statistical test is the paired t-test. 

# 4. Do the data meet the assumptions of this test?  Give specific information to support your choice for each
# assumption.

# The assumptions of a paired t-test are:
# 1. Experimental units are randomly sampled from population
# 2. The differences observed are normally distrubted 
# 3. Both experimental condidtions are conducted on each experimental unit in the sample
# 4. The predictor variable is continous. 

# To determine if the data is normally distrubted by generating a histogram, boxplot, and q-q plot
# Since the assumptions of normality apply to differences for a paired t-test,
# have to add column called diff: diff = After - Before 
Bakers <- Bakers %>%
  mutate(diff = After - Before)

# Generate histogram
ggplot(Bakers) +
  geom_histogram(aes(diff), binwidth = .5)

# Generate boxplot (mean point function not working right now)
ggplot(Bakers) +
  geom_boxplot(aes(x = "", y = diff))+
  stat_summary(aes(x = "", y = diff), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate qq plot
ggplot(Bakers)+
  geom_qq(aes(sample = diff))

# Anaylsis: The histogram has a right skew. The boxplot has several outliers past the upper whisker, the mean is also 
# located outside the IQR and at the end of the upper whisker. The lower whisker is shorter than the upper indicating
# the skew. The q-q plot shows sever points that do not fall along the trendline though the sample size of not very
# large (n = 20). In conclusion, the data is not normally distributed violating an assumption of the pair t-test. 

# Can log transform the data to see if possible improvement emerge 

Bakers <- Bakers %>%
  mutate(logAfter = log(After))

Bakers <- Bakers %>%
  mutate(logBefore = log(Before))

Bakers <- Bakers %>%
  mutate(diff_logs = logAfter - logBefore)

# Generate transformed histogram
ggplot(Bakers) +
  geom_histogram(aes(diff_logs), binwidth = .085)

# Generate transformed boxplot
ggplot(Bakers) +
  geom_boxplot(aes(x = "", y = diff_logs))+
  stat_summary(aes(x = "", y = diff_logs), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate transformed qq plot
ggplot(Bakers)+
  geom_qq(aes(sample = diff_logs))

# Anaylsis: Some improvement was made to histogram symmetry. The boxplot now has the mean within the IqR, though it
# is above the median. There are now two outliers instead of three and the boxplot whiskers are even. Execpt for two
# values, the observations fall on a line. 

# I would say the log-transformed data provided enough correction of the data skew to justify using a 
# paired t-test. Paired t-test are highly suspectiable to the impact of outliers,, but with significant normalization 
# from the transformation it's use can be justified.  


# 5. Perform the appropriate statistical analysis in R and report the results as you would in a scientific paper.

# Two-sided
t.test(Bakers$logAfter, Bakers$logBefore, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)
# The mean concentration of streptococcus antibody within healthy volunteers after vaccination is statistically 
# different from the mean concentration of streptococcus antibody before vaccination.
#(Two-sided Paired t-test: t= 3.3577, df= 19, P= 0.003306)

### Question 18####

# install.packages("abd", repos="http://R-Forge.R-project.org")
library("abd")

algae <- AlgaeCO2

# 1. Identify the variable or variables. For example,the variables in the diatom experiment are: response = diversity,
# predictor = zinc level.
# Reponse = Numerical, continous: algae growth rate
# Predictor = categorical: CO2 treatment  

# 2. What is the statistical null hypothesis?  If you cannot find the μ symbol, you can just write mu.

# H0: The mean algae growth rate within the high CO2 environment is the same as the 
# mean algae growth rate within the normal CO2 enviornment.  

# 3. What is the default appropriate statistical test (if all assumptions were met)?

# The default appropriate statistical test is a two sample t-test. 

# 4. Do the data meet the assumptions of this test?  Give specific information to support your choice for each
# assumption.

# The assumptions of a two sample t-test are:
# 1. Experimental units are randomly sampled from population and independent. 
# 2. The observations are normally distrubted. 
# 3. The two groups have homoscedasticity.

# Look at the summary statistics
summ_algae <- algae %>%
  group_by(treatment) %>% 
  summarise(mean_algae = mean(growthrate),
            sd_algae = sd(growthrate),
            n_algae = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity (how normally distributed is the data noise)
ratio <-(max(summ_algae$sd_algae))/(min(summ_algae$sd_algae))
# Ratio is 1.13, less than 3 so there is homogenous variance allowing us to proceed with the parametric test

# To determine if the data is normally distrubted by generating a histogram, boxplot, and q-q plot

ggplot(algae) +
  geom_histogram(aes(growthrate), binwidth = 0.37)+
  facet_wrap(~treatment)

# Generate box plot
ggplot(algae) +
  geom_boxplot(aes(x = treatment, y = growthrate))+
  stat_summary(aes(x = treatment, y = growthrate), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(algae)+
  geom_qq(aes(sample = growthrate, color = treatment))

# Anaylsis: The two histograms for high and normal CO2 are normally distributed with a possible slight right skew 
# for the high CO2 group. The boxplots had no outliers, both means were just above the median
# inside the IQR. The upper whisker for the high CO2 group was longer than he lower whisker indicating slight
# the skew. The q-q plot shows all points fall along two condition trendline. In conclusion, this data is 
# satisfactory to complete a two-side two sample t-test.

# 5. Perform the appropriate statistical analysis in R and report the results as you would in a scientific paper.

# Two-sided
t.test(growthrate ~ treatment, data = algae, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# The mean algae growth rate within the high CO2 environment is not statistically different from the mean algae growth rate 
# within the normal CO2 enviornment. (Two-sided two sample t-test: t= -0.53606, df= 12, P= 0.6017)  
