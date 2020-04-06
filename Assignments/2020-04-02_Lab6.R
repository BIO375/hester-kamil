### Lab 6 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

### Install and load packages ####
# The following commands will install these packages if they are not already installed, 
# and then load them!

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}

# Open tidyverse
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

# Check for updates
tidyverse_update()

### Question 13 ####

# Read in Craig and Foote 2001) data file
SalmonColor <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

# 13-20a. #####
# List two methods that would be appropriate to test whether there was a difference in mean
# skin color between the two groups.

# Two methods that would be appropriate are a Mann-Whitney-U test, and a two-sample t-test, because two means are being 
# compared that do not have multiple observations per experimental unit. 
# Two-sample t-tests have high power, the two mean sample sizes are similar, and  this test can withstand violation of the 
# assumption that there is equal distribution in both groups. 
# The Mann-Whitney-U test is a nonparametric version of the two-sample t-test, however rejecting H0
# only conveys one group's distrubtion has shifted left or right, as the null hypothesis is that the distributions are the same. 

# Two sample t-test

# HA:Kokanee Sockeye salmon mean skin color is different from standard Sockeye salmon. 
# H0:Kokanee Sockeye salmon mean skin color is equal to that of standard Sockeye salmon.

# Pooled variances
# Look at the summary statistics
summ_salmon <- SalmonColor %>%
  group_by(species) %>% 
  summarise(mean_sal = mean(skinColor),
            sd_sal = sd(skinColor),
            n_sal = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity (how normally distributed is the data noise)
ratio <-(max(summ_salmon$sd_sal))/(min(summ_salmon$sd_sal))
# 4.3, ratio is over 3 suggesting data noise that is not normally distrubted 


# Look at histogram
ggplot(SalmonColor) +
  geom_histogram(aes(skinColor), binwidth = 0.2)+
  facet_wrap(~species)

# Generate box plot
ggplot(SalmonColor) +
  geom_boxplot(aes(x = species, y = skinColor))+
  stat_summary(aes(x = species, y = skinColor), 
               fun=mean, 
               colour="blue", 
               fill = "grey",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(SalmonColor)+
  geom_qq(aes(sample = skinColor, color = species))

# Anaylsis: A right skew indicated in the standaed sockeye histograms. 
#The boxplots also indicate some right skew in the sockeye group: there is a high outlier, however it appears the lower
# whisker is longer than the upper. 
# The q-q plot shows that for both groups the data mostly falls on a line. 
# However the sample sizes are is large so a parametric test is still OK.

# Two-sided
t.test(skinColor ~ species, data = SalmonColor, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# HA:Kokanee Sockeye salmon mean skin color is different from standard Sockeye salmon. 
# (Two-sided Two-sample t-test: t= 10.297; df= 33; P= 7.747e-12)

# Mann-Whitney-U test
# The Mann-Whitney U Test is equivalent to the Wilcoxon rank-sum test.  

# Two-sided
wilcox.test(skinColor ~ species, data = SalmonColor, alternative = "two.sided", conf.level = 0.95)
# HA:Kokanee Sockeye salmon mean skin color is different from standard Sockeye salmon. 
# (Wilcoxon rank sums test: W= 303; P= 6.153e-07)

# 13-20b. #### 
# Use a transformation to test whether there is a difference in mean between these two
# groups. Is there a difference in the mean of kokanee and sockeye skin color?
  
# Add new column of logtransformed skin measurements to the SalmonColor dataset
SalmonColor<-mutate(SalmonColor, skinColor = log(skinColor))

# Recalculate summary statistics
summ_salmon2 <- SalmonColor %>%
  group_by(species) %>% 
  summarise(mean_sal = mean(skinColor),
            sd_sal = sd(skinColor),
            n_sal = n())

# Recalculate the ratio between the standard deviations as a loose test of homoscedasticity (how normally distributed is the data noise)
ratio2 <-(max(summ_salmon2$sd_sal))/(min(summ_salmon2$sd_sal))
# 2.6, ratio is below 3 suggesting data noise that is now normally distrubted, making two-sample t-test most appropiate test

# Look at histogram
ggplot(SalmonColor) +
  geom_histogram(aes(skinColor), binwidth = 0.1)+
  facet_wrap(~species)

# Generate box plot
ggplot(SalmonColor) +
  geom_boxplot(aes(x = species, y = skinColor))+
  stat_summary(aes(x = species, y = skinColor), 
               fun=mean, 
               colour="blue", 
               fill = "grey",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(SalmonColor)+
  geom_qq(aes(sample = skinColor, color = species))

# Anaylsis: The log transformation appears to have normalized the sockeye boxplot 

# Two-sided
t.test(skinColor ~ species, data = SalmonColor, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# HA:Kokanee Sockeye salmon mean skin color is different from standard Sockeye salmon. 
# (Two-sided Two-sample t-test: t= 12.133; df= 33; P= 1.038e-13)

### 13-25. ####
# Test whether there is a change in biomass of rainforest areas following clear-cutting.

# readLaurance et al. (1997) file 
Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

# The most appropriate test for whether there is change in biomass within rainforest areas is
# the non-parametric sign test, because the null hypothesis of this question that the median of difference between
# the different areas will be 0. A sign test also shows perference one way or another. 

# One-sample, Two-sided
SignTest(Clearcuts$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
# H0: The median of differences of change in biomass is zero. (S= 21; number of differences=36; P= 0.405)

### 13-26. ####
# Choose an appropriate method and test whether females preferred one type of male over the
# other type.

# Read in (Blount et al. 2003) file. 
ZebraFinchBeaks <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")

# HA: Female Zebra Finches have a preference for carotenoid-supplemented male Zebra Finches. 
# H0: Female Zebra Finches have no preference for carotenoid-supplemented male Zebra Finches. 
# Because the null hypothesis is that the means are the same and both higher or lower mean could reject the null the test is two sided
# A one sample t-test would be most appropriate because its designed to actual mean from one observation per mean with 
# the mean proposed by the null. 

# One-sample t-test can be calculate using t.test.(Output in the console) 
# The mu argument gives the value stated in the null hypothesis.

# The code below ASSUMES that you have read in the data file
# Now you have to specify which dataset the values are coming from using the form dataset$variable_name.

# Two-sided
t.test(ZebraFinchBeaks$preference, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# Female Zebra Finches have a preference for carotenoid-supplemented male Zebra Finches. 
# (one sample t-test; t= 5.6198, df= 9, P= 0.0003259)

### Review 2-16a. ####
# With these data, estimate the magnitude of the effect of the mutation (the difference between the means) 
# on the amount of time spent in aggressive activity. Put appropriate bounds on your estimate of the effect.

# HA: The Zebrafish with the spiegeldanio (spd) mutation at the Fgfrla gene spent greater time in aggressive activity than wildtype Zebrafish
# H0: The Zebrafish with the spiegeldanio (spd) mutation at the Fgfrla gene spent less than or equal time in aggressive activity than wildtype Zebrafish

# The most appropriate test is the two-sample t-test though two means are being compared both experimental treatments
# were not applied to each experimental unit ruling out at paired t-test

# Read in Norton et al. (2011). file
ZebrafishAggression <- read_csv("datasets/abd/review2/rev2q16ZebrafishAggression.csv")

# Pooled variances
# Look at the summary statistics
summ_Zebrafish <- ZebrafishAggression  %>%
  group_by(genotype) %>% 
  summarise(mean_Zebrafish = mean(timeInAggression),
            sd_Zebrafish = sd(timeInAggression),
            n_Zebrafish = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity (how normally distributed is the data noise)
ratio <-(max(summ_Zebrafish$sd_Zebrafish))/(min(summ_Zebrafish$sd_Zebrafish))
# 1.39, less than 3 so data is normally distributed

# Look at histogram
ggplot(ZebrafishAggression) +
  geom_histogram(aes(timeInAggression), binwidth = 10)+
  facet_wrap(~genotype)

# Generate box plot
ggplot(ZebrafishAggression) +
  geom_boxplot(aes(x = genotype, y = timeInAggression))+
  stat_summary(aes(x = genotype, y = timeInAggression), 
               fun.y=mean, 
               colour="blue", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(ZebrafishAggression)+
  geom_qq(aes(sample = timeInAggression, color = genotype))

# Anaylsis: Both histograms, appear to have relatively normal distributions
# Boxplots indicate some leftward skew in the wild type group: the upper whisker is longer than 
# the lower whisker, and the mean is smaller than the median.  
# In the Q-Q plot, the extreme points do not lay in a line, there is not a large sample size for either group n1=10 & n2=11.
# Because, the two-sample t-test is a robust test that can withstand minor violations we are okay to proceed with this parametric test. 


# NOTE: Group 1 and Group 2 are ordered alphabetically unless you specify otherwise
# In the output of the t-test, the first mean under "sample estimates" is group 1, the second is group 2
# One-sided, HA is greater than 0
t.test(timeInAggression ~ genotype, data = ZebrafishAggression, var.equal = TRUE, alternative = "greater", conf.level = 0.95)
# The Zebrafish with the spiegeldanio (spd) mutation at the Fgfrla gene spent greater time in aggressive activity than wildtype Zebrafish.
# (one-sided two-sample t-test: t= 3.3802; df= 19; P= 0.001571)

### Review 2-16b. ####
# What is the weight of evidence that this effect is not zero? Perform an appropriate
# statistical test of the difference.

# The most appropriate statistical test to determine if the effect shown is not zero is a non-parametric Mann-Whitney U test 
# This is because the null hypothesis of this question that the distribution between the mutant and 
# wildtype zebrea will identical. A test can show the shift in distrution if HA is true and there is a non zero effect.

# Two-sided
wilcox.test(timeInAggression ~ genotype, data = ZebrafishAggression, alternative = "two.sided", conf.level = 0.95)
# The Zebrafish with the spiegeldanio (spd) mutation at the Fgfrla gene spent statistically different amounts of time in aggressive activity than wildtype Zebrafish.
# (Two-sided Wilcoxon rank sum test W= 90; P= 0.01272)
