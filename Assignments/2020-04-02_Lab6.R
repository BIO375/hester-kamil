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

# 13-20a. List two methods that would be appropriate to test whether there was a difference in mean
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

# 13-20b. Use a transformation to test whether there is a difference in mean between these two
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

# Anaylsis: A right skew indicated in the standaed sockeye histograms. 
#The boxplots also indicate some right skew in the sockeye group: there is a high outlier, however it appears the lower
# whisker is longer than the upper. 
# The q-q plot shows that for both groups the data mostly falls on a line. 
# However the sample sizes are is large so a parametric test is still OK.

# Two-sided
t.test(skinColor ~ species, data = SalmonColor, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# HA:Kokanee Sockeye salmon mean skin color is different from standard Sockeye salmon. 
# (Two-sided Two-sample t-test: t= 10.297; df= 33; P= 7.747e-12)




# 13c.
# 13d.
# 13e.
# 13f. 


