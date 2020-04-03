### Lab 5: Confidence intervals, t-tests and friends ####

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

### Confidence Interval of the mean ####

# To show an example of code for a confidence interval (calcium example from class)

coelomic <- tribble(
  ~calcium,
  28,
  27,
  29,
  29,
  30,
  30,
  31,
  30,
  33,
  27,
  30,
  32,
  31
)

# For certain problems, you will be given the mean, standard deviation and sample size.  Here I 
# calculate them directly and name them, mean <- mean(coelomic$calcium), but you can also just type in 
# the number, mean <- 29.76923.
data <- coelomic
summary <- coelomic %>%
  summarise(mean = mean(calcium),
            sd = sd(calcium),
            n = n(),
            se = sd(calcium)/sqrt(n()))

# Import these values manually for confidence interval
alpha <- 0.05
mean <- summary$mean
se <- summary$se
df <- summary$n -1

# In words, the confidence interval equation is 
# mean plus or minus the product of the critical value of t, given alpha and df, and the standard error of the mean.
# the mean you can calculate
# the "plus or minus" is accomplished with a short vector c(-1,-2) that you multiply by...
# the critical value of t using the function qt():  qt(1-alpha, df = n-1) which is also multiplied by...
# the standard error of the mean

# NOTE that in the swirl text it refers to the critical value of t as t_(n-1)

# If summarise was usedto calculate the descriptive statistics, then the code is to calculate confidence interval
mean + c(-1,1)*qt(1-alpha, df )*se

# mean <- 29.76923	
# sd <- 1.786703	
# n <- 13	
# se <- 0.4955423
# df <- n-1
# alpha = 0.05

# mean + c(-1,1)*qt(1-alpha, df)*se

### One sample t-test ####

# Option A: Have t-sample and p-value calculated by equation
# The function pt() calculates the probability of t less than or equal to a sample value. (Opposite t-table)

# Calculate t_sample.  Define what the sample mean, null hypothesis mean, sample 
# standard deviation, and sample size are.  

# If given the values for the sample mean, sd, and n, you can define each value as an object in the environment
sample_mean <- 39.3
sample_sd <- 30.7
sample_n <- 31
df <- sample_n -1

# If given raw data, read in the data file and define each summary statistic with a simple equation
# Note: Don't use summarise here because it will create a table instead of named objects.

# Read in data from Question 1 in Chapter 11 of your book
range_shift <- read_csv("datasets/abd/chapter11/chap11q01RangeShiftsWithClimateChange.csv")

# Identify your response variable using the form dataset$variable_name
# Provides y values
y<-range_shift$elevationalRangeShift

# Calculate summary statistics (Don't use summarise because will make a table, input into environment)
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# Specify your null mean, the certain value specified by the null hypothesis (Typically 0 for no change)
null_mean <- 0

# Whether you are given the values for mean/sd/n or calculate them, your next step is calculating t_sample
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

# The value I call "negative tail" is the exact probability of obtaining t less than or equal to your t-sample
# If you are testing an alternate hypotheses of "sample mean is less than a certain number" then this is your
# p-value
negative_tail <- pt(t_sample, df)

# If you are testing an alternate hypothesis of "sample mean is greater than a certain number" then you have
# to calculate 1 - negative_tail.
positive_tail <- 1 - negative_tail

# For a two-sided test, the exact probability of obtaining t equal to t_sample or more extreme is calculated
# as:
two_tailed <- 2*(1-pt(abs(t_sample), df))

# Option B: One-sample t-test can be calculate using t.test.(Output in the console) 
# The mu argument gives the value stated in the null hypothesis.

# The code below ASSUMES that you have read in the data file
# Now you have to specify which dataset the values are coming from using the form dataset$variable_name.

# Two-sided
t.test(range_shift$elevationalRangeShift, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sided, HA that sample mean is greater than null mean
t.test(range_shift$elevationalRangeShift, 
       alternative = "greater", mu = 0, conf.level = 0.95)

# One-sided, HA that sample mean is less than null mean
t.test(range_shift$elevationalRangeShift, 
       alternative = "less", mu = 0, conf.level = 0.95)

### Paired t-test####

# Start with a dataset with groups not defined by a categorical variable, two observations (or 
# more) in each row.  Later you will use this untidy dataset to perform the statistical test.
# Data comes from Example 12.2 in Chapter 12 of your book.
untidy_blackbird <- read_csv("datasets/abd/chapter12/chap12e2BlackbirdTestosterone.csv")

# Begin by exploring the data with histograms, boxplots, and q-q plots


# Since the assumptions of normality apply to differences, use mutate() to add a column called diff.
# Note that here diff = After - Before (Standard way to do diff)
untidy_blackbird <- untidy_blackbird %>%
  mutate(diff = afterImplant - beforeImplant)

# Generate histogram
ggplot(untidy_blackbird) +
  geom_histogram(aes(diff), binwidth = 10)

# Generate boxplot (mean point function not working right now)
ggplot(untidy_blackbird) +
  geom_boxplot(aes(x = "", y = diff))+
  stat_summary(aes(x = "", y = diff), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate qq plot
ggplot(untidy_blackbird)+
  geom_qq(aes(sample = diff))

# So are the differences normally distributed?
# Histogram: weak left skew, not super informative with n = 13
# Box plot: lower whisker longer than upper whisker and mean a small bit less than median (again indicating weak
#   left skew).  However, the median is centered in the IQR box, there are no outliers, and mean and median are close.
# Q-Q plot: the extreme points do not lay in a line but there are not very many points total (13)

# You could justify not transforming ("In boxplot, the median is centered in the IQR box, there are no outliers, and
# mean and median are close).
# You could also justify transforming ("Histogram shows weak left skew and in the boxplot, the lower whisker is
#   longer than upper whisker and the mean is slightly less than median (again indicating weak left skew))
# What is important, is that you justify your choice. (Important distinction)

# Do a transformation and re-evaluate. Happily, there are already log-transformed
# data in the dataset, so I just calculate diff_logs
untidy_blackbird <- untidy_blackbird %>%
  mutate(diff_logs = logAfterImplant - logBeforeImplant)

# Generate transformed histogram
ggplot(untidy_blackbird) +
  geom_histogram(aes(diff_logs), binwidth = .1)

# Generate transformed boxplot
ggplot(untidy_blackbird) +
  geom_boxplot(aes(x = "", y = diff_logs))+
  stat_summary(aes(x = "", y = diff_logs), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate transformed qq plot
ggplot(untidy_blackbird)+
  geom_qq(aes(sample = diff))

# In professor opinion, log-transforming the data does not change much, possibly some small improvement in
# the histogram's symmetry.

# There are (at least) two methods for paired t-tests.  
# Option 1: is a one sample t-test on the differences, using the function pt().(Not shown)
# Option 2: uses the function t.test().  Unlike using t.test() for a one sample t-test, a two sample t-test
# specifies each group (i.e., before and after), does not take the argument mu = , and takes the argument 
# paired = TRUE.
# Note that the confidence intervals are for the mean difference.

# Two-sided
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# One-sided, HA that afterImplant is greater than beforeImplant
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)

# One-sided, HA that afterImplant is less than beforeImplant
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "less", paired =  TRUE, conf.level = 0.95)

# The most straight-forward way to show var.equal data is to connect each pair with a line.  To do this, you
# first have to make data tidy (each variable has its own column, one observation in each row).

# Generic code to transform untidy data to tidy data
# <new_name> <- <untidy_dataset_name> %>% 
# gather(<one_group>, <other_group>, key = "<heading_for_grouping_variable>", value = "<heading_for_response>")

tidy_blackbird <- untidy_blackbird %>%
  gather(beforeImplant, afterImplant, key="treatment", value = "antibody")

ggplot(tidy_blackbird, aes(x=treatment, y=antibody, group=blackbird)) +
  geom_point(aes(colour=treatment), size=4.5) +
  geom_line(size=1, alpha=0.5) +
  xlab('Testosterone Treatment') +
  ylab('Antibody Production (mOD/min)') +
  scale_colour_manual(values=c("#009E73", "#D55E00"), guide=FALSE) + 
  theme_bw()

### Non-parametric Sign Test ####

# Although not necessary, it is instructive to perform a sign test on the elevational range shift data.

# One-sample, Two-sided
SignTest(range_shift$elevationalRangeShift, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sample, One-sided, HA that sample mean is greater than null mean
SignTest(range_shift$elevationalRangeShift, 
         alternative = "greater", mu = 0, conf.level = 0.95)

# One-sample, One-sided, HA that sample mean is less than null mean
SignTest(range_shift$elevationalRangeShift, 
         alternative = "less", mu = 0, conf.level = 0.95)

# Although not necessary , it is instructive to perform a sign test on the untidy_blackbird data.

# NOTE, for paired you need to specify the difference variable (in this case diff)

# Two-sided
SignTest(untidy_blackbird$diff, alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sided, HA that afterImplant is greater than beforeImplant
SignTest(untidy_blackbird$diff, alternative = "greater", mu = 0, conf.level = 0.95)

# One-sided, HA that afterImplant is less than beforeImplant
SignTest(untidy_blackbird$diff, alternative = "less", mu = 0, conf.level = 0.95)

# If you compare results with the parametric paired t-test, you can see that the P-value is larger
# when performing the Sign Test.


# The below code is just to generate figures in the lab handout.  # data from example 13.4 in your book.
conflict <- read_csv("datasets/abd/chapter13/chap13e4SexualConflict.csv")

# Generate histogram
ggplot(conflict) +
  geom_histogram(aes(difference), binwidth = 30)

# Generate boxplot
ggplot(conflict) +
  geom_boxplot(aes(x = "", y = difference))+
  stat_summary(aes(x = "", y = difference), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(conflict)+
  geom_qq(aes(sample = difference))


### Two sample t-test ####

# Pooled variances
# Read in the Ward & Quinn dataset looking at the egg production of predatory snails
ward <- read_csv("datasets/quinn/chpt3/ward.csv")

# Look at the summary statistics
summ_eggs <- ward %>%
  group_by(ZONE) %>% 
  summarise(mean_eggs = mean(EGGS),
            sd_eggs = sd(EGGS),
            n_eggs = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity (how normally distributed is the data noise)
ratio <-(max(summ_eggs$sd_eggs))/(min(summ_eggs$sd_eggs))

# Look at histogram
ggplot(ward) +
  geom_histogram(aes(EGGS), binwidth = 2)+
  facet_wrap(~ZONE)

# Generate box plot
ggplot(ward) +
  geom_boxplot(aes(x = ZONE, y = EGGS))+
  stat_summary(aes(x = ZONE, y = EGGS), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(ward)+
  geom_qq(aes(sample = EGGS, color = ZONE))

# Anaylsis: A little right skew indicated in both histograms, with a longer tail in the Mussel group.  The boxplots
# also indicate some right skew in the Mussel group: there is a high outlier, the upper whisker is longer than 
# the lower whisker, and the mean is larger than the median.  However the sample sizes are quite large (n =37
# and n = 42) and so a parametric test is still OK.

# For the two-sample t-test with pooled variance, there are additional arguments.  You need to give the 
# formula (response ~ predictor), identify the data, include var.equal = TRUE.

# Two-sided
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# NOTE: Group 1 and Group 2 are ordered alphabetically unless you specify otherwise
# In the output of the t-test, the first mean under "sample estimates" is group 1, the second is group 2
# One-sided, HA that Littor - Mussel is greater than 0
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

# One-sided, HA that Littor - Mussel is less than zero
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "less", conf.level = 0.95)

## Welch's t-test ####

# Read in the Levin et al dataset from example 12.4 from your book.  
salmon <- read_csv("datasets/abd/chapter12/chap12e4ChinookWithBrookTrout.csv")

# Suppose we are interested in potential differences in the proportion of surviving native chinook salmon
# in the presence and absence of invasive brook trout.
# Examine the ratio of the variances
summ_surv <- salmon %>%
  group_by(troutTreatment) %>% 
  summarise(mean_surv = mean(proportionSurvived),
            sd_surv = sd(proportionSurvived),
            n_surv = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_surv$sd_surv))/(min(summ_surv$sd_surv))

# Examine plots for evidence of non-normality.  

#Histogram is pretty worthless because n is so small.
ggplot(salmon) +
  geom_histogram(aes(proportionSurvived), binwidth = 0.05)+
  facet_wrap(~troutTreatment)

# Generate boxplot
ggplot(salmon) +
  geom_boxplot(aes(x = troutTreatment, y = proportionSurvived))+
  stat_summary(aes(x = troutTreatment, y = proportionSurvived), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(salmon)+
  geom_qq(aes(sample = proportionSurvived, color = troutTreatment))

# Go forward assuming that normality has been met but homogeneity of variances has not.
# To perform Welch's t-test, all you need to do is remove the argument var.equal = TRUE

# Two-sided
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "two.sided", conf.level = 0.95)


# One-sided, HA that absent is greater than present
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "greater", conf.level = 0.95)

# One-sided, HA that absent is less than present
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "less", conf.level = 0.95)

### Non-parametric Mann-Whitney U or Wilcoxon Test ####

# For this we are going to return to the cannibal crickets from Exam 1 Extra Credit
cricket <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

# Generate histogram
ggplot(cricket) +
  geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

# Generate boxplot
ggplot(cricket) +
  geom_boxplot(aes(x = feedingStatus, y = timeToMating))+
  stat_summary(aes(x = feedingStatus, y = timeToMating), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(cricket)+
  geom_qq(aes(sample = timeToMating, color = feedingStatus))

# Both groups are right skewed.

# The Mann-Whitney U Test is equivalent to the Wilcoxon rank-sum test.  Similar to our 2-sample t-test 
# examples, we give a formula in the form y ~ x or response ~ predictor.

# Two-sided
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "two.sided", conf.level = 0.95)

# One-sided, HA that fed greater than starved
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "greater", conf.level = 0.95)

# One-sided, HA that fed less than starved
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "less", conf.level = 0.95)

### Question 1####

# Read .csv file of historical accounts
Obliquity <- read_csv("datasets/Earth's spin.csv")

# 1-1. What is your (astronomical) hypothesis of interest?
# The angle of obliquity measured in 1738 was statistically different from the angles measured in the past

# 1-2. What is the null hypothesis?
# The angle of obliquity measured in 1738 was not indepdendent of the angles measured in the past

# 1-3. Use R to perform a one-sample t-test

# Identify your response variable using the form dataset$variable_name
x<-Obliquity$Obliquity

# Calculate summary statistics (Don't use summarise because will make a table, input into environment)
sample_Omean <-mean(x)
sample_Osd <- sd(x)
sample_On <- as.numeric(length(x))
df <- sample_On-1

# Specify your null mean, the certain value specified by the null hypothesis (Typically 0 for no change)
null_mean <- 0

# Whether you are given the values for mean/sd/n or calculate them, your next step is calculating t_sample
t_Osample <- (sample_Omean - null_mean)/(sample_Osd/sqrt(sample_On))

# For a two-sided test, the exact probability of obtaining t equal to t_sample or more extreme is calculated
# as:
two_tailed <- 2*(1-pt(abs(t_sample), df))

# Two-sided
t.test(Obliquity$Obliquity, alternative = "two.sided", mu = 0, conf.level = 0.95)

# We found the obliquity measured in Paris in 1738 was different from the angles measured in the past when compared with earlier measurements
# (two-sided one-sample t-test: t= 2679.1; df= 4; p=1.173-13)

### Question 2####

# Read the heart attack file 
Heart_Attack <- read_csv("datasets/demos/HeartAttack_short.csv", col_types = cols(group = col_character()))

# 2-1. 
# The statistical alternate hypothesis (Ha): 
# The cholesterol level measured in heart-attack patients 2 days post heart attack was greater than in individuals who have not had a heart attack.

# The statistical null hypothesis (Ho):
# The cholesterol level measured in heart-attack patients 2 days post heart attack was less than or equal to individuals who have not had a heart attack.

# 2-2. For the null hypothesis test of interest, what are the Degrees of Freedom?
# The degrees of freedom of group 1 is 27 and of group 2 is 29.

# 2-4. Is there any evidence that the assumption of normality has been violated? Be specific. 
# Must check is the observations are from normally distributed population.

# We must check the two observations variance for homoscedasticity (how normally distributed is the data noise)
summ_Attack <- Heart_Attack %>%
  group_by(group) %>% 
  summarise(mean_attack = mean(cholest),
            sd_attack = sd(cholest),
            var_attack = var(cholest),
            n_attack = n())

# Generate histogram
ggplot(Heart_Attack) +
  geom_histogram(aes(cholest), binwidth = 15)+
  facet_wrap(~group)

# Generate box plot
ggplot(Heart_Attack) +
  geom_boxplot(aes(x = group, y = cholest))+
  stat_summary(aes(x = group, y = cholest), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Generate q-q plot
ggplot(Heart_Attack)+
  geom_qq(aes(sample = cholest, color = group))

# Anaylsis: Group 1 histogram appears to have a normal distribution, while group 2 is a little right skew   
# The boxplots also indicate some right skew in group 2 there is a high outlier, the upper whisker is longer than 
# the lower whisker, and the mean is slightly larger than the median. For group 1 the mean is smaller than the median,
# indication of weak left skew. The medians are not located int the center of the IQR box.
# Q-Q plot: the extreme points do not lay in a line with sample sizes that are large (n =28 and n = 30).
# However there is not enough evidence that the assumption of normality had been violated to prevent the parametric test from being done.
# the ratio of variance is less than 3 suggesting a student t-test rather than Welch's t-test should be conducted.


# What is the ratio smax/smin? Round your answer to 2 decimal places. 
# Calculate the ratio between the standard deviations as a loose test of homoscedasticity 
ratio2 <-(max(summ_Attack$sd_attack))/(min(summ_Attack$sd_attack))
# 2.14, ratio is less than 3 

# 2-5. Preform a two sample t-test or Welch's separate variance t-test

# One-sided, HA that group 1 is greater than group 2
t.test(cholest ~ group, data = Heart_Attack, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

# We found that blood cholesterol in heart attack patients 2 days post heart attack was greater than
# blood cholesterol in individuals who have not had a heart attack
# (one-side two-sample t-test: t= 6.29; df= 56; p = 2.6e-08)

### Question 3####

# Read Furness & Bryant (1996) file
Fulmars <- read_csv("datasets/quinn/chpt3/furness.csv")

# 3-1. What are your options if the assumption of normality are not met? When do you use each option?
# My options if the assumptions of normality are not met are to use a nonparametric tests, such as the Mann-Whitney U or Wilcoxon.
# Wilcoxon test is used when the distrubtion measurements of a population are symmetical.
# Mann-Whitney U is used when the distribution of the two groups being compared is the same shape. 

# 3-2. What null hypothesis does this test actually evaluate? How is that different from a standard two-sample t-test?
# The Mann-Whitney U test evaluates the null hypothesis that the two groups being compared have the same distrbution.
# This is different from a two-sample t-test in that rejecting H0 it doesn't convey information on if the mean is 
# larger or smaller in one group compared to the other. Mann-Whitney U focuses on overall distribution not location information like mean and median. 

# 3-3. What are the underlying assumptions of a Mann-Whitney U (or Wilcoxon) test? 
# Why might this not be a great test for this particular dataset?
# Wilcoxon test assmues when the distrubtion measurements of a population is symmetical.
# Mann-Whitney U assumes when the distribution of the two groups being compared is the same shape. 
# This assumptions are not great for this particular dataset because if we look at histograms, boxplots and q-q plots of the data:

# histogram
ggplot(Fulmars) +
  geom_histogram(aes(METRATE), binwidth = 12)+
  facet_wrap(~SEX)

# boxplot
ggplot(Fulmars) +
  geom_boxplot(aes(x = SEX, y = METRATE))+
  stat_summary(aes(x = SEX, y = METRATE), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# q-q plot
ggplot(Fulmars)+
  geom_qq(aes(sample = METRATE, color = SEX))

# Anaylsis: Male Fulmars have a symmetrical distribution within its histogram, while female Fulmars have a weak right skew.
# Boxplots: For the male boxplot mean = median, while for the female boxplot the mean is higher than the median indicating the right skew. 
# Both have larger upper whiskers than lower whiskers though
# The q-q plot this less revelant because n1 and n2 are small, but there is a good amount of outlier points for the male sex
# This data does not adhear to the assumptions of either test.

# 3-4. Preform Wilcoxon test

# Two-sided
wilcox.test(METRATE ~ SEX, data = Fulmars, alternative = "two.sided", conf.level = 0.95)

# We found that body mass of male fulmar was not statisially different from body mass of female fulmars 
# (Wilcoxon rank sums test: W= 21; P= 0.75).

### Question 4 ####

# Read Elgar et al. (1996) file

Spiders <- read_csv("datasets/quinn/chpt3/elgar.csv")

# 4-1. What is an appropriate statistical test for testing a hypothesis about the difference in horizontal diameter
# of webs spun in light versus dark conditions? Explain why. 

# An appropiate statistical test for testing a hypothesis about the difference in horizontal diameter of webs spun in
# light versus dark conditiond is a two sample t-test because the purpose of the test is to compare means of two 
# groups/conditions.

# 4-2. What is the null hypothesis?
# H0: The horizontal diameter of an orb-spinning spider's web in light conditions is not different from horizontal diameter in dark conditions. 

# 4-3. Do these data meet the assumptions of a standard two sample test? Why not?
# This data meets the assumptions of a standard two sample test because the observations are randomized.
# Calculate the ratio between the standard deviations as a loose test of homoscedasticity 
summ_SpiderLit <- Spiders %>%
  summarise(mean_Spider = mean(HORIZLIG),
            sd_Spider = sd(HORIZLIG),
            var_Spider = var(HORIZLIG))

summ_SpiderDim <- Spiders %>%
  summarise(mean_Spider = mean(HORIZDIM),
            sd_Spider = sd(HORIZDIM),
            var_Spider = var(HORIZDIM))

ratio3 <-(max(summ_SpiderLit$sd_Spider))/(min(summ_SpiderDim$sd_Spider))
# 1.08, ratio is less than 3 

# 4-4. Reform statistical test.

Simplified_Spider <- tribble(
  ~Lighting,   ~Data,
  "Lit",         60,
  "Lit",         140,
  "Lit",         160,
  "Lit",         120,
  "Lit",         180,
  "Lit",         90,
  "Lit",         120,
  "Lit",         220,
  "Lit",         210,
  "Lit",         150,
  "Lit",         160,
  "Lit",         330,
  "Lit",         100,
  "Lit",         240,
  "Lit",         190,
  "Lit",         170,
  "Lit",         100,
  "Dim",         295,
  "Dim",         260,
  "Dim",         280,
  "Dim",         250,
  "Dim",         160,
  "Dim",         150,
  "Dim",         290,
  "Dim",         120,
  "Dim",         210,
  "Dim",         120,
  "Dim",         240,
  "Dim",         270,
  "Dim",         150,
  "Dim",         210,
  "Dim",         200,
  "Dim",         160,
  "Dim",         160
)


# Two-sided
t.test(Data ~ Lighting, data = Simplified_Spider, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# We found that horizontal diameter of an orb-spinning spider's web in light conditions was statistically different
# from the horizontal diameter in dark conditions
# (Two-sided two-sample t-test; t= -2.14; df= 32; P=0.04).