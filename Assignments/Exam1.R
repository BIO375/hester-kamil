### Exam 1 ####

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
if(!require(tidyverse)){install.packages("tidyverse")}

### Question 14####
# Open brine shrimp file
polyploid <- read.csv("datasets/demos/polyploid.csv")

#Calculate summary statistics 
summ_polyploid <- polyploid %>%
  group_by(ï..ploidy) %>%
  summarise(n_length = n(),
            mean_length = mean(length),
            median_length = median(length),
            sd_length = sd(length),
            IQR_length = IQR(length),
            var_length = var(length),
            se_length = sd(length)/sqrt(n()))

# Generate boxplots for diploid and polyploid
ggplot(polyploid)+
  geom_boxplot(aes(x = ï..ploidy, y = length), varwidth = TRUE)

# Transform dataset by adding new column of log(y+1) transformed time to mating to the Crickets dataset
polyploid<-mutate(polyploid, log_length = log(length+1))

#Generate boxplot for log transformed diploid and polyploid
ggplot(polyploid)+
  geom_boxplot(aes(x = ï..ploidy, y = log_length), varwidth = TRUE)

### Question 15####
# Open lizard file
Lizards <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
Lizards <- Lizards %>% slice(-105)

#Calculate summary statistics 
summ_Lizards <- Lizards %>%
  group_by(Survival) %>%
  summarise(n_squamosalHornLength = n(),
            mean_SHL = mean(squamosalHornLength),
            median_SHL = median(squamosalHornLength),
            sd_SHL = sd(squamosalHornLength),
            IQR_SHL = IQR(squamosalHornLength),
            var_SHL = var(squamosalHornLength),
            se_SHL = sd(squamosalHornLength)/sqrt(n()))

### Extra Credit ####
# Open crickets file
Crickets <- read.csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

# Create 4 histograms of the data
# Execute code for 2 histograms of time to mating untransformed data
ggplot(Crickets) +
  geom_histogram(aes(timeToMating), binwidth = 4.7)+
  facet_wrap(~feedingStatus)

# Transform dataset by adding new column of log(y+1) transformed time to mating to the Crickets dataset
Crickets<-mutate(Crickets, log_timeToMating = log(timeToMating+1))

# Execute code for 2 histograms of time to mating transformed data
ggplot(Crickets) +
  geom_histogram(aes(log_timeToMating), binwidth = .4)+
  facet_wrap(~feedingStatus)

