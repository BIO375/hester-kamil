### Lab 3. Data manipulation and graphing

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

### Lovett data file ####

# Load and save lovett.csv
Lovett <- read_csv("datasets/quinn/chpt2/lovett.csv")

# Calculate summary statistics

summary(Lovett)

summ_Lovett <- Lovett %>%
  summarise(mean_SO4 = mean(SO4),
            median_SO4 = median(SO4),
            IQR_SO4 = IQR(SO4),
            sd_SO4 = sd(SO4),
            var_SO4 = var(SO4),
            se_SO4 = sd(SO4)/sqrt(n()))

summ_LovettMOD <- Lovett %>%
  summarise(mean_modSO4 = mean(SO4MOD),
            median_modSO4 = median(SO4MOD),
            IQR_modSO4 = IQR(SO4MOD),
            sd_modSO4 = sd(SO4MOD),
            var_modSO4 = var(SO4MOD),
            se_modSO4 = sd(SO4MOD)/sqrt(n()))

### Plot histograms of SO4 and Modified SO4 ####

# Excute code for SO4 histogram
ggplot(Lovett)+
       geom_histogram(aes(SO4), binwidth = 1)

# Excute code for Modified SO4 boxplot
ggplot(Lovett)+
       geom_histogram(aes(SO4MOD), binwidth = 3)

### Plot boxplots of SO4 and Modified SO4 ####

# Excute code for SO4 boxplot
ggplot(Lovett)+
       geom_boxplot(aes(x = "", y = SO4), varwidth = TRUE)

# Excute code for Modified SO4 boxplot

# The code below modifies the dataset so it only contains SO4 and Modified SO4
# using select{dplyr}, and is oriented in long form using gather{tidyr}
Lovett_tidy <- Lovett %>%
  select(contains("SO4"))%>%
  gather(key = "type", value = "measurement", SO4, SO4MOD)

# The code below plots the two variables as boxplots, zooming in on the
# 40-75 range where most of the values are found (coord_cartesian).  The red 
# dots indicate the means (stat_summary).
ggplot(data = Lovett_tidy)+
  geom_boxplot(aes(x = type, y = measurement))+
  coord_cartesian(ylim = c(40, 75))+
  stat_summary(aes(x = type, y = measurement), fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3)


### Sanchez data file ####

# Load and save sanchez.cvs
Sanchez <- read_csv("datasets/demos/sanchez.csv")

# Calculate summary statistics

summary(Sanchez)

summ_Sanchez <- Sanchez %>%
    group_by(COLTYPE) %>%
    summarise(mean_beetle = mean(BEETLE96),
            median_beetle = median(BEETLE96),
            IQR_beetle = IQR(BEETLE96),
            sd_beetle = sd(BEETLE96),
            var_beetle = var(BEETLE96),
            se_beetle = sd(BEETLE96)/sqrt(n()))

### Transform dataset ####

# Add new column of log(y+1) transformed beetle densities to the sanchez dataset
Sanchez<-mutate(Sanchez, log_BEETLE96 = log(BEETLE96+1))

### Plot histograms of beetle density ####

# Plot two histograms bycolony type, before and after data transformation, using facet_wrap()

# Excute code for before data transformation beetle density histogram
ggplot(Sanchez) +
       geom_histogram(aes(BEETLE96), binwidth = 5)+
       facet_wrap(~COLTYPE)

# Excute code for after data transformation beetle density histogram
ggplot(Sanchez) +
       geom_histogram(aes(log_BEETLE96), binwidth = 1.2)+
       facet_wrap(~COLTYPE)


### Plot boxplots of beetle density ####

# Excute code for beetle density before data transformation boxplot 
ggplot(Sanchez)+
            geom_boxplot(aes(x = COLTYPE, y = BEETLE96), varwidth = TRUE)

# Excute code for beetle density after data transformation boxplot
ggplot(Sanchez)+
            geom_boxplot(aes(x = COLTYPE, y = log_BEETLE96), varwidth = TRUE)



### Great job annotating your code! 
### GRADE: 10/10 code runs without breaking ####