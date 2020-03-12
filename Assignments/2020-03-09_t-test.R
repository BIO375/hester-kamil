### 2020-03-09 t-test packet ####

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

# 3.1.1 Open tidyverse
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

### Scenario 1 ####

# d. Calculate the sample test statistic
# Create dataset
birth_rate <- tribble(
  ~country,        ~birth_1982,  ~birth_2000,
  "Afghanistan",   48,           44,
  "Algeria",       44,           37,
  "Argentina",     24,           20,
  "Austria",       12,           12,
  "Bangladesh",    49,           42,
  "Barbados",      17,           18,
  "Belgium",       12,           12,
  "Bolivia",       42,           35,
  "Brazil",        31,           26,
  "Canada",        15,           14,
  "Chile",         24,           21,
  "Colombia",      28,           27,
  "CostaRica",     31,           28,
  "Cuba",          16,           18,
  "Czechoslov",    15,           14
)

View(birth_rate)

# Calculate summary statistic

summary(birth_rate)

summ_birth_rate1982 <- birth_rate %>%
  summarise(mean_1982 = mean(birth_1982),
            median_1982 = median(birth_1982),
            IQR_1982 = IQR(birth_1982),
            sd_1982 = sd(birth_1982),
            var_1982 = var(birth_1982),
            se_1982 = sd(birth_1982)/sqrt(n()))

summ_birth_rate2000 <- birth_rate %>%
  summarise(mean_2000 = mean(birth_2000),
            median_2000 = median(birth_2000),
            IQR_2000 = IQR(birth_2000),
            sd_2000 = sd(birth_2000),
            var_2000 = var(birth_2000),
            se_2000 = sd(birth_2000)/sqrt(n()))

### Scenario 2 ####

# Open the data file in R
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>% slice(-105)


# g. Use summary statistics calculated in R to determine the sample test statistic
# Calculate summary statistic 

summary(data01)

summ_data01 <- data01 %>%
  group_by(Survival) %>%
  summarise(mean_SHL = mean(squamosalHornLength),
            median_SHL = median(squamosalHornLength),
            IQR_SHL = IQR(squamosalHornLength),
            sd_SHL = sd(squamosalHornLength),
            var_SHL = var(squamosalHornLength),
            se_SHL = sd(squamosalHornLength)/sqrt(n()))

