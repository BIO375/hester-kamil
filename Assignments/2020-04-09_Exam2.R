### Exam 2 R section ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

### Install and load packages ####
# The following commands will install these packages if they are not already installed, 
# and then load them!

if(!require(Rtools)){install.packages("Rtools")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}

# Open tidyverse
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)
tidyverse_update()

### Question 17####

Bakers <- read_csv("/datasets/demos/baker.csv")

### Question 18####

install.packages("abd", repos="http://R-Forge.R-project.org")
library("abd")

algae <- AlgaeCO2

