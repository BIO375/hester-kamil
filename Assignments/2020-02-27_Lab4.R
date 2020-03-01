### Chapter 3 Data Visualization####

# 3.1.1 Open tidyverse
install.packages("tidyverse")
library(tidyverse)

# 3.2.2 Create a ggplot of mpg (US Environmental Protection Agency on 38 models of car) 
# where displ is car engine size, and hwy is car fuel efficiency 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# Cars with big engines using more fuel confirms my hypothesis that smaller car engines have greater fuel efficiency

# 3.2.3 Example graphing template used in other data set

# Open compensation file 
compensation <- read_csv("Analyses/hester-kamil/datasets/r4all/compensation.csv")

# Execute sample graphing template 
ggplot(data = compensation) +
  geom_point(mapping = aes(x = Root, y = Fruit))

# 3.2.4 Exercises

# 1. Run ggplot(data = mpg). What do you see?
ggplot(data = mpg)
# I don't see anything after running this line of code. 

# 2. How many rows are in mpg? How many columns?
View(mpg)
# The mpg data set has 234 rows and 11 columns

# 3. What does the drv variable describe? Read the help for ?mpg to find out.
?mpg
# drv refers the type of drive of the car model. 
# The possible imputs for the variable are f=front-wheel drive, r=rear wheel drive, and 4=four wheel drive

# 4. Make a scatterplot of hwy vs cyl.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

# 5. What happens if you make a scatterplot of class vs drv? Why is the plot not useful?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))
# When a scatterplot of class vs drv is generated the type of car is on the z-axis while the type of drive is on the y-axis. 
# This type of plot is not useful because neither variable used is numberical, so quantified data cannot be extracted from the plot

# 3.3 Aesthetic mappings

# Some large engine cars have a higher mileage than expected. How can you explain these cars?
# For now we hypothesize they are hybrids 

# Can convey further information about dat by mapping aesthetics onto plots

# Create ggplot where displ is car engine size, and hwy is car fuel efficiency 
# With class mapped as different colors 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Now that we have color mapped we know these outlier points are 2-seater sports cars

# Can also create plot with class mapped as different size points
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Can create plot with class mapped as alpha aesthetic (changes transparency)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Can create plot with class mapped as different shapes
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
# SUV not shown because only 6 variables can be seen at a time as shapes

# It is also possible to manually override an aesthetic onto a plot
# Example, blue dots only
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# 3.3.1 Exercises 

# 1. What's gone wrong with this code? Why are the points not blue?
# This code is running improperly because There is not a closed parentheses 

# 2. Which variables in mpg are categorical? 
# Which variables are continuous?  
# How can you see this information when you run mpg?

# 3. Map a continuous variable to color, size, and shape. 
# How do these aesthetics behave differently for categorical vs. continuous variables?

# 4. What happens if you map the same variable to multiple aesthetics?

# 5. What does the stroke aesthetic do? What shapes does it work with?

# 6. What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)