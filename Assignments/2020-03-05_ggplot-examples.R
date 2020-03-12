### Lab 4. Data manipulation and graphing ####

# R for Data Science, Chapter 3 ####
# https://r4ds.had.co.nz/data-visualisation.html

# Tidyverse book Ch.3 (online version, actually Ch.1 in text version)
rm(list = ls())

### 3.1 Introduction ####

#install.packages("tidyverse")
library("tidyverse")
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(gapminder)){install.packages("gapminder")}
if(!require(Lahman)){install.packages("Lahman")}
tidyverse_update()

### 3.2 First steps ####

#Generate a ggplot (scatterplot)
?geom_point
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# ggplot template
# ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
# If you plot two categorical variables, the plot is not informative.

### 3.3. Aesthetic mappings ####

# Organize categorical data by color on scatterplot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Organize categorical data by size on scatterplot(Only works for specific datasets)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Organize categorical data by alpha (transparency) on scatterplot (Only works for specific datasets)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Organize categorical data by shape on scatterplot 
# (Only works on 6 variables at a time before more need to be added manual)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# Generate scatterplot with entirely blue dots
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

#       When I map a continuous varialbe to size, smaller
#     values have smaller size points.  When I map a continuous variable to shape, R throws up an error
#     "Error: A continuous variable can not be mapped to shape"

# Mapping a continuous variable to color, provides a legend with a gradient with lighter shades for larger numbers and darker shades for lower numbers
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))

# Mapping a continuous variable to size, provides a legend with smaller values as smaller size points and larger values as larger size points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))

# Mapping a continuous variable to shape, R throws up an error
#     "Error: A continuous variable can not be mapped to shape"

# Mapped continuous variable cty to both color and size is possible. But not necessary! 
#     Generate two legends.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty, size = cty))

# Stroke aesthetic changed the thickness of the line outlining a shape.  As the vignette ggplot2-specs states, "Note that 
#     shapes 21-24 have both stroke colour and a fill. The size of the filled part is controlled by size, 
#     the size of the stroke is controlled by stroke. Each is measured in mm, and the total size of the 
#     point is the sum of the two
?geom_point
# Looking up specs in the package ggplot2
??ggplot2::specs

# Stroke does not necessarily have to be a number, could also be a variable
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class, stroke = cyl))

# R hates this because only 21-24 can do stroke.  If I instead made class different colors and specify
# that all points are shape = 21, a circle, R is happier.
# Stroke better with circular shapes 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class, stroke = cyl), shape = 21)

# displ < 5 is a logical statement, it has two possible outcomes: FALSE and TRUE.  The color
#     blue is assigned to TRUE and the color red is assigned to FALSE
#     paratheses are around the whole aes section of the plot, not just x, y specs
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5))

### 3.4. Common Problems ####
ggplot(data = mpg) + 
 geom_point(mapping = aes(x = displ, y = hwy))


### 3.5.  Facets ####

# nrow controls the number of rows in a facet wrap panel.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# facet wrap panel where nrow (number of rows) is populated by drv and ncol (number of columns) is populated by cty
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# Faceting an integer that behaves like a continous variable like cty I get a different plot for
#     every value of cty.  If I facet on a truly continuous variable like displ it does the same
#     thing for every value.  In this dataset it ends up looking not insane, but if you had a continuous
#     variable in which every observation was unique, then you would have one point per plot.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty)

# can no longer read x-axis of plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, y = hwy)) + 
  facet_wrap(~ displ, nrow = 2)

# Facet grid formula is (variable) ~ . that means organize variable vertically.  If the facet grid formula
#     is . ~ (variable), that means organize variable horizontally. 
# The period allows for the variable to be made a facet as a row or column without a second variable shown.

#  This code generates a scatterplot of hwy vs. displ sorted into three rows representing the categories of drv (4, f, r).
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

# This code generates a scatterplot of hwy vs. displ sorted into four column representing the categories of cyl (4, 5, 6, 8).
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# Faceting is not very useful when some of the categories that you are faceting by (in this case 2seater)
#     don't include very many data points.  Color might be more useful because then the overall trends
#     between hwy and displ can be discerned.  If there were more observations, then faceting can help
#     pick out differences in the hwy ~ displ relationship among car types that would be harder to see 
#     in a single graph
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# Facets argument, graph will look the same with either ~cyl or vars(cyl).  When using facet_grid, the 
#     number of rows is defined by a variable like cyl, which is why you don't give an actual number like 2.

?facet_wrap
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(vars(cyl))

# Can also use vars for two variables
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+
  facet_wrap(vars(cyl,drv))

### 3.6.  Geometric objects ####

# geom_point vs. geom smooth
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# geom_smooth removes scatterplot points, creates a line with confidence interval
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Makes three seperate lines for categorical drv variable
?geom_smooth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# regular geom_smooth plot vs geom_smooth plot with categorical variable mapped
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

# Color coded dots for categorical class data, and added layer for line plot for subcompact only
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)


# Depending on what you want, you can use geom_line for a jagged line like a time series, geom_abline
#     for a straight line specified by slope and intercept, geom_path to trace follow from timepoint 1 to 
#     timepoint 2, to 3 etc.

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = 37, slope = -5)
# But this is easier to do with geom_smooth:
p + geom_smooth(method = "lm", se = FALSE)

# geom_line() is suitable for time series
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_line()

# geom_path lets you explore how two variables are related over time,
# e.g. unemployment and personal savings rate
m <- ggplot(economics, aes(unemploy/pop, psavert))

# cannot understand plot without color and legend
m + geom_path()

m + geom_path(aes(colour = as.numeric(date)))

# show.legend = FALSE removes the legend.  It was used earlier in the chapter because three graphs
#     were presented together and they did not use the same legend.

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# According to the handy dandy help file, se argument will "Display confidence interval around smooth? (Gray around line) 
#     (TRUE by default, see level to control.)
?geom_smooth

# They do not look different because both use the same mappings, the same x variable and the same y
#     variable.  The only difference is the second plot puts that info in the first line, and the first has
#     to specify that information in both the second and third line.  The second is more efficient.

# Standard code
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Simplified code
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# Generate scatterplot with larger points and no confidence interval in the trendline.Trendline made thicker
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 5)+
  geom_smooth(se = FALSE, size = 2)

# Same as the plot above with trend lines for categorical drv variable mapped
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 5)+
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)

# Same plot as above, but drv is not just mapped with trendlines the three categories are also colored with attached legend
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)

# Removal of three seperate trendline, but left drv dots color coated with legend
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(se = FALSE, size = 2)

# Same plot as above but there are three different trendlines for the drv categories 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(mapping = aes(group = drv, linetype = drv), se = FALSE, size = 2)

# Drv colored and white border generated around a data points 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point( size = 5, stroke = 2, color = "white")+
  geom_point( mapping = aes(color = drv), size = 3)

### 3.7. Statistical transformations ####

# Generate bar graph were x-axis is cut and y-axis is count (number of observations in each dataset) 
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

# geom_bar can be replaced with stat_count
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# create brand new dataset
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

# Generated bar graph from new dataset (stat = "identity", what R is calculating behind your back and then displaying)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

# Changes stat to prop (groupwise proportion) and then organized bar graphs from lowest to highest
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

# Plots median depth for each cut type with whiskers for max and min
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# The default geom is pointrange
?stat_summary
?geom_pointrange

# Generate a table and then plot median depth with max and min shown (Second way to make same plot?)
diamonds_summary <- diamonds %>%
  group_by(cut) %>%
  summarise(min_depth = min(depth),
            max_depth = max(depth),
            med_depth = median(depth))
ggplot(data = diamonds_summary, mapping = aes(x = cut, y = med_depth)) +
  geom_pointrange(mapping = aes(ymin = min_depth,
                                ymax = max_depth) )

# geom_bar() makes the height proportional to the number of cases in each group.  In other words,
#     geom_bar() shows the count of each group in the dataset.  So if you had a dataset with groups
#     a, b, c, and d and each there were 10 observations in each group, each bar would be 10 high.
#     geom_bar() only requires a single categorical variable
#     geom_col() requires a categorical variable and a continuous variable and shows group sums.

# Will autopopulate y-axis with count
ggplot(data = mpg, aes(x = fl))+
  geom_bar()

# geom_col needs x and y 
ggplot(data = mpg, aes(x = fl, y = hwy))+
  geom_col()

summ_hwy <- mpg %>%
  group_by(fl) %>%
  summarise(sum = sum(hwy),
            mean = mean(hwy))

#     geom_histogram, stat = "bin"
#     geom_bar, stat = "bar"
#     geom_col stat = "count"
#     geom_point stat = "identity"
#     geom_pointrange stat = "identity"
#     In terms of what they have in common, it is what R is calculating and then displaying


# stat_smooth() is calculating a predicted value for y and then a confidence interval around that
#     point by point.  The predicted value of y comes from a fit equation.  The default fitting method is
#     method = "loess" which according to Wikipedia is "LOESS curve fitted to a population sampled from a 
#     sine wave with uniform noise added." (Not ideal for class)

#   INSTEAD, for a linear regression, we use method = "lm" which stands for linear model. 
#     the equation of the best fit line allows us to plug in each value of x and then get the predicted y.
#     So for example, in the graph below, I'm plotting a simple dataset looking at the relationship
#     between rainfall and biomass.  I write the equation in the form y ~ x, or biomass ~ rainfall.
environment <- read_csv("datasets/r4all/environment.csv")
plot01 <-ggplot(data = environment, mapping = aes(x = rainfall.m, y = biomass.g.per.m2))+
  geom_point()

# Excute the plot just saved in environment 
plot01

# Fit the linear model, biomass.g.per.m2 ~ rainfall.m, part of the output is the slope and intercept
model01 <- lm(biomass.g.per.m2 ~ rainfall.m, environment)

# Excute the linear model just saved in environment 
model01

# Can create a new column in the datatable called predicted OR, just plot the fitted values
plot02 <- plot01+
  geom_point(mapping = aes(x = environment$rainfall.m, y=model01$fitted.values), color = "red")

# Layers plot01 over plot02. The dots for plot02 are red
plot02

# Can generate 95% confidence interval and make a nice smooth curve, specify the smoothing method as lm.
plot03 <- plot02+
  geom_smooth(method = "lm")

# Adds another layer of confidence interval 
plot03

# Of course, the default method is this messy looking loess fit instead of a pretty linear fit.  
plot04 <- plot02 +
  geom_smooth()

# geom_smooth without lm makes a much more bumpy confidence interval, less useful
plot04

# Bars are showing groupwise proportions on the y axis, unuseful because the number of fair cut diamonds/ total number of fair cut diamonds is 1.00.
.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))

# Each bar can show the amount of the D cut diamonds through color out of total for each category 
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))


### 3.8.  Position adjustments ####

# Outlines the bars in color
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

# Completely fills in the color of the bar
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))


# Fills the graph bars based on clarity of the diamond
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# same as plot above but slight transparent (harder to read honestly)
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

# Same as plot above but the clarity is outlined in color, but the bars are clear
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# Position = "fill" makes all the bars in the graph the same height for easy comparison
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# Position = "dodge" makes all overlapping values right beside each other for easy comparison of individual values
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# Position = "jitter" adds random noise to scatterplot points to represent missing data
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, y = hwy), position = "jitter")
?position_dodge
?position_fill
?position_identity
?position_jitter
?position_stack

# width and height control jitter in the geom_jitter()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(width=0.2, height = 0)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(width = 0, height=0.2)
?geom_jitter
?geom_count

#geom_jitter makes overlapping points more visible by jittering them slightly.
#    geom_count  makes the point size scale with the number of points that overlap.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_jitter()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_count()

# The boxplots are ordered alphabetically by category and use the position = "dodge"
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=class))

### 3.9.  Coordinate systems ####

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

# coord_flip() switches the x and y axes
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# Install packages to do maps
install.packages("maps")
library("maps")
nz <- map_data("nz")

# coord_quickmap () corrects the aspect ratio of maps, and coord_polar () uses polar coordinates
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_quickmap () and coord_polar () are useful for bar graph data
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

# Turn a stacked bar chart into a pie chart using coord_polar().
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill = cut))+
  coord_flip()
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill = cut))+
  coord_polar()

# labs() allows you to specify axis labels, titles, subtitles, etc.
?labs

# coord_map is a projection that preserves straight lines and is computationally intensive, whereas
#     coord_quickmap is, well, quicker, but is going to be very distorted near the poles.

# geom_abline () adds refernce lines to plots
# coord_fixed() changes the aspect ratio, makes x and y axis equal.
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()
?geom_abline
?coord_fixed

### 3.10.  The layerd grammar of graphics ####

# Template
# ggplot(data = <DATA>) + 
# <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>