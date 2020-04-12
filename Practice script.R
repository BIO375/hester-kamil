
Yield <- tribble(
  ~"land",
  11062,
  10268,
  10935,
  13221,
  10852,
  10553,
  13760,
  14627,
  14927
)

# One-sided, HA that sample mean is less than null mean
t.test(Yield$land, 
       alternative = "less", mu = 12500, conf.level = 0.95)

# Import these values manually for confidence interval
alpha <- 0.05
mean <- 0.208
sd <- 0.091
se <- sd/sqrt(df)
df <- 16 -1

# In words, the confidence interval equation is 
# mean plus or minus the product of the critical value of t, given alpha and df, and the standard error of the mean.
# the mean you can calculate
# the "plus or minus" is accomplished with a short vector c(-1,-2) that you multiply by...
# the critical value of t using the function qt():  qt(1-alpha, df = n-1) which is also multiplied by...
# the standard error of the mean

# NOTE that in the swirl text it refers to the critical value of t as t_(n-1)

# If summarise was usedto calculate the descriptive statistics, then the code is to calculate confidence interval
mean + c(-1,1)*qt(1-alpha, df )*se
