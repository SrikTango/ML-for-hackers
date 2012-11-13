###
# Anthony Doan
# Going through the Machine Learning for Hackers 
# by Drew Conway & John Myles White
# Chapter 2 
###

####
# Numeric Summaries
####

data.file <- file.path('data', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')
heights <- with(heights.weights, Height)
#summary(heights)
# Ouput:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  54.26   63.51   66.32   66.37   69.17   79.00 
# Summary function will give you
# 1. min value in vector
# 2. First quartile (25th percentile and is the smallest number that's
#    bigger than 25% of your data)
# 3. The median (50% percentile) 
# 4. The mean
# 5. The 3rd quartile (75th percentile)
# 6. The max value

# Mean function
my.mean <- function(x) {
  return(sum(x) / length(x))
}

# Median function
my.median <- function(x) {
  sorted.x <- sort(x)

  if (length(x) %% 2 == 0) {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  }
  else
  {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

# Variance
my.var <- function(x) {
  m <- mean(x)
  return(sum((x-m)^2) / (length(x) - 1))
} 

#my.var(heights) - var(heights)

# Standard Deviation
my.sd <- function(x) {
  return(sqrt(my.var(x)))
}

#my.sd(heights) - sd(heights)

####
# Exploration Data Visualization
####

# Two forms of exploratory data visualization
# 1. single-column visualizations
#      highlight the shape of your data
# 2. two-column visualizations
#      highligh the relatinoship between pairs of columns

library('ggplot2')
data.file <- file.path('data', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')
#ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)
# oversmoothing (can't see the bellcurve anymore)
#ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 5)
# undersmoothing (can't see the bellcurve anymore)
#ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 0.001)

###
# Kernel Density Estimates (KDE)
###
# because finding the bindwidths is too tedious
#ggplot(heights.weights, aes(x = Height)) + geom_density()

# Graph doesn't really look like a bell curve since it's flat at the peak...
# hidden data set?
# Split up plot by qualitative variables you have available 
# to see if there are any hidden data set

# Used gender of each point to split up data into two parts
#ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density()
# Two different bell curves representing each genders. 
# Males and females have different averages.

# KDE for weights instead of heights
#ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density()

# separate the two bell curves representing each genders
#ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density() + facet_grid(Gender ~ .)

###
# play around with normal distributions
###
#m <- 20 
#s <- 20 
#ggplot(data.frame( X = rnorm(100000, m,s)), aes(x = X)) + geom_density()

###
# Normal VS Cauchy Distributions
###
#set.seed(1)
#normal.values <- rnorm(250, 0, 1)
#cauchy.values <- rcauchy(250, 0, 1)

#range(normal.values)
#range(cauchy.values)

#ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density()
#ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()

###
# Gamma Distributions
###
#gamma.values <- rgamma(100000, 1, 0.001)
#ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density()

###
# Scatterplot of Height x Weight
###
#ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()
# from sccatterplot there is a relationship between Height and Weight
# taller people tends to weigh more.

###
# Regression
###
# Smoothing tool (seems like a regression line)
#ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

# the more data we get the better the smoothing line/prediction gets
#ggplot(heights.weights[1:20,],   aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
#ggplot(heights.weights[1:200,],  aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
#ggplot(heights.weights[1:2000,], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()


###
# Classification 
###
ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point()
#coloring the gender (female, male)

####
# Teaser for Chapter 3
####

heights.weights <- transform(heights.weights, Male = ifelse(Gender == 'Male', 1, 0))
logit.model <- glm(Male ~ Height + Weight, data = heights.weights, 
	           family = binomial(link = 'logit'))
ggplot(heights.weights, aes(x = Weight, y = Height, color = Gender)) + geom_point() +
  stat_abline(intercept = - coef(logit.model)[1] / coef(logit.model)[2],
	      slope = - coef(logit.model)[3] / coef(logit.model)[2],
              geom = 'abline',
              color = 'black')
