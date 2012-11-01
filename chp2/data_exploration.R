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
    index <- ceiling(length[x]) / 2)
    return(sorted.x[index])
  }
}
