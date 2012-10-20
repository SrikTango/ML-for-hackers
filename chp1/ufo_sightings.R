###
# Anthony Doan
# Going through the Machine Learning for Hackers 
# by Drew Conway & John Myles White
# Chapter 1
###

###
# Library
###
library(ggplot2)
library(plyr)

###
# Load data from text file ufo_awesome.tsv
###
ufo<-read.delim("data/ufo/ufo_awesome.tsv",sep="\t", stringsAsFactors=FALSE,
		 header=FALSE, na.strings="")
# sep
# The ufo_awesome.tsv is a tab-delimited file so we set sep to Tab character
#
# stringsAsFactors
# Default setting for read.delim is to convert all string to factor type. 
# Factor type make number strings into identifier so R doesn't see those number
# as number. An example is each subjects are know as number or id (those should
# be convert to factor).
#
# header
# There is no header in this file.
#
# na.strings
# Any empty elements in the data will be set to R special value NA
#
# ufo
# The dataframe that holds our data.

###
# Checking out our data
###
#names(ufo)
#head(ufo)

###
# Add some Column names (there were no header in the data)
###
headers<-c("DateOccurred","DateReported","Location","ShortDescription","Duration","LongDescription")
names(ufo)<-headers

###
# Checkout our new headers
###
#names(ufo)

###
# Checkout the first row 
###
#ufo[1,]

###
# Checkout our new headers with some data 
# (ignoring the last column because it's hard to see in terminal)
###
#head(ufo[, !names(ufo) %in% headers[6]])

###
# Convert string that represent date to type Date
###
# ufo$DateOccurred<-as.Date(ufo$DateOccurred,format="%Y%m%d")
###
# Output
# Error in strptime(x, format, tz = "GMT") : input string is too long
# Calls: as.Date -> as.Date.character -> strptime
# Execution halted
#
# Some of the date in the records are bad or mistyped.
###

###
# Let see some of the corrupted Date rows
###
#head(ufo[which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8),1])
#                                                                        ^
#                                                                        |
#                                 This is the first column the DateOcurred

###
# Constructing a vector of true/false. True for correct date format,
# False if the date format is incorrect in DateOccurred or DateReported column
###
good.rows<-ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8,FALSE,TRUE)

# good.rows 
# a vector of true/false telling us which rows are good and which are bad
# in term of date format
#
# ifelse 
# is a vectorized form of if else. For more info ?ifelse in R console
#
# nchar
# return number of chracters
#
# So basically this ifelse loop checks each row in ufo to see if the DateOccurred
# or DateReported is not equal to 8. If it's not equal to eight then set that vector
# row to False else set it to True

###
# See the number of bad rows
###
#length(which(!good.rows))
#length(which(good.rows))
#'% of bad rows in the overall data'
#percent.of.bad<-length(which(!good.rows))/length(which(good.rows))*100
#paste(percent.of.bad,'%')

###
# Let's just keep the good rows and throw away the bad ones
###
ufo<-ufo[good.rows,]

###
# Now we can format the DateOccurred and DateReported columns to date
###
ufo$DateOccurred<-as.Date(ufo$DateOccurred,"%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported,"%Y%m%d")

###
# Function we're going to use to split city and state of the location column
###
# see wtf this does
# strsplit(ufo[1,3],",")[[1]]
get.location<-function(l) {
  # strsplit 
  # take a string and split it by the first comma character ","
  # strsplit returns a list with one index [[1]] but we just want the result not a list 
  # therefore after getting the result as [[1]] we access it right away 
  # which just returns a one dimension vector.
  #
  # tryCatch will try the strsplit function if not return a vector of NA,NA
  split.location<-tryCatch(strsplit(l,",")[[1]], error= function(e) return(c(NA, NA)))
  # so the previous statement will split the ufo$Location to the format city state.
  # but there are spaces in front of it an example is " Iowa City" " WA"
  # so we need to clean it up substituting the beginning space with nothing
  # the ^ in reg expression is the beginning of string
  # "^ " means the first space at the beginning at the string and replace it with ""
  clean.location<-gsub("^ ","",split.location)
  # the if statment checks if there is only two columns in the vector
  # so if there are more than 2 comma and the strsplit splits it into more than 2 element 
  if (length(clean.location)>2) {
    return(c(NA,NA))
  } 
  else {
    return(clean.location)
  }
}

###
# Time to use the function above for ufo$Location
###
# lapply short for list apply, so we're going to apply the function get.location to
# ufo$Lcation and store the return result to the dataframe city.state
city.state<-lapply(ufo$Location, get.location) # return a list
# check out the list
#head(city.state)

###
# Add two new column city and state to the ufo dataframe
###
# Convert city.state list to a two column matrix *x2
location.matrix<-do.call(rbind, city.state)
# do.call executes a function call over a list
# see it 
# head(location.matrix) 
# to get the columns into the ufo data frame
ufo<-transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]),
stringsAsFactors=FALSE)
# Created two column USCity and USState
# USCity = location.matrix[,1] first column of the location.matrix
# USState = location.matrix[,2] second column of the location.matrix
# check the modified ufo
#names(ufo)

###
# Canada are in the ufo dataframe we want just us data need to get rid
###
# list ca 
us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il",
"in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh",
"nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt",

"wa","wi","wv","wy")
# list of usa state 
ufo$USState<-us.states[match(ufo$USState,us.states)]
# match
# To find the entries in the USState column that do not match a US state abbreviation
# 1st arg -> to match
# 2nd arg -> to match against
# if it doesn't match any element in the list us.states 
# then we'll set the value to NA
ufo$USCity[is.na(ufo$USState)]<-NA
# set all USCity to NA if the USState column value isn't in the
# usa state list using is.na function 

# So we basically set anything that isn't a valid US state to NA
# for USCity and USState column

###
# USA only dataframe of ufo
###
ufo.us<-subset(ufo,!is.na(USState))
#let see the ufo.us data
#head(ufo.us[, !names(ufo.us) %in% c(headers[6],headers[4])])


######
######
# Analyze the Data 
######
######

# Summary Statistics
summary(ufo.us)
#Date back 1400s

# Histogram all ufo in usa 
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram() + 
  scale_x_date(breaks = "50 years")
  
ggsave(plot = quick.hist,
       filename = file.path("images", "quick_hist.pdf"),
       height = 6,
       width = 16)
# UFO occurs mostly from the 1960s and up

###
# We want just 1990s and up
###
ufo.us<-subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))
#nrow(ufo.us)
# number of rows in the subset of just 1990 and up

# Histogram 1990 and up
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram() + 
  scale_x_date(breaks = "50 years")
  
ggsave(plot = quick.hist,
       filename = file.path("images", "quick_hist.pdf"),
       height = 6,
       width = 16)

# Year month combination in our histogram
ufo.us$YearMonth<-strftime(ufo.us$DateOccurred, format="%Y-%m")
# This this will actually add a new column, YearMonth to the ufo.us dataframe
# We just refer to a column that doesn't exist and R will add it to the
# dataframe

# Number of UFO sightings for each state by YearMonth
sightings.counts<-ddply(ufo.us,.(USState,YearMonth), nrow)
# nrow function to reduce the data by the number of rows in each group

# Let's look at the data
#head(sightings.counts)
# Output:
# 	USState YearMonth V1
# 1	ak	1990-01	  1	
# 2	ak	1990-03   1
# 3	ak	1990-05   1
# ...
#
# There are missing values. Say in 1990 there is only 3 ufo sighting in ak
# for month 1,3,5. 2 & 4 are... missing? So there are no ufo sighting for
# month 2 & 4. Data doesn't have entries for nonsighting.

# date.strings vector with all year-months and states 
date.range<-seq.Date(from=as.Date(min(ufo.us$DateOccurred)),
		     to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings<-strftime(date.range, "%Y-%m")
# So we create a date range from the min date in ufo.us to the max ufo.us
# base on the DateOccurred column base on month
# Then we format it to %Y-%m

# Dataframe with all year-months and states (all possible combinations)
states.dates<-lapply(us.states,function(s) cbind(s,date.strings))
states.dates<-data.frame(do.call(rbind, states.dates), stringsAsFactors=FALSE)
# Convert to a matrix and then into a dataframe
#head(states.dates)

# All sighting
all.sightings<-merge(states.dates,sightings.counts,by.x=c("s","date.strings"),
                     by.y=c("USState","YearMonth"),all=TRUE)
# So we merge the states.dates and sightings.counts dataframe to one so that we
# get all the possible months in a year per state combination.
# so we match the columns in states.dates (s, date.string) with the column 
# we want to merge in sightings.counts (USState and YearMonth)
# The all=TRUE include entries do not match and fill them with NA value

# Check out the data
#head(all.sightings)

# Making our data more sensible
names(all.sightings)<-c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
# Convert all NA to 0
all.sightings$YearMonth<-as.Date(rep(date.range,length(us.states)))
# Data is currently as string type we convert it to date datatype
#length(us.states)
# output: 50
#date.range
# output: all the years and months each with day 1
#rep(date.range,length(us.states))
# output: all the years and months each with day 1
#length(date.range)
#Output: 248
#length(rep(date.range,length(us.states)))
#[1] 12400
# how?! 50*248 = 12400 oh...
all.sightings$State<-as.factor(toupper(all.sightings$State))
# Convert to datatype factor

# See the Data
head(all.sightings)
