###
# Anthony Doan
# Going through the Machine Learning for Hackers 
# by Drew Conway & John Myles White
# Chapter 3 - Classification: Spam Filtering 
###


library(tm) # text mining - need for text classification
library(ggplot2)

# loading our data's paths into variables
# 3 categories: spam, easy ham, hard ham
# spam - is unwanted email advertisements
# easy ham - email that is not spam
# hard ham - difficult emails to be classify between spam and not spam
spam.path <- "data/spam/"
spam2.path <- "data/spam_2/"
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

# function that opens each file, finds 1st line break
# returns text below the 1st line break as character vector
get.msg <- function(path) {
  con <- file(path, open="rt", encoding="latin1")
  text <- readLines(con)
  # The message always begins after the first full line break
  msg <- text[seq(which(text=="")[1]+1,length(text),1)]
  close(con)
  return(paste(msg,collapse="\n"))
}

# creating vector containing all of the msgs (each element in vector
# is a single email)
# grab all files in the directory spam.path
spam.docs <- dir(spam.path)
# keep all files in directory spam.path except for "cmds" 
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(
		spam.docs, 
		function(p) get.msg(
		  paste(
		    spam.path,
                    p,
                    sep=""
                  )
                ))

#wtf is paste?
# wtf is p?
#all.spam <- sapply(
#		spam.docs, 
#		function(p) {
#		  print(p)
#		}
# output
#[1] "00001.7848dde101aa985090474a91ec93fcf0"
#[1] "00002.d94f1b97e48ed3b553b3508d116e6a09"
#[1] "00003.2ee33bc6eacdb11f38d052c44819ba6c"
#[1] "00004.eac8de8d759b7e74154f142194282724"
# p is an iterator that iterate through the spam.docs vector of files
# (all these files reside in spam.path)
# Note function(p) is an anonymous function

# wtf is paste really?
#all.spam <- sapply(
#		spam.docs, 
#		function(p) { 
#		  print(paste(
#		    spam.path,
#                    p,
#                    sep=""
#                  ))
#                })
# Output
#[1] "data/spam/00001.7848dde101aa985090474a91ec93fcf0"
#[1] "data/spam/00002.d94f1b97e48ed3b553b3508d116e6a09"
#[1] "data/spam/00003.2ee33bc6eacdb11f38d052c44819ba6c"
#[1] "data/spam/00004.eac8de8d759b7e74154f142194282724"
# It seems to paste together the spam.path (data/spam/) and
# p (the files in data/spam/ folder)

# What does the sep="" parameter in paste function do?
#all.spam <- sapply(
#		spam.docs, 
#		function(p) { 
#		  print(paste(
#		    spam.path,
#                    p
#                  ))
#                })
# Output:
#[1] "data/spam/ 00001.7848dde101aa985090474a91ec93fcf0"
#[1] "data/spam/ 00002.d94f1b97e48ed3b553b3508d116e6a09"
#[1] "data/spam/ 00003.2ee33bc6eacdb11f38d052c44819ba6c"
# There's a space between spam.path and p. So default sep is a space character
#[1] "data/spam/ 00004.eac8de8d759b7e74154f142194282724"

# inspect our result
#head(all.spam)
#names(all.spam)
# each vector's index is name after the file (email)
# each vector's element is the email's (index) body

#####
####
# Corpus represented by Term Document Matrix (TDM)
#####
#####

# TDM function
get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE,
    minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# VectorSource function
# tdm let's us construct corpus in several ways
# since our corpus is coming from a vector of emails
# we use VectorSource 

# Corpus function
# Creates a corpus object

# control object
# control is a list of options to tell TD
# how to "distill" the text
# parameter
#   stopwords - remove 488 common English stop words from doc.corpus
#   removePunctuation - remove punctuations
#   removeNumbers - remove numbers
#   minDocFreq - terms that appear more than once will be rows of TDM

##
# Create our TDM
##
spam.tdm <- get.tdm(all.spam)

##
# Let's check out our data
##
#spam.tdm
#head(spam.tdm)
# Ouput
#A term-document matrix (6 terms, 500 documents)
#
#Non-/sparse entries: 7/2993
#Sparsity           : 100%
#Maximal term length: 76 
#Weighting          : term frequency (tf)
#names(spam.tdm)
# Ouput
#[1] "i"        "j"        "v"        "nrow"     "ncol"     "dimnames"
#dump("spam.tdm", "")
# basically a PHP's var_dump for R (the output is huge)
# i - # of times term i was found in doc j
# j - doc

#####
#####
# Classifier
#
# Note: I'm using mostly the book code and code reviewing it now
#       I'm a bit at loss with the naive bayes algorithm
#       I need to brush up my probability after finishing this book
#####
#####

# going to use TDM to build a set of training data for spam

# "train classifier to know the probability that an email is spam
# given the observation of some term"

spam.matrix <- as.matrix(spam.tdm)
# convert TDM object to standard R matrix

spam.counts <- rowSums(spam.matrix)
# rowSums is used to create vector
# that "contains total frequency counts for each term across
# all docs".
#names(spam.counts)
# output
# [1] "\024\001\025"
# [2] "áµµ"
# The index are the terms
#spam.counts["\024\001\025"]
# 1 
#spam.counts["áµµ"]
# 1 
#spam.counts["goods"]
# 2
# The elements are the frequency of these terms

spam.df <- data.frame(cbind(names(spam.counts),
  as.numeric(spam.counts)), stringsAsFactors=FALSE)
# We're combining a character vector with a numerical vector
# chracter vector
#head(names(spam.counts))
# Output
#[1] "\024\001\025"
#[2] "áµµ"
#[3] "ááà"
#[4] "å\u009eªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªª\u0082\u0080"
#[5] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
#[6] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaacuxrmplffhxl" 
# numerical vector
#head(as.numeric(spam.counts))
# Output
#[1]  1  1  1  1 30  1
# Seems like the fifth term have 30 frequency
# let's check this:
#spam.counts["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
# 30 yes!!!
#dump("spam.df","")
# First column - terms
# second column - frequency of the terms
# let's test this
#spam.df[5,]
#X1
# 5 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
#X2
#5 30

names(spam.df) <- c("term","frequency")
# so we rename X1 to term and X2 to frequency
spam.df$frequency <- as.numeric(spam.df$frequency)
# we convert the frequency column to numeric 

# "percentage of documents in which a given term occurs"
spam.occurrence <- sapply(1:nrow(spam.matrix),
  function(i) {length(which(spam.matrix[i,] > 0))/ncol(spam.matrix)})

#spam.occurrence 

# 1:nrow(spam.matrix)
# This is a sequence of numbers representing all the numbers of row 
# in the spam.matrix

# i is the iterator and iterator through each row which are postive occurance

# let see just the numerator part
#  length(which(spam.matrix[i,] > 0))
#spam.occurrence <- sapply(1:nrow(spam.matrix),
#  function(i) {
#    length(
#      which( spam.matrix[i,] > 0 )
#  )}
#)
#head(spam.occurrence)
# Output
#[1] 1 1 1 1 2 1
# Seems like the number of occuring on each term

#names(spam.matrix[1,])
# the col of each rows are document

#spam.matrix[1,]
# Oh yeah spam.matrix is just the TDM
# i,j represents i - # times term i was found in doc j

#ncol(spam.matrix)
# 500 docs 

spam.density <- spam.df$frequency/sum(spam.df$frequency)
# For each term's frequency in each row divide it by the total frequencies of all terms
# book wordings: "calculate the frequency of each word within the entire corpus"

# Add spam.occurrence and spam.density vectors to our spam dataframe
spam.df <- transform(spam.df, density=spam.density,
  occurrence=spam.occurrence)

# looking at our data
#head(spam.df[with(spam.df, order(-occurrence)),])
# Output
#        term frequency     density occurrence
#7688   email       792 0.006342850      0.566
#18691 please       422 0.003379650      0.508
#14617   list       407 0.003259520      0.444
#2962    body       376 0.003011252      0.402
#9363    free       523 0.004188524      0.382
#11424   html       364 0.002915148      0.380
#head(spam.df[with(spam.df, order(-frequency)),])
# Output
#        term frequency     density occurrence
#27123 widthd      1519 0.012165138      0.174
#23719  table      1188 0.009514275      0.310
#27109  width      1157 0.009266007      0.162
#9090    font       886 0.007095663      0.322
#7688   email       792 0.006342850      0.566
#21894  sized       713 0.005710167      0.178

# Frequency widthd is the most used term in all of the doc/msg
# Occurence email occur the most often per doc/msg

####
####
# TDM construction for easyHam
####
####
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs!="cmds")]
all.easyham <- sapply(
# limited to 500 (length(spam.docs) == 500)
		  easyham.docs[1:length(spam.docs)],
		  function(p) get.msg(
		    paste(
		      easyham.path,
                      p,
                      sep=""
                    )
                ))
ham.tdm <- get.tdm(all.easyham)
ham.matrix <- as.matrix(ham.tdm)
ham.counts <- rowSums(ham.matrix)
ham.df <- data.frame(cbind(names(ham.counts),
  as.numeric(ham.counts)), stringsAsFactors=FALSE)
names(ham.df) <- c("term","frequency")
ham.df$frequency <- as.numeric(ham.df$frequency)
ham.occurrence <- sapply(1:nrow(ham.matrix),
  function(i) {length(which(ham.matrix[i,] > 0))/ncol(ham.matrix)})
ham.density <- ham.df$frequency / sum(ham.df$frequency)
ham.df <- transform(ham.df, 
                    density=ham.density,
                    occurrence=ham.occurrence)
# looking at our data
#head(ham.df[with(ham.df, order(-occurrence)),])
# Output:
#         term frequency     density occurrence
#13411   wrote       236 0.004280247      0.376
#7206     list       246 0.004461614      0.366
#5094    group       204 0.003699875      0.364
#3690    email       188 0.003409689      0.276
#11630 subject       161 0.002919999      0.270
#12231    time       174 0.003155776      0.248


####
####
# how to handle terms in new emails 
#   1. that match terms in our training set 
#   2. do not match terms in our training set
#
#  Naive Bayes - "Calculate the probability that an email msg is spam or ham"
#  - "need to find the terms that are common between the training data and the message in question"
#
# Conditional probability, if email contain term html and table. And probability with respect to
# each term is 0.30 and 0.10 then conditional probability is: 0.30*0.10 = 0.03 chance that
# email is spam.
#
# Problem: what about unknown terms that aren't in our training set and 
# that we didn't train for?
# For this book... they set probability to 0.0001% for unknown term.
#
# Set prior belief to .5 (50%) equally likely to be ham or spam
####
####

# THE CLASSIFIER
classify.email <- function(path, training.df, prior=0.5, c=1e-6) {
  # get the email
  msg <- get.msg(path)
  # create tdm for the email
  msg.tdm <- get.tdm(msg)
  # get the frequency
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)
  if (length(msg.match) < 1) {
    return(prior*c^(length(msg.freq)))
  } 
  else {
    match.probs <- training.df$occurence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c^(length(msg.freq) - length(msg.match)))
#            ^     ^--- spam           ^---- the unknown term probability
#        50% |          probability      
#        being either
#        spam or ham
  }
}

# This function return our Baesian estimate for the probability that given email
# is spam "given the matching terms in our training data"

#### testing our classifier with hardham

hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")] 

hardham.spamtest <- sapply(hardham.docs,
  function(p) classify.email(paste(hardham.path, p, sep=""),
  training.df=spam.df))

hardham.hamtest <- sapply(hardham.docs,
  function(p) classify.email(paste(hardham.path, p, sep=""),
  training.df=ham.df))

hardham.res <- ifelse(hardham.spamtest > hardham.hamtest, TRUE, FALSE)
#summary(hardham.res)
# Output:
#   Mode   FALSE    TRUE    NA's 
#logical     239      10       0 

####
####
# Testing the Clsasifier Against All Email Types
####
####
spam.classifier <- function(path) {
  pr.spam <- classify.email(path, spam.df)
  pr.ham <- classify.email(path, ham.df)
  return(c(pr.spam,pr.ham,ifelse(pr.spam > pr.ham, 1, 0)))
}

# get all docs and put it in a vector
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

# classify each docs in a list
easyham2.class <- lapply(easyham2.docs,
			 function(i) {
			   spam.classifier(file.path(easyham2.path,i))
			})

hardham2.class <- lapply(hardham2.docs,
			 function(i) {
			   spam.classifier(file.path(hardham2.path,i))
			})

spam2.class <- lapply(spam2.docs,
			 function(i) {
			   spam.classifier(file.path(spam2.path,i))
			})

#class(easyham2.class)
# type list
#head(easyham2.class)
# output
#[[1]]
#[1] 0 0 0
#
#[[2]]
#[1] 5e-181  5e-61  0e+00
#
#[[3]]
#[1] 5e-235  5e-55  0e+00
#
#[[4]]
#[1] 0 0 0
#
#[[5]]
#[1] 5e-121  5e-43  0e+00
#
#[[6]]
#[1] 0 0 0


# Create a single, final, data frame with all of the classification data in it

# matrix because of easyham2.class 
easyham2.matrix <- do.call(rbind, easyham2.class)

#class(easyham2.matrix)
# type matrix
#head(easyham2.matrix)
# output
#       [,1]  [,2] [,3]
#[1,]  0e+00 0e+00    0
#[2,] 5e-181 5e-61    0
#[3,] 5e-235 5e-55    0
#[4,]  0e+00 0e+00    0
#[5,] 5e-121 5e-43    0
#[6,]  0e+00 0e+00    0

# append EASYHAM column
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")
#head(easyham2.final)
# output
#     [,1]                    [,2]    [,3] [,4]     
#[1,] "0"                     "0"     "0"  "EASYHAM"
#[2,] "4.99999999999999e-181" "5e-61" "0"  "EASYHAM"
#[3,] "4.99999999999999e-235" "5e-55" "0"  "EASYHAM"
#[4,] "0"                     "0"     "0"  "EASYHAM"
#[5,] "5e-121"                "5e-43" "0"  "EASYHAM"
#[6,] "0"                     "0"     "0"  "EASYHAM"


hardham2.matrix <- do.call(rbind, hardham2.class)
hardham2.final <- cbind(hardham2.matrix, "HARDHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

class.matrix <- rbind(easyham2.final, hardham2.final, spam2.final)
#class(class.matrix)
# output
#[1] "matrix"
#head(class.matrix)
# output
#     [,1]                    [,2]    [,3] [,4]     
#[1,] "0"                     "0"     "0"  "EASYHAM"
#[2,] "4.99999999999999e-181" "5e-61" "0"  "EASYHAM"
#[3,] "4.99999999999999e-235" "5e-55" "0"  "EASYHAM"
#[4,] "0"                     "0"     "0"  "EASYHAM"
#[5,] "5e-121"                "5e-43" "0"  "EASYHAM"
#[6,] "0"                     "0"     "0"  "EASYHAM"
#tail(class.matrix)
# output
#        [,1]                    [,2]                    [,3] [,4]  
#[3040,] "5e-19"                 "5e-25"                 "1"  "SPAM"
#[3041,] "0"                     "0"                     "0"  "SPAM"
#[3042,] "4.99999999999999e-187" "0"                     "1"  "SPAM"
#[3043,] "5e-109"                "5e-127"                "1"  "SPAM"
#[3044,] "0"                     "0"                     "0"  "SPAM"
#[3045,] "5e-19"                 "4.99999999999999e-259" "1"  "SPAM"

class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
#class(class.df)
# type dataframe

names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)

#head(class.df)
# Output: (64-bit system)
#  Pr.SPAM Pr.HAM Class    Type
#1   0e+00  0e+00 FALSE EASYHAM
#2  5e-181  5e-61 FALSE EASYHAM
#3  5e-235  5e-55 FALSE EASYHAM
#4   0e+00  0e+00 FALSE EASYHAM
#5  5e-121  5e-43 FALSE EASYHAM
#6   0e+00  0e+00 FALSE EASYHAM

# to be code review

# Create final plot of results
class.plot <- ggplot(class.df, aes(x = log(Pr.HAM), log(Pr.SPAM))) +
    geom_point(aes(shape = Type, alpha = 0.5)) +
    stat_abline(yintercept = 0, slope = 1) +
    scale_shape_manual(values = c("EASYHAM" = 1,
                                  "HARDHAM" = 2,
                                  "SPAM" = 3),
                       name = "Email Type") +
    scale_alpha(guide = "none") +
    xlab("log[Pr(HAM)]") +
    ylab("log[Pr(SPAM)]") +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(plot = class.plot,
       filename = file.path("images", "03_final_classification.pdf"),
       height = 10,
       width = 10)

get.results <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

# Save results as a 2x3 table
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, hardham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)

# Save the training data for use in Chapter 4
write.csv(spam.df, file.path("data", "spam_df.csv"), row.names = FALSE)
write.csv(ham.df, file.path("data", "easyham_df.csv"), row.names = FALSE)


# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 3. In this case we introduce the notion of binary classification.
#                   In machine learning this is a method for determining what of two categories a 
#                   given observation belongs to.  To show this, we will create a simple naive Bayes 
#                   classifier for SPAM email detection, and visualize the results.
# Data Used:       Email messages contained in data/ directory, source: http://spamassassin.apache.org/publiccorpus/
# Packages Used:   tm, ggplot2

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.
