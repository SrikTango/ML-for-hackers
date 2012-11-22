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
all.spam <- sapply(
		spam.docs, 
		function(p) { 
		  print(paste(
		    spam.path,
                    p
                  ))
                })
# Output:
#[1] "data/spam/ 00001.7848dde101aa985090474a91ec93fcf0"
#[1] "data/spam/ 00002.d94f1b97e48ed3b553b3508d116e6a09"
#[1] "data/spam/ 00003.2ee33bc6eacdb11f38d052c44819ba6c"
# There's a space between spam.path and p. So default sep is a space character
#[1] "data/spam/ 00004.eac8de8d759b7e74154f142194282724"
