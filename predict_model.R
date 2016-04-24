#library(tm)
library(stringr)
#library(dplyr)

#setwd("/Users/Beasock/R/Capstone/project_app")

#load('ngrams.RData')

predict <- function(input) {
  # grab the last 3 words & clean input 
  input <- str_trim(input)
  input <- str_extract(input,'[A-Za-z]*[ ]*[A-Za-z]*[ ]*[A-Za-z]+$')
  input <- tolower(input)
  input_split <- strsplit(input,' ')

  # recompose last 3 words for 4-gram
  input3 <- input
  # get the subset of the 4gram data table with a matching trigram start
  sgram4 <- as.data.frame(NULL)
  if (is.na(input_split[[1]][3])==FALSE){
    sgram4 <- gram4[gram4$input == input3,]      
  }
  
  # recompose last 2 words for tri-gram
  input2 <- str_extract(input,'[A-Za-z]+[ ]+[A-Za-z]+$')
  # get the subset of the trigram data table with a matching bigram start
  sgram3<- gram3[gram3$input == input2,]
  
  # recompose last 1 word for bi-gram
  input1   <- str_extract(input,'[A-Za-z]+$')
  # get the subset of the bigram data table with a matching unigram start    
  sgram2<- gram2[gram2$input == input1,]
  
  topsgram4 <- NULL
  topsgram3 <- NULL
  topsgram2 <- NULL
  topmatches <- NULL
  predictions <- NULL
  finalpredict <- NULL

  #check if trigram was found in the 4gram table
  if(nrow(sgram4) > 0) {
    # order by highest probability
    sgram4 <- sgram4[with(sgram4,order(-p)),]
    colnames(sgram4)[2] <- "gram"
    topsgram4 <- sgram4
    topsgram4 <- topsgram4[is.na(topsgram4$input)==FALSE,]
  } 
  #check if bigram was found in the trigram table
  if(nrow(sgram3) > 0) {
    # order by highest probability
    sgram3 <- sgram3[with(sgram3,order(-p)),]
    colnames(sgram3)[2] <- "gram"
    topsgram3 <- sgram3   
    topsgram3 <- topsgram3[is.na(topsgram3$input)==FALSE,]      
  }  
  #check if unigram was found in the bigram table
  if(nrow(sgram2) > 0) {
    # order by highest probability
    sgram2 <- sgram2[with(sgram2,order(-p)),]
    colnames(sgram2)[2] <- "gram"
    topsgram2 <- sgram2
    topsgram2 <- topsgram2[is.na(topsgram2$input)==FALSE,]    
  } 

  if((length(topsgram4)>0) | (length(topsgram3)>0)) {
    #Stupid Back-off to compare fourgrams & trigrams and order by highest probability
    topsgram3$p <- topsgram3$p * 0.4
    topmatches <- rbind(topsgram4,topsgram3)
    topmatches <- topmatches[with(topmatches,order(-p)),]
    #Remove duplicate matches in case the same output word is high on the probability list due to being in 4grams & trigrams
    topmatches <- topmatches[!duplicated(topmatches[,4]),]
    
    #get the top 3 words
    predictList <- topmatches[1:3,'output']
    predictions <- predictList[is.na(predictList)==FALSE]  
  }
  
  #If 3 matching 4grams or trigrams then print the prediction words
  if(length(predictions)==3){
    return(predictions)
  }
  
  # If less than 3 matching 4grams or trigrams or no matches then use bigrams with highest probability to get the remaining prediction words
  if((length(predictions)<3) & (length(topsgram2)>0)) {
    predictList2 <- topsgram2[1:10, 'output'] #Get the top 10 in case there are any of the same words already in the 4 or 3-gram strings
    predictions2 <- predictList2[is.na(predictList2)==FALSE]   
    
    newpredictions <- c(predictions, predictions2) #Combine matching 4, 3, & 2-grams in a string
    newpredictions <- paste(unique(newpredictions)) #Remove any duplicated words
    finalpredict <- newpredictions[1:3] #Take the top 3 words
    return(finalpredict)
  }
  if((length(predictions)==0) & (length(finalpredict)==0)){
    print(paste("No prediction found"))
  }  
}  
  


