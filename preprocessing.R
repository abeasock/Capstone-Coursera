library(stringi)
library(stringr)
library(tm)
library(slam)
library(RWeka)
library(reshape)

setwd("/Users/Beasock/R/Capstone")

# Read in the data
blogs <- readLines("final/en_US/en_US.blogs.txt", encoding="UTF-8")
news <- readLines("final/en_US/en_US.news.txt", encoding="UTF-8")
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding="UTF-8")


## Data Sample
# Given the large size of the text files, a random sample of 25,000 lines from each of the three files is used.
set.seed(320)
sample_blogs <- blogs[sample(1:length(blogs),25000)]
sample_news <- news[sample(1:length(news),25000)]
sample_twitter <- twitter[sample(1:length(twitter),25000)]

sample_data <- c(sample_blogs, sample_news, sample_twitter)

# Remove white space
sample_data <- str_trim(gsub('[ ]+',' ',sample_data))

## Construct the Corpus
#The `tm` package is used to create and clean a corpus. 
docs <- Corpus(VectorSource(sample_data))

# Change special characters to spaces
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/|@|\\|")
docs <- tm_map(docs, content_transformer(function(x) iconv(x, to="ASCII", sub=" ")))

# Convert to lowercase
docs <- tm_map(docs, content_transformer(tolower))

# Remove punctuation
docs <- tm_map(docs, removePunctuation)

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove bad words
googlebadwords <- read.delim("google_badwords.txt",sep = ":",header = FALSE)
googlebadwords <- googlebadwords[,1]
docs <- tm_map(docs, removeWords, googlebadwords)

# Eliminate white space
docs <- tm_map(docs, stripWhitespace)

# Save the Corpus
corpus_df <- data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=FALSE)
write.csv(corpus_df, file="corpus_df.csv",row.names=FALSE)
corpus <- read.csv("corpus_df.csv")
mycorpus <- Corpus(DataframeSource(corpus))


## N-gram Tokenization
# Using the `RWeka` package, 3 term-document matrics are used to create bigrams, trigrams, and 4-grams.
# Sets the default number of threads to use
options(mc.cores=1)

#Bigrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bitdm <- TermDocumentMatrix(mycorpus, control = list(tokenize = BigramTokenizer))
bitdm <- rollup(bitdm, 2, na.rm=TRUE, FUN = sum)
bi_freq <- sort(rowSums(as.matrix(bitdm)),decreasing=TRUE)

value2_1 <- sapply(strsplit(names(bi_freq), ' '), function(a) a[1])
value2_2 <- sapply(strsplit(names(bi_freq), ' '), function(a) a[2])

gram2 <- data.frame(names(bi_freq),bi_freq,value2_1,value2_2,stringsAsFactors = F)
names(gram2) <- c('bigram','count','input','output')
melt2 <- melt(gram2, id=c('bigram','input','output'), measure=c('count'))
summed2 <- cast(melt2, input ~ variable, sum)
names(summed2)[names(summed2)=="count"] <- "sum"
gram2 <- gram2[order(gram2$count, decreasing=TRUE),]
#gram2 <- gram2[!duplicated(gram2$input),]
gram2 <- merge(gram2, summed2, by="input")
gram2 <- transform(gram2, p = count/sum)
#gram2 <- gram2[,!(names(gram2) %in% c("count","sum"))]
gram2 <- gram2[order(gram2$bigram, -gram2$p),]


#Trigrams
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tritdm <- TermDocumentMatrix(mycorpus, control = list(tokenize = TrigramTokenizer))
tritdm <- rollup(tritdm, 2, na.rm=TRUE, FUN = sum)
tri_freq <- sort(rowSums(as.matrix(tritdm)),decreasing=TRUE)

value3_1  <- sapply(strsplit(names(tri_freq), ' '),function(a) a[1])
value3_2  <- sapply(strsplit(names(tri_freq), ' '),function(a) a[2])
value3_3  <- sapply(strsplit(names(tri_freq), ' '),function(a) a[3])

gram3 <- data.frame(names(tri_freq),tri_freq,paste(value3_1,value3_2),value3_3,stringsAsFactors = F)
names(gram3) <- c('trigram','count','input','output')
melt3 <- melt(gram3, id=c('trigram','input','output'), measure=c('count'))
summed3 <- cast(melt3, input ~ variable, sum)
names(summed3)[names(summed3)=="count"] <- "sum"
gram3 <- gram3[order(gram3$count, decreasing=TRUE),]
#gram3 <- gram3[!duplicated(gram3$input),]
gram3 <- merge(gram3, summed3, by="input")
gram3 <- transform(gram3, p = count/sum)
#gram3 <- gram3[,!(names(gram3) %in% c("count","sum"))]
gram3 <- gram3[order(gram3$trigram, -gram3$p),]


# 4-grams
FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
fourtdm <- TermDocumentMatrix(mycorpus, control = list(tokenize = FourgramTokenizer))
fourtdm <- rollup(fourtdm, 2, na.rm=TRUE, FUN = sum)
four_freq <- sort(rowSums(as.matrix(fourtdm)),decreasing=TRUE)

value4_1  <- sapply(strsplit(names(four_freq), ' '),function(a) a[1])
value4_2  <- sapply(strsplit(names(four_freq), ' '),function(a) a[2])
value4_3  <- sapply(strsplit(names(four_freq), ' '),function(a) a[3])
value4_4  <- sapply(strsplit(names(four_freq), ' '),function(a) a[4])

gram4 <- data.frame(names(four_freq),four_freq,paste(value4_1,value4_2,value4_3),value4_4,stringsAsFactors = F)
names(gram4) <- c('fourgram','count','input','output')
melt4 <- melt(gram4, id=c('fourgram','input','output'), measure=c('count'))
summed4 <- cast(melt4, input ~ variable, sum)
names(summed4)[names(summed4)=="count"] <- "sum"
gram4 <- gram4[order(gram4$count, decreasing=TRUE),]
#gram4 <- gram4[!duplicated(gram4$input),]
gram4 <- merge(gram4, summed4, by="input")
gram4 <- transform(gram4, p = count/sum)
#gram4 <- gram4[,!(names(gram4) %in% c("count","sum"))]
gram4 <- gram4[order(gram4$fourgram, -gram4$p),]


#Simple Good Turing Algorithm - Gale And Simpson
#Good Turing Smoothing
SimpleGT <- function(table_N){  
  # table_U is a table of frequency of frequencies
  # The frequencies are stored as names of the list in the table structure
  # the values are the frequency of frequencies.
  # In Good Turing Smoothing, we are concerned with the frequency of frequencies
  # So, to extract the number of times that words of frequency n occur in the training set, we need to do:
  # table(freq_B)[[as.character(pairCount)]]
  # In a tables with a number of holes or non-contiguous sequence of frequency of words,
  # we can compute N(c+1) as the mean of all frequencies that occur more than Nc times
  # to do this, create a vector that is in the numerical form of the names of the table
  
  # create a data table
  # r is the frequencies of various n-grams
  #n is the frequency of frequencies
  SGT_DT <- data.frame(r=as.numeric(names(table_N)),n=as.vector(table_N),Z=vector("numeric",length(table_N)), 
                       logr=vector("numeric",length(table_N)),
                       logZ=vector("numeric",length(table_N)),
                       r_star=vector("numeric",length(table_N)),
                       p=vector("numeric",length(table_N)))
  #p=vector("numeric",length(table_N)),key="r")
  
  str(SGT_DT)
  
  num_r <- nrow(SGT_DT)
  for (j in 1:num_r) {
    if(j==1) {r_i<-0} else {r_i <- SGT_DT$r[j-1]}
    if(j==num_r){r_k<-SGT_DT$r[j]} else {r_k <- SGT_DT$r[j+1]}
    SGT_DT$Z[j] <- 2*SGT_DT$n[j] / (r_k-r_i)
    #print(paste(r_i,j,r_k))
  }
  SGT_DT$logr <- log(SGT_DT$r)
  SGT_DT$logZ <- log(SGT_DT$Z)
  linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
  print(linearFit$coefficients)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
  plot(SGT_DT$logr, SGT_DT$logZ)
  abline(linearFit,col="red")
  
  use_y = FALSE
  for (j in 1:(num_r-1)) {
    r_plus_1 <- SGT_DT$r[j] + 1
    
    s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
    s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
    y<-r_plus_1 * s_r_plus_1/s_r
    
    if(use_y) {
      SGT_DT$r_star[j] <- y
    } else { 
      n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
      n_r <- SGT_DT$n[j]
      x<-(r_plus_1) * n_r_plus_1/n_r
      
      if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
        SGT_DT$r_star[j] <- x
      }else {
        SGT_DT$r_star[j] <- y
        use_y = TRUE
      }
    }
    if(j==(num_r-1)) {
      SGT_DT$r_star[j+1] <- y
    }
    
  }
  N <- sum(SGT_DT$n * SGT_DT$r)
  Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
  Po <- SGT_DT$n[1] / N
  SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
  
  return(SGT_DT)  
}

#SGT tables for 4grams 
four_SGT_DT <- SimpleGT(table(four_freq))

#SGT tables for trigrams 
tri_SGT_DT <- SimpleGT(table(tri_freq))

#SGT tables for bigrams 
bi_SGT_DT <- SimpleGT(table(bi_freq))

save(bi_SGT_DT,tri_SGT_DT,four_SGT_DT,file = 'SGT.RData')

# use the counts in the Simple GT table to extract the probability and replace the probability in 4-gram table with it by matching counts
gram4$p <- sapply(gram4$count,FUN=function(x) four_SGT_DT$p[four_SGT_DT$r==x]) 
# use the counts in the Simple GT table to extract the probability and replace the probability in trigram table with it by matching counts
gram3$p <- sapply(gram3$count,FUN=function(x) tri_SGT_DT$p[tri_SGT_DT$r==x])
# use the counts in the Simple GT table to extract the probability and replace the probability in bigram table with it by matching counts
gram2$p <- sapply(gram2$count,FUN=function(x) bi_SGT_DT$p[bi_SGT_DT$r==x])

# Save the bigrams, trigrams, and 4-grams to one file
save(gram2,gram3,gram4,file = 'ngrams.RData')