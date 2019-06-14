#loading required packages
library(tidyverse)
library(stringr)
library(caret)
library(tidytext)
library(quanteda)
library(e1071)
if (!require("SparseM")) install.packages("SparseM")
library(SparseM)

#loading in preprocessed data and concatenating to one data frame
lyricstrain <- read.csv("lyrics_preprocessed1.csv")
lyricstrain <- lyricstrain[1:12000,]
#lyrics2 <- read.csv("lyrics_preprocessed2.csv")
#lyricstrain <- rbind(lyrics1,lyrics2)

#forming a corpus
lyricstrain$lyrics <- as.character(lyricstrain$lyrics)
traincorpus <- corpus(lyricstrain, text_field = "lyrics")

#creating a document-feature matrix
dfm.matrixtrain <- dfm(traincorpus,
                ngrams = 1,
                remove = stopwords("english"),
                stem = TRUE,
                remove_punct = TRUE)
# dfm.matrixtrain <- convert(traindfm, to = "matrix")

#putting examples in a random order
#putting examples in a random order
set.seed(nrow(dfm.matrixtrain))
rand <- sample(nrow(dfm.matrixtrain))
dfm.matrixtrain <- dfm.matrixtrain[rand,]
lyricstrain$genre <- lyricstrain$genre[rand]

#creating indices for splitting between training and testing
end <- floor(dim(dfm.matrixtrain)[1])
cutoff <- floor(dim(dfm.matrixtrain)[1]*.8)

#Training with 80% data split
ml1 <- svm(dfm.matrixtrain[1:cutoff,],
                    lyricstrain$genre[1:cutoff],
           kernel = 'nonlinear', cost=1, probability=TRUE)

#making predictions with 20% data split
preds <- predict(ml1, dfm.matrixtrain[cutoff:end,])

#making and outputting a data frame of predictions
accuracy <- function(ypred, y){
  tab <- table(ypred, y)
  return(sum(diag(tab))/sum(tab))
}
# function to compute precision
precision <- function(ypred, y){
  tab <- table(ypred, y)
  return((tab[2,2])/(tab[2,1]+tab[2,2]))
}
# function to compute recall
recall <- function(ypred, y){
  tab <- table(ypred, y)
  return(tab[2,2]/(tab[1,2]+tab[2,2]))
}
table(preds, lyricstrain$genre[cutoff:end])


accuracy(preds, lyricstrain$genre[cutoff:end])
precision(preds, lyricstrain$genre[cutoff:end])
recall(preds, lyricstrain$genre[cutoff:end])

#predictions <- as.data.frame(predictions)
#predictions <- cbind(predictions, lyricstrain$genre[cutoff:end])
#predictions
