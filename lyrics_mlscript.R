#loading required packages
library(tidyverse)
library(stringr)
library(caret)
library(tidytext)
library(quanteda)
library(e1071)
if (!require("SparseM")) install.packages("SparseM")
library(SparseM)
if (!require("keras")) install.packages("keras")
library(keras)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("purrr")) install.packages("purrr")
library(purrr)


#loading in preprocessed data and concatenating to one data frame
lyrics1 <- read.csv("allgenresupdatefinal1.csv")
lyrics2 <- read.csv("allgenresupdatefinal2.csv")
lyricstrain <- rbind(lyrics1,lyrics2)

#forming a corpus
lyricstrain$lyrics <- as.character(lyricstrain$lyrics)
traincorpus <- corpus(lyricstrain, text_field = "lyrics")

#creating a document-feature matrix
dfm.matrixtrain <- dfm(traincorpus,
                ngrams = 1,
                remove = stopwords("english"),
                stem = TRUE,
                remove_punct = TRUE)
dfm.matrixtrain <- dfm_trim(dfm.matrixtrain, min_docfreq = 10)
# dfm.matrixtrain <- convert(traindfm, to = "matrix")
topfeatures(dfm.matrixtrain, 20)
dfm.matrixtrainsample <- dfm.matrixtrain[1:5,]
save(dfm.matrixtrainsample, file='dfm.matrix.rda')

#putting examples in a random order
#putting examples in a random order
set.seed(nrow(dfm.matrixtrain))
rand <- sample(nrow(dfm.matrixtrain))
dfm.matrixtrain <- dfm.matrixtrain[rand,]
lyricstrain <- lyricstrain[rand,]

lyricstrain <- lyricstrain %>%
  select(genre, lyrics)
  
#creating indices for splitting between training and testing
end <- floor(dim(dfm.matrixtrain)[1])
cutoff <- floor(dim(dfm.matrixtrain)[1]*.8)

#Training with 80% data split
ml1 <- svm(dfm.matrixtrain[1:cutoff,],
                    lyricstrain$genre[1:cutoff],
           kernel = 'linear', probability=TRUE)
summary(ml1)
save(ml1, file='ml2.rda')

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

#Outputting Confusion Matrix, Accuracy, Precision and Recall Scores
confusionmatrix <- table(preds, lyricstrain$genre[cutoff:end])
paste0("Accuracy is ", accuracy(preds, lyricstrain$genre[cutoff:end]))
paste0("Precision is ", precision(preds, lyricstrain$genre[cutoff:end]))
paste0("Recall is ", recall(preds, lyricstrain$genre[cutoff:end]))

