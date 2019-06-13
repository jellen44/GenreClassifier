#loading required packages
library(tidyverse)
library(stringr)
library(caret)
library(tidytext)
library(quanteda)
library(e1071)

#loading in preprocessed data and concatenating to one data frame
lyrics1 <- read.csv("lyrics_preprocessed1.csv")
lyrics2 <- read.csv("lyrics_preprocessed2.csv")
lyricstrain <- rbind(lyrics1,lyrics2)

#forming a corpus
lyricstrain$lyrics <- as.character(lyricstrain$lyrics)
traincorpus <- corpus(lyricstrain, text_field = "lyrics")

#creating a document-feature matrix
traindfm <- dfm(traincorpus,
                ngrams = 1,
                remove = stopwords("english"),
                stem = TRUE,
                remove_punct = TRUE)
dfm.matrixtrain <- convert(traindfm, to = "matrix")

#putting examples in a random order
set.seed(nrow(dfm.matrixtrain))
rand <- sample(nrow(dfm.matrixtrain))
dfm.matrixtrain <- dfm.matrixtrain[rand,]
lyrics$genre <- lyrics$genre[rand]

#creating indices for splitting between training and testing
end <- dim(dfm.matrixtrain)[1]
cutoff <- dim(dfm.matrixtrain)[1]*.8

#Training with 80% data split
ml1 <- caret::train(dfm.matrixtrain[1:cutoff,],
                    lyricstrain$genre[1:cutoff],
                    method = "rpart")

#making predictions with 20% data split
predictions <- predict.train(ml1, newdata=dfm.matrixtrain[cutoff:end,], type="prob")

#making and outputting a data frame of predictions
predictions <- as.data.frame(predictions)
predictions <- cbind(predictions, lyricstrain$genre[cutoff:end])
predictions
