#Installing required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stringr")) install.packages("stringr")
if (!require("caret")) install.packages("caret")
if (!require("tidytext")) install.packages("tidytext")
if (!require("quanteda")) install.packages("quanteda")
if (!require("e1071")) install.packages("e1071")

#packages
library(tidyverse)
library(stringr)
library(caret)
library(tidytext)
library(quanteda)
library(e1071)

#Loading in dataset (found at https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics)
lyrics <- read.csv("/Users/jacobellen/downloads/lyrics.csv", stringsAsFactors = FALSE)

#Preprocessing
#filtering dataset
lyricstrain <- lyrics %>%
  filter(genre!="Not Available" & year!="Not Available" & lyrics!="Not Available" & artist!="Not Available" & song!="Not Available" & genre != "Other" & genre != "Folk" & genre != "Indie" & genre != "Jazz" & genre != "Electronic" & genre!="R&B") %>%
  filter(is.na(lyrics)==FALSE & is.na(genre)==FALSE & is.na(year)==FALSE & is.na(artist)==FALSE & is.na(song)==FALSE) %>%
  filter(lyrics!="" & genre!="" & year!="" & artist!="" & song!="") %>%
  filter(year>1969 & year<2017) %>%
  select(genre, lyrics)

#changing Class distributions and reforming lyricstrain
pop <- lyricstrain %>%
  filter(genre=="Pop")
pop <- pop[1:15000,]
hiphop <- lyricstrain %>%
  filter(genre=="Hip-Hop")
hiphop <- hiphop[1:15000,]
rock2 <- lyricstrain %>%
  filter(genre=="Rock") 
rock2 <- rock2[1:15000,]
metal <- lyricstrain %>%
  filter(genre=="Metal")
metal <- metal[1:13000,]
country <- lyricstrain %>%
  filter(genre=="Country")
country <- country[1:13000,]
lyricstrain <- rbind(rock2, pop, hiphop, metal, country)
lyricstrain <- lyricstrain[sample(nrow(lyricstrain)),]

#writing preprocessed excel file
write.csv(lyricstrain, file = '/Users/jacobellen/desktop/lyrics_preprocessed.csv') #local pathway
  
