if(!require("tidyverse")) install.packages("tidyverse")
if(!require("caret")) install.packages("caret")
if(!require("quanteda")) install.packages("quanteda")
if(!require("tidytext")) install.packages("tidytext")
if(!require("shiny")) install.packages("shiny")
if(!require("rvest")) install.packages("rvest")
if(!require("shinyWidgets")) install.packages("shinyWidgets")
if(!require("shinydashboard")) install.packages("shinydashboard")
if(!require("e1071")) install.packages("e1071")
if(!require("wordcloud")) install.packages("wordcloud")
library(caret)
library(tidyverse)
library(ggplot2)
library(stringr)
library(caret)
library(tidytext)
library(quanteda)
library(rsconnect)
library(shiny)
library(rvest)
library(shinyWidgets)
library(shinydashboard)
library(e1071)
library(wordcloud)
library(SparseM)

#Load in the Trained Model 
load('SVM_Model.rda')
load('Sample_dfmatrix.rda')

#Recreating confusion matrix (takes less memory than loading the file straight in)
hiphop <- c(776, 32, 67, 35, 45)
jazz <- c(37, 532, 202, 127, 142)
pop <- c(62, 123, 444, 106, 151)
rb <- c(42, 79, 94, 226, 96)
rock <- c(92, 204, 241, 173, 554)
conf <- rbind(hiphop, jazz, pop, rb, rock)
colnames(conf) <- c("Hip-Hop", "Jazz", "Pop", "R&B", "Rock")
rownames(conf) <- c("Hip-Hop", "Jazz", "Pop", "R&B", "Rock")
conf <- as.data.frame(conf)

#Function to capitalize the first letter of a word
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


ui <- dashboardPage(
dashboardHeader(title="Genre Classification App"),
dashboardSidebar(
  #hides red error text
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  sidebarMenu(id = "tabs",
    menuItem("Genre Prediction", tabName = "GenrePrediction"),
    menuItem("Model Performance", tabName="mp"))),
dashboardBody(
  tabItems(
    tabItem(tabName="GenrePrediction", 
            
                fluidRow(box(width=6, textInput(inputId = "song", label = "Choose a song name", value= "Good Vibrations"), textInput(inputId = "artist", label = "Select the artist", value = "The Beach Boys"), textOutput(outputId = "introtext")
                ), box(width=6, h4(textOutput(outputId = "genre")), 
                                                                                                                  tagList(htmlOutput('albumpic'))
                                                                                                      )),
            sidebarLayout(
              sidebarPanel(
                sliderInput(inputId = "slider", label="Slide to Adjust the Number of Most Popular Words",
                                       max=30, min=1, value=15)),
              mainPanel(plotOutput(outputId = "songdata"))
              )),
    tabItem(tabName="mp",
             h2(textOutput(outputId = "text1"), align="center"), textOutput(outputId="text2"), fluidRow(tableOutput(outputId = "table1"), align="center")
             )
             
             )
)
)

server <- function(input, output, session) {
  # Predict instance genre
  output$genre <- renderText({
    song <- tolower(str_replace_all(input$song," ", "-"))
    artist <- tolower(str_replace_all(input$artist," ", "-"))
    lyrics.url <- paste0("https://www.genius.com/", CapStr(artist), "-",
                         song, sep = "-","lyrics")
    words <- lyrics.url %>%
      read_html() %>%
      html_nodes('p') %>%
      .[[1]] %>%
      html_text()
    
    #Replacing Extra Words not actually in the songs
    words  <- str_remove_all(words[1], c("\r"))
    words <- str_replace_all(words, c("\n","\r"), " ")[1]
    words  <- str_remove_all(words, "Verse")
    words  <- str_remove_all(words, "Chorus")
    words  <- str_remove_all(words, "Intro")
    words  <- str_remove_all(words, "Outro")
    words  <- str_remove_all(words, "Instrumental")
    words  <- str_remove_all(words, "Bridge")
    words  <- str_remove_all(words, "1")
    words  <- str_remove_all(words, "2")
    words  <- str_remove_all(words, "3")
    words  <- str_remove_all(words, "produced")
    
    #forming a document feature matrix
    words <- as.data.frame(words)
    colnames(words) <- c("lyrics")
    words$lyrics <- as.character(words$lyrics)
    lyricscorpus2 <- corpus(words, text_field = "lyrics")
    lyricscorpus2 <- dfm(lyricscorpus2, 
                         ngrams = 1,
                         remove = stopwords("english"),
                         remove_punct = TRUE)
    
    #binding DFM to training DFM, so testing format is the same as training
    finalcorpus <- rbind(dfm.matrixtrainsample, lyricscorpus2)
    rownum <- dim(finalcorpus)[1]
    colnum <- dim(dfm.matrixtrainsample)[2]
    
    #Prediction
    prediction <- predict(ml1, newdata = finalcorpus[rownum,1:colnum])
    prediction <- as.character(prediction)
    paste0("Prediction: ", CapStr(input$song), " by ", CapStr(input$artist), " is a ", prediction, " song")
    
  })
  output$songdata <- renderPlot({
    #Same Code as above to form a list of the most common words
    song <- tolower(str_replace_all(input$song," ", "-"))
    artist <- tolower(str_replace_all(input$artist," ", "-"))
    lyrics.url <- paste0("https://www.genius.com/", CapStr(artist), "-",
                         song, sep = "-","lyrics")
    words <- lyrics.url %>%
      read_html() %>%
      html_nodes('p') %>%
      .[[1]] %>%
      html_text()
    words  <- str_remove_all(words[1], c("\r"))
    words <- str_replace_all(words, c("\n","\r"), " ")[1]
    words  <- str_remove_all(words, "Verse")
    words  <- str_remove_all(words, "Chorus")
    words  <- str_remove_all(words, "Intro")
    words  <- str_remove_all(words, "Outro")
    words  <- str_remove_all(words, "Instrumental")
    words  <- str_remove_all(words, "Bridge")
    words  <- str_remove_all(words, "1")
    words  <- str_remove_all(words, "2")
    words  <- str_remove_all(words, "3")
    words  <- str_remove_all(words, "produced")
    #making sure artist's name isn't included in the lyrics
    a <- str_split(artist, "-")
    a <- unlist(a)
    words  <- str_remove_all(words, CapStr(a[1]))
    words  <- str_remove_all(words, CapStr(a[2]))
    
    #Forming a DFM
    words <- as.data.frame(words)
    colnames(words) <- c("lyrics")
    words$lyrics <- as.character(words$lyrics)
    lyricscorpus2 <- corpus(words, text_field = "lyrics")
    lyricscorpus2 <- dfm(lyricscorpus2, 
                         ngrams = 1,
                         remove = stopwords("english"),
                         remove_punct = TRUE)
    
    #Finding most common words
    topf <- topfeatures(lyricscorpus2, input$slider)
    topf <- as.data.frame(topf)
    topf2 <- as.data.frame(cbind(rownames(topf), topf$topf))
    colnames(topf2) <- c("PopularWords","Frequency")
    topf2$Frequency <- as.numeric(topf2$Frequency)

    #plotting most common words
    topf2 %>%
      ggplot(aes(x=reorder(PopularWords, -Frequency), y=Frequency)) + geom_col(fill="blue", color = "black") + labs(x=NULL, y="Frequency", title = paste0("Most Common Words in ", CapStr(input$song), " (", CapStr(input$artist), ")")) + 
      theme_classic() + theme(axis.text.x = element_text(angle = 30, vjust = .5, size=15), axis.text.y = element_text(size=16), plot.title = element_text(size=18), axis.title=element_text(size=16))  
  })
  output$introtext <- renderText({
    paste("This machine learning model uses the lyrics of a song to predict one of five genres: hip-hop, pop, rock, jazz and R&B.")
  })
  output$albumpic <- renderUI({
   #Scraping album cover of whatever song is chosen
  song <- tolower(str_replace_all(input$song," ", "-"))
  artist <- tolower(str_replace_all(input$artist," ", "-"))
  pic.url <- paste0("https://www.genius.com/", CapStr(artist), "-",
                                song, sep = "-","lyrics")
  image <- read_html(pic.url) %>%
      html_nodes("img") %>%
      .[[1]] %>%
      html_attr(name = "src")
  tags$img(src = image)
  
  })
  output$text1 <- renderText({
    #Header
    paste("Project and Model Information")
  })
  output$text2 <- renderText({
    #Explanation Text
  paste("This project attempts to answer one specific question: is there a connection between the lyrics of a song and its genre? 
        In other words, are there words that are more indicative of a specific genre than others? 
        I used a dataset from Kaggle (https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics) containing over 380,000 songs from MetroLyrics, with each song’s title, genre, year and lyrical content. 
        This dataset was made publicly available in 2016, and contains songs from date 1967 until 2016. 
        For the purposes of this project, I used a subsetted version of the dataset of 25,000 songs, with 5,000 from each of five genres (Hip-Hop, Pop, Rock, Jazz and R&B). The lyrics of each song were transformed into one large document-feature matrix using the 'quanteda' package. This matrix was subsequently used as the input for the machine learning model. 
I used a support vector machine model, which is a commonly used supervised machine learning classification algorithm.
        The predictions of the SVM model on my testing dataset are below, with the model's predictions in the rows, and the true genre of the song in the rows. ")
  })
  output$table1 <- renderTable({conf}, rownames = TRUE, bordered = TRUE, digits = 0,
  hover = TRUE)
    
}

# deployApp()
shinyApp(ui,server)
