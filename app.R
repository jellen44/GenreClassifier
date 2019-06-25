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
load('ml2.rda')
load('dfm.matrix.rda')


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



# rapcorpusa <- corpus_subset(traincorpus, genre=="Hip-Hop")
# rapcorpus <- dfm(rapcorpusa,
#                  ngrams = 1,
#                  remove = stopwords("english"),
#                  stem = TRUE,
#                  remove_punct = TRUE)
# dfm.matrixrap <- convert(rapcorpus, to = "matrix")
# vrap <- sort(colSums(dfm.matrixrap), decreasing=TRUE)
# drap <- data.frame(word = names(vrap),freq=vrap)
# 
# 
# popcorpusa <- corpus_subset(traincorpus, genre=="Pop")
# popcorpus <- dfm(popcorpusa,
#                  ngrams = 1,
#                  remove = c(stopwords("english"), "i", "ï", "î", "í", "ī", "į", "ì"),
#                  stem = TRUE,
#                  remove_punct = TRUE)
# dfm.matrixpop <- convert(popcorpus, to = "matrix")
# vpop <- sort(colSums(dfm.matrixpop), decreasing=TRUE)
# dpop <- data.frame(word = names(vpop),freq=vpop)
# 
# rockcorpusa <- corpus_subset(traincorpus, genre=="Rock")
# rockcorpus <- dfm(rockcorpusa,
#                   ngrams = 1,
#                   remove = stopwords("english"),
#                   stem = TRUE,
#                   remove_punct = TRUE)
# dfm.matrixrock <- convert(rockcorpus, to = "matrix")
# vrock <- sort(colSums(dfm.matrixrock), decreasing=TRUE)
# drock <- data.frame(word = names(vrock),freq=vrock)
# 
# 
# jazzcorpusa <- corpus_subset(traincorpus, genre=="R&B")
# jazzcorpus <- dfm(jazzcorpusa,
#                   ngrams = 1,
#                   remove = stopwords("english"),
#                   stem = TRUE,
#                   remove_punct = TRUE)
# dfm.matrixjazz <- convert(jazzcorpus, to = "matrix")
# vjazz <- sort(colSums(dfm.matrixjazz), decreasing=TRUE)
# djazz <- data.frame(word = names(vjazz),freq=vjazz)
# 
# rbcorpusa <- corpus_subset(traincorpus, genre=="R&B")
# rbcorpus <- dfm(rbcorpusa,
#                 ngrams = 1,
#                 remove = stopwords("english"),
#                 stem = TRUE,
#                 remove_punct = TRUE)
# dfm.matrixrb <- convert(rbcorpus, to = "matrix")
# vrb <- sort(colSums(dfm.matrixrb), decreasing=TRUE)
# drb <- data.frame(word = names(vrb),freq=vrb)
# 
# 
# 



ui <- dashboardPage(
dashboardHeader(title="Genre Classification App"),
dashboardSidebar(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  sidebarMenu(id = "tabs",
    menuItem("Genre Prediction", tabName = "GenrePrediction"),
    #menuItem("Word Clouds", tabName = "wc"),
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
#    tabItem(tabName="wc", fluidRow(sliderInput(inputId = "maxword", label="Slide to Adjust the Number of Words in Each Wordcloud",
#                                     max=125, min=1, value=75), align="center"),
#              box(plotOutput(outputId = "rockwordcloud")),  box(plotOutput(outputId = "popwordcloud")), box(plotOutput(outputId = "hipwordcloud")),
#            box(plotOutput(outputId = "rbwordcloud")), box(plotOutput(outputId = "jazzwordcloud"))
                 
#             ),
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
    words <- as.data.frame(words)
    colnames(words) <- c("lyrics")
    words$lyrics <- as.character(words$lyrics)
    lyricscorpus2 <- corpus(words, text_field = "lyrics")
    lyricscorpus2 <- dfm(lyricscorpus2, 
                         ngrams = 1,
                         remove = stopwords("english"),
                         remove_punct = TRUE)
    finalcorpus <- rbind(dfm.matrixtrainsample, lyricscorpus2)
    rownum <- dim(finalcorpus)[1]
    colnum <- dim(dfm.matrixtrainsample)[2]
    prediction <- predict(ml1, newdata = finalcorpus[rownum,1:colnum])
    prediction <- as.character(prediction)
    paste0("Prediction: ", CapStr(input$song), " by ", CapStr(input$artist), " is a ", prediction, " song")
    
  })
  output$songdata <- renderPlot({
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
    a <- str_split(artist, "-")
    a <- unlist(a)
    words  <- str_remove_all(words, CapStr(a[1]))
    words  <- str_remove_all(words, CapStr(a[2]))
    words <- as.data.frame(words)
    colnames(words) <- c("lyrics")
    words$lyrics <- as.character(words$lyrics)
    lyricscorpus2 <- corpus(words, text_field = "lyrics")
    lyricscorpus2 <- dfm(lyricscorpus2, 
                         ngrams = 1,
                         remove = stopwords("english"),
                         remove_punct = TRUE)
    topf <- topfeatures(lyricscorpus2, input$slider)
    topf <- as.data.frame(topf)
    topf2 <- as.data.frame(cbind(rownames(topf), topf$topf))
    colnames(topf2) <- c("PopularWords","Frequency")
    topf2$Frequency <- as.numeric(topf2$Frequency)
    topf2
    topf2 %>%
      ggplot(aes(x=reorder(PopularWords, -Frequency), y=Frequency)) + geom_col(fill="blue", color = "black") + labs(x=NULL, y="Frequency", title = paste0("Most Common Words in ", CapStr(input$song), " (", CapStr(input$artist), ")")) + 
      theme_classic() + theme(axis.text.x = element_text(angle = 30, vjust = .5, size=15), axis.text.y = element_text(size=16), plot.title = element_text(size=18), axis.title=element_text(size=16))  
  })
  output$introtext <- renderText({
    paste("This machine learning model uses the lyrics of a song to predict one of five genres: hip-hop, pop, rock, jazz and R&B.")
  })
  output$albumpic <- renderUI({
  song <- tolower(str_replace_all(input$song," ", "-"))
  artist <- tolower(str_replace_all(input$artist," ", "-"))
  pic.url <- paste0("https://www.genius.com/", CapStr(artist), "-",
                                song, sep = "-","lyrics")
  #replace lyrics with spaces
  image <- read_html(pic.url) %>%
      html_nodes("img") %>%
      .[[1]] %>%
      html_attr(name = "src")
  tags$img(src = image)
  
  })
  output$popwordcloud <- renderPlot({
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 13))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, "Pop Plot")
    wordcloud(words = dpop$word, freq = dpop$freq, min.freq = 1,
              max.words=input$maxword, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"), main="Pop Plot")
  })
  output$hipwordcloud <- renderPlot({
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 13))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, "Hip-Hop Plot")
    wordcloud(words = drap$word, freq = drap$freq, min.freq = 1,
              max.words=input$maxword, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"), main="Hip-Hop Plot")
  })
  output$rockwordcloud <- renderPlot({
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 13))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, "Rock Plot")
    wordcloud(words = drock$word, freq = drock$freq, min.freq = 1,
              max.words=input$maxword, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"), main="Rock Plot")
  })
  output$rbwordcloud <- renderPlot({
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 13))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, "R&B Plot")
    wordcloud(words = drb$word, freq = drb$freq, min.freq = 1,
              max.words=input$maxword, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"), main="R&B Plot")
  })
  output$jazzwordcloud <-renderPlot({
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 13))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, "Jazz Plot")
    wordcloud(words = djazz$word, freq = djazz$freq, min.freq = 1,
              max.words=input$maxword, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"), main="Jazz Plot")
  })
  output$text1 <- renderText({
    paste("Project and Model Information")
  })
  output$text2 <- renderText({
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
