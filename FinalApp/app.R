library(shiny)
library(shinydashboard)
#install.packages(rtweet)
library(rtweet)
library(tidyverse)
library(tidytext)
nrc <- get_sentiments("nrc")
### This package was used to create a csv file which was modified
#install.packages(tm)
library(tm)
#install.packages(foreach)
library(foreach)

stop_words <- read.csv("./stopwords.csv")
twitter_token <- readRDS("./twitter_token.rds")
cat(paste0("TWITTER_PAT=", "./twitter_token.rds"),
    file = file.path("./", ".Renviron"),
    append = TRUE)

search_terms = 'politics OR democrats OR republicans OR socialist OR healthcare OR Gun+Control OR Syria OR Mueller
OR blacklivesmatter OR NSA OR drone+strikes OR Obama OR Sanders OR Clinton OR Trump OR progressive 
OR conservative OR liberal OR immigration OR right+wing OR left+wing OR war OR United+States
OR US OR education OR congress OR senate OR president OR VP OR POTUS OR SCOTUS OR law
OR bill OR hor OR vote
'
get_tweets <- function(search_terms, name="tweets2.rds"){
  #Download tweets with search terms into a df
  tw <-  distinct(rtweet::search_tweets(search_terms, n = 1e2, type="popular", retryonratelimit = 1e3, token=twitter_token))
  #Collect column for text and column for the amount of shares and favorites
  tw <-  tw %>% mutate(popularity = retweet_count + favorite_count) %>% select(text, popularity)
  #Remove duplicate text
  textpop <- tw[!duplicated(tw[,c('text')]),]
  #Now we have the popularity of each tweet
  print("Saving file")
  saveRDS(textpop, file=name)
  print("File saved")
  return(textpop)
}
textpop <- readRDS("tweets.rds")
get_tweets(search_terms)
textpop2 <- readRDS("tweets2.rds")

text <- textpop %>% select(text) 
text <- text %>% # Word COUNT
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  inner_join(nrc) %>%
  count(word, sort = TRUE)
text2 <- textpop2 %>% select(text)
text2 <- text2 %>% # Word COUNT
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  inner_join(nrc) %>%
  count(word, sort = TRUE)



get_sentimentss <- function(df, sentiments, na.rm=F){
  #given a list of sentiments, cross references words in the data frame with list, may remove NA rows
  #NA rows are ones where the df contains a word which is not in the list of words/sentiments nrc
  nrc <- filter(nrc, sentiment %in% sentiments)
  ds <- df %>% left_join(nrc, by="word")
  if(na.rm){
    ds <- ds[complete.cases(ds), ]
  }
  return(ds)
}
get_pct_occur <- function(df, sentiments){
  #get df of pct occurence for each sentiment in a column dataframe
  occ <- as.data.frame(sentiments)
  n = nrow(df)
  l <- length(sentiments)
  occ$occurrences <- foreach(s= sentiments) %do% (nrow(df %>% select("sentiment") %>% filter(sentiment == s)) / n*100)
  return(occ)
}




#build shiny app
header <- dashboardHeader(
  title="Political Tweets May 7th 2018 to Now",
  titleWidth = 350             
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("twitter")),
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Analysis", tabName = "anal", icon = icon("bar-chart-o"), 
             menuSubItem("Tweet Length and Popularity", tabName = "len", icon = NULL), 
             menuSubItem("Popular Tweet Sentiments", tabName = "sent", icon = NULL), 
             menuSubItem("Most Popular Sentiments", tabName = "pop", icon = NULL), 
             menuSubItem("Word Count", tabName = "count", icon = NULL)
    ),
    menuItem("Conclusions", tabName = "conc", icon = icon("server"))
  )
)

get_sum_pop <- function(df, sentiments){
  #get df of pct occurence for each sentiment in a column dataframe
  occ <- as.data.frame(sentiments)
  n = nrow(df)
  l <- length(sentiments)
  occ$popularity <- foreach(s=sentiments) %do% sum(df %>% filter(sentiment == s) %>% select(popularity))
  return(occ)
}

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intro",
            fluidRow(
              box(width=700,
                p("We have analysed data from twitter to determine the properties of popular political tweets."),
                p("Use the tabs to navigate to the charts and analysis of the data."),
                p("The data from today consists of fewer tweets to improve user experience")
              )
            ),
            fluidRow(
              box(width=700,
                img(src="trump.jpg", align="right", width="33%", height=500),
                img(src="court.png", align="left", width="33%", height=500)
              )
            )
    ),
    tabItem(tabName = "about",
            fluidRow(
              box(width=100,
                  p("This app was created by Matthew D. Ciaramitaro as a part of the MA615 Tidy Final Project. The application requires shiny, tidyverse, foreach, rtweet, shinydashboard, and tidytext. See the rest of the assignment at"),
                  a("https://github.com/Tea-Script/MA615Final")
              )
            )        
    ),
    tabItem(tabName = "len",
            fluidRow(
              box(
                title = "Tweet Length by Popularity"
              )
            ),
            fluidRow(
              box(plotOutput("plot1")),
              box(plotOutput("plot2"))          
            ),
            fluidRow(
              box(
                p("The above shows the change in length to popularity of political tweets from May 7th 2018 to today")
              )
            )
    ),
    tabItem(tabName="count",
            fluidRow(
              box(
                title = "Word Frequency in Popular Tweets"
              )
            ),
            fluidRow(
              box(
                p("The below graphs demonstrate the changes in the most popular words of the tweets now vs May 7th 2018")
              )
            ),
            fluidRow(
              box(width="80%",
                plotOutput("count1")
              )
            ),
            fluidRow(
              box(width="80%",
                plotOutput("count2")
              )
            )        
    ),
    tabItem(tabName="sent",
            fluidRow(
              box(
                title = "Percent Sentiment Occurrence in Popular Tweets"
              )
            ),
            fluidRow(
              box(
                p("The below graphs demonstrate the changes in the sentiments of popular political tweets now vs May 7th 2018"),
                p("The graphs may take a while to initially render")
              )
            ),
            fluidRow(
              box(width="80%",
                  plotOutput("sent1")
              )
            ),
            fluidRow(
              box(width="80%",
                  plotOutput("sent2")
              )
            )
            
    ),
    tabItem(tabName="pop",
            fluidRow(
              box(
                title = "Sentiment Frequency in Popular Tweets"
              )
            ),
            fluidRow(
              box(
                p("The below graphs demonstrate the changes in the frequencies of sentiments of popular political tweets now vs May 7th 2018"),
                p("The graphs may take a while to initially render")
              )
            ),
            fluidRow(
              box(width="70%",
                  plotOutput("pop1")
              )
            ),
            fluidRow(
              box(width="70%",
                  plotOutput("pop2")
              )
            )
              
    ),
    tabItem(tabName = "conc",
            fluidRow(
              box(
                  width="50%",
                  h1("Conclusion")
              )
            ),
            fluidRow(
              box(width="50%",
                p("A popular political tweet should be near the top of the character limit without a link. The subject of the tweet should be the previous election, and collusion with Russia, specifically naming Trump. The tweet should be angry, negative, and use fearful language. This is the profile of the tweets from the first week of May 2018."),
                p("This may be very volatile data, so my shiny application allows the user to compare the current data to this data from May 1st to May 7th 2018 to determine if any changes have occurred."),
                p("My methodology may be biased by the search terms I chose. Future experiments may be improved by a larger list that gets the tweets in batches to deal with the maximum queries possible with the API.")
              )
            )
    )
  )
)


ui <- dashboardPage(skin="green", header, sidebar, body)
server <- function(input, output){
  output$count1 <- renderPlot({
    #plot 10 values
    ggplot(data=head(text,10), aes(x=head(text$word,10), y=head(text$n,10))) + xlab("Word") + ylab("Count") +
      geom_bar(stat="identity") + ggtitle("May 7th 2018 Word Frequencies")
  })
  
  output$count2 <- renderPlot({
    #plot 10 values
    ggplot(data=head(text2,10), aes(x=head(text2$word,10), y=head(text2$n,10))) + xlab("Word") + ylab("Count") +
      geom_bar(stat="identity") + ggtitle("Current Word Frequencies")
  })


  #Now we have the number of occurences of each word in the popular tweets
  #Sentiment Analysis
  output$sent1 <- renderPlot({
    # senttable <- get_sentimentss(text, nrc$sentiment)
    # pctsent <- get_pct_occur(senttable, nrc$sentiment)
    # pctsent <- pctsent[!duplicated(pctsent[,c('sentiments')]),]
    # pctsent <- as.data.frame(lapply(pctsent, unlist))
    # #plot values
    # saveRDS(pctsent,"pctsent.RDS")
    pctsent <- readRDS("pctsent.RDS")
    ggplot(data=pctsent, aes(x=pctsent$sentiments, y=pctsent$occurrences)) + xlab("Sentiment") + ylab("Percent") +
      geom_bar(stat="identity")+ ggtitle("May 7th 2018 Sentiment Distribution")
    
  })
  output$sent2 <- renderPlot({
    senttable <- get_sentimentss(text2, nrc$sentiment)
    pctsent <- get_pct_occur(senttable, nrc$sentiment)
    pctsent <- pctsent[!duplicated(pctsent[,c('sentiments')]),]
    pctsent <- as.data.frame(lapply(pctsent, unlist))
    #plot values
    ggplot(data=pctsent, aes(x=pctsent$sentiments, y=pctsent$occurrences)) + xlab("Sentiment") + ylab("Percent") +
      geom_bar(stat="identity") + ggtitle("Current Sentiment Distribution")
    
  })
   
  # #Now we have percent sentiment of all the popular tweets
  
  #Let's determine the length of tweet vs popularity
  output$plot1 <- renderPlot({
    lenpop <- textpop %>% mutate(length = nchar(text))
    
    ggplot(lenpop %>% select(length,popularity), aes(x=length, y=popularity))+
    geom_line(color="red")+
    xlab("Tweet Length")+
    ylab("Popularity of tweet")+
    ggtitle("Popularity of Political Tweets by Length")
  })
  output$plot2 <- renderPlot({
    lenpop2 <- textpop2 %>% mutate(length = nchar(text))

    ggplot(lenpop2 %>% select(length,popularity), aes(x=length, y=popularity))+
    geom_line(color="red")+
    xlab("Tweet Length")+
    ylab("Popularity of tweet")+
    ggtitle("Popularity of Political Tweets by Length")
  })
  
  
  
  #Let's determine the most popular sentiments
  
  output$pop1 <- renderPlot({
    # tidypop <- textpop %>% # Word COUNT
    #   unnest_tokens(word, text) %>%
    #   anti_join(stop_words, by=c("word"="word")) %>%
    #   inner_join(nrc) %>% select(sentiment, popularity)
    # 
    # out <- get_sum_pop(tidypop, nrc$sentiment)
    # sentimentpop <- as.data.frame(lapply(out, unlist)) %>% distinct()
    # saveRDS(sentimentpop, "sentpop.RDS")
    sentimentpop <- readRDS("sentpop.RDS")
    #plot values
    ggplot(data=sentimentpop, aes(x=sentimentpop$sentiments, y=sentimentpop$popularity)) + xlab("Sentiment") + ylab("Popularity") +
      geom_bar(stat="identity") + ggtitle("May 7th Sentiment Popularity")
  })
  output$pop2 <- renderPlot({
    tidypop2 <- textpop2 %>% # Word COUNT
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by=c("word"="word")) %>%
      inner_join(nrc) %>% select(sentiment, popularity)
    out <- get_sum_pop(tidypop2, nrc$sentiment)
    sentimentpop2 <- as.data.frame(lapply(out, unlist)) %>% distinct()
    #plot values
    ggplot(data=sentimentpop2, aes(x=sentimentpop2$sentiments, y=sentimentpop2$popularity)) + xlab("Sentiment") + ylab("Popularity") +
      geom_bar(stat="identity") + ggtitle("Current Sentiment Popularity")
    
  })
  
}
shinyApp(ui, server)