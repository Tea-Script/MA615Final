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


#words to ignore 
#stop_words <-  data.frame(stopwords(kind="en"))
#colnames(stop_words) <- c("word")
#write.csv(stop_words, "stopwords.csv")
###
stop_words <- read.csv("stopwords.csv")
###run this code once
#key <- "DhEKno4O4jyDE7H4tgxw2KCJv"
#secret <- "dR581H8j6PMAWbaDKu4ncLEsxCEA1cKK2cxpgtY4hnzFFenFB9"
#
# twitter_token <- create_token(
#   app = "MA615Final",
#   consumer_key = key,
#   consumer_secret = secret,
#   set_renv=F
# )
### saveRDS(twitter_token, "./twitter_token.rds")
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
#Download tweets with search terms into a df
tw <-  distinct(rtweet::search_tweets(search_terms, n = 1e6, type="popular", retryonratelimit = 1e4, token=twitter_token))
#Collect column for text and column for the amount of shares and favorites
tw <-  tw %>% mutate(popularity = retweet_count + favorite_count) %>% select(text, popularity)
#Remove duplicate text
textpop <- tw[!duplicated(tw[,c('text')]),]
#Now we have the popularity of each tweet

text <- textpop %>% select(text) 
text <- text %>% # Word COUNT
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  inner_join(nrc) %>% 
  count(word, sort = TRUE) 
#Now we have the number of occurences of each word in the popular tweets
#Sentiment Analysis
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


senttable <- get_sentimentss(text, nrc$sentiment)
pctsent <- get_pct_occur(senttable, nrc$sentiment)
pctsent <- pctsent[!duplicated(pctsent[,c('sentiments')]),]

View(senttable)
View(pctsent)
#Now we have percent sentiment of all the popular tweets

#Let's determine the length of tweet vs popularity
lenpop <- textpop %>% mutate(length = nchar(text))
View(lenpop)
ggplot(lenpop %>% select(length,popularity), aes(x=length, y=popularity))+
  geom_line(color="red")+
  xlab("Tweet Length")+
  ylab("Popularity of tweet")+
  ggtitle("Popularity of Political Tweets by Length")

#Let's determine the most popular sentiments



