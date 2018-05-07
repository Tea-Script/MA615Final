#install.packages(rtweet)
library(rtweet)
library(tidyverse)
library(tidytext)
nrc <- get_sentiments("nrc")
### This package was used to create a csv file which was modified
#install.packages(tm)
library(tm)
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

cat(paste0("TWITTER_PAT=", "./twitter_token.rds"),
     file = file.path("./", ".Renviron"),
     append = TRUE)

search_terms = '#realDonaldTrump OR #HillaryClinton OR politics OR democrats OR republicans OR socialist
               OR healthcare OR Gun+Control OR Syria OR Mueller OR blacklivesmatter OR NSA OR FBI OR CIA OR
               drone+strikes OR Obama OR Sanders OR Clinton OR Trump OR progressive OR conservative OR liberal
               OR immigration OR right+wing OR left+wing OR war OR Syria OR ISIS OR United+States OR US OR education
               '
#Download tweets with search terms into a df
tw <-  distinct(rtweet::search_tweets(search_terms, n = 1e4, type="popular", retryonratelimit = 1e3, token=twitter_token))
#Collect column for text and column for the amount of shares and favorites
tw <-  tw %>% mutate(popularity = retweet_count + favorite_count) %>% select(text, popularity)
#Remove duplicate text
textpop <- tw[!duplicated(tw[,c('text')]),]
text <- textpop %>% select(text) 
text <- text %>% # Word COUNT
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  inner_join(nrc) %>% 
  count(word, sort = TRUE) 
View(text)
View(nrc)

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
senttable <- get_sentimentss(text, nrc)
View(senttable)



