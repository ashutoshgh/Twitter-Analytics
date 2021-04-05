setwd("D:/Social Media Analytics/")
rm(list=ls())

install.packages('plyr', 'dplyr', 'stringr', 'ggplot2', 'httr', 'wordcloud',' RCurl', 'syuzhet', 'tm')
install.packages('devtools')

devtools::install_github("tutuchan/chartjs", ref = "dev")

#.libPaths ()
library(radarchart)
library(chartjs)
library(tidyverse)
library(rtweet)
library(qdapRegex)
library(plyr)
library(ggplot2)
library(SnowballC)
library(tm)
library(twitteR)
library(wordcloud)
library(syuzhet)
library(stringr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(cluster)
library(wordcloud2)
library(corrplot)
library(rpart)
library(C50)
library(RWeka)
library(randomForest)


### Twitter Application
consumer_key <- "T8hc9AQuZ4cF5BEzbHV7qMBvw"
consumer_secret <- "slgULkaaXEl5Ty2Lq8s9aslMiCTPhfuDbFvArbYYQdBfLxywJ2"
access_token <- "962844637-Gb6dMFy3bbxJrWbPetFos7Wwp7fawWMDm6PxFwZh"
access_secret <- "QDQD23fv3h4timGfW6aOKwE3r0TqYe8tRrAuuIvHKnPka"
token <- create_token(
  app = "Text_Analytics Trial",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

numberofTweets = 13000
since<-"2020-08-15"
until<-"2020-08-19"

###ANDROID

android <- search_tweets('android', n = numberofTweets, since = since, 
                          until = until, include_rts = FALSE, 
                          retryonratelimit = TRUE, lang='en')

View(android)
write.csv(android, "android.csv")

andro <- read.csv('android.csv')
View(andro)

#total number of tweets downloaded = 15611
length(andro$text)

#cleaning the text
andro$text <- gsub("&amp", "", andro$text)
andro$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", andro$text)
andro$text <- gsub("@\\w+", "", andro$text)
andro$text <- gsub("[[:punct:]]", "", andro$text)
andro$text <- gsub("[[:digit:]]", "", andro$text)
andro$text <- gsub("http\\w+", "", andro$text)
andro$text <- gsub("[ \t]{2,}", "", andro$text)
andro$text <- gsub("^\\s+|\\s+$", "", andro$text)
andro$text <- iconv(andro$text, "UTF-8", "ASCII", sub="")
#andro$text <- gsub("android", "", andro$text)

#Total number of unique tweets
andro <- andro %>% distinct(text, .keep_all= TRUE)
length(andro$text)
View(andro)

# Exporting to Excel

write.csv(andro, "android_clean.csv")
View(andro)
class(andro)

# Number of unique users who's tweets have been downloaded
uniqueusers<-length(unique(andro$screen_name))
uniqueusers

summary(andro$is_quote)
View(andro)

# Number of people who liked the tweets in total
sum(andro$favorite_count)

# Number of people who retweeted the tweets
sum(andro$retweet_count)

# Source from where the tweet was tweeted
View(andro$source)
table(andro$source)
# x <- andro %>% 
#   select(source) %>% 
#   group_by(source) %>%
#   summarize(count=n())
# 
# androx <- andro$source[count(andro$source > 20)]

# Sentiment Analysis
emotions <- get_nrc_sentiment(andro$text)
head(emotions)
View(emotions)

andro_sentiment <- get_sentiment(andro$text)

# Most positive tweet
most.positive <- andro$text[andro_sentiment == max(andro_sentiment)]
most.positive

# Most negative tweet
most.negative <- andro$text[andro_sentiment == min(andro_sentiment)]
most.negative

# Table of different emotions
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Bar graph of different emotions
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for tweets related to Android")
p


# Create a corpus
docs <- Corpus(VectorSource(andro$text))

# Clean the text
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("android"))
#docs = tm_map(docs, stemDocument)

# Create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)

# Word frequencies
barplot(word_frequency[1:10,]$freq, las = 2, names.arg = word_frequency[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")

# Wordcloud
set.seed(69)
wordcloud(words = word_frequency$word, freq = word_frequency$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.4, 
          colors=brewer.pal(8, "Dark2"))

andro_v <- get_sentences(andro$text)
andro_v_sentiment <- get_sentiment(andro_v)
simple_plot(andro_v_sentiment)

wordcloud2(data = p, color = 'random-light', backgroundColor = 'black')
w

tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)

labs <- c("Negative", "Positive", "Anger", "Anticipation", "Disgust", "Fear", 
          "Joy", "Sadness", "Surprise", "Trust")
c <- grDevices::col2rgb(c("green", "blue"))
scores <- list(
  "Android" = c(6006, 9149, 2742, 5285, 1967, 3220, 3426, 2906, 2158, 5222),
  "iPhone" = c(6370, 8814, 3008, 5124, 2081, 3541, 3615, 3282, 2529, 4927 ))

chartJSRadar(scores=scores, labs=labs, colMatrix = c)

# Fix the max score
# chartJSRadar(scores=scores, labs=labs, maxScale=10000)
# 
# # Fix max and spacing
# chartJSRadar(scores=scores, labs=labs, maxScale=9500, scaleStepWidth = 2)
# 
# # Change title and remove legend
# chartJSRadar(scores=scores, labs=labs, main = "Data Science Radar", showLegend = FALSE)
# 
# # Add pass through settings for extra options
# chartJSRadar(scores=scores, labs=labs, maxScale = 9500, scaleLineWidth= 2000)
# chartJSRadar(colMatrix = )




##############################IPHONE##############################

iphone <- search_tweets("iphone", n = 10000, type = 'recent', 
                         include_rts = FALSE, retryonratelimit = TRUE, lang='en')
View(iphone)
rm(list=ls())
#Removing the duplicates

write_as_csv(iphone, "iphone.csv")

iphone <- read.csv('iphone.csv')
View(iphone)
#length(iphone$text)

#total number of tweets downloaded = 15598
iphone <- iphone[!(iphone$screen_name == ""), ]
length(iphone$text)
View(iphone)

iphone$text <- gsub("&amp", "", iphone$text)
iphone$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", iphone$text)
iphone$text <- gsub("@\\w+", "", iphone$text)
iphone$text <- gsub("[[:punct:]]", "", iphone$text)
iphone$text <- gsub("[[:digit:]]", "", iphone$text)
iphone$text <- gsub("http\\w+", "", iphone$text)
iphone$text <- gsub("[ \t]{2,}", "", iphone$text)
iphone$text <- gsub("^\\s+|\\s+$", "", iphone$text)
iphone$text <- iconv(iphone$text, "UTF-8", "ASCII", sub="")

#total unique tweets in the data = 14140
iphone <- iphone %>% distinct(text, .keep_all= TRUE)
length(iphone$text)
View(iphone)

# Exporting to Excel

write.csv(iphone, "iphone_clean.csv")
iphone <- read.csv('iphone_clean.csv')

iphone_v <- get_sentences(iphone$text)
iphone_v_sentiment <- get_sentiment(iphone_v)
simple_plot(iphone_v_sentiment)

pwdw <- round(length(andro_v_sentiment)*0.8)
poa_rolled <- zoo::rollmean(andro_v_sentiment, k=pwdw)
bwdw <- round(length(iphone_v_sentiment)*0.8)
bov_rolled <- zoo::rollmean(iphone_v_sentiment, k=bwdw)
unlo(dplyr)

poa_list <- rescale_x_2(poa_rolled)
bov_list <- rescale_x_2(bov_rolled)

plot(poa_list$x, 
     poa_list$z, 
     type="l", 
     col="blue", 
     xlab="Narrative Time", 
     ylab="Emotional Valence")
lines(bov_list$x, bov_list$z, col="red")


#number of unique users who's tweets have been downloaded = 12251
uniqueusers <- length(unique(iphone$screen_name))
uniqueusers

summary(iphone$is_quote)
View(iphone)

#number of people who liked the tweets in total = 49708
sum(iphone$favorite_count)

#number of people who retweeted the tweets = 10824
sum(iphone$retweet_count)

#source from where the tweet was tweeted
View(iphone$source)
table(iphone$source)
x <- iphone %>%
   select(source) %>%
   group_by(source) %>%
   summarize(count=n())
# 
# androx <- andro$source[count(andro$source > 20)]
#Sentiment Analysis

emotionsi <- get_nrc_sentiment(iphone$text)
head(emotionsi)
View(emotionsi)

iphone_sentiment <- get_sentiment(iphone$text)

#the most positive tweet
most.positivei <- iphone$text[iphone_sentiment == max(iphone_sentiment)]
most.positivei

#the most negative tweet
most.negativei <- iphone$text[iphone_sentiment == min(iphone_sentiment)]
most.negativei

emo_bari <- colSums(emotionsi)
emo_sumi <- data.frame(count = emo_bari, emotion = names(emo_bari))
emo_sumi$emotion <- factor(emo_sumi$emotion, levels = emo_sumi$emotion[order(emo_sumi$count, 
                                                                             decreasing = TRUE)])

pi <- plot_ly(emo_sumi, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for tweets related to iPhone")
pi

#create corpus
docsi <- Corpus(VectorSource(iphone$text))

#text cleaning
docsi = tm_map(docsi, tolower)
docsi = tm_map(docsi, removePunctuation)
docsi = tm_map(docsi, removeWords, c(stopwords("english")))
docsi = tm_map(docsi, removeNumbers)
docsi = tm_map(docsi, stripWhitespace)
docsi = tm_map(docsi, removeWords, c("iphone"))
#docs = tm_map(docs, stemDocument)

# create document term matrix
View(dtm)
View(dtmi)
dtmi = TermDocumentMatrix(docsi)
mi = as.matrix(dtmi)
vi = sort(rowSums(mi),decreasing=TRUE)
frequent_words = data.frame(word = names(vi),freq = vi)
head(frequent_words,10)
q = head(frequent_words, 300)

#word frequencies
barplot(frequent_words[1:10,]$freq, las = 2, names.arg = frequent_words[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(69)
wordcloud(words = frequent_words$word, freq = frequent_words$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data = q, color = 'random-light', backgroundColor = 'black')

# mi %>%
#   inner_join(bing) %>%
#   count(text, sentiment, sort = TRUE) %>%
#   acast(text ~ sentiment, value.var = "n", fill = 0) %>%
#   comparison.cloud(colors = c("red", "dark green"),
#                    max.words = 100)
# # 
# library(stringr)
# install.packages('tidytext')
# library(tidytext)
# bing <- get_sentiments("bing")
# positive_senti <- get_sentiments("bing") %>% filter(sentiment == "positive")
# positive_senti <- get_sentiments("bing") %>%
#   filter(sentiments == "positive")
# 
# environment(filter)
# stats::filter
# iphone$text %>%
#    %>%
#   semi_join(positive_senti) %>%
#   count(word, sort = TRUE)
# positive_senti <- 
# comparison.cloud(mi, max.words=40,random.order=FALSE,
#                   title.colors=c("red","blue"),title.bg.colors=c("grey40","grey70")) 
#  
#  
#  
# data(SOTU)
# View(SOTU)
# corp <- SOTU
# corp <- tm_map(corp, removePunctuation)
# corp <- tm_map(corp, content_transformer(tolower))
# corp <- tm_map(corp, removeNumbers)
# corp <- tm_map(corp, function(x)removeWords(x,stopwords()))
#  
# term.matrix <- TermDocumentMatrix(corp)
# View(term.matrix)
# term.matrix <- as.matrix(term.matrix)
# colnames(term.matrix) <- c("SOTU 2010","SOTU 2011")
# comparison.cloud(term.matrix,max.words=40,random.order=FALSE)
# comparison.cloud(term.matrix,max.words=40,random.order=FALSE,
#                 title.colors=c("red","blue"),title.bg.colors=c("grey40","grey70"))
# comparison.cloud(term.matrix,max.words=40,random.order=FALSE,
#                 match.colors=TRUE)
#  
# 
