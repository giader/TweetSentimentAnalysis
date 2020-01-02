### Donald Trump
#getwd()
# MS WIN-10
setwd("C:/Users/Gianni/Dropbox/R_Miscellanea/rTweet")
load("./TweetHashTagDonald.RData")

save.image("./TweetHashTagDonald.RData") 

library(rtweet)

library(ggplot2)
library(syuzhet)
library(reshape2)


library(httpuv)
###debug(create_token) 

twitter_tokens <- c(
  create_token(app = "rtweet_tokens_default",
               consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
               consumer_secret = "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy")
)

#tw <- search_tweets("realdonaldtrump", n = 120000, token = twitter_tokens, lang = "it")

#######################################################
###install.packages(c("syuzhet", "reshape2"))
# load rtweet and ggplot
#######################################################

DTrump <- search_tweets("realdonaldtrump", n = 100000)


# conducing a sentiment analysis is super easy 
# https://github.com/mjockers/syuzhet
# conducing a sentiment analysis is super easy
sa_DTrump <- syuzhet::get_nrc_sentiment(DTrump$text)


# output contains 8 emotion scores (columns) for each row
head(sa_DTrump)

# subset rtweet data (for ease) and combine with sentiment data
sa_DTrump <- cbind(
  DTrump[, c("status_id", "favorite_count", "retweet_count")],
  sa_DTrump)

# transform data to long form
dt_DTrump <- reshape2::melt(dt_DTrump,
                           variable.name = "emotion",
                           value.name = "sentiment",
                           id.vars = c("status_id", "favorite_count", "retweet_count"))

# plot using ggplot
plot_dt_DTrump <- ggplot(dt_DTrump, aes(x = emotion, y = sentiment,
                                      fill = emotion)) + theme_minimal() +
  coord_cartesian(ylim = c(0, 7)) +
  geom_jitter(color = "#ffffff", shape = 21,
              size = 2, alpha = .7, stroke = .15) +
  coord_flip() + labs(y = "", x = "",
                      title = "Sentiment Analysis #realDonaldTrump (by gdr)") +
  theme(legend.position = "none",
        text = element_text("Arial", size = 18),
        axis.text.x = element_blank())
plot_dt_DTrump

# save plot image
png("./plot_dt_DTrump_definitive.png", 500,500)
plot_dt_DTrump
dev.off()

# build a corpus using the text mining (tm) package
library(tm)  ## Text Mining package  --> need:
##  - NLP Package: Natural Package Language
##  - slam package
library(wordcloud)
#INTIt_corpus <- Corpus(VectorSource(INTIt$text))
DTrump_corpus <- VCorpus(VectorSource(DTrump$text))
DTrump_corpus_clean <- tm_map(DTrump_corpus, tolower)
DTrump_corpus_clean <- tm_map(DTrump_corpus_clean, removeNumbers)
DTrump_corpus_clean <- tm_map(DTrump_corpus_clean, removeWords, stopwords('english'))
DTrump_corpus_clean <- tm_map(DTrump_corpus_clean, removePunctuation)
DTrump_corpus_clean <- tm_map(DTrump_corpus_clean, stripWhitespace)

# create a document-term sparse matrix
#   sms_dtm <- DocumentTermMatrix(corpus_clean, content_transformer(tolower))
### Error: inherits(doc, "TextDocument") is not TRUE
### Error: is.list(control) is not TRUE
DTrump_corpus_clean <- tm_map(DTrump_corpus_clean, PlainTextDocument)

#usableDTrump_corpus_clean=iconv(DTrump$text, "ASCII", "UTF-8", sub="")

wordcloud(DTrump_corpus_clean, min.freq = 300, random.order = FALSE)
wc_DTrump<-wordcloud(DTrump_corpus_clean, min.freq = 300, random.order = FALSE)

# save plot image
png("./wc_DTrump.png", 500,500)
wordcloud(DTrump_corpus_clean, min.freq = 300, random.order = FALSE)
dev.off()

