load("tweets.Rda") 
library(quanteda)
library(quanteda.textstats) 
library(quanteda.textplots)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidytext) 
library(glmnet)
tweetCorpus <- corpus(tweets$text, docvars = tweets)


#i. Use appropriate tools to describe the tweets. In particular, what words are associated with negative or positive sentiment? How does word usage differ across the different airlines? 
dfm_tweets <- tweetCorpus %>%
  tokens(remove_numbers = T,
         remove_punct=T) %>%
  tokens_remove(stopwords("en")) %>%
  dfm(tolower = T) %>%
  dfm_weight(scheme = "prop") %>%
  dfm_trim(min_docfreq = 3) %>%
  dfm_remove(c("@americanair","@united", "@USAirways", "@SouthwestAir", "@JetBlue", "@VirginAmerica","flight",letters))
dim(dfm_tweets)
# before removing unique words: 11541 13066, after: 11541  1339

# from now, we are going to use TF-IDF to remove uninformative words
dfm_tweets_tfidf <- dfm_tweets%>%dfm_tfidf(force = TRUE)

# frequent words
freq_words_tfidf <- textstat_frequency(dfm_tweets_tfidf,n=10,groups=sentiment,force=TRUE)
#by airline
freq_words_tfidf_airline <- textstat_frequency(dfm_tweets_tfidf,n=10,groups= airline,force=TRUE)

#most positive and negative terms
sentiment <- tweets$sentiment
which.max(sentiment$totalneg)
tweets$text[which.max(sentiment$totalneg)]
tweets$text[which.max(sentiment$totalpos)]

#What words are associated with positive and negative sentiment?
# to do this, make sentiment into factor
tweets$sentiment <- as.factor(tweets$sentiment)
summary(tweets$sentiment)
by_sentiment <- textstat_frequency(dfm_tweets_tfidf,25, groups = sentiment, force = TRUE)
# different word usage between airlines
by_airline <- textstat_frequency(dfm_tweets_tfidf, groups = airline, force=T)
by_airline
byairline2 <- textstat_frequency(dfm_tweets_tfidf, n=15, groups=c("airline" , "sentiment"), force = T)
table(by_airline$group)
comparison_airlines <- textstat_frequency(dfm_tweets_tfidf,15, groups=dfm_tweets_tfidf$airline, force = TRUE)

#Renaming categories
#turn 0 and 1 into negative and positive
by_sentiment$group[by_sentiment$group=="1"] <- "Negative"
by_sentiment$group[by_sentiment$group=="0"] <- "Positive"

#plot positive and negative words
ggplot(by_sentiment,aes(x=frequency,y=reorder(feature,frequency))) +
  facet_wrap(~group, scales = "free") +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF Weighted)") +
  theme_bw()
# proportion of positive and negative for every airline
prop_airline <- tapply(tweets$sentiment, tweets$airline, table)

#plot in a frequency table
ggplot(comparison_airlines,
       aes(x=frequency,y=reorder(feature,frequency))) +
  facet_wrap(~group, scales = "free") +
  geom_point() +
  ylab("Most Frequent Words/airline") +
  xlab("Frequency (TF-IDF Weighted") +
  ggtitle("Different Words Usage Across Airlines") +
  theme_bw()

#plot of proportion of negative and positive tweets by airline
prop_sentiment_byairline<- ggplot(data=tweets, aes(x=sentiment))+geom_bar(fill="yellow", stat="count", color="orange")+facet_wrap(~airline)+scale_x_discrete(limits=c(0,1), labels=(c("Positive","Negative")))+ggtitle("Proportion of Negative and Positive Tweets by Airline")+theme(legend.position="none", plot.title=element_text(hjust=0.5))+labs(y="Frequency", x="Sentiment")+geom_text(stat='count', aes(label=..count..),vjust=1,size=3, colour="black")
prop_sentiment_byairline

#ii) build a short dictionary of negative and positive words
neg.words <- c("get", "cancelled", "now", "hold", "hours", "service","can","just", "still", "delayed" )
pos.words <- c("thank", "thanks", "great", "love", "awesome", "much", "best", "good", "just", "guys")
mydict <- dictionary(list(negative = neg.words,
                          positive = pos.words))

#classify tweets with dictionary A
dictionary_A <- tweetCorpus %>% tokens(remove_numbers = T,
                                       remove_punct=T) %>%
  tokens_remove(stopwords("en")) %>% 
  dfm(tolower = T) %>%
  dfm_weight(scheme="prop") %>%
  dfm_lookup(dictionary = mydict)

dictionary_table <- data.frame(Negative = neg.words, Positive = pos.words)
pos.words <- c("thanks", "thank", "get" , "service", "great", "love", "awesome","good")
sentiment_mydict <- dfm_lookup(dfm_tweets_tfidf,dictionary=mydict)
sentiment_mydict[1:10,]
sentiment_mydict_df <- convert(sentiment_mydict, to="data.frame")
tweets$text[which.max(sentiment_mydict_df$negative)]
tweets$text[which.max(sentiment_mydict_df$positive)]

# training, validation and test data
sentiment_mydict_df$score <- ifelse((sentiment_mydict_df$positive - sentiment_mydict_df$negative)>0,0,1)
table(sentiment_mydict_df$score)
table(sentiment_mydict_df$score,tweets$sentiment)
# test error rate
(942+790)/(1421+790+942+8388)
# sensitivity
8388/(790+8388)
# specificity
1421/(1421+942)

##iii) Using lasso logit model to classify tweets
dfm_tweets_tfidf2 <- as.matrix(cbind(tweets$sentiment, dfm_tweets_tfidf))
cv.rows <- sample(nrow(dfm_tweets_tfidf2),(nrow(dfm_tweets_tfidf2)/2))
cv.data <- dfm_tweets_tfidf2[cv.rows,]
test.data <- dfm_tweets_tfidf2[-cv.rows,]
lasso.rev <- cv.glmnet(x=cv.data[,2:ncol(dfm_tweets_tfidf2)],y=cv.data[,1],
                       family="binomial",type.measure="class")
lasso.coef <- as.matrix(coef(lasso.rev))
length(lasso.coef[lasso.coef!=0])
lasso.coef <- coef(lasso.rev)
lasso.coef <- as.matrix(lasso.coef[lasso.coef[,1]!=0,])
lasso.coef <- as.matrix(lasso.coef[order(lasso.coef[,1],
                                         decreasing = TRUE),])
# Strongest predictors of negative reviews
lasso.coef[1:10,]
# Strongest predictors of positive reviews
lasso.coef[(nrow(lasso.coef)-10):nrow(lasso.coef),]

plot(lasso.rev)
tweets_preds <- predict(lasso.rev,
                        test.data[,2:ncol(dfm_tweets_tfidf2)],
                        type='class')
table(tweets_preds,test.data[,1])
#error rate
(430+118)/(765+118+430+4458)
#specificity rate
8388/(8388+790)
#sensitivity rate
1421/(1421+942)

