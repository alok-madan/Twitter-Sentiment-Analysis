### install ###

install.packages("twitteR")
install.packages("ROAuth")
install.packages(utils)

### load ###
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("twitteR")
library("ROAuth")
library(RColorBrewer)
library("wordcloud")
library(ggplot2)
        
        #twitter authentication
        consumer_key <- 'INSERT CONSUMER KEY'
        consumer_secret <- 'INSERT CONSUMER SECRET'
        access_token <- 'INSERT ACCESS TOKEN'
        access_secret <- 'INSERT ACCESS SECRET'
        
        setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
        
        
        ##### SEARCH QUERY #############
        tweets_s <- searchTwitter("QUERY", n=500,lang = "en")
        
        
        #convert to dataframe
        Query_tweets <- twListToDF(tweets_s)
        
        
        
        # convert to text Only
        Query_text<- Query_tweets$text
        
        
        
        ######## Clean text ###############
        #convert to lower case
        Query_text<- tolower(Query_text)
        # replace blank space & "rt"
        Query_text <- gsub("rt", "", Query_text)
        # Replace @UserName
        Query_text <- gsub("@\\w+", "", Query_text)
        # Remove punctuation
        Query_text <- gsub("[[:punct:]]", "", Query_text)
        # Remove links
        Query_text <- gsub("http\\w+", "", Query_text)
        # Remove tabs
        Query_text <- gsub("[ |\t]{2,}", "", Query_text)
        # Remove blank spaces at the beginning
        Query_text <- gsub("^ ", "", Query_text)
        # Remove blank spaces at the end
        Query_text <- gsub(" $", "", Query_text)
        
       
      ### Sentiment Analysis ###  
       
       mysentiment_Query<-get_nrc_sentiment((Query_text))
        
        Sentimentscores_Query<-data.frame(colSums(mysentiment_Query[,]))
        
        names(Sentimentscores_Query)<-"Score"
        Sentimentscores_Query<-cbind("sentiment"=rownames(Sentimentscores_Query),Sentimentscores_Query)
        rownames(Sentimentscores_Query)<-NULL
        
       
       ### Plot sum of Sentiment Scores by emotion ###
       
       ggplot(data=Sentimentscores_Query,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
          theme(legend.position="none")+
          xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Frank Ocean")
      
  
        
        
        # Wordcloud generator
        wordcloud(Query_text,min.freq = 10,colors=brewer.pal(6, "Dark2"),random.color = TRUE,max.words = 500)
        
        
         ### Export tweets as CSV ###
        write.csv(Query_text, file='tweets3.csv')
