#hier soll eine Funktion rein, die uns interessante Daten erstellt mit denen wir den ein oder anderen interessanten 
# plot bauen können
library(dplyr)
tweetvariation<-function(data){
  data$day<-as.POSIXct(strptime(sub("\ .*","", data$created_at),"%Y-%m-%d"))
  data$hour<- sub(".*\ ","", data$created_at)
 # data$hour_lab<- as.character(sub(".*\ ","", data$created_at))
  data$quoted_created_at<-as.POSIXct(strptime(sub("\ .*","", data$quoted_created_at),"%Y-%m-%d"))
  data$retweet_created_at<-as.POSIXct(strptime(sub("\ .*","", data$retweet_created_at),"%Y-%m-%d"))
  
  data2<-select(data,c("day", "hour", "screen_name", "text", "source", "reply_to_screen_name", "is_quote", "is_retweet",
                       "favorite_count", "retweet_count", 
                      
                       "quoted_text", "quoted_created_at", "quoted_source","quoted_favorite_count", "quoted_retweet_count",
                       "quoted_screen_name", "quoted_name", "quoted_followers_count", "quoted_friends_count",
                       
                       "retweet_text", "retweet_created_at", "retweet_source","retweet_favorite_count", "retweet_retweet_count",
                       "retweet_screen_name", "retweet_name", "retweet_followers_count", "retweet_friends_count",
                       
                       "followers_count", "friends_count", "account_created_at"
                       ))
  
  data2$hashtag_count<-0
  data2$mention_count<-0
  
  for(i in 1:length(data2$text)){
    data2$hashtag_count[i]<- str_count(data2$text[i], "#")
    data2$mention_count[i]<- str_count(data2$text[i], "@")
    data2$URLcontained[i]<- ex_url(data2$text[i], trim=T, clean=T)
    ifelse(is.na(data2$URLcontained[i])==F,data2$URL_dummy[i]<- T,data2$URL_dummy[i]<- F)
    if(isTRUE(data2$is_quote[i])==T){data2$type[i]<-"quote"}
    if(isTRUE(data2$is_retweet[i])==T){data2$type[i]<-"retweet"}
    if(isTRUE(data2$is_quote[i])==F&isTRUE(data2$is_retweet[i])==F){data2$type[i]<-"original"}
  }  
  data2$URLcontained<-as.character(data2$URLcontained)
  
  return(data2) 
}
