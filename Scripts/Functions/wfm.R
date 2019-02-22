library(tm)

wfm<-function(data, by.screen){
  
  if(by.screen==T){
    t<-Corpus(VectorSource(c(data$retweet_screen_name[is.na(data$retweet_screen_name)==F])))
  }
  if(by.screen==F){
    t<-Corpus(VectorSource(c(data$clean)))
  }
  
#  t <- tm_map(t, removeWords, c("dass"))
#  t <- tm_map(t, removePunctuation)
  
  dtm <- TermDocumentMatrix(t)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
  
  
}
