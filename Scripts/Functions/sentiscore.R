###
# Evaluate a text using a senti corpus
###

### you need a sentiment corpus with a value and a word

sentiscore<-function(tweet,senti){
  tweets.position <- unlist(strsplit(tweet,' '))
  tweets.position<- tweets.position[tweets.position!='']
  tweetscore<-sum(senti$values[senti$word %in% tweets.position])
  tweetpol<-sum(abs(senti$values[senti$word %in% tweets.position]))
  p<-c(tweetscore,tweetpol)
  return(p)
}
