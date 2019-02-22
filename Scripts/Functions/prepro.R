
stopwords<-read.table('C:/Users/Admin/Dropbox/Sentiment Analysis/Dictionary/stopwordsplus.txt')
stopwords_full<-stopwords$V1

cleantweet<-function(x){
  x<-gsub('(#\\w*\\b)','',x)
  x<-gsub('(@\\w*\\b)','',x)
  return(x)
}

twittertract<-function(x){
  library(stringr)
  tags<-str_extract_all(x,regex('(#\\w*\\b)'))
  p<-as.character(x)
  for(i in 1:length(tags)){
    if(length(tags[[i]])==0){p[i]<-NA}
    if(length(tags[[i]])>1){p[i]<-paste(gsub('[[:punct:]]','',tags[i]))
    p[i]<-gsub('^c','',p[i])}
    if(length(tags[[i]])==1){p[i]<-gsub('#','',tags[[i]][1])}
  }
  mens<-str_extract_all(x,regex('(@\\w*\\b)'))
  m<-as.character(x)
  for(i in 1:length(mens)){
    if(length(mens[[i]])==0){m[i]<-NA}
    if(length(mens[[i]])>1){m[i]<-paste(gsub('[[:punct:]]','',mens[i]))
    m[i]<-gsub('^c','',m[i])}
    if(length(mens[[i]])==1){m[i]<-gsub('@','',mens[[i]][1])}
  }
  return(cbind.data.frame(m,p))
}

stripURL2 = function(x) {
  gsub("www[^[:space:]]+|htt[^[:space:]]+|//t\\.[^[:space:]]+", " ", x)
}

latinize<-function(x){x<-iconv(x,to='latin1')}



stripsplits = function(x) {
  gsub('\n', " ", x)
}




