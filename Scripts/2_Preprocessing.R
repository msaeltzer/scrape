###
# Preprocessing Script
###

cleaner<-function(working,stem=F){
setwd(paste0(root,'\\scripts\\functions'))
## get prepro
source('prepro.R')

#### Preprocessing function
prep<-function(set){
  library(tm)
  v<-VectorSource(set$text)
  docs<-Corpus(v)
#  docs <- tm_map(docs, PlainTextDocument) ## clean documents
  
  docs <- tm_map(docs, content_transformer(stripURL2))
  docs <- tm_map(docs, content_transformer(stripsplits))
  docs <- tm_map(docs, content_transformer(cleantweet))
  docs <- tm_map(docs, content_transformer(latinize))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removePunctuation)
# docs <- tm_map(docs, goosekill)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, content_transformer(function(x) tolower(x)))
  docs <- tm_map(docs, removeWords, stopwords_full)
  if(stem==T){docs <- tm_map(docs, content_transformer(function(x) stemDocument(x,language='german')))} 
  return(docs$content)
}
clean<-prep(working)
return(clean)
}




