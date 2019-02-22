

apply_senti<-function(set,dict='sentiws',stemmed=F){

setwd(root)

if(dict=='rauh'){dictionary<-paste0(root,'\\dictionary\\rauh')
setwd(dictionary)
load('Rauh_SentDictionaryGerman.Rdata')
senti<-sent.dictionary
names(senti)[1]<-'word'
names(senti)[2]<-'values'
senti$values<-as.numeric(senti$values)
senti$word<-trimws(senti$word)

}

  
  
  
if(dict=='sentiws'){dictionary<-paste0(root,'\\dictionary\\sentiws')
setwd(dictionary)
senti<-read.csv('fullistneg.csv')
senti2<-read.csv('fullistpos.csv')
senti<-rbind(senti,senti2)
}

if(dict=='hj'){dictionary<-paste0(root,'\\dictionary\\haselmeyer')
setwd(dictionary)
senti<-read.table('german_senti_dic.txt',sep=',',header=T)
names(senti)[3]<-'values'
}


# source(negativity)  

if(stemmed==T){
  library(tm)
  x<-tm_map(
    Corpus(VectorSource(senti$word)),
    content_transformer(function(x) stemDocument(x,language='german')))
  senti$word<-x$content
  senti$dup<-duplicated(senti$word)
  senti<-senti[senti$dup==FALSE,]
  }

sentiscore<-function(tweet){
  tweets.position <- unlist(strsplit(tweet,' '))
  tweets.position<- tweets.position[tweets.position!='']
  tweetscore<-sum(senti$values[senti$word %in% tweets.position])
  tweetpol<-sum(abs(senti$values[senti$word %in% tweets.position]))
  p<-c(tweetscore,tweetpol)
  return(p)
}

text<-set$clean
scores<-sapply(text,FUN=sentiscore)

names(scores[1,])<-NULL
dim(scores)
score<-scores[1,]
pol<-scores[2,]
set<-cbind(set,score)
set<-cbind(set,pol)
rownames(set)<-NULL
return(set)
}

get_senti<-function(){
  dictionary<-paste0(root,'\\dictionary\\sentiws')
  setwd(dictionary)
  senti<-read.csv('fullistneg.csv')
  senti2<-read.csv('fullistpos.csv')
  senti<-rbind(senti,senti2)
  return(senti)
}