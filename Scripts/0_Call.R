### calling the twitter api

call_api<-function(input='input.csv',number=3200,toklink){
library(rtweet)
tok<-read.csv(toklink,stringsAsFactors = F)
  
d<-gsub('-','_',as.character(Sys.Date()))
## choose the number of tweets
startp<-1
tokenize_key<-function(i,tok){
create_token(app=tok[i,2],tok[i,3],tok[i,4],tok[i,5],tok[i,6],set_renv=F) 
}
if(is.vector(input)==F){input2<-read.csv(input,header=F,stringsAsFactors = F)
input<-input$screenname
}
i<-1
for(i in startp:length(input)){
  ID<-input[i]
  ID<-trimws(ID)
  error <- tryCatch(tweets<-get_timeline(user=ID,n=number, verbose=T,retryonratelimit=T), error=function(e) e)
  if (inherits(error, 'error')) {
    cat("Error! On to the next one...")
    next} else{ warning <- tryCatch(tweets<-get_timeline(ID,n=number,retryonratelimit = T)
          , warning=function(w) w)
    if (inherits(warning, 'warning')){
    i<-ifelse(i==nrow(tok),1,i+1)  
    tokenize_key(i,tok)
    tweets<-get_timeline(ID,n=number,retryonratelimit = T)}
    tweets<-tweets[,!sapply(FUN='class',tweets)=='list']
    
    setwd(dat)
    pname<-paste0(ID,'_',d,'tw.csv')
    write.csv(tweets,pname)
    }
}
rm(tok)
}