### calling the twitter api

call_api<-function(searchlist='input.csv',number=3200){
library(rtweet)
library(fst)
d<-gsub('-','_',as.character(Sys.Date()))
tok<-read.csv('https://www.dropbox.com/s/msxbxx0rc3898yq/tokens.csv?dl=1')
## choose the number of tweets
startp<-1
tokenize_key<-function(i,tok){
create_token(app=tok[i,2],tok[i,3],tok[i,4],tok[i,5],tok[i,6],set_renv=F) 
}

input<-read.csv(searchlist,header=F,stringsAsFactors = F)
i<-1
for(i in startp:nrow(input)){
  ID<-input$follower[i]
  ID<-trimws(ID)
  error <- tryCatch(tweets<-get_timeline(user=ID,n=number, verbose=T,retryonratelimit=T), error=function(e) e)
  if (inherits(error, 'error')) {
    cat("Error! On to the next one...")
    next} else{ warning <- tryCatch(tweets<-get_timeline(ID,n=tc,retryonratelimit = T)
          , warning=function(w) w)
    if (inherits(warning, 'warning')){
    i<-ifelse(i==nrow(tok),1,i+1)  
    tokenize_key(i,tok)
    tweets<-get_timeline(ID,n=tc,retryonratelimit = T)}
    setwd(dat)
    pname<-paste0(ID,'_',d,'tw.csv')
    write.csv(tweets,pname)
    }
}
rm(tok)
}