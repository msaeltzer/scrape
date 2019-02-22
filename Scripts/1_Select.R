#####
# Selecting the data which is supposed to be analyzed
#
####

import_tw<-function(dir,type='.csv',inp=NULL){
  setwd(dir)
  l<-list.files(getwd())
  l<-l[grepl(type,l)]
  if(is.null(inp)==F){l<-l[grepl(inp,l)]}
  base<-list()
  for(i in 1:length(l)){
    base[[i]]<-read.csv(l[i],stringsAsFactors = F,header = T)
  }
  
  full<-c()
  for(i in 1:length(base)){
    b<-base[[i]]
    nom<-intersect(names(base[[1]]),names(b))
    c<-b[,nom]
    full<-full[,nom]
    full<-rbind(full,c)
  }
  w<-full
  return(w)
}

