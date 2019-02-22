
setwd('C:\\Users\\admin\\Documents\\twitter landtage\\SentiWS_v1.8c')

neg<-readLines("neg.txt",encoding='UTF-8')
neg[2]
?strsplit
s<-strsplit(neg,'\t')

l1<-lapply(s,'[',1)
l2<-lapply(s,'[',2)
l3<-lapply(s,'[',3)

l1<-unlist(l1)
l2<-unlist(l2)


all<-c()
for(i in 1:length(l3)){
  s<-unlist(strsplit(l3[[i]],','))
  mna<-28-length(s)
  s<-c(s,rep(NA,mna))
  all<-rbind(all,s)  
}

full1<-cbind(l1,l2,all)
full1<-as.data.frame(full1)
negative_frame<-full1
write.csv(full1,'neg2.csv')

neg<-readLines("pos.txt",encoding='UTF-8')
neg[2]

s<-strsplit(neg,'\t')

l1<-lapply(s,'[',1)
l2<-lapply(s,'[',2)
l3<-lapply(s,'[',3)

l1<-unlist(l1)
l2<-unlist(l2)


all<-c()
for(i in 1:length(l3)){
  s<-unlist(strsplit(l3[[i]],','))
  mna<-33-length(s)
  s<-c(s,rep(NA,mna))
  all<-rbind(all,s)  
}


full<-cbind(l1,l2,all)
full<-as.data.frame(full)

positive_frame<-full
write.csv(full,'pos2.csv')

#
i<-11
pos<-full
pps<-c()
for(i in 1:nrow(pos)){
  ncol(pos)-sum(is.na(pos[i,]))
  v<-pos[i,][!is.na(pos[i,])]
  val<-v[2]
  v<-v[-2]
  v<-cbind(v,val)
  pps<-rbind(pps,v)
}

pps<-as.data.frame(pps)
names(pps)<-c('word','values')

npss<-gsub('(\\|\\w*\\b)','',pps$word)

pps$word<-npss

write.csv(pps,'fullistpos.csv')

neg<-full1
nps<-c()
for(i in 1:nrow(neg)){
  ncol(neg)-sum(is.na(neg[i,]))
  v<-neg[i,][!is.na(neg[i,])]
  val<-v[2]
  v<-v[-2]
  v<-cbind(v,val)
  nps<-rbind(nps,v)
}

## homogenität -> extremere
## mehr -> weniger extrem
## incentive amplification schlimmer




nps<-as.data.frame(nps)
names(nps)<-c('word','values')

npss<-gsub('(\\|\\w*\\b)','',nps$word)
nps$word<-npss
write.csv(nps,'fullistneg.csv')

senti<-rbind(nps,pps)

write.csv(senti,'senti_clean.csv')

