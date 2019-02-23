
root<-'C:\\Users\\admin\\scrape'
fun<-paste0(root,'\\scripts\\functions')
script<-paste0(root,'\\scripts')
docu<-paste0(root,'\\documentation')
docufiles<-paste0(root,'\\documentation\\files')
examples<-paste0(root,'\\examples')
dat<-paste0(root,'\\data')


setwd(fun)
#source('unpack.R')


library(rtweet)
library(stringr)
library(ggplot2)
library(dplyr)
library(qdapRegex)
library(wordcloud)
library(utils)




toklink<-''


setwd(script)
source('0_Call.R')
input<-c('spdbt')
call_api(input,toklink=toklink)


setwd(script)
source('1_select.R')
## here we should specify
working<-import_tw(dat) #inp=input if your want a specific account, not all 


setwd(fun)
source('tweetvariation.R')
working<-tweetvariation(working)


daytime<-ggplot(working, aes(x=as.POSIXct(hour, format="%H:%M:%S"),fill=source))
daytime +scale_x_datetime(labels = function(x) format(x, format = "%H:%M"))+
  labs(x="Tageszeit", y="tweets", fill="Gerät")+
  geom_histogram(binwidth = 3600)


setwd(script)

source('2_Preprocessing.R')

working$clean<-cleaner(working)


setwd(fun)
source('wfm.R')

d<-wfm(working)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=F, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


timetrend<-ggplot(working, aes(x=day, col=type))
timetrend+geom_freqpoly(binwidth=2604800, size=1)+
  labs(x="Monat", y="tweets", col="Art")

working$clean<-cleaner(working,stem=T)

setwd(script)
source('3_Dictionary.R')
dict<-get_senti()
print(dict[sample(nrow(dict),10),])


res_senti<-apply_senti(working,'sentiws',stemmed=T)

rau_senti<-apply_senti(working,'rauh',stemmed=T)


rauhscore<-rau_senti$score
rauhpol<-rau_senti$pol
res<-cbind(res_senti,rauhscore,rauhpol)

lastweek<- ggplot(res[res$day>"2019-02-15",], aes(x=day, y=score, col=type))
lastweek+geom_jitter()+
  labs(x="Tag", y="Ton", col="Art")

reaction<- ggplot(res[res$retweet_count>0,], aes(x=score, y=retweet_count, col=type))
reaction+geom_point(alpha=0.4)+
  labs(x="Ton", y="Retweets", col="Art")


library(MASS)
m<-glm.nb(retweet_count~score+hashtag_count+URL_dummy, data=res[res$is_retweet==F,])

setwd(fun)
source('predictdata.R')
pred_score<-predict.by.score(model=m, data=res[res$is_retweet==F,])

t.effect<- ggplot(pred_score, aes(x=score, y=pred))
t.effect+geom_line(size=1, col="red")+
  labs(x="Ton", y="Retweets")


pred_prof<-predict.by.prof(model=m, data=res[res$is_retweet==F,])

prof.effect<- ggplot(pred_prof, aes(x=hashtag_count, y=pred, col=URL_dummy))
prof.effect+geom_line(size=1)+
  labs(x="Hashtags", y="Retweets", col="URL")

setwd(examples)
tweet.data<- read.csv("tweet_data.csv")

library(lme4)

Plot.model<- glmer.nb(reach~neg+hashtag_count+URL_dummy+
                        (1|screen)+(1|party),
                      data=tweet.data, nAGQ=0)

setwd(fun)
source('predictdata.R')
candidate.prediction<- predict.full(Plot.model, tweet.data, 
                                    c("gerdmannesafd", "markus_soeder", "atesguerpinar"),
                                    c("AfD", "Union", "DIE LINKE"))
pred.curve<- ggplot(candidate.prediction, aes(x=neg, y=pred, col=screen))
pred.curve+geom_line(size=1)+
  labs(x="Negativity", y="Reach", col="Screen Name")


setwd(examples)
individual.data<- read.csv("secondary_data.csv")
individual.data<- individual.data[complete.cases(individual.data),]
my.colors<- c("cornflowerblue","deeppink","gold", "orange","green","red", "black")
aud.str<-ggplot(individual.data, aes(x=median_pos, y=var_pos, col=party, size=followers_count))
aud.str+geom_point(alpha=0.5)+
  labs(x="Median Follower", y="Variance", col="Partei", size="Anzahl Follower")+  scale_color_manual(values = my.colors) 


neg.model<- lm(neg.x~var_pos+median_pos+log(followers_count)+party-1, data=individual.data)

setwd(fun)
source('predictdata.R')
my.colors<- c("cornflowerblue","deeppink","gold", "orange","green","red", "black")

effect.var<- predict.secondary(neg.model, individual.data, by.var = T)
var.effect<- ggplot(effect.var, aes(x=var_pos, y=pred, col=party))
var.effect+geom_line()+
  labs(x="Varianz", y="Negativity Incentive", col="Partei")+  scale_color_manual(values = my.colors) 

effect.med<- predict.secondary(neg.model, individual.data, by.var = F)
my.colors<- c("cornflowerblue","deeppink","gold", "orange","green","red", "black")
med.effect<- ggplot(effect.med, aes(x=median_pos, y=pred, col=party))
med.effect+geom_line()+
  labs(x="Median", y="Negativity Incentive", col="Partei")+  scale_color_manual(values = my.colors) 
