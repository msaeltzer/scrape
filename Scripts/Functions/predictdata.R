#Funktionen enthalten keine Überprüfung für die Daten die eingehen, also aufpassen, dass die auch passen!

predict.by.score<-function(model, data){
  p.dat<-data.frame(
    score = seq(from = min(data$score), to = max(data$score), length.out = 1000))
  p.dat$URL_dummy<-F
  p.dat$hashtag_count<-median(data$hashtag_count)
  p.dat$pred<-predict(model, p.dat, type="response")
  return(p.dat)
}

predict.by.prof<-function(model, data){
  p.dat<-data.frame(
    hashtag_count = rep(seq(from = min(data$hashtag_count), to = max(data$hashtag_count), length.out = 1000),2)) 
  p.dat$URL_dummy[1:1000]<-F
  p.dat$URL_dummy[1001:2000]<-T
  p.dat$score<-median(data$score)
  p.dat$pred<-predict(model, p.dat, type="response")
  return(p.dat)
}

predict.full<-function(model, data, screen_names, party){
  p.dat<-data.frame(
    neg = rep(seq(from = min(data$neg), to = max(data$neg), length.out = 1000),length(screen_names))) 
  p.dat$URL_dummy<-F
  p.dat$hashtag_count<-median(data$hashtag_count)
  for(i in 1:length(screen_names)){
    k<-(i-1)*1000+1
    p.dat$party[k:(k+999)]<-party[i]
    p.dat$screen[k:(k+999)]<-screen_names[i]
    
  }
  p.dat$pred<-predict(model, p.dat, type="response")
  
  return(p.dat)
}

predict.secondary<- function(model, data, by.var){
  if(by.var==T){
     p.dat<-data.frame(
  var_pos = rep(seq(from = min(data$var_pos), to = max(data$var_pos), length.out = 1000),7))
  p.dat$followers_count<-median(data$followers_count)
  party<-c("AfD", "DIE LINKE", "FDP", "FREIE WÄHLER", "GRUENE", "SPD", "Union")
  for(i in 1:7){
    k<-(i-1)*1000+1
    p.dat$party[k:(k+999)]<-party[i]
    p.dat$median_pos[k:(k+999)]<-median(data$median_pos[data$party==party[i]])
  }
  }
 
  if(by.var==F){
    p.dat<-data.frame(
      median_pos = rep(seq(from = min(data$median_pos), to = max(data$median_pos), length.out = 1000),7))
    p.dat$followers_count<-median(data$followers_count)
    party<-c("AfD", "DIE LINKE", "FDP", "FREIE WÄHLER", "GRUENE", "SPD", "Union")
    for(i in 1:7){
      k<-(i-1)*1000+1
      p.dat$party[k:(k+999)]<-party[i]
      p.dat$var_pos[k:(k+999)]<-median(data$var_pos[data$party==party[i]])
    }
  }
  p.dat$pred<-predict(model, p.dat, type="response")
  
  return(p.dat)  
  
}