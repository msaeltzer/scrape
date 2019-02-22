senti$neg<-paste0('(nicht|nichts|kein|keine|keinen)',senti$word)

remove_neg<-function(senti,tweet){
  for (i in 1:nrow(senti)){
    tweet <- gsub(trimws(senti$neg[i]), negation$replacement[i], text[i], fixed = FALSE)
  }
  tweet<-