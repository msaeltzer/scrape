
### comparing stemmed results to unstemmed ones

res_senti_n<-apply_senti(working,'sentiws',stemmed=T)

rau_senti_n<-apply_senti(working,'rauh',stemmed=T)

res_senti<-apply_senti(working,'sentiws',stemmed=T)

rau_senti<-apply_senti(working,'rauh',stemmed=T)

# increases the number on identified words by 10 percent. we will have to seewhat works 
# better

sum(rau_senti$pol)
sum(res_senti$pol)

stemmed<-res_senti$clean
unstemmed<-rau_senti_n$clean
stemres<-res_senti$pol
unsres<-res_senti_n$pol
stemrau<-rau_senti$pol
unsrau<-rau_senti_n$pol

val<-cbind.data.frame(res_senti$text,stemmed,unstemmed,stemres,unsres,stemrau,unsrau)


cor(rau_senti$score,res_senti$score)



setwd('C:\\Users\\admin\\Documents\\Sentiment Analysis')


'''
git

sql

scraping

markdown 

'''


## Machine Learning Sentiment Analysis Fede

  ## theory



  ## machine learning
  
  ## text preprocessing? Stemming, compound words

  ## compare different dictionaries

      ### which are there for german?



## Class: R markdown

## Workshop: R markdown + sentiment analysis







