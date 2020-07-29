text<- read.csv("~/Downloads/datasets_2050_3494_SPAM text message 20170820 - Data.csv")



text.length<-nchar(text$Message)                            #to check the amount of characters in each text
text$Category<-as.factor(text$Category)


#text analysis

library(tm)
library(wordcloud)
library(ggplot2)

ggplot(text,aes(text.length,fill=Category))+geom_histogram(binwidth = 5)

spam<-subset(text,Category=="spam")
ham<-subset(text,Category=="ham")
#spam analysis
docs<-Corpus(VectorSource(spam$Message))
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeWords,stopwords("english"))
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,removeNumbers)

dtm<-DocumentTermMatrix(docs)

freqr<-colSums(as.matrix(dtm))

#most frequent words in spam message
wordcloud(names(freqr),freqr,min.freq = 25,colors = rainbow(20))


#ham analysis
docs1<-Corpus(VectorSource(ham$Message))
docs1<-tm_map(docs1,removePunctuation)
docs1<-tm_map(docs1,content_transformer(tolower))
docs1<-tm_map(docs1,removeWords,stopwords("english"))
docs1<-tm_map(docs1,stripWhitespace)
docs1<-tm_map(docs1,removeNumbers)

dtm1<-DocumentTermMatrix(docs1)

freqr1<-colSums(as.matrix(dtm1))

#most frequent words in spam message
wordcloud(names(freqr1),freqr1,min.freq = 50,colors = rainbow(20))


#splitting

library(caret)

intrain<-createDataPartition(y=text$Category,p=0.7,list = FALSE)
train<-text[intrain,]
test<-text[-intrain,]

library(quanteda)

train.token<-tokens(train$Message,what = "word",remove_numbers = TRUE,
                    remove_punct = TRUE,remove_symbols = TRUE,split_hyphens = TRUE)

train.token<-tokens_tolower(train.token)

train.token<-tokens_select(train.token,stopwords(),selection = "remove")


train.token<-tokens_wordstem(train.token,language = "english")


train.token.dfm<-dfm(train.token,tolower = FALSE)

train.token.matrix<-as.matrix(train.token.dfm)

View(train.token.matrix[1:25,1:100])


train.token.df<-cbind(label=train$Category,as.data.frame(train.token.dfm))


names(train.token.df)<-make.names(names(train.token.df))

train.token.df<-train.token.df[,-1475]


model<-train(label~.,data=train.token.df,method="rpart",tunelength=7)

pred<-predict(model,newdata=test)

confusionMatrix(pred,test$Category)





