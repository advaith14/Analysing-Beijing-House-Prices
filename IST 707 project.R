setwd("C:/Users/advai/Documents")
x<- read.csv('new.csv',stringsAsFactors =TRUE,fileEncoding="latin1") 

#Removing URL
df <- x[,-1]

#Replacing the missing (NA) days on market values with the median
df$DOM<- ifelse(is.na(df$DOM),median(df$DOM,na.rm=T),df$DOM)
library(dplyr)
#Removing id and cid
df2 <- data.frame(df %>% dplyr::select(-id, -Cid))

#Numeric to Nominal
df2$livingRoom <- as.numeric(df2$livingRoom)
df2$bathRoom <- as.numeric(df2$bathRoom)
df2$drawingRoom <- as.numeric(df2$drawingRoom)
df2$district <- as.factor(df2$district)

#Obtaining Building Types
makeBuildingType <- function(x){
  if(!is.na(x)){
    if(x==1){
      return('Tower')
    }
    else if (x==2){
      return('Bungalow')
    }
    else if (x==3){
      return('Mix plate tower')
    }
    else if (x==4){
      return('plate')
    }
    else return('wrong_coded')
  }
  else{return('missing')}
}
df2$buildingType <- sapply(df2$buildingType, makeBuildingType)
df2 <- data.frame(df2 %>% filter(buildingType != 'wrong_coded' & buildingType !='missing'))

#obtaining renovation condition
makeRenovationCondition <- function(x){
if(x==1){
  return('Other')
}
else if (x==2){
  return('Rough')
}
else if (x==3){
  return('Simplicity')
}
else if (x==4){
  return('Hardcover')
}
}
df2$renovationCondition <- sapply(df2$renovationCondition, makeRenovationCondition)

#Obtaining building structure
makeBuildingStructure <- function(x){
  if(x==1){
    return('Unknown')
  }
  else if (x==2){
    return('Mix')
  }
  else if (x==3){
    return('Brick_Wood')
  }
  else if (x==4){
    return('Brick_Concrete')
  }
  else if (x==5){
    return('Steel')
  }
  else if (x==6){
    return('Steel_Concrete')
  }
}
df2$buildingStructure <- sapply(df2$buildingStructure, makeBuildingStructure)
df2$elevator <- ifelse(df2$elevator==1,'has_elevator','no_elevator')
df2$constructionTime <-as.numeric(df2$constructionTime)

df2$district <-as.factor(df2$district)
df2$subway <- ifelse(df2$subway==1,'has_subway','no_subway')

df2$fiveYearsProperty <- ifelse(df2$fiveYearsProperty==1,'owner_less_5y','owner_more_5y')

df3 <- data.frame(df2 %>% na.omit())
df3$buildingType <- as.factor(df3$buildingType)
df3$buildingStructure <- as.factor(df3$buildingStructure)
df3$elevator <- as.factor(df3$elevator)
df3$fiveYearsProperty <- as.factor(df3$fiveYearsProperty)
df3$subway <- as.factor(df3$subway)
df3$district <- as.factor(df3$district)
df3$renovationCondition <- as.factor(df3$renovationCondition)

str(df3)
any(is.na(df3))

# Correlation Plot
install.packages("corrplot")
library(corrplot)
corrplot(cor(
  df3 %>% select_if(is.numeric) %>% select(-Lng, -Lat) ,use = "pairwise.complete.obs",
  method='pearson')
  ,method='ellipse',
  tl.cex=1,
  col = viridis::viridis(50),
  tl.col='black')

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
makeFeatureCatEDA <- function(x, numFeatures){
  if(numFeatures < 13){
    
    mypalette <-'Paired'
    mycols <- 2
    
    mybox <- df3 %>% ggplot(aes_string(x,'price')) + geom_boxplot(aes_string(color=x)) + 
      scale_color_brewer(name='', palette=mypalette) + theme_minimal(12) + 
      theme(axis.title =element_blank(), legend.position='None') + 
      labs(title='average price of homes') + coord_flip()
  }
  else{
    
    mypalette <- colorRampPalette(brewer.pal(12,'Paired'))(numFeatures)
    mycols <- 3
    
    mybox <- df3 %>% ggplot(aes_string(x,'price')) + geom_boxplot(aes_string(color=x)) + 
      scale_color_manual(name='',values=mypalette) + theme_minimal(12) + 
      theme(axis.title =element_blank(), legend.position='None') + 
      labs(title='average price of homes') + coord_flip()
  }
  
  grid.arrange(mybox)
}

makeFeatureCatEDA('buildingStructure', length(unique(df3$buildingStructure)))
makeFeatureCatEDA('buildingType', length(unique(df3$buildingType)))
makeFeatureCatEDA('renovationCondition', length(unique(df3$renovationCondition)))
makeFeatureCatEDA('elevator', length(unique(df3$elevator)))
makeFeatureCatEDA('subway', length(unique(df3$subway)))
makeFeatureCatEDA('fiveYearsProperty', length(unique(df3$fiveYearsProperty)))
makeFeatureCatEDA('district', length(unique(df3$district)))


#Association Rule Mining
str(df3)
df3 <- data.frame(df2 %>% dplyr::select(-floor))
a<- df3$price
quantile(a)

dum<-replicate(length(df3$price), "Medium")
dum[df3$price < 28050]<-"Low"
dum[df3$price > 53819]<-"High"
df3$price<- dum

dum<-replicate(length(df3$DOM), "Medium")
dum[df3$DOM < 420]<-"Low"
dum[df3$DOM > 1250]<-"High"
df3$DOM<- dum

a<- df3$livingRoom
quantile(a)

dum<-replicate(length(df3$livingRoom), "Less than 4")
dum[df3$livingRoom > 4]<-"High"
df3$livingRoom<- dum

a<- df3$followers
quantile(a)

dum<-replicate(length(df3$followers), "Medium")
dum[df3$followers < 285]<-"Low"
dum[df3$followers > 857]<-"High"
df3$followers<- dum

a<- df3$square
max(a)

dum<-replicate(length(df3$square), "Medium")
dum[df3$square < 230]<-"Low"
dum[df3$square > 691]<-"High"
df3$square<- dum

a<- df3$drawingRoom
max(a)

dum<-replicate(length(df3$drawingRoom), "Medium")
dum[df3$drawingRoom < 4]<-"Low"
dum[df3$drawingRoom > 12]<-"High"
df3$drawingRoom<- dum

a<- df3$kitchen
max(a)

dum<-replicate(length(df3$kitchen), "Medium")
dum[df3$kitchen < 1]<-"Low"
dum[df3$kitchen > 2]<-"High"
df3$kitchen<- dum

a<- df3$bathRoom
max(a)

dum<-replicate(length(df3$bathRoom), "Medium")
dum[df3$bathRoom < 4]<-"Low"
dum[df3$bathRoom > 12]<-"High"
df3$bathRoom<- dum

a<- df3$constructionTime
max(a)

dum<-replicate(length(df3$constructionTime), "Medium")
dum[df3$constructionTime < 18]<-"Low"
dum[df3$constructionTime > 55]<-"High"
df3$constructionTime<- dum

a<- df3$ladderRatio
max(a)

dum<-replicate(length(df3$ladderRatio), "Medium")
dum[df3$ladderRatio < 0.25]<-"Low"
dum[df3$ladderRatio > 0.75]<-"High"
df3$ladderRatio<- dum

library(arules)

arm <- data.frame(df3$DOM, df3$followers, df3$price, df3$square, df3$livingRoom, df3$drawingRoom, df3$kitchen, df3$bathRoom, df3$buildingType, df3$constructionTime, df3$renovationCondition, df3$buildingStructure, df3$ladderRatio, df3$elevator, df3$fiveYearsProperty,df3$subway, df3$district)
armx<- as(arm,"transactions")

rules<-apriori(armx,parameter = list(support=0.1,confidence=0.2,minlen=1),appearance = list(rhs=c("df3.price=High")))
inspect(rules) 
goodrules<-rules[quality(rules)$lift>1.385]
inspect(goodrules)

library(arulesViz) 
plot(goodrules,method="graph",engine="htmlwidget")

df3$bathRoom = df2$bathRoom
lb <- subset(df3, df3$bathRoom <=5)

dum<-replicate(length(lb$bathRoom), "Medium")
dum[lb$bathRoom < 2]<-"Low"
dum[lb$bathRoom > 2]<-"High"
lb$bathRoom<- dum

arm1 <- data.frame(lb$DOM, lb$followers, lb$price, lb$square, lb$livingRoom, lb$drawingRoom, lb$kitchen, lb$bathRoom, lb$buildingType, lb$constructionTime, lb$renovationCondition, lb$buildingStructure, lb$ladderRatio, lb$elevator, lb$fiveYearsProperty,lb$subway, lb$district)
armx1<- as(arm1,"transactions")

rules1<-apriori(armx1,parameter = list(support=0.2,confidence=0.2,minlen=1),appearance = list(rhs=c("lb.price=High")))
inspect(rules1) 
goodrules1<-rules1[quality(rules1)$lift>1.06]
inspect(goodrules1)

plot(goodrules1,method="graph",engine="htmlwidget")

#install.packages("e1071")
#library(e1071)

trainx <- sample(seq_len(nrow(df3)),size = floor(0.66*nrow(df3)))
train <- df3[trainx, ]
test <- df3[-trainx, ]
train[] <- lapply(train, factor) 
test[] <- lapply(test, factor)

#nb_default <- naiveBayes(price ~., laplace = 1, data = train, na.action = na.pass)
#nb_predict <- predict(nb_default, test[,-(7)], type = c("class"))
#matrix <- table(nb_predict, test$price)
#matrix
#accuracy <- sum(diag(matrix)) / sum(matrix)
#accuracy

install.packages("RWeka")
library("RWeka")
Naive <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
bayesmodel=Naive(price~., data=train)

predbayes = predict (bayesmodel, newdata = test, type = c("class"))
predbayes

#Perform 10 fold cross validation
evalbayes <- evaluate_Weka_classifier(bayesmodel, numFolds = 10, seed = 1, class = TRUE) 
evalbayes

#Perform 3 fold cross validation
#evalbayes3 <-evaluate_Weka_classifier(bayesmodel,numFolds = 3,seed = 1, class = TRUE)
#evalbayes3

#morebaths <- subset(df3, df3$bathRoom > 4)