library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(Metrics)
library(rattle)
library(ROCR)
library(randomForest)
library(dummies)

credit<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\EDX_Scripts\\Principles-of-Machine-Learning-R-master\\Module4\\German_Credit_Preped.csv',stringsAsFactors = F)

str(credit)


credit$Customer_ID<-NULL

str(credit)

table(credit$bad_credit)

credit$bad_credit<-ifelse(credit$bad_credit==1,'Yes','No')

str(credit)

### Partitioning dataset
set.seed(42)
ind<-createDataPartition(credit$bad_credit,p = 0.7,list = F)

credit_train<-credit[ind,]
credit_test<-credit[-ind,]


str(credit_train)
### Constructing decision tree

mod1<-rpart(bad_credit~.,data=credit_train)

prp(mod1,type = 2,extra=2)

mod2<-rpart(bad_credit~.,data=credit_train,control = rpart.control(maxdepth = 4))

prp(mod2,type = 2,extra=1)

fancyRpartPlot(mod2,type = 4)

pred_class<-predict(mod2,credit_test,type = 'class')

accuracy(credit_test$bad_credit,pred_class)

asRules(mod2)

confusionMatrix(factor(pred_class),factor(credit_test$bad_credit),positive='Yes')

confusionMatrix(table(predict(mod2,credit_test, type="prob")[,"Yes"] >= 0.20,
                      credit_test$bad_credit == "Yes"))

### Let's also construct ROC curve and calsulate AUC

pred_prob<-predict(mod2,credit_test,type='prob')

p_credit<-prediction(pred_prob[,2],credit_test$bad_credit)

perf<-performance(p_credit,'tpr','fpr')

plot(perf,colorize=T)  ### Real bad result as line is almost goes through the center

### Now let's build random forest on other dataset
credit<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\EDX_Scripts\\Principles-of-Machine-Learning-R-master\\Module4\\German_Credit_Preped.csv',stringsAsFactors = F)
credit$Customer_ID<-NULL
credit$bad_credit<-ifelse(credit$bad_credit==1,'Yes','No')
set.seed(42)
ind<-createDataPartition(credit$bad_credit,p = 0.7,list = F)

credit_train<-credit[ind,]
credit_test<-credit[-ind,]
str(credit_train)

ch_into_factors<-function(df){
  ch <- sapply(df, is.character)
  df[ch]<-data.frame(apply(df[ch],2,as.factor))
  return(df)
}
credit_train<-ch_into_factors(credit_train)
str(credit_train)
credit_test<-ch_into_factors(credit_test)
str(credit_test)

mod3<-randomForest(bad_credit~., data=credit_train,ntree=25,do.trace=T)

pr<-predict(mod3,credit_test,type='prob')
pr[1:25,]

p_test<-prediction(pr[,2],credit_test$bad_credit)
perf<-performance(p_test,'tpr','fpr')
plot(perf,colorize=T)  ### Obviously this is better than perfious model
performance(p_test,'auc')@y.values  

### Doing with caret
library(caret)
set.seed(42)
trc<-trainControl(method='cv',number=10)
mtry_grid<-expand.grid(mtry=c(4,7,9,10))

set.seed(42)
mod4<-train(bad_credit~.,data=credit_train,trControl=trc,
            method='rf',ntree=25,
            tuneGrid=mtry_grid)
mod4$results

mod4$bestTune

pr4<-predict(mod4,credit_test,type='prob')
pr4[1:25,]

p_test4<-prediction(pr4[,2],credit_test$bad_credit)
perf<-performance(p_test4,'tpr','fpr')
plot(perf,colorize=T)  ### Obviously this is better than perfious model
performance(p_test4,'auc')@y.values  

set.seed(42)

trc<-trainControl(method = 'cv',
                  classProbs = TRUE,
                  summaryFunction = twoClassSummary,
                  number=5)

set.seed(42)
mm<-train(bad_credit~.,data=credit_train,
          trControl=trc,
          method='rf',ntree=25,
          metric='ROC',tuneGrid=mtry_grid)

plot(mm)

varImpPlot(mm$finalModel)


### Let's use some imputation techniques to balance the data and results
### Doing with caret


