library(caret)
library(e1071)
library(ROCR)

hr_data<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\final\\HR_balanced.csv')

str(hr_data)

table(hr_data$left) ## Cool almost balanced dataset

set.seed(42)
ind<-createDataPartition(hr_data$left,p = 0.7,list=F)

hr_train<-hr_data[ind,]
hr_test<-hr_data[-ind,]

mod1<-naiveBayes(left~.,data = hr_train,laplace = 1)

names(mod1)

mod1$apriori

mod1$tables

mod1$levels

mod1$call

mod1$tables$satisfaction_level

mean(hr_train$satisfaction_level[hr_train$left=='No'])
sd(hr_train$satisfaction_level[hr_train$left=='No'])

pred_test<-predict(mod1,newdata = hr_test)

confusionMatrix(pred_test,hr_test$left,positive = 'Yes')

pred_test_prob<-predict(mod1,newdata = hr_test,type = 'raw')

pred_test_prob

p_test<-prediction(pred_test_prob[,2],hr_test$left)

perf<-performance(p_test,'tpr','fpr')

plot(perf,colorize=T)

performance(p_test,'auc')@y.values
