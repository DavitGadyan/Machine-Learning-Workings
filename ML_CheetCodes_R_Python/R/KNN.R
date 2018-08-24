### K- Nearest Neighbours
library(dplyr)
library(class)
library(caret)

signs<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\signs.csv',stringsAsFactors = F)

str(signs)

table(signs$sign_type)

signs_train<-signs%>%
  filter(sample=='train')

signs_test<-signs%>%
  filter(sample=='test')

signs_train$sample<-NULL
signs_test$sample<-NULL

sign_1<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\sign_1.csv',stringsAsFactors = F)

knn(train = signs_train[,-1],test = sign_1,cl = signs_train$sign_type,k=5 )

knn1<-knn(train = signs_train[,-1],test = signs_test[,-1],cl = signs_train$sign_type,k=5 )
knn1[1:20]

table(knn1,signs_test$sign_type)

mean(knn1==signs_test$sign_type)

knn_p<-knn(train = signs_train[,-1],test = signs_test[,-1],cl = signs_train$sign_type,k=5,prob = T )

attr(knn_p,'prob')

df<-data.frame(class=knn1,probs=attr(knn_p,'prob'))

df


#### Doing with caret KNN

diabetes<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\final\\Diabetes.csv',stringsAsFactors=F)

str(diabetes)

set.seed(42)
trc<-trainControl(method='cv',number=10)

knn_c<-train(Class~.,data=diabetes,method='knn',trControl=trc,preProcess=c('center','scale'),tuneLength=10)

knn_c$results

plot(knn_c)

grid<-expand.grid(k=15:30)

knn_c2<-train(Class~.,data=diabetes,method='knn',trControl=trc,preProcess=c('center','scale'),tuneGrid=grid)
plot(knn_c2)
