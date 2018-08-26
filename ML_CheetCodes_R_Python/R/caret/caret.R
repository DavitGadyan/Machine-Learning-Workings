### simple glm
library(caret)


credit<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\EDX_Scripts\\Principles-of-Machine-Learning-R-master\\Module4\\German_Credit_Preped.csv',stringsAsFactors = F)

str(credit)

credit$Customer_ID<-NULL
credit$bad_credit<-as.factor(ifelse(credit$bad_credit==1,'Yes','No'))
## Splitting into training and testing sets

ind<-createDataPartition(credit$bad_credit,p=0.7,list = F) 

credit_train<-credit[ind,]
credit_test<-credit[-ind,]



mod1<-glm(bad_credit~., data = credit_train,family='binomial')

summary(mod1)


b_c_p1<-predict(mod1,credit_test,type='response')

b_c_p1_labels<-factor(ifelse(b_c_p1>0.3,'Yes','No'))

confusionMatrix(data =b_c_p1_labels,reference=credit_test$bad_credit,positive='Yes') # Important always note data and reference parameters namely.. position assignment fail often!!!

table(b_c_p1_labels,credit_test$bad_credit)

pred_prob<-predict(mod1,credit_test,type = 'response')
head(pred_prob)

p_test<-prediction(pred_prob,credit_test$bad_credit)
perf<-performance(p_test,'tpr','fpr')
plot(perf,colorize=T)

performance(p_test,'auc')@y.values

## Now let's use caret package

mod2<-train(bad_credit~.,data=credit_train,method='glmnet',
            tuneGrid=expand.grid(alpha=c(0,1),lambda=seq(0.0001,0.03,length=100)),
            trControl=trainControl(method = 'cv',number = 10,
                                   summaryFunction = twoClassSummary,
                                   classProbs = TRUE, ## Important!!!!
                                   #preProcess = 'medianImpute' ## if NAs in data
                                   verboseIter = TRUE))
plot(mod2)

max(mod2$results$Sens)

plot(mod2$finalModel)

mod2_pred<-predict(mod2,credit_test)

confusionMatrix(data =mod2_pred,reference=credit_test$bad_credit,positive='Yes') # Important always note data and reference parameters namely.. position assignment fail often!!!

table(mod2_pred,credit_test$bad_credit)

pred_prob<-predict(mod2,credit_test,type = 'prob')
head(pred_prob)

p_test<-prediction(pred_prob[,2],credit_test$bad_credit)
perf<-performance(p_test,'tpr','fpr')
plot(perf,colorize=T)

performance(p_test,'auc')@y.values

####### caret with multiple model, preProcessing etc.
library(dplyr)


data(mtcars)
set.seed(42)
mtcars[sample(1:nrow(mtcars), 10), "hp"] <- NA
Y <- mtcars$mpg
X <- mtcars%>%
  select(-mpg)

str(mtcars)
set.seed(42)
mod3<-train(x=X,y=Y,method='lm',trControl=trainControl(method='cv',number = 10),preProcess = c("medianImpute",'center','scale'))

summary(mod3)
min(mod3$results$RMSE)

X$bad <- 1 ## Adding constant

X

set.seed(42)
mod4<-train(x=X,y=Y,method='lm',trControl=trainControl(method='cv',number = 10),preProcess = c('zv','medianImpute','center','scale'))

summary(mod4)
min(mod4$results$RMSE)

## Adding pca principal component analysis
set.seed(42)
mod5<-train(x=X,y=Y,method='lm',trControl=trainControl(method='cv',number = 10),preProcess = c('zv','medianImpute','center','scale','pca'))

summary(mod5) ## Interpretation needed to understand PC's
min(mod5$results$RMSE)

## Testing several model with caret

library(caret)
library(C50)
data(churn)
table(churnTrain$churn) / nrow(churnTrain)

# Create train/test indexes
set.seed(42)
myFolds <- createFolds(churnTrain$churn, k = 5) ## In order to have all models trained on the same folds of data

i <- myFolds$Fold1
table(churnTrain$churn[i]) / length(i)

myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

set.seed(42)
model_glmnet <- train(
  churn ~ ., churnTrain,
  metric = "ROC",
  method = "glmnet",
  tuneGrid = expand.grid(
    alpha = 0:1,
    lambda = 0:10/10
  ),
  trControl = myControl
)
# Plot the results
plot(model_glmnet)

plot(model_glmnet$finalModel)

set.seed(42)
churnTrain$churn <- factor(churnTrain$churn, levels = c("no", "yes"))
model_rf <- train(
  churn ~ ., churnTrain,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)

plot(model_rf)

# Make a list
model_list <- list(
  glmnet = model_glmnet,
  rf = model_rf
)
# Collect resamples from the CV folds
resamps <- resamples(model_list)
resamps

summary(resamps)

bwplot(resamps, metric = "ROC")

dotplot(resamps, metric = "ROC")

densityplot(resamps, metric = "ROC")

xyplot(resamps, metric = "ROC")
