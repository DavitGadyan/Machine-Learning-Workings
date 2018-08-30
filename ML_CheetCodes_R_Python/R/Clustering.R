## Hierarchical clustering

index<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\final\\index2017.csv')

str(index)
rownames(index)<-index$Abbr

index1<-index[1:7,c('Unemployment','GDP.per.Capita.PPP')]
# AS variables are completelly on different scales it would have been better to scale first 
# the variables then calculate distance

d<-dist(index1,method = 'euclidean')
d

cl<-hclust(d,method = 'complete')
cl

cl$merge
index1
# by the matrix from cl$merge we can see the steps in each distance calcualtion with cases with - signs

cl$merge
d

cl$height

cl$merge

d<-dist(index1,method='euclidean')
d

cl$height

plot(cl,hang=-1)

plot(cl,hang=-1)
rect.hclust(cl,3)

index1$cl_membership<-cutree(cl,k=3)
index1

index1[order(index1$cl_membership),]

scale(index1,center = TRUE,scale=TRUE)

index2<-as.data.frame(scale(index1,center = TRUE,scale=TRUE))

d2<-dist(index2,method = 'euclidean')
cl2<-hclust(d2,method = 'complete')
plot(cl2,hang=-1)

cl$cl_membership<-cutree(cl2,k=3)

index1

## K_means clustering
library(ggplot2)

colnames(index)
index2<-index[,c('Unemployment','GDP.per.Capita.PPP')]
index2<-index2[complete.cases(index2),]
index2_sc<-as.data.frame(scale(index2))
km1<-kmeans(index2_sc,centers = 3)

names(km1)
km1$cluster

km1$centers

# Total sum of Squares
km1$totss
sum(km1$withinss)+km1$betweenss
# Within SUm of Squares
km1$withinss
# Between group Sum of Squares
km1$betweenss

km1$betweenss/km1$totss

index2_sc$cl<-km1$cluster

ggplot(data=index2_sc,aes(x=Unemployment,y=GDP.per.Capita.PPP,col=factor(cl)))+geom_point(size=2)

ggplot(data=index2_sc,aes(x=Unemployment,y=GDP.per.Capita.PPP,col=factor(cl)))+geom_point(size=2)+
  geom_point(data=data.frame(km1$centers,cl=factor(1:3)),aes(Unemployment,GDP.per.Capita.PPP,col=cl),
                                                            pch=8,size=10)

# The goal is to increase between sum of squares/total sum of squares, and decrease within sum of squares/ total
# sum of squares


index<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\final\\index2017.csv')
index1<-index[complete.cases(index),]
rownames(index2)<-index2$Abbr
index2<-index1[,c(8:19)]
index2_sc<-as.data.frame(scale(index2))

# simple loop for calculating within sum/total sum
B_T<-c()
for(i in 1:10){
  set.seed(1)
  km1<-kmeans(index2_sc,i)
  B_T[i]<-km1$tot.withinss/km1$totss
}

plot(B_T)

# install.packages('factoextra')
library(factoextra)

set.seed(1)
fviz_nbclust(index2_sc,kmeans,method = 'wss')
