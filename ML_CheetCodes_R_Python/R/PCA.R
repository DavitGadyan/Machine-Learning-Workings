library(ggplot2)

# cereals<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\cereals.txt',sep = '\t')
cereals<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\cereal.csv')
cereals

str(cereals)

mean(cereals$calories)
mean(cereals$rating)
cor(cereals$calories,cereals$rating)

var(cereals$rating)
var(cereals$calories)

ggplot(cereals,aes(x=calories,y=rating))+geom_point()+
  geom_smooth(method="lm",se = F)

pcs<-prcomp(data.frame(cereals$calories,cereals$rating))
summary(pcs)

# weights that are used to project the original points onto the two new directions
pcs$rotation

scores<-pcs$x
head(scores)
r_1<-cereals[1,c('calories','rating')]
sc<-data.frame(scores)

(r_1$calories-mean(cereals$calories))*pcs$rotation[1,1]+(r_1$rating-mean(cereals$rating))*pcs$rotation[2,1]


options(scipen=666)
mean(sc$PC1)
mean(sc$PC2)

var(sc$PC1)
var(sc$PC2)

####
str(cereals)
pcs<-prcomp(na.omit(cereals[,-c(1:3)]))
summary(pcs)

w<-data.frame(pcs$rotation)
w
(cereals[1,]$calories-mean(cereals$calories))*w$PC1[1]+(cereals[1,]$protein-mean(cereals$protein))*w$PC1[2]+
  (cereals[1,]$fat-mean(cereals$fat))*w$PC1[3]+(cereals[1,]$sodium-mean(cereals$sodium))*w$PC1[4]+
  (cereals[1,]$fiber-mean(cereals$fiber))*w$PC1[5]+(cereals[1,]$carbo-mean(cereals$carbo))*w$PC1[6]+
  (cereals[1,]$sugars-mean(cereals$sugars))* w$PC1[7]+
  (cereals[1,]$potass-mean(cereals$potass))*w$PC1[8]+(cereals[1,]$vitamins-mean(cereals$vitamins))*w$PC1[9]+
  (cereals[1,]$shelf-mean(cereals$shelf))*w$PC1[10]+(cereals[1,]$weight-mean(cereals$weight))*w$PC1[11]+
  (cereals[1,]$cups-mean(cereals$cups))*w$PC1[12]+(cereals[1,]$rating-mean(cereals$rating))*w$PC1[13]

pcs$x

### Normalization before PCA

pcs.cor<-prcomp(na.omit(cereals[,-c(1:3)]),scale. = T)

summary(pcs.cor)

pcs.cor$rotation

### Cars dataset PCA

library(ggplot2)

load('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\final\\cars.rda')

str(cars)

cars_n<-cars[,c(8:18)]
cor(cars_n[,c('CityMPG','Retail')])

ggplot(data=cars,aes(x=Retail,y=CityMPG))+geom_point(col='blue')

ggplot(data=cars,aes(x=CityMPG,y=Retail))+geom_point(col='blue')+geom_smooth(method='lm',se=F,col='red',size=1.5)+
  ggtitle('First Component')

pc_1<-prcomp(cars_n[,c('CityMPG','Retail')])
names(pc_1)

head(pc_1$x)

summary(pc_1)

pc_2<-prcomp(cars_n[,c('CityMPG','Retail')],center = T,scale=T)
summary(pc_2)

names(pc_2)

pc_2$sdev^2 ### equal to varinace

sum(pc_2$sdev^2) ## total variance of PC1 and PC2

pc_2$sdev[1]^2/sum(pc_2$sdev^2) ## var explainde by PC1

pc_2$sdev[2]^2/sum(pc_2$sdev^2) ## var explainde by PC2

cars_n1<-as.data.frame(scale(cars_n),center=T,scale=T)

d1<-data.frame(cars_n1[,c('CityMPG','Retail')],pc_2$x)
summary(d1)

pc_2$rotation

head(d1,n=2)

-0.4394746*0.7071068+-0.7071068*0.5335368  # PC1

-0.7071068*-0.4394746+-0.7071068*0.5335368 # PC2

## PRC for the whole dataset

pc_3<-prcomp(cars_n,center = T,scale. = T)
summary(pc_3)

sum(pc_3$rotation[,1]^2)
sum(pc_3$rotation[1,]^2)

round(pc_3$rotation^2,3)

screeplot(pc_3,main='Screeplot for PCA',type='lines')
# Drop components with less than 1 value (eigenvalue>1 rule)

round(pc_3$rotation[,1:2],2)

ggplot(as.data.frame(pc_3$x),aes(PC1,PC2))+geom_point()

round(cor(pc_3$x),4) # no correlation between components

# FactomineR

# install.packages('FactoMineR')
# install.packages('Factoshiny')

library(FactoMineR)

pca1<-PCA(cars_n,graph = F,ncp=3)

names(pca1)

a<-dimdesc(pca1,axes = c(1:2))

options(scipen=20) # scientific notation
a$Dim.1$quanti

a$Dim.2$quanti

plot(pca1,choix='var',invisible='quanti.sup')

pca1$var$coord

pca1$var$coord[1,1:2]

plot(pca1,choix='var',invisible='quanti.sup',select='Retail')

plot(pca1,choix='var',invisible='quanti.sup',axes=c(2:3))

plot(pca1,choix='ind',invisible='quanti.sup')

# install.packages("factoextra")

library(factoextra)

get_eig(pca1)

fviz_screeplot(pca1,addlabels=T)

fviz_pca_var(pca1,col.var = 'black')

# Contribution of variables to dimension

fviz_contrib(pca1,choice = 'var',axes = 1) #PCA1

fviz_contrib(pca1,choice = 'var',axes = 2) #PCA2

pca_scores<-pca1$ind$coord

head(pca_scores,5)

pca_scores<-pca_scores[,1:2]
head(pca_scores)

km.res<-kmeans(pca_scores,centers = 4)
fviz_cluster(km.res,data=pca_scores)
cars_n$clust<-km.res$cluster
aggregate(cars_n,list(cars_n$clust),mean)
