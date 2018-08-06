library(dummies)
library(ggplot2)
library(tidyr)
library(dendextend)
# install.packages('gower')
library(gower)
library(cluster)
library(dplyr)

dir<-getwd()
setwd(dir)

wholesale<-read.csv('Wholesale customers data.csv',stringsAsFactors = TRUE) ## Put downloaded file 
## in your working directory

str(wholesale)

### Making factors from REGION & CHANNEL variables
table(wholesale$Region)

wholesale$Region<-factor(wholesale$Region,labels = c('Lisbon','Oporto','Other Region'),levels = c(1,2,3))

table(wholesale$Region)

table(wholesale$Channel)

wholesale$Channel<-factor(wholesale$Channel,labels = c('Horeca','Retail'),levels = c(1,2))

table(wholesale$Channel)

### Making dummies from categorical variables

wholesale_d<-dummy.data.frame(wholesale)

### As variables are now on different scale we need scale them back ... effect of dummies

wholesale_sc<-scale(wholesale_d)

### Let's start from hierarchical clustering as we yet don't know the actual number of k

### BUt before that let's also check on NA's 

anyNA(wholesale)
### Good!!!
### Important!!! Let's identify wheteher there is significant correlation beetween variables
### and whether there are any outliers

cor(wholesale_d)
cor(wholesale_d)[cor(wholesale_d)>0.8] #only two variables have high correlation of 0.92
### Detergents_Paper and Grocery so we don't have strong need of using statistical distance
### method

wholesale_gat<-gather(data=wholesale_d,
                      key = variables,
                      value = Value
                      )

ggplot(wholesale_gat,aes(x=variables,y=Value))+geom_boxplot()+
  labs(title='Distribution of Variables')
## We may clealry observe that there are pretty much outliers, hence we should employ
##suitable for this situation distance measurement method!!! Hence there is some common 
### sense of using Manhattan distance which is resilient to outliers!!


## Let's calculate distances using simple euclidean method, then we also use Manhattan 
## distance measuring method on wholesale scaled dataset

whole_dist<-dist(wholesale_sc,method='euclidean')

hc_whole<-hclust(whole_dist,method='average')

plot(hc_whole)

hc_whole_col<-color_branches(hc_whole,h=5)
plot(hc_whole_col)
#### As we may observe this path had not show satisfying resultshence let's use Gower method of 
### distance measurement which takes into account categorical and numerical variables alike


wholesale
dow_dist<-daisy(wholesale,metric='gower')

hc_whole_gower<-hclust(dow_dist,method='mcquitty')

plot(hc_whole_gower)

hc_whole_gower_col<-color_branches(hc_whole_gower,h=0.20)

plot(hc_whole_gower)


df_gow<-cutree(hc_whole_gower,h = 0.30)

table(df_gow)

whole_mcquitty<-wholesale%>%
  mutate(cluster=df_gow)

whole_mcquitty_tidy<-gather(data =whole_mcquitty,
                            key=Goods,
                            value = Annual_Spending,
                            -cluster,-Channel,-Region)

ggplot(whole_mcquitty_tidy,aes(x=factor(cluster),y=Annual_Spending,color=Goods))+geom_boxplot()

summary(wholesale)       

### Again not that inforamtive results


### Using only numerical variables

wholesale

wholesale_num<-wholesale[,c(-1,-2)]

whole_dist<-dist(wholesale_num,method='manhattan')

whole_hc<-hclust(whole_dist,method='mcquitty')

plot(whole_hc)

summary(wholesale_num)

hc_col_whole<-color_branches(whole_hc,h=40000)
plot(hc_col_whole)

hc_df<-cutree(whole_hc,k=3)

table(hc_df)

whole_mcquitty_3k<-wholesale%>%
  mutate(cluster=hc_df)

whole_mcquitty_tidy<-gather(data =whole_mcquitty_3k,
                            key=Goods,
                            value = Annual_Spending,
                            -cluster,-Channel,-Region)

ggplot(whole_mcquitty_tidy,aes(x=factor(cluster),y=Annual_Spending,color=Goods))+geom_boxplot()

### Using k-means algorithm

### Elbow-method

library(purrr)

tot_withinss <- map_dbl(1:15,  function(k){
  model <- kmeans(x = wholesale_num, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:15,
  tot_withinss = tot_withinss
)

ggplot(elbow_df,aes(x=k,y=tot_withinss))+geom_line()+
  scale_x_continuous(breaks = 1:15)

## The elbow method show value k=3

## Let's calculate silhoutte method 

library(purrr)
sil_width <- map_dbl(2:15, function(k){
  model <- pam(x = wholesale_num, k = k)
  model$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:15,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15)

## It appears silhoutte method shows value k=2 or 3


library(cluster)
pam_k7 <- pam(wholesale_num, k = 7)
pam_k7$silinfo$widths

sil_plot <- silhouette(pam_k7)
plot(sil_plot)

pam_k3 <- pam(wholesale_num, k = 3)
pam_k3$silinfo$widths

sil_plot <- silhouette(pam_k3)
plot(sil_plot)

pam_k12 <- pam(wholesale_num, k = 12)
pam_k12$silinfo$widths

sil_plot <- silhouette(pam_k12)
plot(sil_plot)


clus3_df<-kmeans(wholesale_num,3)

whole_3clus<-wholesale%>%
  mutate(cluster=clus3_df$cluster)

whole_mcquitty_tidy_3clus<-gather(data =whole_3clus,
                            key=Goods,
                            value = Annual_Spending,
                            -cluster,-Channel,-Region)

ggplot(whole_mcquitty_tidy_3clus,aes(x=factor(cluster),y=Annual_Spending,color=Goods))+geom_boxplot()

summary(wholesale)       

ggplot(whole_mcquitty_tidy_3clus,aes(x=factor(cluster),y=Annual_Spending,color=Goods))+geom_boxplot()

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(clus3_df$centers), max(clus3_df$centers)), xlim = c(0, 6),main = 'Graph of centroids')
# label x-axes
axis(1, at = c(1:6), labels = names(wholesale_num))
# plot centroids
for (i in c(1:3))
  lines(clus3_df$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3),
                                                       "black", "dark grey"))
# name clusters
text(x = 0.5, y = clus3_df$centers[, 1], labels = paste("Cluster", c(1:3)))

## To conclude the most reasomable result we get with k=3
## In addition le's look on our results through the prospectives of two locations
## namely:Channel and Region

summ_by_region<-whole_mcquitty_tidy_3clus%>%
  group_by(cluster,Channel)%>%
  select(Annual_Spending)%>%
  summarise(Av_sp=mean(Annual_Spending),Count=n())%>%
  arrange(cluster)

## From this simple data manipulation we get info by geographic location taking deeper look 
## on Cluster # 2 as it appears to be the most profitable by its average prices compared to its
## goods variations. The least profitable appearrto be the third cluster. The second cluster
## also shows good results in profitability. In more details the distinction between #1 and 
## #2 cluster is in the fact that more deals are made in Horeca Channel in case of #1 cluster.
## However the pattern changes within # 2 cluster which clearly has dominant position.

## Let's visualize

ggplot(summ_by_region,aes(x=Channel,y=Av_sp,color=factor(cluster)))+geom_line(aes(group=factor(cluster)))+
  labs(title='Average Spending of Channels by clusters')


summ_by_region_goods<-whole_mcquitty_tidy_3clus%>%
  group_by(cluster,Channel,Goods)%>%
  select(Annual_Spending)%>%
  summarise(Av_sp=mean(Annual_Spending),Count=n())%>%
  arrange(cluster)

ggplot(summ_by_region_goods,aes(x=Goods,y=Av_sp,color=factor(cluster)))+geom_line(aes(group=factor(cluster)))+
  labs(title='Average Spending of Goods by clusters')

### The above graph depicts the spending patterns in Average Spending of identified clusters
## In particular # 1 cutomer group has dominant role in purchasing power of Fresh,
## whereas #2 customer group shows clear leading results by average amount spend on Grocery and Milk
## Knowwing this and company's management should define more tailored business policies 
## to this cotomer segements which suit their needs. In addition #3 customer segement does not show
## any particular patters and hence they may be considered the ordinary cutomer base of company.


# Next we visulaize spending patterns by Goods and Channel types.
summ_by_goods_av_sp<-whole_mcquitty_tidy_3clus%>%
  group_by(Goods,cluster,Channel)%>%
  select(Annual_Spending)%>%
  summarise(Av_sp=mean(Annual_Spending),Count=n())%>%
  arrange(cluster)

ggplot(summ_by_goods_av_sp,aes(x=Goods,y=Av_sp,color=factor(cluster)))+geom_line(aes(group=factor(cluster)))+
  labs(title='Average Spending of Goods by Channels')+facet_grid(Channel~.)

## Clearly Fresh appears to play the dominant role in both Horeca and Retail.
## Probably this product is the specification of the company. Nevertheless, we should
## gain insights and capitalize on them in sectors which are not so clear and specific'
## to various niches and segments in order to better meet the needs of those that 
## are included in these segments. Thus detergent paper is more demanded by av_spending
## in Retail hence operations should take this into account by providing more choice
## in this location. The similar situation is with Grocery in Retail... On the other hand Milk is 
## more demanded in Horeca!!! To summarise, company's management should take tjis into account.

## In reality many more analyses are possible and needed nevertheless there is not enough info.
## More specifically we don't have some domain or company specific info.. 

## To conclude this pet project serves as very simplified analysis of pet project,
## but in reality numerous considerations and indicators should be taken into while 
## conducting customer segementation.