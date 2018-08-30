# install.packages('arules')
# install.packages('arulesViz')

library(arules)
library(arulesViz)

faceplate1<-read.csv('C:\\Users\\Gaya\\Desktop\\R\\Data mining\\final\\faceplate.csv')

head(faceplate1)

# Transforming data frame object into matrix object

faceplate<-as.matrix(faceplate1[,-1])

# Transforming matrix object into transaction object

txn<-as(faceplate,'transactions')

inspect(txn)

itemFrequencyPlot(txn)

rules<-apriori(txn,parameter = list(minlen=2,maxlen=2,
                                    supp=0.001,conf=0.001))

inspect(rules)

rules_s<-sort(rules,by='support')
inspect(rules_s)


inspect(rules_s[1:5,])

rules_df<-as(rules,'data.frame')
rules_df[1,]

# Calcualting support by hand

table(faceplate1$Orange,faceplate1$Red)

1/(3+5+1)

# Calculating confidence by hand
rules_df[1,]

# confidence= P(Red|Orange)=(P(Red)*P(Orange|Red))/P(Orange)=P(Red and Orange)/P(Orange)

prop.table(table(faceplate1$Orange,faceplate1$Red),1) #0.5

# Calcualting lift by hand
# Lift=P(Red|Orange)/P(Red)=1/2 / 6/10=0.5/0.6

table(faceplate1$Red)

prop.table(table(faceplate1$Orange,faceplate1$Red),1)

0.5/0.6

## Let's ease the rules by taking out minlen and maxlen

rules1<-apriori(txn,parameter = list(supp=0.001,conf=0.001))

inspect(rules1)

rules1<-apriori(txn,parameter = list(minlen=2,supp=0.001,conf=0.001))

inspect(rules1)

rules1_s<-sort(rules1,by='support')
inspect(rules1_s)

rules1_l<-sort(rules1,by='lift')
inspect(rules1_l)

rules1_r<-subset(rules1, rhs %in% 'Red')
inspect(rules1_r)

# lhs with Orange or White
rules_3<-subset(rules1, lhs %in% c('Orange','White'))
inspect(rules_3)

# lhs with Orange and White
rules_4<-subset(rules1, lhs %ain% c('Red','White'))
inspect(rules_4)

plot(rules,type='scatterplot')

plot(rules1,type='scatterplot',
     measure=c('lift','confidence'),shading='support')

plot(rules1,type='scatterplot',
     measure=c('confidence','support'),shading='lift')

plot(rules1,type='scatterplot',
     measure=c('confidence','support'),shading='lift',
     xlim=c(0.001,1),ylim=c(0.001,0.5))

plot(rules1[10:20],method='graph')

plot(rules1,method='grouped')
