data(Titanic)
d1 <- as.data.frame(Titanic)
d <- d1[rep(seq_len(nrow(d1)), d1$Freq),1:4]
row.names(d) <- NULL


plot(d$Class,col='#2f95f5', xlab='Passenger class', main='HMS Titanic passengers by class', ylab = 'Number of people' )

barplot(prop.table(table(d$Class)),col='#1372ca', ylab='Passenger class', main='HMS Titanic passengers by class', xylab = 'Proportion of people',horiz=T )

pie(prop.table(table(d$Class)),col=c('#1372ca','#ed7a37','#ba4ad9','#2ddc64'), main='HMS Titanic passengers by class' )

barplot(table(d$Sex,d$Class),
args.legend =list(x=1.5,y=600) ,xlab='Passenger class', main='HMS Titanic passengers by class', ylab = 'Number of people', col=c('#fce84d','#1f813b'),legend.text=c('Male','Female') )
barplot(table(d$Sex,d$Class),
args.legend =list(x=5.5,y=600) , beside=T,xlab='Passenger class', main='HMS Titanic passengers by class', ylab = 'Number of people', col=c('#fce84d','#1f813b'),legend.text=c('Male','Female') )


plot(density(na.omit(cen$age)),type='l', main='Age distribution of village TZ',xlab='Age', yla='Density', col='#f07373')
plot(density(na.omit(cen$age), kernel='triangular'),type='l', main='Age distribution of village TZ',xlab='Age', yla='Density', col='#f07373')

plot(density(na.omit(cen$age), n=20),type='l', main='Age distribution of village TZ',xlab='Age', yla='Density', col='#f07373')

data(AirPassengers )
d <- AirPassengers
plot(d,type='l',main='Monthly Airline passengers', ylab='Number of passengers')

plot(d,type='b',main='Monthly Airline passengers', ylab='Number of passengers')

plot(d,type='s',main='Monthly Airline passengers', ylab='Number of passengers')

plot(d,type='h',main='Monthly Airline passengers', ylab='Number of passengers')

plot(iris$Petal.Length, iris$Sepal.Length,pch=20, main='Relationship between petal and sepal length in irises', ylab='Sepal length (cm)', xlab='Petal length (cm)')

i1 <- iris[iris$Species=='versicolor',]
i2 <- iris[iris$Species=='setosa',]
i3 <- iris[iris$Species=='virginica',]
plot(i1$Petal.Length, i1$Sepal.Length,pch=20,xlim = c(1,7), ylim=c(4.5,8), main='Relationship between petal and sepal length in irises', ylab='Sepal length (cm)', xlab='Petal length (cm)', col='#f5e621')
points(i2$Petal.Length, i2$Sepal.Length,pch=20, col='#72f521')
points(i3$Petal.Length, i3$Sepal.Length,pch=20,  col='#f59921')
legend(1,8,title='Species',legend=c('Versicolor','Setosa','Virginica'), fill=c('#f5e621','#72f521','#f59921'))
