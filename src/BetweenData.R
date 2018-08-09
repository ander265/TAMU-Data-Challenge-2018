#Goal: Analyze Taxi Activity occuring within the same Neighborhood (i.e. Pickup & Dropoff in same Region)
rm(list=ls())
setwd("D:/Users/Jared/Downloads/Class Lectures and Videos/Data Challenge")
#install.packages('data.table')
library(data.table)
install.packages('plotrix')
library(plotrix)
bvars<-c('Pickup Community Area', 'Dropoff Community Area','Fare')
Sides <- list(
  'FarNorth' <- c(1,2,3,4,9,10,11,12,13,14,76,77),
  'North' <- c(5,6,7,21,22),
  'Northwest' <- c(15,16,17,18,19,20),
  'West' <- c(23,24,25,26,27,28,29,30,31), 
  'Central' <- c(8,32,33),
  'Southwest' <- c(56,57,58,59,61,62,63,64,65,66,67,68),
  'South' <- c(34,35,36,37,38,39,40,41,42,43,60,69),
  'FarSouthwest' <- c(70,71,72,73,74,75),
  'Southeast' <- c(44,45,46,47,48,49,50,51,52,53,54,55)
)
names(Sides) <- c('FarNorth','North','Northwest','West','Central','Southwest','South','FarSouthwest','Southeast')
sides <- rep(0,77)
for (i in 1:length(Sides))
  sides[Sides[[i]]] <- names(Sides)[i]

# Visualization Demo:
# slices <- c(10, 12,4, 16, 8)
# lbls <- c("US", "UK", "Australia", "Germany", "France")
# pie(slices, labels = lbls, main="Pie Chart of Countries")

par(mfrow=c(5,3))
names = c('Side','Within','Incoming (Dropoffs)','Outgoing (Pickups)')

df2013 <- fread('data2013.csv')
df2013 <- df2013[,bvars,with=F]
df2013$`Pickup Community Area` <- sides[(df2013$`Pickup Community Area`)]
df2013$`Dropoff Community Area` <- sides[(df2013$`Dropoff Community Area`)]
df2013 <- df2013[-(which(df2013$Fare==''))] #na.omit does not exclude these values
df2013$Fare <- as.numeric(gsub('[$,]', '', df2013$Fare))
df2013 <- df2013[-(which(df2013$Fare==0))] 
write.csv(df2013,'between2013.csv',row.names=F)

between2013 <- df2013[which(df2013$`Pickup Community Area`==df2013$`Dropoff Community Area`), ]
between2013 <- between2013[,sum(Fare)/mean(Fare),keyby=c('Pickup Community Area')]
out2013 = df2013[, sum(Fare)/mean(Fare), keyby =c('Pickup Community Area')]
in2013 = df2013[, sum(Fare)/mean(Fare), keyby =c('Dropoff Community Area')]
trips2013 <-data.frame(between2013,in2013[,2],out2013[,2])
write.csv(trips2013,'trips2013.csv',row.names=F)

df2014 <- fread('data2014.csv')
df2014 <- df2014[,bvars,with=F]
df2014$`Pickup Community Area` <- sides[(df2014$`Pickup Community Area`)]
df2014$`Dropoff Community Area` <- sides[(df2014$`Dropoff Community Area`)]
df2014 <- df2014[-(which(df2014$Fare==''))] #na.omit does not exclude these values
df2014$Fare <- as.numeric(gsub('[$,]', '', df2014$Fare))
df2014 <- df2014[-(which(df2014$Fare==0))] 
write.csv(df2014,'between2014.csv',row.names=F)

between2014 <- df2014[which(df2014$`Pickup Community Area`==df2014$`Dropoff Community Area`), ]
between2014 <- between2014[,sum(Fare)/mean(Fare),keyby=c('Pickup Community Area')]
out2014 = df2014[, sum(Fare)/mean(Fare), keyby =c('Pickup Community Area')]
in2014 = df2014[, sum(Fare)/mean(Fare), keyby =c('Dropoff Community Area')]
trips2014 <-data.frame(between2014,in2014[,2],out2014[,2])
write.csv(trips2014,'trips2014.csv',row.names=F)


df2015 <- fread('data2015.csv')
df2015 <- df2015[,bvars,with=F]
df2015$`Pickup Community Area` <- sides[(df2015$`Pickup Community Area`)]
df2015$`Dropoff Community Area` <- sides[(df2015$`Dropoff Community Area`)]
df2015 <- df2015[-(which(df2015$Fare==''))] #na.omit does not exclude these values
df2015$Fare <- as.numeric(gsub('[$,]', '', df2015$Fare))
df2015 <- df2015[-(which(df2015$Fare==0))] 

out2015 = df2015[, sum(Fare)/mean(Fare), keyby =c('Pickup Community Area')]
in2015 = df2015[, sum(Fare)/mean(Fare), keyby =c('Dropoff Community Area')]
between2015 <- df2015[which(df2015$`Pickup Community Area`==df2015$`Dropoff Community Area`), ]
between2015 = between2015[, sum(Fare)/mean(Fare), keyby =c('Pickup Community Area')]
trips2015 <-data.frame(between2015,in2015[,2],out2015[,2])
write.csv(trips2015,'trips2015.csv',row.names=F)
for (i in 2:4)
  pie3D(trips2017[,i], explode=0.3, main=(names(trips2017)[i]))


df2016 <- fread('data2016.csv')
df2016 <- df2016[,bvars,with=F]
df2016$`Pickup Community Area` <- sides[(df2016$`Pickup Community Area`)]
df2016$`Dropoff Community Area` <- sides[(df2016$`Dropoff Community Area`)]
df2016 <- df2016[-(which(df2016$Fare==''))] #na.omit does not exclude these values
df2016$Fare <- as.numeric(gsub('[$,]', '', df2016$Fare))
df2016 <- df2016[-(which(df2016$Fare==0))] 
write.csv(df2016,'between2016.csv',row.names=F)
#between2016 <- fread('between2016.csv')
between2016 <- df2016[which(df2016$`Pickup Community Area`==df2016$`Dropoff Community Area`), ]
between2016 <- between2016[,sum(Fare)/mean(Fare),keyby=c('Pickup Community Area')]
out2016=df2016[, sum(Fare)/mean(Fare), keyby =c('Pickup Community Area')]
in2016=df2016[, sum(Fare)/mean(Fare), keyby =c('Dropoff Community Area')]
par(mfrow=c(1,3))
trips2016 <-data.frame(between2016,in2016[,2],out2016[,2])
write.csv(trips2016,'trips2016.csv',row.names=F)
trips2016 <- read.csv('trips2016.csv')
for (i in 2:4)
  pie3D(trips2016[,i], explode=0.3, main=(names(trips2016)[i]))

df2017 <- fread('data2017.csv')
df2017 <- df2017[,bvars,with=F]
df2017$`Pickup Community Area` <- sides[(df2017$`Pickup Community Area`)]
df2017$`Dropoff Community Area` <- sides[(df2017$`Dropoff Community Area`)]
df2017 <- df2017[-(which(df2017$Fare==''))] #na.omit does not exclude these values
df2017$Fare <- as.numeric(gsub('[$,]', '', df2017$Fare))
df2017 <- df2017[-(which(df2017$Fare==0))] 
write.csv(df2017,'between2017.csv',row.names=F)
out2017 = df2017[, sum(Fare)/mean(Fare), keyby =c('Pickup Community Area')]
in2017 = df2017[, sum(Fare)/mean(Fare), keyby =c('Dropoff Community Area')]
between2017<- fread('sumsW2017.csv')
between2017 <- between2017[,sum(na.omit(V3)),keyby=c('Side')]
trips2017 <-data.frame(between2017,in2017[,2],out2017[,2])
write.csv(trips2017,'trips2017.csv',row.names=F)
trips2017 <- read.csv('trips2017.csv')
for (i in 2:4)
  pie3D(trips2017[,i], explode=0.3, main=(names(trips2017)[i]))
names = c('Side','Within','Dropoffs','Pickups')
names(trips2013) <- names(trips2014) <- names(trips2015) <- names(trips2016) <- names(trips2017) <- names
ggplot(trips2013, aes(Side, fill=Within)) + 
  geom_bar(position = 'identity', alpha = .3)

barplot(trips2013[,3], col=5)
for (i in c(2,4))
  barplot(trips2013[,i], col=i, add=TRUE) 

# adding a legend
legend('top', bty = 'n', title = 'Legend',
       legend = c('A', 'B'), fill = c('red', 'green'))


