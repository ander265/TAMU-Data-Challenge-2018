#Goal: Visualize Driver Tip as % of Fare for each year. 
setwd("D:/Users/Jared/Downloads/Class Lectures and Videos/Data Challenge")
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  na.omit(y)
}
library(data.table)
library(ggplot2)
full2013 <- fread('Chicago_taxi_trips2013.csv')
d2013 <- na.omit(full2013[,c('Tips','Fare')]); rm(full2013)
d2013$Fare <- as.numeric(gsub('[$,]', '', d2013$Fare))
d2013$Tips <- as.numeric(gsub('[$,]', '', d2013$Tips))
d2013 <- d2013[-(which(d2013$Fare==0))] 
d2013$ppc <- d2013$Tips/d2013$Fare
head(d2013$ppc,30)
mean(na.omit(d2013$ppc))
median(na.omit(d2013$ppc))
boxplot(d2013$ppc,ylim=c(0,2))
percent2013=(remove_outliers(d2013$ppc)*100)
write.csv(percent2013,'p2013.csv')

full2014 <- fread('Chicago_taxi_trips2014.csv')
d2014 <- na.omit(full2014[,c('Tips','Fare')]); rm(full2014)
d2014$Fare <- as.numeric(gsub('[$,]', '', d2014$Fare))
d2014$Tips <- as.numeric(gsub('[$,]', '', d2014$Tips))
d2014 <- d2014[-(which(d2014$Fare==0))] 
d2014$ppc <- d2014$Tips/d2014$Fare
mean(na.omit(d2014$ppc))
median(na.omit(d2014$ppc))
boxplot(d2014$ppc,ylim=c(0,2))
percent2014=(remove_outliers(d2014$ppc)*100)
write.csv(percent2014,'p2014.csv')

full2015 <- fread('Chicago_taxi_trips2015.csv')
d2015 <- na.omit(full2015[,c('Tips','Fare')]); rm(full2015)
d2015$Fare <- as.numeric(gsub('[$,]', '', d2015$Fare))
d2015$Tips <- as.numeric(gsub('[$,]', '', d2015$Tips))
d2015 <- d2015[-(which(d2015$Fare==0))] 
d2015$ppc <- d2015$Tips/d2015$Fare
mean(na.omit(d2015$ppc))
median(na.omit(d2015$ppc))
boxplot(d2015$ppc,ylim=c(0,2))
percent2015=(remove_outliers(d2015$ppc)*100)
write.csv(percent2015,'p2015.csv')


full2016 <- fread('Chicago_taxi_trips2016.csv')
d2016 <- na.omit(full2016[,c('Tips','Fare')]); rm(full2016)
d2016$Fare <- as.numeric(gsub('[$,]', '', d2016$Fare))
d2016$Tips <- as.numeric(gsub('[$,]', '', d2016$Tips))
d2016 <- d2016[-(which(d2016$Fare==0))] 
d2016$ppc <- d2016$Tips/d2016$Fare
mean(na.omit(d2016$ppc))
median(na.omit(d2016$ppc))
boxplot(d2016$ppc,ylim=c(0,2))
percent2016=(remove_outliers(d2016$ppc)*100)
write.csv(percent2016,'p2016.csv')


full2017 <- fread('Chicago_taxi_trips2017.csv')
d2017 <- na.omit(full2017[,c('Tips','Fare')]); rm(full2017)
d2017$Fare <- as.numeric(gsub('[$,]', '', d2017$Fare))
d2017$Tips <- as.numeric(gsub('[$,]', '', d2017$Tips))
d2017 <- d2017[-(which(d2017$Fare==0))] 
d2017$ppc <- d2017$Tips/d2017$Fare
head(d2017$ppc,30)
mean(na.omit(d2017$ppc))
median(na.omit(d2017$ppc))
boxplot(d2017$ppc,ylim=c(0,2))
percent2017=(remove_outliers(d2017$ppc)*100)
write.csv(percent2017,'p2017.csv')

p2013 <- fread('p2013.csv')
p2013[,1] <- '2013'
p2014 <- fread('p2014.csv')
p2014[,1] <- '2014'
p2015 <- fread('p2015.csv')
p2015[,1] <- '2015'
p2016 <- fread('p2016.csv')
p2016[,1] <- '2016'
p2017 <- fread('p2017.csv')
p2017[,1] <- '2017'
percentages<- rbind(p2013,p2014,p2015,p2016,p2017)
write.csv(percentages,'percentages.csv', row.names=F)
ggplot(data.frame(percentages))
names(percentages)<- c('year', 'ttf')
percentages$year <- factor(percentages$year,
                           labels = c("2013", "2015", "2017"))
boxplot <- ggplot(percentages, aes(x = year, y = ttf)) +
  geom_boxplot(colour = lines,
               size = 1, notch = TRUE) +
  scale_y_continuous(name = "Tip to Fare Percentage",
                     breaks = seq(0, 175, 25),
                     limits=c(0, 175)) +
  scale_x_discrete(name = "Year") +
  ggtitle("Boxplot of Annual Distribution of Tip/Fare")
