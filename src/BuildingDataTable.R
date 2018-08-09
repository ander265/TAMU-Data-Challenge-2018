#Goal: Reshape data as needed for input of the Bayesian model:
rm(list=ls())
setwd("D:/Users/Jared/Downloads/Class Lectures and Videos/Data Challenge")
#install.packages('data.table')
library(data.table)

VARS <- c('Taxi ID', 'Trip Start Timestamp', 'Pickup Community Area', 'Dropoff Community Area', 'Company','Trip Seconds','Trip Miles','Fare')

full2016 <- fread('Chicago_taxi_trips2016.csv') #raw data for year 1
d2016 <- na.omit(full2016[,VARS,with=FALSE]); rm(full2016)
write.csv(d2016, file = "data2016.csv",row.names=F)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
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

#> sides[head(d2016$`Pickup Community Area`)]
# taxi ID --> total fare/count by week --> weekly median of taxi ID totals
# taxi ID --> total fare/count by day of week --> median of taxi ID totals
# taxi ID --> total fare/count by hour --> hourly median of taxi ID totals
#Fare needs to be numeric:
d2016 <- fread('data2016.csv')
d2016 <- na.omit(d2016)
d2016$`Pickup Community Area` <- sides[(d2016$`Pickup Community Area`)]
d2016$`Dropoff Community Area` <- sides[(d2016$`Dropoff Community Area`)]
d2016 <- d2016[which(d2016$`Pickup Community Area`==d2016$`Dropoff Community Area`), ]
str(d2016) #char variable
d2016 <- d2016[-(which(d2016$Fare==''))] #na.omit does not exclude these values
d2016$Fare <- as.numeric(gsub('[$,]', '', d2016$Fare))
d2016 <- d2016[-(which(d2016$Fare==0))] 
#Month and week counters
d2016$date <- as.Date(substr(d2016$`Trip Start Timestamp`,1,10),"%m/%d/%Y")
d2016$month = as.integer(substr(d2016$date,6,7))
d2016$year = as.integer(substr(d2016$date,1,4))
d2016$day = weekdays(d2016$date)
d2016$week<-as.integer(strftime(d2016$date, format = "%V"))
#> head(d2013[,-c(2,3,10)]) get rid of date data and one of sides
#d2013$hour <- sub(':','',substr(d2013$`Trip Start Timestamp`,11,13))
#d2013$time <- substrRight(d2013$`Trip Start Timestamp`,2)
d2016$hour <- trimws(paste(sub(':','',substr(d2016$`Trip Start Timestamp`,11,13)),substrRight(d2016$`Trip Start Timestamp`,2)))

names(d2016)[names(d2016) == 'Pickup Community Area'] <- 'Side'


sumsW2016 <- d2016[, list(sum(Fare),sum(`Trip Seconds`), sum(Fare)/mean(Fare)), keyby =c('year','week','Side','Taxi ID','Company')]
mediansW2016 <- sumsW2016[, list(median(V1),median(V2),median(V3)), keyby =c("year","week","Side")]

sumsD2016 <- d2016[, list(sum(Fare),sum(`Trip Seconds`), sum(Fare)/mean(Fare)), keyby =c('year','day','Side','Taxi ID','Company')]
mediansD2016 <- sumsD2016[, list(median(V1),median(V2),median(V3)), keyby =c("year","day","Side")]

sumsH2016 <- d2016[, list(sum(Fare),sum(`Trip Seconds`), sum(Fare)/mean(Fare)), keyby =c('year','hour','Side','Taxi ID','Company')]
mediansH2016 <- sumsH2016[, list(median(V1),median(V2),median(V3)), keyby =c("year","hour","Side")]
