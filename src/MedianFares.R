#77 different community areas in chicago
#ID by trip and ID by taxi

#only interested in trips within the SAME community area. 
#estimated response: median fare for a trip per community area
# throw out records with NA (census track)

#Trip start/end
#Community Area
#Fare

#weekly by year
rm(list=ls())
setwd("D:/Users/Jared/Downloads/Class Lectures and Videos")
#install.packages('data.table')
library(data.table)

?data.table('')
#d2013 <- fread('Chicago_taxi_trips2013.csv')
#d2014 <- fread('Chicago_taxi_trips2014.csv')
#d2015 <- fread('Chicago_taxi_trips2015.csv')
#d2016 <- fread('Chicago_taxi_trips2016.csv')
#d2017 <- fread('Chicago_taxi_trips2017.csv')
# ^

#Some columns are type 'integer64' 
#but package bit64 is not installed. 
#Those columns will print as strange looking floating point data. 
#There is no need to reload the data. 
#Simply install.packages('bit64') 
#to obtain the integer64 print method and print the data again.
# install.packages('bit64')
library(bit64)

#Here is how I subset each original dataframe we were given:
names(d2016)
VARS <- c('Trip ID','Trip Start Timestamp','Trip End Timestamp',
          'Pickup Community Area', 'Dropoff Community Area','Fare')
head(d2013)
df2016 <- d2016[,VARS,with=FALSE]
df2016 <- df2016[which(df2016$`Pickup Community Area`==df2016$`Dropoff Community Area`), ]
df2017 <- d2017[which(d2017$`Pickup Community Area`==d2017$`Dropoff Community Area`), ]
write.csv(df2017, file = "df2017.csv")

years <- c(2013,2014,2015,2016,2017)


#Fare needs to be numeric:
d2013 <- fread('df2013.csv')
d2013 <- na.omit(d2013)
str(d2013$Fare) #char variable
d2013 <- d2013[-(which(d2013$Fare==''))] #na.omit does not exclude these values
d2013$Fare <- as.numeric(gsub('[$,]', '', d2013$Fare))
#Month and week counters
d2013$date <- as.Date(substr(d2013$`Trip Start Timestamp`,1,10),"%m/%d/%Y")
d2013$month = as.integer(substr(d2013$date,6,7))
d2013$year = as.integer(substr(d2013$date,1,4))
d2013$week<-as.integer(strftime(d2013$date, format = "%V"))
m2013 <- d2013[, median(Fare), keyby =c("Pickup Community Area",'year','month')]
w2013 <- d2013[, median(Fare), keyby =c("Pickup Community Area",'year','week')]

d2014 <- fread('df2014.csv')
d2014 <- na.omit(d2014)
d2014 <- d2014[-(which(d2014$Fare==''))] #na.omit does not exclude these values
d2014$Fare <- as.numeric(gsub('[$,]', '', d2014$Fare))
#Month and week counters
d2014$date <- as.Date(substr(d2014$`Trip Start Timestamp`,1,10),"%m/%d/%Y")
d2014$month = as.integer(substr(d2014$date,6,7))
d2014$year = as.integer(substr(d2014$date,1,4))
d2014$week<-as.integer(strftime(d2014$date, format = "%V"))
m2014 <- d2014[, median(Fare), keyby =c("Pickup Community Area",'year','month')]
w2014 <- d2014[, median(Fare), keyby =c("Pickup Community Area",'year','week')]

d2015 <- fread('df2015.csv')
d2015 <- na.omit(d2015)
str(d2015$Fare) #char variable
d2015 <- d2015[-(which(d2015$Fare==''))] #na.omit does not exclude these values
d2015$Fare <- as.numeric(gsub('[$,]', '', d2015$Fare))
#Month and week counters
d2015$date <- as.Date(substr(d2015$`Trip Start Timestamp`,1,10),"%m/%d/%Y")
d2015$month = as.integer(substr(d2015$date,6,7))
d2015$year = as.integer(substr(d2015$date,1,4))
d2015$week<-as.integer(strftime(d2015$date, format = "%V"))
m2015 <- d2015[, median(Fare), keyby =c("Pickup Community Area",'year','month')]
w2015 <- d2015[, median(Fare), keyby =c("Pickup Community Area",'year','week')]

d2016 <- fread('df2016.csv')
d2016 <- na.omit(d2016)
str(d2016$Fare) #char variable
d2016 <- d2016[-(which(d2016$Fare==''))] #na.omit does not exclude these values
d2016$Fare <- as.numeric(gsub('[$,]', '', d2016$Fare))
#Month and week counters
d2016$date <- as.Date(substr(d2016$`Trip Start Timestamp`,1,10),"%m/%d/%Y")
d2016$month = as.integer(substr(d2016$date,6,7))
d2016$year = as.integer(substr(d2016$date,1,4))
d2016$week<-as.integer(strftime(d2016$date, format = "%V"))
m2016 <- d2016[, median(Fare), keyby =c("Pickup Community Area",'year','month')]
w2016 <- d2016[, median(Fare), keyby =c("Pickup Community Area",'year','week')]

#train
trainM <- rbind(m2013,m2014,m2015,m2016)
trainW <- rbind(w2013,w2014,w2015,w2016)

d2017 <- fread('df2017.csv')
d2017 <- na.omit(d2017)
str(d2017$Fare) #char variable
d2017 <- d2017[-(which(d2017$Fare==''))] #na.omit does not exclude these values
d2017$Fare <- as.numeric(gsub('[$,]', '', d2017$Fare))
#Month and week counters
d2017$date <- as.Date(substr(d2017$`Trip Start Timestamp`,1,10),"%m/%d/%Y")
d2017$month = as.integer(substr(d2017$date,6,7))
d2017$year = as.integer(substr(d2017$date,1,4))
d2017$week<-as.integer(strftime(d2017$date, format = "%V"))
testM <- d2017[, median(Fare), keyby =c("Pickup Community Area",'year','month')]
testW <- d2017[, median(Fare), keyby =c("Pickup Community Area",'year','week')]

setwd("D:/Users/Jared/Downloads/Class Lectures and Videos/Data Challenge")
write.csv(trainW, file="trainW.csv")
write.csv(trainM, file="trainM.csv")
write.csv(testW, file = "testW.csv")
write.csv(testM, file = "testM.csv")



dcast(d2016, 'Pickup Community Area'~month) #rides per month
dcast(d2016, 'Pickup Community Area'~month, fun=median, value.var ='Fare') 

#work in progress: wide version of monthly data, where each month is its own column
melt.data.table(d2016, id.vars=c('Pickup Community Area','month'), measure.vars='Fare') 

#Work in progress: using a for loop to create entire training data.frame
datas <- c(d2013,d2014,d2015,d2016,d2017)
mdatas <- wdatas <- rep(0,5)
for (i in 1:length(datas))
  data <- datas[i]
data <- na.omit(data)
data <- data[-(which(data$Fare==''))]
data$Fare <- as.numeric(gsub('[$,]', '', data$Fare))
#Month and week counters
data$date <- as.Date(substr(data$`Trip Start Timestamp`,1,10),"%m/%d/%Y"))
data$month = as.integer(substr(data$date,6,7))
data$year = as.integer(substr(data$date,1,4))
data$week<-as.integer(strftime(data$date, format = "%V"))
mdatas[i] <- data[, median(Fare), keyby =c("Pickup Community Area",'month')]
wdatas[i] <- data[, median(Fare), keyby =c("Pickup Community Area",'week')]



