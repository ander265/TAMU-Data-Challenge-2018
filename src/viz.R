setwd("D:/Users/Jared/Downloads/Class Lectures and Videos/Data Challenge")
trainM <- read.csv('trainM.csv')
trainM<-read.csv('allWM.csv')
n = length(unique(trainM$community))
n = length(unique(trainM$Side))
#n = length(unique(trainM$Sides))
nfiles = ceiling(n/5) #3

library('tidyverse')

pdf(file = "CommunitiesSumByYear.pdf", onefile = T, width = 12, height = 5)
for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% filter(community %in% index)%>%
    ggplot(aes(x = month, y = sum, col = as.factor(year)))+ facet_grid(.~community) + geom_line(size = 2) + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = c(3,6,9,12))

  print(p)
}
dev.off()


for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% filter(community %in% index)%>%
    ggplot(aes(x = month, y = sum))+ facet_grid(.~community) + geom_point(size = 2)+ geom_smooth() + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = c(3,6,9,12))
  
  print(p)
}


pdf(file = "CommunitiesMedianByYear.pdf", onefile = T, width = 12, height = 5)
for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% filter(community %in% index)%>%
    ggplot(aes(x = month, y = median, col = as.factor(year)))+ facet_grid(.~community) + geom_line(size = 2) + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = c(3,6,9,12))
  
  print(p)
}
dev.off()

trainM = trainM %>% 
  mutate(SideN = as.numeric(Side))

n = length(unique(trainM$SideN))
nfiles = ceiling(n/5)

pdf(file = "SidesbyYear.pdf", onefile = T, width = 12, height = 5)
for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% 
    filter(SideN %in% index)%>%
    ggplot(aes(x = week, y = V1, col = as.factor(year)))+ facet_grid(.~Side) + geom_line(size = 2) + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = seq(1,53,by= 2))
  
  print(p)
}
dev.off()


for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% 
    filter(SideN %in% index)%>%
    ggplot(aes(x = week, y = V3, col = as.factor(year)))+ facet_grid(.~Side) + geom_line(size = 2) + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = seq(1,53,by= 2))
  
  print(p)
}

for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% 
    filter(SideN %in% index)%>%
    ggplot(aes(x = week, y = V1/V3, col = as.factor(year)))+ facet_grid(.~Side) + geom_line(size = 2) + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = seq(1,53,by= 2))
  
  print(p)
}


for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% 
    filter(SideN %in% index)%>%
    filter(V2 > 60)%>%
    ggplot(aes(x = week, y = (V1/V2)*60 * 7, col = as.factor(year)))+ facet_grid(.~Side) + geom_line(size = 2) + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = seq(1,53,by= 4)) + ylim(c(0,20))
  
  print(p)
}


pdf(file = "CommunitiesCountByYear.pdf", onefile = T, width = 12, height = 5)
for (i in 1:nfiles){
  index = c(((i-1)*5 + 1):min(i*5, n))
  p = trainM %>% filter(community %in% index)%>%
    ggplot(aes(x = month, y = count, col = as.factor(year)))+ facet_grid(.~community) + geom_line(size = 2) + scale_color_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + scale_x_continuous(breaks = c(3,6,9,12))
  
  print(p)
}
dev.off()
#area 14 - example of investigating outliers, look at price per mile
#median fare per mile * count is a good measure
#locations over time? 
# predict what? 
# for every taxi, find sum of taxi

library(maptools)
library(ggmap)
ChicagoMap = qmap("Chicago", zoom = 14, color = "color", legend = "topleft")
ChicagoMap
chicago <- readShapePoly("Chicago") 
chicago <- fortify(chicago)
ggplot(chicago) + geom_polygon(aes(x=long, y=lat, group=group))
ggplot(chicago) + geom_path(aes(x=long, y=lat, group=group)) 

#chicago dataset but with counts 
# by pickup AND dropoff 
#  by longitude AND latitude coordinates


#pie chart: one by company, other by community area
