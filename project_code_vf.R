#run the first time
# install.packages(c("httr","jsonlite"))
#install.packages("gmapsdistance")
# install.packages(c("rgdal", "sp", "rgeos"))
# install.packages('PROJ.4')
# install.packages('maptools')
# install.packages('ggmap')

setwd("C:/Users/Vania Fong/Google Drive/142 Project")

library(gmapsdistance)
library(httr)
library(jsonlite)
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(maptools)
library(reshape2)
library(ggplot2)
library(rgeos)
library(sp)
library(rgdal)
library(ggmap)
library(data.table)


#shapefile reading
taz.map <- readOGR("TAZ/bayarea_taz2000.shp", layer="bayarea_taz2000")
#Filter to only keep TAZs in city of SF
taz.sf = taz.map[taz.map$FIRST_COUN == '06075', ]

#Convert a data frame to a Spatial Points data frame and joins with TAZ polygon
#outputs the transformed coordinate shapefile as well as the joined table as a dataframe
#data frame input must have column names "Longitude" and "Latitude"
convert_join <- function(df) {
  xy = df[,c(which(names(df) == "Longitude"), which(names(df) == "Latitude"))]
  sp = SpatialPointsDataFrame(coords = xy, data = df,proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.transf = spTransform(sp, CRSobj = CRS(proj4string(taz.sf)))
  joined = as.data.frame(spCbind(sp.transf, over(sp.transf, taz.sf))) %>% 
    select(-c(Latitude.1, Longitude.1))
  joined$TAZ = as.numeric(joined$TAZ)
  return(list(TransfSp = sp.transf, Joined = joined))
}
#calculate sf centroids
sfcentroids = coordinates(taz.sf)
centroid2taz = cbind(taz.sf@data, sfcentroids)
names(centroid2taz)[c(4,5)] = c('Longitude', 'Latitude')
centroid2taz$TAZ = as.numeric(centroid2taz$TAZ)


#random gps points
rando = read_excel('Random-SF-Locations.xlsx') %>% 
  transform(geoloc=paste(Latitude, Longitude, sep="+"))
key = 'AIzaSyCzHKDj9bsmhkS3o300TL9VET0XwywQJjs'	

#ieor142 key
#key = "AIzaSyCtM0KUEC1PiENc5ivhEPSo9PPoKLRJ3Hk"
coord.rand = rando
#coordinates(coord.rand) = ~Latitude + Longitude 



#select popular TAZs
rideshare = read.csv('trip_stats_taz.csv')

#use the top 50 in SF county
pop.rideshare = rideshare %>% 
  filter(taz <=733) %>% 
  group_by(taz) %>% 
  mutate(rides = pickups + dropoffs) %>% 
  summarise(tot.rides = sum(rides)) %>% 
  arrange(desc(tot.rides)) %>% 
  head(20)

#sort day + hour combinations by popularity
time.rideshare = pop.rideshare   %>% 
  left_join(rideshare) %>% 
  left_join(centroid2taz, by = c('taz' = 'TAZ')) %>% 
  group_by(day_of_week, hour) %>% 
  mutate(combined.rides = pickups + dropoffs, hour.new = hour-3) %>% 
  arrange(desc(combined.rides)) %>% 
  transform(geoloc=paste(Latitude, Longitude, sep="+"), DayHour = paste(day_of_week, hour, sep="D")) %>% 
  select(-c(taz, tot.rides, hour), taz)

View(time.rideshare)
summary(time.rideshare$combined.rides)
ggplot(time.rideshare, aes(DayHour, combined.rides)) + geom_col() + ylab('Total Rides Per Day/Hour Combination')

#popular hours
pop.hours = time.rideshare %>% 
  group_by(hour) %>% 
  summarise(combined.rides= mean(combined.rides)) %>% 
  mutate(hour.new = hour - 3) 
  
#plot popular hours
ggplot(pop.hours, aes(hour.new, combined.rides)) + geom_col(fill = 'lightblue') + geom_text(aes(label = round(combined.rides))) +
  ylab('Average # of Rides During Each Hour Per TAZ') + xlab('Hour')

#popular days
pop.days = time.rideshare %>% 
  group_by(day_of_week) %>% 
  summarise(combined.rides= mean(combined.rides))
ggplot(pop.days, aes(day_of_week, combined.rides)) + geom_col(fill = 'orange') + geom_text(aes(label = round(combined.rides))) +
  ylab('Average # of Rides During Each Day of Week Per TAZ') + xlab('Day of Week')


joined = convert_join(time.rideshare)

#overlay plots
plot(joined[[1]], col = 'darkgreen',add= TRUE,  pch= 13, axes = TRUE)
plot(taz.sf, axes = TRUE, border = 'lightgray')


##EDIT DEP TIME
select.times = c('2017-12-17 03:00', '2017-12-17 09:00', '2017-12-17 11:00', '2017-12-17 19:00',
                 '2017-12-18 03:00', '2017-12-18 09:00', '2017-12-18 11:00', '2017-12-18 19:00',
                 '2017-12-22 03:00', '2017-12-22 09:00', '2017-12-22 11:00', '2017-12-22 19:00',
                 '2017-12-23 03:00', '2017-12-23 09:00', '2017-12-23 11:00', '2017-12-23 19:00')                 
time_vec = as.POSIXct(format(as.POSIXct(select.times), tz = "GMT"))
time_vec.int = as.integer(time_vec)
# format(as.POSIXct(gmt, tz = 'GMT'), tz = 'PST8PDT')
time.car = list()
time.car.list  = list()

#run to find trip duration for different 
for (a in c(10, 20)){
  
  for (i in 1:16){
    carmat = gmapsdistance(time.rideshare$geoloc[a-9:a], time.rideshare$geoloc[a-9:a], mode = 'driving', shape = 'long',
                           ,key  = key, departure = time_vec.int[i])
    time.car[[i]]= carmat[[1]] %>% 
      cbind(Dep_Time.pst  = format(as.POSIXct(as.character(time_vec[i]), tz = 'GMT'), tz = 'PST8PDT'))
    print(i)
    Sys.sleep(3)
  }
  time.car.list[[a/10]] = time.car
  print(a)
  
}
#make a large dataframe with all car time
time.car.final = rbindlist(time.car.list[[2]])
write.csv(time.car.final, 'car1120.csv')

time.transit = list()
time.transit.list = list()
for (a in c(10,20)){
  for (j in 1:16){
    transitmat = gmapsdistance(time.rideshare$geoloc[a-9:a], time.rideshare$geoloc[a-9:a], key = key,
                               shape = 'long', mode = 'transit', departure = time_vec.int[j])
    time.transit[[j]]= transitmat[[1]] %>% 
      cbind(Dep_Time.pst  = format(as.POSIXct(as.character(time_vec[j]), tz = 'GMT'), tz = 'PST8PDT'))
    Sys.sleep(3)
    print(j)
  }
  time.transit.list[[a/10]] = time.transit[[a/10]]
  print(a)
}


#make a large dataframe with all transit time
time.transit.List1 = time.transit #has all times for geoloc[1:10]

time.transit.final = rbindlist(time.transit.List)
write.csv(rbindlist(time.transit.List1), 'transit110.csv')


#calculate delta
df = carmat[[1]] %>% 
  merge(transitmat[[1]], by = c('or' = 'or', 'de' = 'de')) %>% 
  left_join(rando, by = c('or' = 'geoloc')) %>% 
  mutate(CarTime = Time.x, TransitTime = Time.y, delta = TransitTime - CarTime) %>% 
  select(-c(Time.x, Time.y), Orig.Lat = Latitude, Orig.Lon = Longitude, Orig.Address = Address )  

#join address to latlon and prepare data for heatmap
all.table = left_join(df, rando, by = c('de' = 'geoloc')) %>% 
  select(everything(),Dest.Address = Address, Dest.Lat = Latitude, Dest.Lon = Longitude) %>% 
  arrange(desc(delta)) %>% 
  mutate(Orig.Addr = gsub('(, San Fran).*$', '', Orig.Address), Dest.Addr = gsub('(, San Fran).*$', '', Dest.Address), 
         min.delta =delta/60)
#generate heatmap
ggplot(all.table, aes(Orig.Addr, Dest.Addr)) + geom_tile(aes(fill = min.delta), color = "white") +
  scale_fill_gradient(low = "white", high = 'magenta4') +
  ylab("Origin ") +
  xlab("Destination") + labs(fill = "Minutes Longer", title= 'Public Transit vs Driving Trip Duration') +
  theme( axis.text.x = element_text(angle = 50, hjust = 1), 
         axis.title=element_text(size=12,face="bold"))

Orig = all.table[, grepl( "(Or.*)" , names( all.table ) ) ]
coordinates(Orig) <- ~ Orig.Lat + Orig.Lon
#Orig = spTransform(Orig, CRSobj = CRS(proj4string(taz.map)))
gridded(Orig) = TRUE

Dest = all.table[, grepl( "(Dest.*)" , names( all.table ) ) ]
coordinates(Dest) <- ~ Dest.Lat + Dest.Lon







# ###SAMPLE RBIND
# DT1 = data.frame(A=1:3,B=letters[1:3])
# DT2 = data.frame(B=letters[4:5],A=4:5)
# l = list(DT1,DT2)
# rbindlist(l)
