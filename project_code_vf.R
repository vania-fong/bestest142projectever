#run the first time
# install.packages(c("httr","jsonlite", 'data.table'))
#install.packages("gmapsdistance")
# install.packages(c("rgdal", "sp", "rgeos"))
# install.packages('maptools')
# install.packages('ggmap')
#install.packages('raster')



setwd("C:/Users/Vania Fong/Google Drive/142 Project")

library(gmapsdistance)
library(httr)
library(jsonlite)
library(readxl)
library(tidyr)
library(lubridate)
library(maptools)
library(reshape2)
library(ggplot2)
library(rgeos)
library(sp)
library(rgdal)
library(ggmap)
library(raster)
library(data.table)
library(dplyr)



#shapefile reading
#from stanford https://purl.stanford.edu/tm960wp6354
taz.map3 <- readOGR("TAZ/bayarea_taz2000.shp", layer="bayarea_taz2000")
# taz.map2 <- readOGR("Traffic Analysis Zones/geo_export_6cc7cbbc-0bf6-4d73-8909-3efa166a9e04.shp")
#from http://www.sfcta.org/tncstoday
taz.map <- readOGR("UL_taz/TAZ981.shp")

#calculate area
taz.sf = taz.map
taz.sf@data$area_sqft = area(taz.sf)

#calibrate CRS
taz.sf = spTransform(taz.sf, CRSobj = CRS(proj4string(taz.map3)))
head(coordinates(taz.sf))

#Convert a data frame to a Spatial Points data frame and joins with TAZ polygon
#outputs the transformed coordinate shapefile as well as the joined table as a dataframe
#data frame input must have column names "Longitude" and "Latitude"
convert_join <- function(df) {
  xy = df[,c(which(names(df) == "Longitude"), which(names(df) == "Latitude"))]
  sp = SpatialPointsDataFrame(coords = xy, data = df, proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.transf = spTransform(sp, CRSobj = CRS(proj4string(taz.sf)))
  joined = as.data.frame(spCbind(sp.transf, over(sp.transf, taz.sf))) %>% 
    dplyr::select(-c(Latitude.1, Longitude.1))
  return(list(TransfSp = sp.transf, Joined = joined))
}
#calculate sf centroids
sfcentroids = coordinates(taz.sf)
centroid2taz = cbind(taz.sf@data, sfcentroids)
names(centroid2taz)[c(length(centroid2taz) - 1,length(centroid2taz))] = c('Longitude', 'Latitude')
centroid2taz = centroid2taz %>% 
  mutate(TAZ = as.numeric(TAZ)) %>% 
  transform(geoloc =  paste(Latitude, Longitude, sep="+"))



#API KEY
#carrol's
key = 'AIzaSyAejZ8T9sQkQgWHDPZYLKxozSkeo0laK_o'
#set.api.key(key)
#gradey's
key = 'AIzaSyAPOlDjhu_qvXObLwLHwdSYAjkI6BCAqCo'

#ieor142 key
# key = "AIzaSyCtM0KUEC1PiENc5ivhEPSo9PPoKLRJ3Hk"

#select popular TAZs
# from http://www.sfcta.org/tncstoday
rideshare = read.csv('trip_stats_taz.csv')

#use the top 50 in SF county
pop.rideshare = rideshare %>% 
  filter(taz <=733) %>% 
  group_by(taz) %>% 
  mutate(rides = pickups + dropoffs) %>% 
  summarise(tot.rides = sum(rides)) %>% 
  arrange(desc(tot.rides)) %>% 
  left_join(centroid2taz, by = c('taz' = 'TAZ')) %>% 
  head(20)

#sort day + hour combinations by popularity
time.rideshare = pop.rideshare   %>% 
  left_join(rideshare) %>% 
  left_join(centroid2taz, by = c('taz' = 'TAZ')) %>% 
  group_by(day_of_week, hour) %>% 
  mutate(combined.rides = pickups + dropoffs) %>% 
  arrange(desc(combined.rides)) %>% 
  transform(DayHour = paste(day_of_week, hour, sep="D")) 


#View(time.rideshare)
summary(time.rideshare$combined.rides)
ggplot(time.rideshare, aes(DayHour, combined.rides)) + geom_col() + ylab('Total Rides Per Day/Hour Combination')

#popular hours
pop.hours = time.rideshare %>% 
  group_by(hour) %>% 
  summarise(combined.rides= mean(combined.rides)) 
#plot popular hours
ggplot(pop.hours, aes(hour, combined.rides)) + geom_col(fill = 'lightblue') + geom_text(aes(label = round(combined.rides))) +
  ylab('Average # of Rides During Each Hour Per TAZ') + xlab('Hour')

#popular days
pop.days = time.rideshare %>% 
  group_by(day_of_week) %>% 
  summarise(combined.rides= mean(combined.rides))
ggplot(pop.days, aes(day_of_week, combined.rides)) + geom_col(fill = 'orange') + geom_text(aes(label = round(combined.rides))) +
  ylab('Average # of Rides During Each Day of Week Per TAZ') + xlab('Day of Week')


joined = convert_join(pop.rideshare)

#overlay plots
plot(taz.sf, axes = TRUE, border = 'lightgray')
plot(joined[[1]], col = 'darkgreen',add= TRUE,  pch= 13, axes = TRUE)



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
for (a in c(20)) {
for (i in 10:16){
  carmat = gmapsdistance(pop.rideshare$geoloc[(a-9):a], pop.rideshare$geoloc[(a-9):a], 
                         shape = 'long', mode = 'driving', departure = time_vec.int[i])    
  time.car[[i]]= carmat[[1]] %>% 
    cbind(Dep_Time.pst  = as.POSIXct(select.times[i]))
  print(i)
  Sys.sleep(3)
}
#   time.car.list[[a/10]] = time.car
#   print('--------')
 }
write.csv(rbindlist(time.car), 'car2.csv')

# all.car = rbind(rbindlist(time.car.list[[1]]), rbindlist(time.car.list[[2]]))
# write.csv(all.car, 'car_time.csv')
all.car = read.csv('car_time.csv') %>% 
  select(-c(X))

time.transit = list()
time.transit.list = list()
for (a in c(10,20)){
  for (j in 1:16){
    transitmat = gmapsdistance(pop.rideshare$geoloc[(a-9):a], pop.rideshare$geoloc[(a-9):a], key = key,
                               shape = 'long', mode = 'transit', departure = time_vec.int[j])
    time.transit[[j]]= transitmat[[1]] %>% 
      cbind(Dep_Time.pst  = as.POSIXct(select.times[j]))
    Sys.sleep(3)
    print(j)
  }
  time.transit.list[[a/10]] = time.transit
  print("------")
}


#make a large dataframe with all transit time
# time.transit.List1#has all times for geoloc[1:10]
#write.csv(rbindlist(time.transit.list[[2]]), 'transit1120.csv')
# write.csv(rbindlist(time.transit.List1), 'transit110.csv')
time.transit.final = rbind(rbindlist(time.transit.list[[1]]), rbindlist(time.transit.list[[2]]))
write.csv(time.transit.final, 'transit_time.csv')
all.transit = read.csv('transit_time.csv')

#calculate delta
df = all.car %>% 
  left_join(all.transit, by = c('or', 'de', 'Dep_Time.pst'))%>% 
  mutate(CarTime = Time.x, TransitTime = Time.y, delta = TransitTime - CarTime, min.delta = delta/60) %>% 
  filter(CarTime!=0, TransitTime !=0) %>% 
  arrange(desc(delta)) %>% 
  left_join(centroid2taz, by = c('or' = 'geoloc'))%>% 
  dplyr::select(-c(Time.x, Time.y, X), 
                Orig.Lat = Latitude, Orig.Lon = Longitude, Orig.taz = TAZ)

#join address to latlon and prepare data for heatmap
all.table = left_join(df, centroid2taz, by = c('de' = 'geoloc')) %>% 
  dplyr::select(Dest.taz = TAZ, Dest.Lat = Latitude, Dest.Lon = Longitude,everything(), -c(or, de))%>% 
  arrange(desc(delta))

#generate heatmap
ggplot(all.table, aes(as.factor(Orig.taz), as.factor(Dest.taz))) + geom_tile(aes(fill = min.delta)) +
  scale_fill_gradient(low = "white", high = 'magenta4') +
  ylab("Origin TAZ") +
  xlab("Destination TAZ") + labs(fill = "Minutes Longer", title= 'Public Transit vs Driving Trip Duration') +
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
