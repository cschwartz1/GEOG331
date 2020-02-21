#create a function
assert<-function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement==FALSE){
    print(err.message)
  }
}
#check how the statement works
#evaluate a false statement
assert(1==2, "error: unequal values")

#evaluate a true statement
assert(2==2, "error: unequal values")
#set up assert to check if two vectors are the same length
a<-c(1,2,3,4)
b<-c(8,4,5)
assert(length(a)==length(b), "error: unequal length")

#reading in the data file
#skip the first 3 rows since there is additional column info
#specify that the NA is designated differently 

##reading in from server
#datW<-read.csv("y:\\Students\\cschwartz1\\a03\\bewkes_weather.csv",
              # na.strings=c("#N/A"),skip=3, header=FALSE)

#reading in from my computer
datW<- read.csv(file="/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity3/bewkes_weather.csv",
                na.strings=c("#N/A"),skip=3,header=FALSE)
#preview the data
print(datW[1,])

#get sensor info from the file
#this data table will contain all relevent units
#reading in from server
#sensorInfo<-read.csv("y:\\Students\\cschwartz1\\a03\\bewkes_weather.csv",
                     #na.strings=c("#N/A"),nrows=2)

#reading in from my computer
sensorInfo<-read.csv(file="/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity3/bewkes_weather.csv",
                     na.strings=c("#N/A"),nrows=2)
print(sensorInfo)

#get column names from sensorInfo table
#set weather station colnames to be the same
colnames(datW)<-colnames(sensorInfo)

#preview data
print(datW[1,])

#data is from june 12 to july 26 2018
#use install.packages to install lubridate
#install.packages(c("lubridate"))
library(lubridate)
#convert to standardized format
#date format is m/d/y
dates<-mdy_hm(datW$timestamp, tz="America/New_York")
#calculate day of year
datW$doy<-yday(dates)
#calculate hour in the day
datW$hour<-hour(dates)+(minute(dates)/60)
#calculate decimal day of year
datW$DD<-datW$doy+(datW$hour/24)
#quick preview of the new date calculations
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
###no missing data for weather station sensors!!
#soil moisture
length(which(is.na(datW$soil.moisture)))
#soil temperature
length(which(is.na(datW$soil.temp)))
#many missing values from the soil sensor: 707
#make a plot with fill in points (using pch)
plot(datW$DD, datW$soil.moisture,pch=19,type="b",xlab="Day of Year",
     ylab="Soil Moisture (cm3 water per cm3 soil)")
#NA is not plotted in this plot
######setting up tests for QA/QC
#make a plot with filled in points (using pch) looking at air temp
plot(datW$DD,datW$air.temperature,pch=19,type="b",xlab="Day of Year",
     ylab="Air Temperature (degrees C)")

#make a new column to work with that indicates conducting QA/QC
#ifelse function
#first argument is a logical statement to be evaluated as true or fase on a vector
#second argument is value that air.tempQ1 column will be given if statement is true
#last value is value given to air.tempQ1 if statement is false
datW$air.tempQ1<-ifelse(datW$air.temperature<0,NA,datW$air.temperature)
#check values at extreme range of the data and throughout percentiles
quantile(datW$air.tempQ1)
#look at days with really low air temperature
datW[datW$air.tempQ1<8,]
#look at days with really high air temperature
datW[datW$air.tempQ1>33,]

#####measurements outside of sensor capabilities
#plot precipitation and lightning strikes on the same plot
#normalize lightning strikes to match precipitation
lightscale<-(max(datW$precipitation)/max(datW$lightning.acvitivy))*datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD, datW$precipitation,xlab="Day of Year", ylab="Precipitation & Lightning", type="n")
#plot precipitation points only when there is precipitation
#make the points semi-transparent
points(datW$DD[datW$precipitation>0],datW$precipitation[datW$precipitation>0],
       col=rgb(95/255,158/255,160/255,.5),pch=15)
#plot lightning points only when there is lightning
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

####QUESTION 5#####

assert(length(lightscale)==length(datW$DD), "error: unequal length")
assert(length(datW$precipitation)==length(datW$DD), "error: unequal length")

##############
#filter out storms in wind and air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5mm
#create a new air temp column
datW$air.tempQ2<-ifelse(datW$precipitation>=2 & datW$lightning.acvitivy>0, NA,
                        ifelse(datW$precipitation>5, NA, datW$air.tempQ1))

####QUESTION 6######
#first I want to do some visual checks before the filtering
plot(datW$DD, datW$wind.speed, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind Speed")
#potential spike at end of summer
#look at realistic values
quantile(datW$wind.speed)
#look at days with really low wind speed
datW[datW$wind.speed<0.10, ]
#look at days with really high wind speed
datW[datW$wind.speed>1.5, ]
#now filtering out storms in the wind measurements
datW$wind.speedQ6<-ifelse(datW$precipitation>=2 & datW$lightning.acvitivy>0, NA,
                        ifelse(datW$precipitation>5, NA, datW$wind.speed))
#checking that the data was filtered as expected
#the length of the missing air temperature and wind speed values should be the same, so check that
assert(length(which(is.na(datW$air.tempQ2)))==length(which(is.na(datW$wind.speedQ6))), "error: unequal length")

#new plot
plot(datW$DD, datW$wind.speedQ6, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind Speed (ms)")

####QUESTION 7######
#first do a visual check of the data for soil moisture
par(mfrow=c(1,2))
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Moisture (m^3 per m^3)")
#now visual check for soil temperature
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temperature (degrees C)")

#plots for days leading up to outage for soil moisture and precipitation
plot(datW$DD[which(datW$doy>185 & datW$doy<192)], datW$soil.moisture[which(datW$doy>185 & datW$doy<192)], pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Moisture (m^3 per m^3)")
plot(datW$DD[which(datW$doy>185 & datW$doy<192)], datW$precipitation[which(datW$doy>185 & datW$doy<192)], pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")

#checking the values to the days leading up
quantile(datW$soil.moisture[which(datW$doy>185 & datW$doy<192)],na.rm=TRUE)
quantile(datW$precipitation[which(datW$doy>185 & datW$doy<192)],na.rm=TRUE)

#days leading up to outage for soil temperature and air temperature
plot(datW$DD[which(datW$doy>185 & datW$doy<192)], datW$soil.temp[which(datW$doy>185 & datW$doy<192)], pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temperature (degrees C)")
plot(datW$DD[which(datW$doy>185 & datW$doy<192)], datW$air.temperature[which(datW$doy>185 & datW$doy<192)], pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temperature (degrees C)")

#checking values leading up to the outage
quantile(datW$soil.temp[which(datW$doy>185 & datW$doy<192)],na.rm=TRUE)
quantile(datW$air.tempQ2[which(datW$doy>185 & datW$doy<192)],na.rm=TRUE)

####providing results to the researchers###

####QUESTION 8######
#gathering information for the table

#air temperature
#average air temp
mean(datW$air.tempQ2,na.rm=TRUE)
#see how many observations are missing to figure out number of obs that went into calculations
a1<-length(datW$air.tempQ2)  #number of original observations
a2<-length(which(is.na(datW$air.tempQ2)))  #number of missing values taken out due to unreliability
a1-a2  #number of observations in calculation
#measurements were taken everyday 
max(datW$doy)-min(datW$doy)

#wind speed
mean(datW$wind.speedQ6,na.rm=TRUE)
#number of observations calculations
w1<-length(datW$wind.speedQ6)
w2<-length(which(is.na(datW$wind.speedQ6)))
w1-w2 #same number as air temperature
#same time period as air temperature

#soil moisture
mean(datW$soil.moisture,na.rm=TRUE)
length(datW$soil.moisture)-length(which(is.na(datW$soil.moisture)))
192.8542-163.4792 #last day of observation and the first day, information from the dataframe

#soil temperature
mean(datW$soil.temp,na.rm=TRUE)
#same number of observations and time period as soil moisture

#precipitation
sum(datW$precipitation)
length(datW$precipitation)
#observations were made everyday and no missing values

###QUESTION 9#####
par(mfrow=c(2,2))
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Moisture (m^3 per m^3)")
plot(datW$DD, datW$air.tempQ2, pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temperature (degrees C)")
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temperature (degrees C)")

