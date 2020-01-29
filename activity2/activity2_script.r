#Vector practice

#read in weather station file from the data folder
datW<- read.csv("y:\\Students\\cschwartz1\\a02\\2011124.csv")

#get more information about the dataframe
str(datW)

#specify a column with a proper date format
#to get the specific column it's dataframe$column
datW$dateF<-as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
#indicate that it should be treated as numeric data
datW$year<- as.numeric(format(datW$dateF, "%Y"))

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME=="ABERDEEN, WA US"])
#at first this results in NA because there is missing data
#use na.rm argument to set to ignore NA
mean(datW$TMAX[datW$NAME=="ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#halfway between min and max temperature
