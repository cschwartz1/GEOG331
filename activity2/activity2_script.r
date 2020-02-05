#####Vector practice
 
#making a vector of tree heights in meters
heights<-c(30,41,20,22)
#convert to cm
heights_cm<-heights*100
#looking at the 2nd and 3rd tree heights
heights[2:3]

###Matrix practice
#setting up a matrix with 2 columns and fill in by rows
Mat<-matrix(c(1,2,3,4,5,6),ncol=2,byrow=TRUE)
#setting up a matrix that fills in by columns
Mat.by.col<-matrix(c(1,2,3,4,5,6),ncol=2,byrow=FALSE)

########beginning the activity #############

#read in weather station file from the data folder
#reading from school computer and server
#datW<- read.csv("y:\\Students\\cschwartz1\\a02\\2011124.csv")

#reading in weather station file from my computer
datW<- read.csv(file="/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/2011124.csv")

#get more information about the dataframe
str(datW)

#specify a column with a proper date format
#to get the specific column it's dataframe$column
datW$dateF<-as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
#indicate that it should be treated as numeric data
datW$year<- as.numeric(format(datW$dateF, "%Y"))

########################QUESTION 2###################
#create an example vector of each data type with 5 objects in it
#data types: character, numeric, integer, factor data
#charcter vector
char<-c("colgate","raiders","thirteen","university","college")
#check if it is a character vector
mode(char)
str(char)
#numeric vector
num<-c(1,2,3,4,5)
mode(num)
str(num)
#integer vector
int<-as.integer(c(2,4,6,8,10))
typeof(int)
str(int)
#factor vector
fact<-as.factor(c("pineapple","apple","orange","banana","strawberry"))
str(fact)

#######Descriptive Statistics and Histograms######
#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME=="ABERDEEN, WA US"])
#at first this results in NA because there is missing data
#use na.rm argument to set to ignore NA
mean(datW$TMAX[datW$NAME=="ABERDEEN, WA US"], na.rm=TRUE)
#average daily maximum temperature for Aberdeen is 14.68 degree C

#calculate the average daily temperature
#halfway between min and max temperature
datW$TAVE<-datW$TMIN+ ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#by function is a list of one or more variables to index over
#FUN indicates the function we want to use
#use a comma and add arguments after function
averageTemp<-aggregate(datW$TAVE, by=list(datW$NAME),FUN="mean",na.rm=TRUE)
averageTemp
#change the automatic output of column names to be more meaningful
#MAAT --> mean annual air temperature
colnames(averageTemp)<-c("NAME","MAAT")
averageTemp
#convert level to number for factor data type
datW$siteN<-as.numeric(datW$NAME)
#histogram for the first site in our levels

hist(datW$TAVE[datW$siteN==1],
     freq=FALSE,
     main=paste(levels(datW$NAME)[1]),
     xlab="Average daily temperature (degrees C)",
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickeness of 3
abline(v=mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
       col="tomato3",
       lwd=3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness 3
abline(v=mean(datW$TAVE[datW$site==1],na.rm=TRUE)-sd(datW$TAVE[datW$siteN==1],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness 3
abline(v=mean(datW$TAVE[datW$site==1],na.rm=TRUE)+sd(datW$TAVE[datW$siteN==1],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)

###############QUESTION 4###############
par(mfrow=c(2,2))
hist(datW$TAVE[datW$siteN==1],
     freq=FALSE,
     main=paste(levels(datW$NAME)[1]),
     xlab="Average daily temperature (degrees C)",
     ylab="Relative frequency",
     col="grey50",
     border="white")
#first site histogram other than Aberdeen
hist(datW$TAVE[datW$siteN==2],
     freq=FALSE,
     main=paste(levels(datW$NAME)[2]),
     xlab="Average daily temperature (degrees C)",
     ylab="Relative frequency",
     col="blue",
     border="white")
abline(v=mean(datW$TAVE[datW$siteN==2],na.rm=TRUE),
       col="tomato3",
       lwd=3)
abline(v=mean(datW$TAVE[datW$site==2],na.rm=TRUE)-sd(datW$TAVE[datW$siteN==2],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)
abline(v=mean(datW$TAVE[datW$site==2],na.rm=TRUE)+sd(datW$TAVE[datW$siteN==2],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)

#second histogram
hist(datW$TAVE[datW$siteN==4],
     freq=FALSE,
     main=paste(levels(datW$NAME)[4]),
     xlab="Average daily temperature (degrees C)",
     ylab="Relative frequency",
     col="red",
     border="white")
abline(v=mean(datW$TAVE[datW$siteN==4],na.rm=TRUE),
       col="tomato3",
       lwd=3)
abline(v=mean(datW$TAVE[datW$site==4],na.rm=TRUE)-sd(datW$TAVE[datW$siteN==4],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)
abline(v=mean(datW$TAVE[datW$site==4],na.rm=TRUE)+sd(datW$TAVE[datW$siteN==4],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)
#third histogram
hist(datW$TAVE[datW$siteN==3],
     freq=FALSE,
     main=paste(levels(datW$NAME)[3]),
     xlab="Average daily temperature (degrees C)",
     ylab="Relative frequency",
     col="yellow",
     border="white")
abline(v=mean(datW$TAVE[datW$siteN==3],na.rm=TRUE),
       col="tomato3",
       lwd=3)
abline(v=mean(datW$TAVE[datW$site==3],na.rm=TRUE)-sd(datW$TAVE[datW$siteN==3],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)
abline(v=mean(datW$TAVE[datW$site==3],na.rm=TRUE)+sd(datW$TAVE[datW$siteN==3],na.rm=TRUE),
       col="tomato3",
       lty=3,
       lwd=3)

####probability distributions####
par(mfrow=c(1,1))  #changing back to one graph view mode
h1<-hist(datW$TAVE[datW$siteN==1],
         freq=FALSE,
         main=paste(levels(datW$NAME)[1]),
         xlab="Average daily temperature (degrees C)",
         ylab="Relative frequency",
         col="grey50",
         border="white")
#seq function --> sequence of numbers we can use to plot normal across range of
#temperature values
x.plot<-seq(-10,30,length.out=100)
#dnorm creates probability density based on mean and sd
y.plot<-dnorm(seq(-10,30,length.out=100),
              mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
              sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
y.scaled<-(max(h1$density)/max(y.plot))*y.plot
#points function adds points or lines to a graph
#first 2 arguments are x and y coordinates
points(x.plot,
       y.scaled,
       type="l",
       col="royalblue3",
       lwd=4,
       lty=2)

help(dnorm)
pnorm(0,
      mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#pnorm with 5 gives probability below 5
pnorm(5,
      mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#pnorm for 0-5
pnorm(5,
      mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE)) - pnorm(0,
      mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#subtracting pnorm from 1 gives area above a number
1-pnorm(20,
        mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
        sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#qnorm returns value associated with a probability
qnorm(0.95,
      mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))

######QUESTION 6##############
qnorm(0.95,
      mean(datW$TAVE[datW$siteN==1]+4,na.rm=TRUE),
      sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))
#This is the value associated with 95%
#now let's look at probability above that value
1-pnorm(22.51026,
        mean(datW$TAVE[datW$siteN==1]+4,na.rm=TRUE),
        sd(datW$TAVE[datW$siteN==1],na.rm=TRUE))


######QUESTION 7###############
hist(datW$PRCP[datW$siteN==1],
     freq=FALSE,
     main=paste(levels(datW$NAME)[1]),
     xlab="Daily Precipitation",
     ylab="Relative frequency",
     col="grey50",
     border="white")

######QUESTION 8##############
precip<-aggregate(datW$PRCP, by=list(datW$year,datW$NAME),FUN="sum",na.rm=TRUE)
colnames(precip)<-c("Year","Name","Precipitation for Year")
#histogram of annual precipitation for Aberdeen

hist(precip$`Precipitation for Year`[precip$Name=="ABERDEEN, WA US"],
     freq=FALSE,
     main=paste(levels(datW$NAME)[1]),
     xlab="Annual Precipitation",
     ylab="Relative frequency",
     col="grey50",
     border="white")

#####QUESTION 9#############
#this first aggregate function calculates the mean daily preciptation value for each site
averagePrecip<-aggregate(datW$PRCP,by=list(datW$NAME),FUN="mean",na.rm=TRUE)
colnames(averagePrecip)<-c("Name","Mean Daily Precipitation")
#calculates mean annual precipitation at each site
averagePrecip1<-aggregate(precip$`Precipitation for Year`, by=list(precip$Name),FUN="mean",na.rm=TRUE)
colnames(averagePrecip1)<-c("Name","Mean Annual Precipitation")

