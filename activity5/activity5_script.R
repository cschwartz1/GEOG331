#load in lubridate
library(lubridate)

#read in streamflow data from server
datH<-read.csv("y:\\Students\\cschwartz1\\a05\\stream_flow_data.csv",
               na.strings=c("Eqp"))

#read in from my computer
datH<-read.csv(file="/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity5/stream_flow_data.csv",
               na.strings=c("Eqp"))

head(datH)

#read in precipitation data
#hourly precipitation is in mm
#read in from server
datP<-read.csv("y:\\Students\\cschwartz1\\a05\\2049867.csv")
#read in from my computer
datP<-read.csv(file="/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity5/2049867.csv")
head(datP)

#subset data to use the most reliable measurements
datD<-datH[datH$discharge.flag=="A",]

####define time fot streamflow####
#convert date and time
datesD<-as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy<-yday(datesD)
#calculate year
datD$year<-year(datesD)
#define time
timesD<-hm(datD$time)

#### define time for precipiation ####
dateP<-ymd_hm(datP$DATE)
#get day of year
datP$doy<-yday(dateP)
#get year
datP$year<-year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))    

#plot discharge
plot(datD$decYear,datD$discharge,type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

####Basic Plot Formatting####

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#dev.new function starts a new plot window with a standard size
#mai function helps with margin so text is not cut off

#start new plot
dev.new(width=8, height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#bigger margins
dev.new(width=8, height=8)
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  

#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

#adding tick marks
#bigger margins
dev.new(width=8, height=8)
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle

#adding a legend
dev.new(width=8, height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

dev.new(width=8, height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
lines(obs2017,col="red")
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#####QUESTION 5#####
#discharge observations from 2017
obs2017<-datD$discharge[which(datD$year==2017)]

dev.new(width=8, height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
lines(datD$decDay[which(datD$year==2017)], obs2017,col="red")
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend(240, 80, c("mean","1 standard deviation", "2017 Observations"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"red"),#colors
       pch=c(NA,15,NA),#symbols
       bty="n")#no legend border

#####Question 7######
#dataframe that indicates what days have a full 24 hours of precipitation measurements
precip<-aggregate(datP$hour, by=list(datP$doy,datP$year),length)
colnames(precip)<-c("doy","year","hour_total")
precip24<-precip[which(precip$hour_total==24),]

#convert precip data to decimal year
precip24$decYear <- ifelse(leap_year(precip24$year),precip24$year + ((precip24$doy-1)/366),
                           precip24$year + ((precip24$doy-1)/365))

#plot of discharge measurements and symbolize days that have all precipitation measurements
dev.new(width=8, height=8)
#bigger margins
par(mai=c(1,1,1,1))
plot(datD$decYear,datD$discharge,type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)
points(precip24$decYear, y=rep(0,length(precip24$decYear)), pch=19,col="red")  #putting points below the graph
legend(2013.5,400, c("Discharge Measurement","Days with Full Precipitation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black","red"),#colors
       pch=c(NA,19),#symbols
       bty="n")#no legend border

###making a hydrograph 
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

###Question 8###
hydroD8 <- datD[datD$doy >= 357 & datD$doy < 359 & datD$year == 2012,]
hydroP8 <- datP[datP$doy >= 357 & datP$doy < 359 & datP$year == 2012,]
min(hydroD8$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl8 <- floor(min(hydroD8$discharge))-1
#celing rounds up to the integer
yh8 <- ceiling(max(hydroD8$discharge))+1
#minimum and maximum range of precipitation to plot
pl8 <- 0
pm8 <-  ceiling(max(hydroP8$HPCP))+.5
#scale precipitation to fit on the 
hydroP8$pscale <- (((yh8-yl8)/(pm8-pl8)) * hydroP8$HPCP) + yl8

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD8$decDay,
     hydroD8$discharge, 
     type="l", 
     ylim=c(yl8,yh8), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

for(i in 1:nrow(hydroP8)){
  polygon(c(hydroP8$decDay[i]-0.017,hydroP8$decDay[i]-0.017,
            hydroP8$decDay[i]+0.017,hydroP8$decDay[i]+0.017),
          c(yl8,hydroP8$pscale[i],hydroP8$pscale[i],yl8),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

####making box plots and violin plots######
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()


###Question 9####
#violin plot that shows streamflow over the seasons by each year (2016 and 2017)

#first make a separate data frame for data just from 2016
year16<-datD[which(datD$year==2016),]

#label what season each observation falls under
for (i in 1:length(year16$year)){
  if (year16$doy[i]<80 | year16$doy[i]>=357){
    year16$season16[i]<-"Winter"}
  else if(year16$doy[i]>=80 & year16$doy[i]<173){
    year16$season16[i]<-"Spring"}
  else if(year16$doy[i]>=266 & year16$doy[i]<357){
    year16$season16[i]<-"Fall"}
  else {year16$season16[i]<-"Summer"}
}

#violin plot
ggplot(data=year16, aes(season16,discharge))+
  geom_violin()+
  xlab("Seasons")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
  ggtitle("2016 Discharge by Season")

#dataframe for 2017
year17<-datD[which(datD$year==2017),]
#labeling what season each observation is 
for (i in 1:length(year17$doy)){
  if (year17$doy[i]<79 | year17$doy[i]>=356) {
    year17$season[i]<-"winter"
  } else if (year17$doy[i]>=79 & year17$doy[i]<173) {
    year17$season[i]<-"spring"
  } else if (year17$doy[i]>=173 & year17$doy[i]<265) {
    year17$season[i]<-"summer"
  } else {
    year17$season[i]<-"fall"
  }
}
#violin plot
ggplot(data=year17, aes(season,discharge))+
  geom_violin()+
  xlab("Seasons")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
  ggtitle("2017 Discharge by Season")
   