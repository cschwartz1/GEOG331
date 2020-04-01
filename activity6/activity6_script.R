#loading necessary packages for activity 
library(raster)
library(sp)  #working with vector data
library(rgdal)  
library(rgeos)  #vector operations
library(plyr)  #old version of dplyr

#NDVI --> normalized difference vegetation index - range from 0 to 1

#read in the shapefiles
#readOGR in rgdal does this
#g1966 <- readOGR("Y:\\Students\\cschwartz1\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
#g1998 <- readOGR("Y:\\Students\\cschwartz1\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
#g2005 <- readOGR("Y:\\Students\\cschwartz1\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
#g2015 <- readOGR("Y:\\Students\\cschwartz1\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

g1966 <- readOGR("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/GNPglaciers/GNPglaciers_1966.shp")
g1998 <- readOGR("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/GNPglaciers/GNPglaciers_2015.shp")

#39 features for each shapefile

str(g2015)
#data stores all accompanying info/measurements for each spatial object
head(g2015@data)  
#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]
g1966@proj4string

#looking at 1966 glaciers by glacier name
spplot(g1966,"GLACNAME")  #spplot allows to map vector data and show different colors for a data value

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME
#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#####Working with raster data#######
#read in rgb imagery from landsat
redL <- raster("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs  #same coordinate system as vector data - can plot them together
#brick - series of raster files with same extent and resolution- helps with overlaying
#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)   #plots a color image of the raster
#add polygons to the plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#zoom in on specific areas
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in 
ndviYear<-seq(2003,2016)
#read all files into a list
NDVIraster<- list()
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("/Users/carlyschwartz/Documents/Colgate/ColgateRound4/GEOG331/activity6/NDVI/NDVI_",ndviYear[i],".tif"))
  
}
str(NDVIraster[[1]])
#get projection
NDVIraster[[1]]@crs   #Lambert Azimuthal Equal Area projection
par(mai=c(1,1,1,1))
plot(NDVIraster[[1]])

g1966@polygons[[1]]@Polygons[[1]]

plot(g1966,axes=TRUE)

#######QUESTION 3############
plot(NDVIraster[[1]])
plot(g1966,axes=TRUE)

par(mfrow=c(1,2))
#par(mai=c(1,1,1,1))
plot(NDVIraster[[1]])
plot(g1966, axes=TRUE)
xaxs="i"
yaxs="i"

###vector data analysis: glacier retreat###
#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

####QUESTION 4######
#NDVI 2015 data is NDVI[[13]]
par(mfrow=c(1,1))
plot(NDVIraster[[13]], axes=FALSE) 
plot(g2015p, col="transparent", border="black", add=TRUE)

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)
#join data together into table
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")
#make a plot of the area for each glacier using the table
par(mai=c(1,1,1,1))
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   

#####QUESTION 5######
#percent change in area between 1966 and 2015
percent_change<-list()
for (i in 1:39){
  percent_change[[i]] <- ((gAll$a2015m.sq[i]-gAll$a1966m.sq[i])/gAll$a1966m.sq[i])*100
}

g2015@data$percent_change <- percent_change
spplot(g2015, "percent_change")

diffPoly<-gDifference(g1966p, g2015p)
plot(diffPoly)
#glacier retreat, all areas of glacial loss in black
#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

####QUESTION 6####
#find the glacier with the largest % loss
percent_change<-as.numeric(percent_change)
#the glacier with the biggest negative percent change means the most loss
c<-min(percent_change) #now find the index
which(percent_change==c)  #index 5
#find which glacier is index 5 and check it is the same for each year
g1966@data$GLACNAME[5]
g1998@data$GLACNAME[5]
g2005@data$GLACNAME[5]
g2015@data$GLACNAME[5]

#find coordinates of glacier to zoom in on for map
g1966@data$X_COORD[5]
g2015@data$X_COORD[5]

g1966@data$Y_COORD[5]
g2015@data$Y_COORD[5]

par(mai=c(1,1,1,1))
plotRGB(rgbL, ext=c(273200,274830,5426400,5428800), stretch="lin", axes=TRUE, main="Boulder Glacier with 84.72067 Percent Loss")   #plots a color image of the raster
#add polygons to the plot
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

####Raster data analysis: does more vegetation grow with glacial retreat?####
#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

#examine change in vegetation around the glaciers
#see if there are any individual differences in each glacier
#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize glaciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#stats for rate of vegetation change in area around rasterized buffer using zonal stats
meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

####QUESTION 9####
#remove zone 0
meanChange<-meanChange[-1,]
g2015@data$meanChange <- meanChange[,2]
spplot(g2015, "meanChange")

###QUESTION 10###
meanChange[,2]*15
max(meanChange[,2]*15)
min(meanChange[,2]*15)
quantile(meanChange[,2]*15)

###QUESTION 11####
mean(meanChange[,2])  
#first way i thought to calculate the raster
sum_NDVI<-NDVIraster[[1]]
for(i in 2:length(NDVIraster)){
  sum_NDVI<- sum_NDVI + NDVIraster[[i]]
}

average_NDVI <- sum_NDVI/14  #raster dataset of average maximum NDVI across all years 
plot(average_NDVI) 

#second way I thought to calculate the raster
NDVI_average<-calc(NDVIstack, mean, na.rm=TRUE)
plot(NDVI_average)  

average_buffer <- zonal(NDVI_average, 
                    glacZones,
                    "mean")
average_buffer<-average_buffer[-1,]

g2015p@data$average_buffer <- average_buffer[,2]

#assigning colors
for(i in 1:39){
if(g2015p@data$average_buffer[i]<0.6 & g2015p@data$average_buffer[i]>0.377){
  g2015p@data$NDVIcol[i]<-"light blue"
} else if(g2015p@data$average_buffer[i]<0.377 & g2015p@data$average_buffer[i]> 0.33){
  g2015p@data$NDVIcol[i]<-"dark blue"
} else if(g2015p@data$average_buffer[i]<0.33 & g2015p@data$average_buffer[i]>0.26){
  g2015p@data$NDVIcol[i]<-"red"
} else{g2015p@data$NDVIcol[i]<-"purple"}
}

#plotting the map
par(mai=c(1,1,1,1))
plot(NDVI_average)
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)
legend("topright",title="Average NDVI", c("0.377-0.6","0.33-0.377","0.26-0.33","<0.26"), 
       col=c("light blue","dark blue","red","purple"))


