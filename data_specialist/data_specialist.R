#loading data into R from my computer
q4_data<-read.csv(file="/Users/hiding_my_name/Documents/Groupon/Q4_raw_data.csv",header=TRUE)

q42013_data <- Q4_raw_data   #renaming the dataframe

table(q42013_data$Segment)    #how many active deals are in each segment for Q4

#splitting the data into segments (each segment becomes its own dataframe)
q4_goods<- q42013_data[which(q42013_data$Segment=="Goods"),] 
q4_local<- q42013_data[which(q42013_data$Segment=="Local"),]
q4_travel<- q42013_data[which(q42013_data$Segment=="Travel"),]

##############################LOCAL DATA#####################

#check for duplicates 
dup_local<-duplicated(q4_local)
table(dup_local)
dup_local_deals<-duplicated(q4_local$`Deal ID`)   #checking if any deals were recorded twice
table(dup_local_deals)

sum(q4_local$Billings)    #initial sum
sum(q4_local$`Units Sold`)   

q4_local$year<-year(q4_local$`Start Date`)    #splitting the date into day, month, year so it is easier to work with
q4_local$month<-month(q4_local$`Start Date`)  #and easier to look at data from certain days, months, etc
q4_local$day<-day(q4_local$`Start Date`)      #this is adding a new "vector" to the dataframe

#finding number of deals started in november and december 2013
nov_local<-0
for (i in 1:120576){          #i can see in the dataframe there are 120576 observations, which means this is the number of active deals in the quarter for the local segment
  if (q4_local$month[i]==11 & q4_local$year[i]==2013){
    nov_local<-nov_local+1
  }
}

dec_local<-0
for (i in 1:120576){
  if (q4_local$month[i]==12 & q4_local$year[i]==2013){
    dec_local<-dec_local+1
  }
}

oct_local_NA<-0 #num of deals started in october with no adjustments (missing oct 20-30)
for (i in 1:120576){
  if (q4_local$month[i]==10 & q4_local$year[i]==2013){
    oct_local_NA<-oct_local_NA+1
  }
}

#finding the sum of gross billings made my deals started in Q4 (ex how much did deals started oct 1 bill?)
oct_local_billings<-NA    #i create vectors so each element in the vector represents the day of the month
oct_local_units<-NA       #for example, the 2nd element of the vectors represents data for deals started on Oct 2
oct_local_deals<-NA
for (g in 1:31){
  num<-which(q4_local$year==2013 & q4_local$month==10 & q4_local$day==g)
  oct_local_deals[g]<-length(num)
  billing_octlocal<-0
  units_octlocal<-0
  if (length(num)==0){
    oct_local_billings[g]<-0
    oct_local_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_octlocal<-billing_octlocal+q4_local$Billings[num[k]]
    oct_local_billings[g]<-billing_octlocal
    units_octlocal<-units_octlocal+q4_local$`Units Sold`[num[k]]
    oct_local_units[g]<-units_octlocal
  }
}

nov_local_billings<-NA
nov_local_units<-NA
nov_local_deals<-NA
for (g in 1:30){
  num<-which(q4_local$year==2013 & q4_local$month==11 & q4_local$day==g)
  nov_local_deals[g]<-length(num)
  billing_novlocal<-0
  units_novlocal<-0
  for (k in 1:length(num)){
    billing_novlocal<-billing_novlocal+q4_local$Billings[num[k]]
    nov_local_billings[g]<-billing_novlocal
    units_novlocal<-units_novlocal+q4_local$`Units Sold`[num[k]]
    nov_local_units[g]<-units_novlocal
  }
}

dec_local_billings<-NA
dec_local_units<-NA
dec_local_deals<-NA
for (g in 1:31){
  num<-which(q4_local$year==2013 & q4_local$month==12 & q4_local$day==g)
  dec_local_deals[g]<-length(num)
  billing_declocal<-0
  units_declocal<-0
  for (k in 1:length(num)){
    billing_declocal<-billing_declocal+q4_local$Billings[num[k]]
    dec_local_billings[g]<-billing_declocal
    units_declocal<-units_declocal+q4_local$`Units Sold`[num[k]]
    dec_local_units[g]<-units_declocal
  }
}


####################################GOODS
dup_goods<-duplicated(q4_goods)
table(dup_goods)
dup_goods_deals<-duplicated(q4_goods$`Deal ID`)
table(dup_goods_deals)

sum(q4_goods$Billings)
sum(q4_goods$`Units Sold`)

q4_goods$year<-year(q4_goods$`Start Date`)
q4_goods$month<-month(q4_goods$`Start Date`)
q4_goods$day<-day(q4_goods$`Start Date`)

#finding number of deals started in october, november, december 2013
nov_goods<-0
for (i in 1:15234){
  if (q4_goods$month[i]==11 & q4_goods$year[i]==2013){
    nov_goods<-nov_goods+1
  }
}

dec_goods<-0
for (i in 1:15234){
  if (q4_goods$month[i]==12 & q4_goods$year[i]==2013){
    dec_goods<-dec_goods+1
  }
}

oct_goods<-0 
for (i in 1:15234){
  if (q4_goods$month[i]==10 & q4_goods$year[i]==2013){
    oct_goods<-oct_goods+1
  }
}

oct_goods_billings<-NA
oct_goods_units<-NA
oct_goods_deals<-NA
for (g in 1:31){
  num<-which(q4_goods$year==2013 & q4_goods$month==10 & q4_goods$day==g)
  oct_goods_deals[g]<-length(num)
  billing_octgoods<-0
  units_octgoods<-0
  for (k in 1:length(num)){
    billing_octgoods<-billing_octgoods+q4_goods$Billings[num[k]]
    oct_goods_billings[g]<-billing_octgoods
    units_octgoods<-units_octgoods+q4_goods$`Units Sold`[num[k]]
    oct_goods_units[g]<-units_octgoods
  }
}

nov_goods_billings<-NA
nov_goods_units<-NA
nov_goods_deals<-NA
for (g in 1:30){
  num<-which(q4_goods$year==2013 & q4_goods$month==11 & q4_goods$day==g)
  nov_goods_deals[g]<-length(num)
  billing_novgoods<-0
  units_novgoods<-0
  for (k in 1:length(num)){
    billing_novgoods<-billing_novgoods+q4_goods$Billings[num[k]]
    nov_goods_billings[g]<-billing_novgoods
    units_novgoods<-units_novgoods+q4_goods$`Units Sold`[num[k]]
    nov_goods_units[g]<-units_novgoods
  }
}

dec_goods_billings<-NA
dec_goods_units<-NA
dec_goods_deals<-NA
for (g in 1:31){
  num<-which(q4_goods$year==2013 & q4_goods$month==12 & q4_goods$day==g)
  dec_goods_deals[g]<-length(num)
  billing_decgoods<-0
  units_decgoods<-0
  for (k in 1:length(num)){
    billing_decgoods<-billing_decgoods+q4_goods$Billings[num[k]]
    dec_goods_billings[g]<-billing_decgoods
    units_decgoods<-units_decgoods+q4_goods$`Units Sold`[num[k]]
    dec_goods_units[g]<-units_decgoods
  }
}

####looking at how deals started in the previous quarter behave in Q4
sep_goods_billings<-NA
sep_goods_units<-NA
sep_goods_deals<-NA
for (g in 1:30){
  num<-which(q4_goods$year==2013 & q4_goods$month==9 & q4_goods$day==g)
  sep_goods_deals[g]<-length(num)
  billing_sepgoods<-0
  units_sepgoods<-0
  if (length(num)==0){
    sep_goods_billings[g]<-0
    sep_goods_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_sepgoods<-billing_sepgoods+q4_goods$Billings[num[k]]
    sep_goods_billings[g]<-billing_sepgoods
    units_sepgoods<-units_sepgoods+q4_goods$`Units Sold`[num[k]]
    sep_goods_units[g]<-units_sepgoods
  }
}

aug_goods_billings<-NA
aug_goods_units<-NA
aug_goods_deals<-NA
for (g in 1:31){
  num<-which(q4_goods$year==2013 & q4_goods$month==8 & q4_goods$day==g)
  aug_goods_deals[g]<-length(num)
  billing_auggoods<-0
  units_auggoods<-0
  if (length(num)==0){
    aug_goods_billings[g]<-0
    aug_goods_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_auggoods<-billing_auggoods+q4_goods$Billings[num[k]]
    aug_goods_billings[g]<-billing_auggoods
    units_auggoods<-units_auggoods+q4_goods$`Units Sold`[num[k]]
    aug_goods_units[g]<-units_auggoods
  }
}

plot(sep_goods_billings, type="o",col="blue",xlab="September",ylab="Gross Billings",main="Gross Billings for September Deals Still Active")
plot(aug_goods_billings, type="o",col="blue",xlab="August",ylab="Gross Billings",main="Gross Billings for August Deals Still Active")

####################################TRAVEL
dup_travel<-duplicated(q4_travel)
table(dup_travel)
dup_travel_deals<-duplicated(q4_travel$`Deal ID`)
table(dup_travel_deals)

sum(q4_travel$Billings)
sum(q4_travel$`Units Sold`)

q4_travel$year<-year(q4_travel$`Start Date`)
q4_travel$month<-month(q4_travel$`Start Date`)
q4_travel$day<-day(q4_travel$`Start Date`)

#finding number of deals started in october, november, december 2013
nov_travel<-0
for (i in 1:2724){
  if (q4_travel$month[i]==11 & q4_travel$year[i]==2013){
    nov_travel<-nov_travel+1
  }
}

dec_travel<-0
for (i in 1:2724){
  if (q4_travel$month[i]==12 & q4_travel$year[i]==2013){
    dec_travel<-dec_travel+1
  }
}

oct_travel<-0 
for (i in 1:2724){
  if (q4_travel$month[i]==10 & q4_travel$year[i]==2013){
    oct_travel<-oct_travel+1
  }
}

oct_travel_billings<-NA
oct_travel_units<-NA
oct_travel_deals<-NA
for (g in 1:31){
  num<-which(q4_travel$year==2013 & q4_travel$month==10 & q4_travel$day==g)
  oct_travel_deals[g]<-length(num)
  billing_oct_travel<-0
  units_oct_travel<-0
  if (length(num)==0){
    oct_travel_billings[g]<-0
    oct_travel_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_oct_travel<-billing_oct_travel+q4_travel$Billings[num[k]]
    oct_travel_billings[g]<-billing_oct_travel
    units_oct_travel<-units_oct_travel+q4_travel$`Units Sold`[num[k]]
    oct_travel_units[g]<-units_oct_travel
  }
}

nov_travel_billings<-NA
nov_travel_units<-NA
nov_travel_deals<-NA
for (g in 1:30){
  num<-which(q4_travel$year==2013 & q4_travel$month==11 & q4_travel$day==g)
  nov_travel_deals[g]<-length(num)
  billing_novtravel<-0
  units_novtravel<-0
  if (length(num)==0){
    nov_travel_billings[g]<-0
    nov_travel_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_novtravel<-billing_novtravel+q4_travel$Billings[num[k]]
    nov_travel_billings[g]<-billing_novtravel
    units_novtravel<-units_novtravel+q4_travel$`Units Sold`[num[k]]
    nov_travel_units[g]<-units_novtravel
  }
}

dec_travel_billings<-NA
dec_travel_units<-NA
dec_travel_deals<-NA
for (g in 1:31){
  num<-which(q4_travel$year==2013 & q4_travel$month==12 & q4_travel$day==g)
  dec_travel_deals[g]<-length(num)
  billing_dectravel<-0
  units_dectravel<-0
  if (length(num)==0){
    dec_travel_billings[g]<-0
    dec_travel_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_dectravel<-billing_dectravel+q4_travel$Billings[num[k]]
    dec_travel_billings[g]<-billing_dectravel
    units_dectravel<-units_dectravel+q4_travel$`Units Sold`[num[k]]
    dec_travel_units[g]<-units_dectravel
  }
}

length(which(oct_travel_deals==0))   #15 days in october with no new deals

boxplot(oct_travel_deals,ylab="Number of Deals",main="Distribution of New Deals Starting in October")
travel_deals<-c(oct_travel_deals,nov_travel_deals,dec_travel_deals)  
length(which(travel_deals==0))     #18 days in Q4 have no new travel deals!
boxplot(travel_deals,ylab="Number of Deals",main="Distribution of New Deals Starting in Q4")
median(travel_deals)

#finding number of deals that were negative GB (gross billings) vs positive GB vs no GB
negative_deals<-0     #4374
positive_deals<-0     #107386
no_money_deals<-0     #26774
for (i in 1:138534){
  if (q42013_data$Billings[i]<0){
    negative_deals<-negative_deals+1
  }
  if (q42013_data$Billings[i]>0){
    positive_deals<-positive_deals+1
  }
  if (q42013_data$Billings[i]==0){
    no_money_deals<-no_money_deals+1
  }
}

####looking at how deals started in the previous quarter behave in Q4
aug_travel_billings<-NA
aug_travel_units<-NA
aug_travel_deals<-NA
for (g in 1:31){
  num<-which(q4_travel$year==2013 & q4_travel$month==8 & q4_travel$day==g)
  aug_travel_deals[g]<-length(num)
  billing_augtravel<-0
  units_augtravel<-0
  if (length(num)==0){
    aug_travel_billings[g]<-0
    aug_travel_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_augtravel<-billing_augtravel+q4_travel$Billings[num[k]]
    aug_travel_billings[g]<-billing_augtravel
    units_augtravel<-units_augtravel+q4_travel$`Units Sold`[num[k]]
    aug_travel_units[g]<-units_augtravel
  }
}


sep_travel_billings<-NA
sep_travel_units<-NA
sep_travel_deals<-NA
for (g in 1:30){
  num<-which(q4_travel$year==2013 & q4_travel$month==9 & q4_travel$day==g)
  sep_travel_deals[g]<-length(num)
  billing_septravel<-0
  units_septravel<-0
  if (length(num)==0){
    sep_travel_billings[g]<-0
    sep_travel_units[g]<-0
    next                          #moves to next iteration if no deals were started that day
  }
  for (k in 1:length(num)){
    billing_septravel<-billing_septravel+q4_travel$Billings[num[k]]
    sep_travel_billings[g]<-billing_septravel
    units_septravel<-units_septravel+q4_travel$`Units Sold`[num[k]]
    sep_travel_units[g]<-units_septravel
  }
}


plot(sep_travel_billings, type="o",col="blue",xlab="September",ylab="Gross Billings",main="Gross Billings for September Deals Still Active")
plot(aug_travel_billings, type="o",col="blue",xlab="August",ylab="Gross Billings",main="Gross Billings for August Deals Still Active")
