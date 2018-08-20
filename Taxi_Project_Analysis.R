#Aleksa Basara
#Created 7/3/18

#Install necessary packages
install.packages("data.table")
install.packages("lubridate")
install.packages("dyplr")
install.packages("gplots")
install.packages("repr")
library(data.table) #Package for large data processing
library(xda)#Package for exploratory analysis
library(dplyr) #R grammar
library(lubridate) #Handling dates
library(ggplot2) #Data viz
library(gplots) #Data viz
library(repr)
library(reshape2)


################
# SETUP Phase
###############

#Read in the data
setwd("C:/Documents/Data Science Projects/1 - NYC Taxi Drivers/Data")
taxiDT = fread("small_taxi_data.csv")

#OPTIONAL - Investigate dimensions and column names of data
dim(taxiDT)
colnames(taxiDT)
head(taxiDT,20)
typeof(taxiDT$lpep_pickup_datetime)
length(unique(taxiDT$DOLocationID)) #There are 259 PUids... 264 DO... maybe diff ways to get into NYC

###########################
# STEP 1: MERGE DATASETS
##########################
# Dataset 1: 2016 NYC Green Taxi Trip
# Dataset 2: NYC Taxi Zones

#PART A -- MERGE DATA WITH PUzoneData ACROSS PULOCATIONID
PUzoneData = fread("PUtaxiZones.csv")
#colnames(PUzoneData)
merge1Data = merge(taxiDT, PUzoneData)
#dim(merge1Data)
#colnames(merge1Data)

#Merge merge1data with zone data based on Drop-off location
DOzoneData = fread("DOtaxiZones.csv")
#colnames(DOzoneData)
merge2Data = merge(merge1Data, DOzoneData, by="DOLocationID")
taxiDT=merge2Data

################################
# Step 2: Initial Data Cleaning
################################

#PART A -- LOOK INTO ROWS WITH INVALID VALUES (e.g. negatives, )
#Investigate Tip_amount broken up by a variable --> By Payment_type shows we must only look @ credit card data
taxiDT[, list(min=min(Tip_amount), mean=round(mean(Extra),3), max=max(Tip_amount)), 
        by=list(PUborough)]

#Look at # of tip values below $0.00 --> 16 below 0
sum(taxiDT$Tip_amount < 0)
summary(taxiDT$Extra)

#PART B -- LOOK AT NA VALUES FOR EACH COLUMN
#Count number of NA values
colSums(is.na(taxiDT))
table(is.na(taxiDT$Pickup_longitude), is.na(taxiDT$PULocationID))
length(is.na(taxiDT$Pickup_latitude))

#PART C -- ADD NEW VARIABLES (e.g. Month of trip, # of hours of trip length)
#Add month variable (must extract from date)
taxiDT$lpep_pickup_datetime = as.character(taxiDT$lpep_pickup_datetime)
taxiDT$trip_date = substr(taxiDT$lpep_pickup_datetime, 0, 10) #Extracts the date (without the timestamp)
taxiDT$month = substr(taxiDT$lpep_pickup_datetime, 0, 2)
length(unique(taxiDT$DOLocationID))

#Add weekday variable
taxiDT$weekday = weekdays(as.Date(taxiDT$trip_date, format = "%m/%d/%Y"))
sum(is.na(taxiDT$weekday))

#Add military_time, pickup_hour, dropoff_hour, time_elapsed
taxiDT$PUhour = paste(substr(taxiDT$lpep_pickup_datetime, 12, 13), 
                      substr(taxiDT$lpep_pickup_datetime, 21, 22))


#PART D -- SUBSET DATA AND CREATE NEW, SMALLER DATASET OF CLEANED DATA
#Select only rows that have credit card tip at least 0 --> that will be used for analysis
taxiDT2 = taxiDT[taxiDT$Payment_type == 1 & taxiDT$Fare_amount > 0 &
                   taxiDT$improvement_surcharge >= 0 & taxiDT$Tip_amount >= 0 & !is.na(Trip_type) &
                   !is.na(taxiDT$PULocationID) & !is.na(taxiDT$DOLocationID)]

#Add tip_percent variable
taxiDT2$nonTip = taxiDT2$Total_amount - taxiDT2$Tip_amount
taxiDT2$tip_percent = round((taxiDT2$Tip_amount/taxiDT2$nonTip),4)

#Remove unnecessary variables
taxiDT3 = taxiDT2[, c("Pickup_longitude", "Pickup_latitude", 
                      "Dropoff_longitude", "Dropoff_latitude", "Ehail_fee"):=NULL]

#Write to csv
fwrite(taxiDT3, "small_taxi_data.csv")


#######################
#CREATE VARIABLE Tip20
#######################

#Add tip20 variable: 1 if tipped over 20 percent of total fair, 0 otherwise
taxiDT$tip20 = ifelse(taxiDT$tip_percent > 0.2, 1, 0)

#######################################
# STEP 3 -- EXPLORATORY DATA ANALYSIS
######################################

#Make categorical variables factor 
taxiDT$Payment_type = as.factor(taxiDT$Payment_type)
taxiDT$VendorID = as.factor(taxiDT$VendorID)
taxiDT$RateCodeID = as.factor(taxiDT$RateCodeID)
taxiDT$Trip_type = as.factor(taxiDT$Trip_type)
taxiDT$month = as.factor(taxiDT$month)

#Get basic summary of data
summary(taxiDT)
dim(taxiDT)

#Check breakdown of categorical variables
badCatVar = c('lpep_pickup_datetime', 'Lpep_dropoff_datetime', 'Store_and_fwd_flag', 
              'trip_date') #Exclude variables whose distributions we don't care about
charSummary(taxiDT[,!(names(taxiDT) %in% badCatVar), with=FALSE])

#Check breakdown of continuous variables
numSummary(taxiDT)


###########
#DATA VIZ
###########

#CHART 1 -- AVERAGE DAILY RIDERSHIP line chart
#Define needed variables
ridersByDay = taxiDT %>% group_by(weekday) %>% count()
ridersByDay

dayCount = taxiDT[, length(unique(trip_date)), by = 'weekday']
dayCount

dayRiders = merge(dayCount, ridersByDay, by = 'weekday')
dayRiders

#Re-order by calendar order
dayRiders$weekday = ordered(dayRiders$weekday, levels =  c("Sunday", "Monday", "Tuesday",
                                "Wednesday", "Thursday", "Friday",
                                "Saturday"))
dayRiders

#Make the line plot
ggplot(data = dayRiders[order(dayRiders$weekday)], aes(weekday, 
                                                       (dayRiders$n/dayRiders$V1),group=1)) + 
      geom_point() +
      geom_line() + 
      labs(x="Day of the Week", y="Avg # Riders", title = "Avg # Riders per Day")


#####
#CHART 2 -- PASSENGER COUNT pie chart
#Categorize passenger counts into group types
taxiDT$passCount = ifelse(taxiDT$Passenger_count ==0, "No one", 
                          ifelse(taxiDT$Passenger_count == 1, "Alone",
                          ifelse(taxiDT$Passenger_count == 2, "Pair", 
                          ifelse(taxiDT$Passenger_count > 2, "Group", taxiDT$Passenger_count))))

# Make smaller dataframe with just proportions of passenger group types
numPas = taxiDT %>% group_by(passCount) %>% count() 
passGroupType = transform(numPas, propPass = n/sum(n))

passGroupType$label = paste(pasTrans$passCount, 
                            paste(round(pasTrans$propPass*100, 2), "%"), sep = " - ")

# Draw pie chart of passenger group types
ggplot(data=passGroupType, mapping = aes(x="", y= propPass, fill = passCount)) + 
        geom_bar(width=1, stat="identity", show.legend = FALSE) +
        coord_polar(theta = "y") +
        labs(title = "Passenger Count Breakdown") +
        geom_text(aes(label = label),
                  position = position_stack(vjust=0.5),size=3.5, angle=30) 

mean(taxiDT$Fare_amount)/mean(taxiDT$Trip_distance)


#####
#CHART 3 -- TRIP DISTANCE histogram
qplot(taxiDT[Trip_distance < 20]$Trip_distance, geom="histogram", binwidth = 2) +  
      xlab("Distance") + 
      ggtitle("Distance Travelled Distribution") +
      geom_vline(aes(xintercept = median(taxiDT$Trip_distance)), color = "olivedrab2") 
      #geom_vline(aes(xintercept = mean(taxiDT$Trip_distance)), color = "red")
    

#####
#CHART 4 -- NON-TIP AMOUNT histogram
qplot(taxiDT[nonTip < 100]$nonTip, geom="histogram",breaks = seq(0, 100, 5)) +  
  xlab("Amount ($)") + 
  ggtitle("Amount of Non-Tip Paid Distribution") +
  geom_vline(aes(xintercept = median(taxiDT$nonTip)), color = "olivedrab2")  
  #geom_vline(aes(xintercept = mean(taxiDT$nonTip)), color = "red")


#####
#CHART 5 -- TIP PERCENT histogram
qplot(taxiDT[tip_percent < 0.45]$tip_percent, geom="histogram", breaks = seq(0, 0.45, 0.025)) +  
  xlab("Tip Percent") + 
  ggtitle("Tip Percent Distribution") +
  geom_vline(aes(xintercept = median(taxiDT$tip_percent)), color = "olivedrab2")  
#geom_vline(aes(xintercept = mean(taxiDT$tip_percent)), color = "red")

nrow(taxiDT[taxiDT$Trip_distance > 50])


###### 
#CHART 6 -- CORRELATION PLOT
options(repr.plot.width=8, repr.plot.height=6)
quantData = taxiDT[, list(Passenger_count, Trip_distance, Fare_amount, 
              Extra,MTA_tax, Tip_amount, Tolls_amount, 
              improvement_surcharge, Total_amount, tip_percent, nonTip)]
str(quantData)
head(quantData, 20)

qplot(x=Var1, y=Var2, data=melt(cor(quantData, use='p')), fill=value, geom='tile') +
      scale_fill_gradient2(low='red', high='blue', mid = 'white', midpoint = 0, 
                           limit=c(-1,1), space='Lab', name='Correlation') + 
      theme(axis.text.x = element_text(angle=45, vjust=1, size=8, hjust = 1)) +
      coord_fixed() + ggtitle("Correlation Heatmap") + 
      theme(plot.title = element_text(hjust = 0.4)) +
      geom_text(aes(Var1, Var2, label=round(value,2)), color="black", size = 2)


#####
#CHART 7 -- Weekday x Hour tile chart
TP_time = taxiDT %>% group_by(weekday, PUhour) %>% 
          summarize(Tip_Percent = mean(tip_percent)) %>%
          mutate(Morning = substr(PUhour, 4, 5)) 

TP_time= TP_time[order(TP_time$Morning),]

TP_time
colnames(TP_time)


ggplot(TP_time, aes(x=ordered(weekday, levels = c("Sunday", 
                                        "Monday", "Tuesday","Wednesday", "Thursday", 
                                        "Friday", "Saturday")), 
                                        y=ordered(PUhour, levels = c("12 AM", "01 AM", "02 AM",
                                        "03 AM", "04 AM", "05 AM", "06 AM", "07 AM", "08 AM", 
                                        "09 AM", "10 AM", "11 AM", "12 PM", "01 PM", "02 PM",
                                        "03 PM", "04 PM", "05 PM", "06 PM", "07 PM", 
                                        "08 PM", "09 PM", "10 PM", "11 PM")))) +
        geom_tile(aes(fill = Tip_Percent), color = "white") + 
        scale_fill_gradient(low = "white", high = 'darkgreen') + 
        xlab("Day of Week") + ylab("Hour of Day") + ggtitle("Tip Percent by Hour of Day Heatmap")



