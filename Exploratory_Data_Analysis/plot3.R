setwd("F:/Britannia/data_science/coursera/John_Hopkins/Exploratory_data_analysis/week_1")
getwd()

library(lubridate)


#Extracting the data from the zip file to a data location
unzip("exdata_data_household_power_consumption.zip",exdir="wk1_asngmt_data")

#Check the file availability afer extraction
file.exists("./wk1_asngmt_data/household_power_consumption.txt")

#storing sample data to view the date format in the file
sample_data <-read.csv("./wk1_asngmt_data/household_power_consumption.txt", nrows=90000, blank.lines.skip = FALSE, sep=";")
unique(sample_data$Date)

#removing the variable for memory usuage
rm(powerData_1)

#Installing the package sqldf, to load the records
#install.packages("sqldf")
library(sqldf)

#Filtering records with only dates 2007-02-01 and 2007-02-02.
df <- read.csv.sql("./wk1_asngmt_data/household_power_consumption.txt",sql="Select * from file where Date  = '1/2/2007' or Date ='2/2/2007'", sep=";")
work_data <- df

#Creating a new date time variable using lubridate  package
work_data$date_time <- dmy_hms(paste(work_data$Date, work_data$Time)) 

#Converting to numeric variables
work_data[,c(3:9)] <- sapply(work_data[,c(3:9)], as.numeric)
str(work_data)

#df$Date <- as.Date(df$Date,format="%d/%m/%Y")
#df$Time < strptime(df$Time,"%H:%M:%S")
head(work_data)

#PLOT 3
par(mfrow=c(1,1))
png("./plot3.png",width=480, height=480)
plot(y=work_data$Sub_metering_1, 
     x=work_data$date_time,
     type="l",
     ylab="Energy sub metering",
     xlab="")
points(y=work_data$Sub_metering_2,
       x=work_data$date_time,
       type="l",
       col="red")
points(y=work_data$Sub_metering_3,
       x=work_data$date_time,
       type="l",
       col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=1)
dev.off()







