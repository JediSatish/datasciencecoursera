setwd("F:/Britannia/data_science/coursera/John_Hopkins/Exploratory_data_analysis/week_4")
getwd()

library(dplyr)
library(ggplot2)

#Extracting the data from the zip file to a data location
unzip("exdata_data_NEI_data.zip",exdir="wk4_asngmt_data")
list.files("./wk4_asngmt_data")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./wk4_asngmt_data/summarySCC_PM25.rds")
SCC <- readRDS("./wk4_asngmt_data/Source_Classification_Code.rds")
str(SCC_coal)
str(NEI)


#1) Compare emissions from motor vehicle sources in Baltimore City with 
#emissions from motor vehicle sources in Los Angeles County, California fips=="06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

#Extracting the records from source codes where coal combusion related information are available
SCC_vechicle_EI.Sector <-select(SCC[grep("vehicle",SCC$EI.Sector,ignore.case = TRUE),],EI.Sector)
unique(SCC_vechicle_EI.Sector)
#              EI.Sector
#542  Mobile - On-Road Gasoline Light Duty Vehicles
#939  Mobile - On-Road Gasoline Heavy Duty Vehicles
#1164   Mobile - On-Road Diesel Light Duty Vehicles
#1364   Mobile - On-Road Diesel Heavy Duty Vehicles

#For the above combustion types the SCC codes are extracted
SCC_vechicle <-select(SCC[grep("vehicle",SCC$EI.Sector,ignore.case = TRUE),],SCC)


#For the above extracted SCC codes the emission data are filtered for the matching
Baltimore_data <- filter(NEI,fips=="24510")

Baltimore_data_vehicle <- Baltimore_data[(Baltimore_data$SCC %in% SCC_vechicle$SCC),c(1,6,4)]
LA_data <- filter(NEI,fips=="06037")
LA_data_vehicle <- LA_data[(LA_data$SCC %in% SCC_vechicle$SCC),c(1,6,4)]
LA_BALT_vehicle_data <-Baltimore_data_vehicle
LA_BALT_vehicle_data <- rbind(LA_BALT_vehicle_data,LA_data_vehicle)


LA_BALT_vehicle_data$fips<- as.factor(LA_BALT_vehicle_data$fips)
levels(LA_BALT_vehicle_data$fips)[levels(LA_BALT_vehicle_data$fips)=="24510"] <- "Baltimore"
levels(LA_BALT_vehicle_data$fips)[levels(LA_BALT_vehicle_data$fips)=="06037"] <- "LA"

LA_BALT_vehicle_tot_emission <- LA_BALT_vehicle_data %>% group_by(year,fips) %>% summarize(Total.Annual.Emissions=sum(Emissions, na.rm=TRUE))


png("./plot6.png",width=480, height=480)
gg <- ggplot(data=LA_BALT_vehicle_tot_emission,aes(x=year,y=Total.Annual.Emissions,color=fips))
gg   + geom_point(color=LA_BALT_vehicle_tot_emission$year,size=5) + geom_line(size=1) + ggtitle("Total Emissions from motor vehicles in Baltimore vs LA") +
   ylab("Total Annual Emissions [Tons]")
dev.off() 

# Answer :Compared to Baltimore LA has significantly high emissions in all recorded years.  
#Also in LA vehicle emissions increased from the period of 1999 to 2005 and then fell significantly during 2008 period
# This drop is also above the 1999 recorded vehicle emissions level in LA
#In Baltimore the emissions have been decreasing over the period and it is lesser than the initial 1999 period

