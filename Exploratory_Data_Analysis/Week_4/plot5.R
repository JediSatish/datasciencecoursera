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


#1) Across the United States, how have emissions 
#from coal combustion-related sources changed from 1999-2008?

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
Baltimore_data_vehicle <- Baltimore_data[(Baltimore_data$SCC %in% SCC_vechicle$SCC),c(6,4)]




Baltimore_vehicle_tot_emission <- Baltimore_data_vehicle %>% group_by(year) %>% summarize(Total.Annual.Emissions=sum(Emissions, na.rm=TRUE))


png("./plot5.png",width=480, height=480)
gg <- ggplot(data=Baltimore_vehicle_tot_emission,aes(x=year,y=Total.Annual.Emissions))
gg + geom_bar(stat="identity", fill="#56B4E9")  + geom_point(color="red",size=3) + geom_line(size=1) + ggtitle("Total Emissions from motor vehicles in Baltimore") +
   ylab("Total Annual Emissions [Tons]")
dev.off() 

# Answer :There as been significant drop for coal combustion related sources

