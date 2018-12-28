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
str(NEI)

#1) Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? 
#the ggplot2 plotting system to make a plot answer this question.

Baltimore_data <- filter(NEI,fips=="24510")
yrly_Baltimore_tot_emission <- Baltimore_data %>% group_by(year,type) %>% summarize(Total.Annual.Emissions=sum(Emissions, na.rm=TRUE))
names(yrly_Baltimore_tot_emission) <- c("year","type","Total.Annual.Emissions")

png("./plot3.png",width=480, height=480)
gg <- ggplot(data=yrly_Baltimore_tot_emission,aes(x=year,y=Total.Annual.Emissions,color=type))
gg + geom_point(color="red",size=2) + geom_line(size=1) + ggtitle("Total Emissions in Baltimore for each emission type") +
   ylab("Total Annual Emissions [Tons]")
dev.off() 

# Answer : For all the emissions Type the trend seems to be drop in emissions for every recorded period.
#Only for Point emissions the trend seems to be increasing from 1999 - 2005 and then it saw a signicant drop in the final recorded period


