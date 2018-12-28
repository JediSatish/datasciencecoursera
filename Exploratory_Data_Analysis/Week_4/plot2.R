setwd("F:/Britannia/data_science/coursera/John_Hopkins/Exploratory_data_analysis/week_4")
getwd()

library(dplyr)

#Extracting the data from the zip file to a data location
unzip("exdata_data_NEI_data.zip",exdir="wk4_asngmt_data")
list.files("./wk4_asngmt_data")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./wk4_asngmt_data/summarySCC_PM25.rds")
SCC <- readRDS("./wk4_asngmt_data/Source_Classification_Code.rds")
str(NEI)

#1) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering 
#this question.

Baltimore_data <- filter(NEI,fips=="24510")
yrly_Baltimore_tot_emission <- Baltimore_data %>% group_by(year) %>% summarize(sum(Emissions, na.rm=TRUE))
names(yrly_Baltimore_tot_emission) <- c("YEAR","Total.Annual.Emissions")

par(mfrow=c(1,1))
png("./plot2.png",width=480, height=480)
barplot(yrly_Baltimore_tot_emission$Total.Annual.Emissions,names.arg = yrly_Baltimore_tot_emission$YEAR,col="blue")
points(yrly_Baltimore_tot_emission$Total.Annual.Emissions,pch=16, col="red")
lines(yrly_Baltimore_tot_emission$Total.Annual.Emissions,type="l", lty=1, lwd=2, col="red")
title(main="Total Annual Emissions in Baltimore  City, Maryland", xlab="YEAR", ylab="Total Annual Emissions [Tons]")
dev.off()

# Answer : The yearly trend of Baltimore emissions rate alternates between rise and fall during the period of 1999-2008.
#During 1999-2002 the total emissions dropped whereas the total emissions of the following period, 2002-05 raised significantly.
#The following period 2005-08 again saw a huge drop in the total emissions 



