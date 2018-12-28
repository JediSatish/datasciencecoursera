setwd("F:/Britannia/data_science/coursera/John_Hopkins/Exploratory_data_analysis/week_4")
getwd()

library(dplyr)

#Extracting the data from the zip file to a data location
unzip("exdata_data_NEI_data.zip",exdir="wk4_asngmt_data")
list.files("./wk4_asngmt_data")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./wk4_asngmt_data/summarySCC_PM25.rds")
SCC <- readRDS("./wk4_asngmt_data/Source_Classification_Code.rds")

#1) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 
#emission

yrly_tot_emission <- NEI %>% group_by(year) %>% summarize(sum(Emissions, na.rm=TRUE))
names(yrly_tot_emission) <- c("YEAR","Total.Annual.Emissions")

par(mfrow=c(1,1))
png("./plot1.png",width=480, height=480)
barplot(yrly_tot_emission$Total.Annual.Emissions,names.arg = yrly_tot_emission$YEAR,col="blue")
points(yrly_tot_emission$Total.Annual.Emissions,pch=16, col="red")
lines(yrly_tot_emission$Total.Annual.Emissions,type="l", lty=1, lwd=2, col="red")
title(main="Total Annual Emissions in US", xlab="YEAR", ylab="Total Annual Emissions [Tons]")
dev.off()

# Answer : The annual emissions trend from 199 to 2008 seems to be declining for every recorded year.
#Significant drops are observed between 1999 to 2002 period and 2005 to 2008 period.



