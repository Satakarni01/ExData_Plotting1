#Author: Satakarni Bommuluri
#Plot2: Plots global active power (in KW) consumption for feb 01 2007 and feb 02 2007

plot2 <- function(filePath = "./household_power_consumption/household_power_consumption.txt"){
  
  #if file doesn't exist, then download and unzip it 
  if(!file.exists(filePath)){
    urlPath <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipFile <- "hpc_data.zip"
    download.file(url = urlPath, destfile = zipFile)
    unzip(zipFile, exdir = "./household_power_consumption")
    file.remove(zipFile)
  }
  
  #Check if file exists or else stop the program 
  if(file.exists(filePath)){
    #read the file
    hec_df <- read.csv(file = filePath, header = TRUE, sep = ";", colClasses = "character", na.strings = "?")
    
    #append new column of DateTIme of POSIXlt format
    hec_df$DataTime <- strptime(paste(hec_df$Date, hec_df$Time, sep = " "), format = "%d/%m/%Y %H:%M:%S")
    
    #Dates required
    startDate <- as.POSIXlt("2007-02-01 00:00:00") 
    endDate <- as.POSIXlt("2007-02-03 00:00:00")
    
    #subset the required data frame rows 
    hec_feb <- hec_df[hec_df$DataTime >= startDate & hec_df$DataTime < endDate, ]
    
    #Convert char to double
    hec_feb$Global_active_power <- as.double(hec_feb$Global_active_power)
    
    #export the plot to png and swtch off the device 
    ylabel <- "Global Active Power (kilowatts)"
    png(filename = "plot2.png", width = 480, height = 480, units = "px")
    plot(x = hec_feb$DataTime, y = hec_feb$Global_active_power, type = "l", xlab = "", ylab = ylabel)
    dev.off()
    
  }else {stop("No H.P.C. file to read")}
}