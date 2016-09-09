#Author: Satakarni Bommuluri
#All plots in Plot4. That include 
#Plot for global active power (in KW), Voltage, and Global reactive power consumption for feb 01 2007 and feb 02 2007
#Plus the Plots of sub meters(in KW) consumption for feb 01 2007 and feb 02 2007

plot4 <- function(filePath = "./household_power_consumption/household_power_consumption.txt"){

  #if file doesn't exist, then download and unzip it 
  if(!file.exists(filePath)){
    urlPath <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipFile <- "hpc_data.zip"
    download.file(url = urlPath, destfile = zipFile)
    unzip(zipFile, exdir = "./household_power_consumption")
    file.remove(zipFile)
  }
  
  #if  file exists
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
    hec_feb$Sub_metering_1 <- as.double(hec_feb$Sub_metering_1)
    hec_feb$Sub_metering_2 <- as.double(hec_feb$Sub_metering_2)
    hec_feb$Sub_metering_3 <- as.double(hec_feb$Sub_metering_3)
    hec_feb$Voltage <- as.double(hec_feb$Voltage)
    hec_feb$Global_reactive_power <- as.double(hec_feb$Global_reactive_power)
    
    #export the plot to png and swtch off the device 
    ylabel_gap <- "Global Active Power"
    ylabel_grp <- "Global Reactive Power"
    legendSubMeter <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
    colSubMeter <- c("black","red","blue")
    reset_par <- par()
    
    png(filename = "plot4.png", width = 480, height = 480, units = "px")
    par(mfcol = c(2,2))
    # Global Active Power - (1,1)
    plot(x = hec_feb$DataTime, y = hec_feb$Global_active_power, type = "l", xlab = "", ylab = ylabel_gap)
    #Energy Sub Metering - (2,1)
    plot(x = hec_feb$DataTime, hec_feb$Sub_metering_1, type = "l", ylab = "Energy sub metering",xlab="")
    lines(x = hec_feb$DataTime, hec_feb$Sub_metering_2, col="red")
    lines(x = hec_feb$DataTime, hec_feb$Sub_metering_3, col="blue")
    legend(x = "topright", legend = legendSubMeter, col = colSubMeter, lwd = 2, cex = 0.7, bty = "n")
    #Voltage - (1,2)
    plot(x = hec_feb$DataTime, y = hec_feb$Voltage, type = "l", ylab = "Voltage", xlab = "datetime")
    #Global Reactive Power (2,2)
    plot(x = hec_feb$DataTime, y = hec_feb$Global_reactive_power, type = "l", ylab = ylabel_grp, xlab = "datetime")
   
    dev.off() 
    par(reset_par)
    
    
    
  } else {stop("No H.P.C. file to read")}
  
  }