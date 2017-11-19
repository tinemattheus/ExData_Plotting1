plot4 <- function() {
        
        ##Read data file.
        ##It is assumed the data file is in the working directory.
        dataFile <- file.path(getwd(),"household_power_consumption.txt")
        householdData <- read.table(dataFile, header = TRUE, sep = ";")
        
        ##Filter the data for February 1 and 2 in 2007.
        ##Change any '?' into 'NA'.
        householdData_filtered <- filter(householdData, Date == "1/2/2007" | Date == "2/2/2007") 
        householdData_filtered[householdData_filtered == "?"] <- NA
        
        ##Add timestamp column.
        householdData_filtered <- mutate(householdData_filtered, timestamp = paste(Date, Time, sep = " "))
        ##Convert factor columns to numeric.
        householdData_filtered[,c(3:8)] <- sapply(householdData_filtered[,c(3:8)], as.character)
        householdData_filtered[,c(3:8)] <- sapply(householdData_filtered[,c(3:8)], as.numeric)
        
        ##Plot 4 grahps, 2 by 2, into plot4.png.
        png("plot4.png", width = 480, height = 480)
        
        par(mfrow = c(2,2))
        
        ##Graph 1,1
        with(householdData_filtered,plot(strptime(timestamp,"%d/%m/%Y %H:%M:%S"), 
                                         Global_active_power, type = "l", xlab = "",ylab = "Global Active Power"))
        
        ##Graph 1,2
        with(householdData_filtered,plot(strptime(timestamp,"%d/%m/%Y %H:%M:%S"), 
                                         Voltage, type = "l", xlab = "datetime",ylab = "Voltage"))
        
        ##Graph 2,1
        with(householdData_filtered, plot(strptime(timestamp,"%d/%m/%Y %H:%M:%S"), Sub_metering_1, type = "n",
                                          xlab = "", ylab = "Energy sub metering"))
        with(householdData_filtered, points(strptime(timestamp,"%d/%m/%Y %H:%M:%S"), Sub_metering_1, type = "l", col = "black"))
        with(householdData_filtered, points(strptime(timestamp,"%d/%m/%Y %H:%M:%S"), Sub_metering_2, type = "l", col = "red"))
        with(householdData_filtered, points(strptime(timestamp,"%d/%m/%Y %H:%M:%S"), Sub_metering_3, type = "l", col = "blue"))
        legend("topright", lty = 1, bty = "n", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        ##Graph 2,2
        with(householdData_filtered,plot(strptime(timestamp,"%d/%m/%Y %H:%M:%S"), 
                                         Global_reactive_power, type = "l", xlab = "datetime",ylab = "Global_reactive_power"))
        
        dev.off()
        
}