plot1 <- function() {
        
        ##Read data file.
        ##It is assumed the data file is in the working directory.
        dataFile <- file.path(getwd(),"household_power_consumption.txt")
        householdData <- read.table(dataFile, header = TRUE, sep = ";")
        
        ##Filter the data for February 1 and 2 in 2007.
        ##Change any '?' into 'NA'.
        householdData_filtered <- filter(householdData, Date == "1/2/2007" | Date == "2/2/2007")
        householdData_filtered[householdData_filtered == "?"] <- NA
        
        ##Convert factor columns to numeric.
        householdData_filtered[,c(3:8)] <- sapply(householdData_filtered[,c(3:8)], as.character)
        householdData_filtered[,c(3:8)] <- sapply(householdData_filtered[,c(3:8)], as.numeric)
        
        ##Plot the graph into plot1.png.
        png("plot1.png", width = 480, height = 480)
        hist(householdData_filtered$Global_active_power, col = "red", 
             xlab = "Global Active Power (kilowatts)", main = "Global Active Power", ylim = c(0,1200))
        dev.off()
        
}