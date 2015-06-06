#------------------------------------------------------------------------------#
## Function to check to see if the data set is downloaded and unzipped
## If data is not present, function will download the data and unzip for the user
## in their current working directory
#------------------------------------------------------------------------------#
check_dataset <- function() {
    ## Checks to see if UCI Electric Power Consumption Dataset file exists
    if (!file.exists("household_power_consumption.txt")) {
        print("Text file household_power_consumption does not exist.")
        file.Found <- FALSE
    }
    
    # Checks to see if Electric power consumption zip file is downloaded
    if (!file.exists("exdata_data_household_power_consumption.zip")) {
        print("Required zip file (exdata_data_household_power) does not exist.")
        print("Zip file will be downloaded for you.")
        #Downloads Electric power consumption zip
        file.Url <- 
            "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(file.Url, 
                      dest = "exdata_data_household_power_consumption.zip",
                      method = "internal")
        
        # Records date of download of file
        dateDownloaded <- date()
        dateDownloaded
    }
    
    if (file.Found == FALSE) {
        # Unzips downloaded file
        unzip("exdata_data_household_power_consumption.zip")
        print("Zip file has been unzipped.")
    }
}

#------------------------------------------------------------------------------#
# Function reads the data, creates a plot of type line. Width and height are 
# set to 480, and the image is copied as a png file.  PNG file is saved in
# current working directory.
#------------------------------------------------------------------------------#
read_Write_Plot <- function() {
    # Read household power consumption text file
    data <- read.csv.sql("household_power_consumption.txt", 
                         sql = "select * from file where Date in ('1/2/2007',
                         '2/2/2007')", header = TRUE, sep = ";")
    
    # Sets necessary variables
    title <- ""
    x.Label <- ""
    y.Label <- "Global Active Power (kilowatts)"
    col.Gap <- data$Global_active_power
    
    # Converting dates
    date.Time <- paste(as.Date(data$Date, format="%d/%m/%Y"), data$Time, sep=" ")
    col.Date <- as.POSIXct(date.Time)
    
    # Creates png file titled plot2
    png("plot2.png", width = 480, height = 480)
    
    # Plots Global active power
    plot(col.Date, col.Gap, type = "l", main = title, xlab = x.Label, ylab = y.Label)
    
    # Turns off device
    dev.off()
}