#  Course Project 1
df <- read.table("household_power_consumption.txt", 
                 sep = ";",stringsAsFactors = FALSE, header = TRUE)
str(df)
df$Date <- as.Date(df$Date,"%d/%m/%Y")
df$Global_active_power <- as.numeric(df$Global_active_power)
df1 <- subset(df, df$Date > "2007-01-31" & df$Date < "2007-02-03")
anyNA(df1)
hist(df1$Global_active_power, xlab = "Global Active Power (kilowatts)", main =
             "Global Active Power", col = "red")
#  the png below follows 
#http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/
#png.html
png(file = "plot1.png", width = 480, height = 480, units = "px",
    pointsize = 12)
dev.copy(png, file = "plot1.png")
