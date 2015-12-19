#  Course Project 1
df <- read.table("household_power_consumption.txt", 
                 sep = ";",stringsAsFactors = FALSE, header = TRUE,
                 na.strings = "?")
#  convert to date class and subset on that
df$Date <- as.Date(df$Date,"%d/%m/%Y")
str(df)
#df$Time <- strptime(df$time, "%H:%M:%S")df1 <- subset(df, df$Date > "2007-01-31" & df$Date < "2007-02-03")
df1 <- subset(df,df$Date > "2007-01-31" & df$Date < "2007-02-03")
df1 <- within(df1, {timestamp = format(as.POSIXct(
        paste(df1$Date,df1$Time)), "%Y/%m/%d %H:%M:%S")})
head(df1$timestamp)
class(df1$timestamp)
#df1$timestamp <- as.Date(df1$timestamp,"%Y/%m/%d %H:%M:%S") 
df1$Global_active_power <- as.numeric(as.character(
        df1$Global_active_power))
df1$timestamp <- as.POSIXct(df1$Date,"%Y/%m/%d %H:%M:%S")
anyNA(df1)
str(df1)
plot(Global_active_power ~ timestamp, df1)
axis.POSIXct(1,df1$timestamp, format(df1$timestamp, %D))
     
       
