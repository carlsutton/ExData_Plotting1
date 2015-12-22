#  Course project 2
#  Across the United States, how have emissions from coal combustion-related sources 
#  changed from 1999–2008?
#  read in data
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

require(data.table)
require(dplyr)
require(ggplot2)
#  Base R method
anyNA(nei$Emissions)  #  check for NA's
anyNA(nei$year)
#  locate coal source of pollution measurement
a <- grepl("coal",scc$Short.Name,ignore.case = TRUE)
b <- sum(a) # number of Short Names that contain coal
b
#  just for yukes lets do this with data table
dt <- data.table(nei)
setkey(dt,year)
#  subset "i" for scc code determined by grepl above and sum emissions by year
dt.emiss.plot <- dt[grepl("coal",scc$Short.Name,ignore.case = TRUE),
                    .(emissions = sum(Emissions)), by = .(year)]
g <- ggplot(dt.emiss.plot, aes(x =year,y = emissions)) + 
        geom_point(color = "red", size = 3) + geom_smooth(method = "lm") + 
        ggtitle("DataTable Version using ggplot, Emissions Type For Coal in the USA")
dt.emiss.plot
str(dt.emiss.plot)
g
plot4 <- g
dev.copy(png, file = "plot4.png")
dev.off()




