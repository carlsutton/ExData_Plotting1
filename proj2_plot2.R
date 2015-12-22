#  Course project 2
#  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#  (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 
#  answering this question.


#  read in data
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

require(data.table)
require(dplyr)
require(ggplot2)
#  Base R method
anyNA(nei$Emissions)  #  check for NA's
anyNA(nei$year)
#  subset data by year, then sum by year
nei.emmis.1999 <- subset(nei, year == 1999)
nei.emmis.1999 <- subset(nei.emmis.1999, fips == "24510")
head(nei.emmis.1999)
emiss.1999 <- sum(nei.emmis.1999$Emissions, na.rm = TRUE)
nei.emmis.2002 <- subset(nei, year == 2002 & fips == "24510")
head(nei.emmis.2002)
emiss.2002 <- sum(nei.emmis.2002$Emissions, na.rm = TRUE)
nei.emmis.2005 <- subset(nei, year == 2005 & fips == "24510")
head(nei.emmis.2005)
emiss.2005 <- sum(nei.emmis.2005$Emissions, na.rm = TRUE)
nei.emmis.2008 <- subset(nei, year == 2008 & fips == "24510")
head(nei.emmis.2008)
emiss.2008 <- sum(nei.emmis.2008$Emissions, na.rm = TRUE)
emiss <- c(emiss.1999,emiss.2002,emiss.2005,emiss.2008)
emiss
years <- c(1999,2002,2005,2008)
plot(years, emiss,ylab = "Emissions", main = "Base Plot of Baltimore City Emissions
     vs Years")

#  just for yukes lets do this with data table
dt <- data.table(nei)
setkey(dt,year)
dt.emiss.plot <- dt[fips == "24510",.(emissions = sum(Emissions)), by = .(year)]
ggplot(dt.emiss.plot, aes(year, emissions)) + 
        geom_point(color = "red", size = 8) +
        ggtitle("DataTable Version using ggplot")
dt.emiss.plot
#  and to show off, how about dplyr
nei.tbl <- tbl_df(nei)
summ.nei <- nei.tbl %>% group_by(year) %>%
        filter(fips == "24510") %>%
        summarise(emissions = sum(Emissions)) 
plot(summ.nei$year, summ.nei$emissions)
title(main = "DPLYR version Baltimore City Emissions vs Years")

plot1 <- plot(years, emiss,ylab = "Emissions", main = "Base Plot of Baltimore City
             Emissions vs Years")
dev.copy(png, file = "plot2.png")
dev.off()
