#  Course project 2
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
unique(nei.emmis.1999$year)
emiss.1999 <- sum(nei.emmis.1999$Emissions, na.rm = TRUE)
nei.emmis.2002 <- subset(nei, year == 2002)
unique(nei.emmis.2002$year)
emiss.2002 <- sum(nei.emmis.2002$Emissions, na.rm = TRUE)
nei.emmis.2005 <- subset(nei, year == 2005)
unique(nei.emmis.2005$year)
emiss.2005 <- sum(nei.emmis.2005$Emissions, na.rm = TRUE)
nei.emmis.2008 <- subset(nei, year == 2008)
unique(nei.emmis.2008$year)
emiss.2008 <- sum(nei.emmis.2008$Emissions, na.rm = TRUE)
emiss <- c(emiss.1999,emiss.2002,emiss.2005,emiss.2008)
emiss
years <- c(1999,2002,2005,2008)
plot1 <- plot(years, emiss)
title(main = "Base Plot of Emissions vs Years")
ylab = "Emissions"
dev.copy(png, file = "plot1.png")
dev.off()
dev.cur()  #  making sure it got back to screen

plot(years, emiss)
title(main = "Base Plot of Emissions vs Years")
ylab = "Emissions"
#  just for yukes lets do this with data table
dt <- data.table(nei)
setkey(dt,year)
dt.emiss.plot <- dt[,.(emissions = sum(Emissions)), by = .(year)]
ggplot(dt.emiss.plot, aes(year, emissions)) + 
        geom_point(color = "red", size = 8) +
        ggtitle("DataTable Version using ggplot")
dt.emiss.plot
#  and t show off, how about dplyr
nei.tbl <- tbl_df(nei)
summ.nei <- nei.tbl %>% group_by(year) %>%
        summarise(emissions = sum(Emissions)) 
plot(summ.nei$year, summ.nei$emissions)
title(main = "DPLYR version Emissions vs Years")

