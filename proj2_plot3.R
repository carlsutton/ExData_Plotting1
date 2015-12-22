#  Course project 2
#  Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
#  variable, which of these four sources have seen decreases in emissions from 
#  1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? 
#  Use the ggplot2 plotting system to make a plot answer this question.
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
nei$type <- as.factor(nei$type)
#  just for yukes lets do this with data table
dt <- data.table(nei)
setkey(dt,year)
dt.emiss.plot <- dt[fips == "24510",.(emissions = sum(Emissions))
                    , by = .(type,year)]
g <- ggplot(dt.emiss.plot, aes(x =year,y = emissions)) + 
        geom_point(color = "red", size = 3, method = "lm") + 
        facet_grid(.~type) +
        ggtitle("DataTable Version using ggplot, Emissions Type")
dt.emiss.plot
g
plot3 <- g
dev.copy(png, file = "plot3.png")
dev.off()

#  and to show off, how about dplyr
nei.tbl <- tbl_df(nei)
summ.nei <- nei.tbl %>% group_by(year,type) %>%
        filter(fips == "24510") %>%
        select(fips,year,Emissions,type) %>%
        summarise(emissions = sum(Emissions)) 
g <- ggplot(summ.nei, aes(x =year,y = emissions)) + 
        geom_point(color = "red", size = 3) + geom_smooth(method = "lm") + 
        facet_grid(.~type) +
        ggtitle("DPLYR Version using ggplot, Baltimore Emissions by Type")
summ.nei
g



