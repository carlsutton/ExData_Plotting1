#  Course project 2 
#  Compare emissions from motor vehicle sources in Baltimore City with emissions from 
#  motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#  Which city has seen greater changes over time in motor vehicle emissions?
#  read in data
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

require(data.table)
require(ggplot2)
#  Base R method
anyNA(nei$Emissions)  #  check for NA's
anyNA(nei$year)

dt <- data.table(nei)
setkey(dt,year)
#  change factor variables to character variable so I use them
scc$SCC.chr <- as.character(scc$SCC)
scc$Short.Name.chr <- as.character(scc$Short.Name)
scc$Data.Category.chr <- as.character(scc$Data.Category)
scc$EI.Sector.chr <- as.character(scc$EI.Sector)
scc$Option.Group.chr <- as.character(scc$Option.Group)
scc$Option.Set.chr <- as.character(scc$Option.Set)
scc$Level.One.chr <- as.character(scc$SCC.Level.One)
scc$Level.Two.chr <- as.character(scc$SCC.Level.Two)
scc$SCC.Level.Three.chr <- as.character(scc$SCC.Level.Three)
scc$SCC.Level.Four.chr <- as.character(scc$SCC.Level.Four)
#  where is the data?  Searching...
a1 <- grepl("motor vehicle",scc$SCC.chr,ignore.case = TRUE)
a2 <- grepl("motor vehicle",scc$Short.Name.chr,ignore.case = TRUE)
a3 <- grepl("motor vehicle",scc$Data.Category.chr,ignore.case = TRUE)
a4 <- grepl("motor vehicle",scc$EI.Sector.chr,ignore.case = TRUE)
a5 <- grepl("motor vehicle",scc$Option.Group.chr,ignore.case = TRUE)
a6 <- grepl("motor vehicle",scc$Option.Set.chr,ignore.case = TRUE)
a7 <- grepl("motor vehicle",scc$Level.One.chr,ignore.case = TRUE)
a8 <- grepl("motor vehicle",scc$Level.Two.chr,ignore.case = TRUE)
a9 <- grepl("motor vehicle",scc$SCC.Level.Three.chr,ignore.case = TRUE)
a10 <- grepl("motor vehicle",scc$SCC.Level.Four.chr,ignore.case = TRUE)
summary(a1); summary(a2); summary(a3); summary(a4); summary(a5); summary(a6)
summary(a7); summary(a8); summary(a9); summary(a10)
#  The only instances of "motor vehicle" are in a2 (Short.Name) and a9 (SCC.Level.Three
#  both show 20 "hits" (true's) and I will make the assumption these are the same 
#  measurements

dt.scc <- as.data.table(scc)                                       
summary(dt.scc)
#  Now to subset data table for all scc$Short.Name.chr that contain "motor vehicle"
#  I have changed the type for the variable from factor to character so data table
#  can use it
dt.emiss <- dt[grepl("motor vehicle",scc$Short.Name.chr,ignore.case = TRUE)]
#  Now to subset the above for Baltimore City and Los Angeles
dt.emiss.plot <- dt.emiss[fips == "24510" | fips == "06037"]
#  and let's see what that gives me
summary(dt.emiss.plot)
unique(dt.emiss.plot$fips)
unique(dt.emiss.plot$Pollutant)
#  I must be missing data somewhere since I am only getting data for LA for 2008
#  and Baltimore City for 2002 and 2008.
dt.emiss.plot <- dt.emiss.plot[, .(emissions = sum(Emissions)), by = .(year,fips)]
dt.emiss.plot$fips <- as.factor(dt.emiss.plot$fips)
ggplot(dt.emiss.plot, aes(x =year,y = emissions)) + 
        geom_point(color = "red", size = 3, method = "lm") + 
        facet_grid(.~fips) +
        ggtitle("DataTable Version using ggplot, Motor Vehicle Emissions Baltimore and LA")
dt.emiss.plot

#  I did notice that data for years 1999, 2005 are missing


#  I will attempt to merge the data tables and see If more data is available
setkey(dt, SCC)
setkey(dt.scc, SCC)
dt.m <- merge(dt, dt.scc)
str(dt.m)
dt.emiss <- dt.m[grepl("motor vehicle",Short.Name.chr,ignore.case = TRUE)]
#  Now to subset the above for Baltimore City and Los Angeles
dt.emiss.plot <- dt.emiss[fips == "24510" | fips == "06037"]
#  and let's see what that gives me
summary(dt.emiss.plot)
unique(dt.emiss.plot$fips)
unique(dt.emiss.plot$Pollutant)
#  I must be missing data somewhere since I am only getting data for LA for 2008
#  and Baltimore City for 2002 and 2008.
dt.emiss.plot <- dt.emiss.plot[, .(emissions = sum(Emissions)), by = .(year,fips)]
dt.emiss.plot$fips <- as.factor(dt.emiss.plot$fips)
g <- ggplot(dt.emiss.plot, aes(x =year,y = emissions)) + 
        geom_point(color = "red", size = 3) + geom_smooth(method = "lm") + 
        facet_grid(.~fips) +
        ggtitle("DataTable Version using ggplot, Motor Vehicle Emissions Baltimore 
                and Los Angeles")
dt.emiss.plot
#  The data for Los Angeles appears.  For reasons unknown to me, the merging of data
#  tables appears to have been necessary.  Still perplexed about the lack of data
#  for Baltimore City.  The plot data appears reasonable.  LA emissions are slightly
#  up, but so is the population, but the smog from 1970 to now is vastly reduced with 
#  much heavier traffic.  Baltimore is just an old East Coast city slowly rotting 
#  away with heavy welfare and few economic prospects.  It's relatively low level of 
#  smog is undoubtedly due to location and population level.
g
plot6 <- g
dev.copy(png, file = "plot6.png")
dev.off()

