#  Course project 2 
#  How have emissions from motor vehicle sources changed 
#  from 1999–2008 in Baltimore City? 
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
setkey(dt.scc, SCC)
setkey(dt, SCC)
dt <- merge(dt, dt.scc)

dt.emiss.plot.mv <- dt[grepl("motor vehicle",scc$Short.Name.chr,ignore.case = TRUE)]
dt.emiss.plot <- dt.emiss.plot.mv[fips == "24510"]
summary(dt.emiss.plot.mv)
unique(dt.emiss.plot$fips)
unique(dt.emiss.plot$Pollutant)
dt.emiss.plot <- dt.emiss.plot[, .(emissions = sum(Emissions)), by = year]
g <- ggplot(dt.emiss.plot, aes(x =year,y = emissions)) + 
        geom_point(color = "red", size = 3) + geom_smooth(method = "lm") + 
        ggtitle("DataTable Version using ggplot, Baltimore City Motor Vehicle Emissions")
dt.emiss.plot
g
plot5 <- g
dev.copy(png, file = "plot5.png")
dev.off()
#  I did notice that data for years 1999, 2005 are missing if data tables are not
#  merged.  
#  Using merged data tables, if in line 50 I use scc$Short.Name.chr 2005 data is 
#  missing.  If I use the merged data table, ie Short.Name.chr, then 1999 and 2008
#  data are missing.
