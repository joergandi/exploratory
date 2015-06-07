plot2 <- function(datadir="exdata-data-NEI_data") {
  nei<-readRDS(paste(datadir,"/","summarySCC_PM25.rds",sep=""))
  #subset to location baltimore
  nei<-subset(nei,fips == "24510")
  #use year as factor
  nei<-transform(nei,year=as.factor(year))
  #sum emissions per year
  totalperyear<-tapply(nei$Emissions,nei$year,sum)
  #convert to dataframe with year column
  years<-rownames(totalperyear)
  totals<-data.frame(totalperyear,years)
  
  png("plot2.png",width=480,height=480)
  #assignment asks for total emissions
  barplot(totals$totalperyear,names.arg=totals$years,ylab="Total pm2.5 emissions (tons)",main="Evolution of total pm2.5 emissions \n between 1999 and 2008 in Baltimore",xlab="year")
  
  dev.off()
}