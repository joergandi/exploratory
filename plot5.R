plot5 <- function(datadir="exdata-data-NEI_data") {
  
  nei<-readRDS(paste(datadir,"/","summarySCC_PM25.rds",sep=""))
  scc<-readRDS(paste(datadir,"/","Source_Classification_Code.rds",sep=""))
  #subset to location baltimore
  nei<-subset(nei,fips == "24510") 
  #merge is too costly: retrieve vehicle-related sccs
  veh = grep("Highway Vehicles",scc$SCC.Level.Two); 
  vehsccs<-scc$SCC[veh]
  #subset to vehicle variables only
  nei<-subset(nei,SCC %in% vehsccs)
  #use year as factor
  nei<-transform(nei,year=as.factor(year))
  #sum emissions per year
  totalperyear<-tapply(nei$Emissions,nei$year,sum)
  #convert to dataframe with years as column
  years<-rownames(totalperyear)
  totals<-data.frame(totalperyear,years)
  
  png("plot5.png",width=960,height=480)
  
  par(mfrow = c(1, 2)) 
  #total emissions over time
  barplot(totals$totalperyear,names.arg=totals$years,ylab="Total pm2.5 motor vehicle emissions (tons)",main="Evolution of total pm2.5 motor vehicle emissions \n in Baltimore between 1999 and 2008",xlab="year")
  #assignment asks for "CHANGE", not total emissions only? mean and var seem interesting too?
  boxplot(log(Emissions)~year,data=nei,ylab="log(pm2.5 motor vehicle emissions in tons)",main="Mean and variation of pm2.5 motor vehicle emissions \n in Baltimore between 1999 and 2008",xlab="year")
  
  dev.off()
}