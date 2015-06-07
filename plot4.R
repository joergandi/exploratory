plot4 <- function(datadir="exdata-data-NEI_data") {
  
  nei<-readRDS(paste(datadir,"/","summarySCC_PM25.rds",sep=""))
  scc<-readRDS(paste(datadir,"/","Source_Classification_Code.rds",sep=""))

  #merge is too costly: retrieve coal-related sccs,assumes that all coal sccs are related to combustion processes
  coal = grep("Coal",scc$Short.Name); 
  coalsccs<-scc$SCC[coal]
  #subset to coal variables only
  nei<-subset(nei,SCC %in% coalsccs)
  #use year as factor
  nei<-transform(nei,year=as.factor(year))
  #sum emissions per year
  totalperyear<-tapply(nei$Emissions,nei$year,sum)
  #convert to dataframe with years as column
  years<-rownames(totalperyear)
  totals<-data.frame(totalperyear,years)
  
  png("plot4.png",width=960,height=480)
  
  par(mfrow = c(1, 2)) 
  #total emissions over time
  barplot(totals$totalperyear,names.arg=totals$years,ylab="Total pm2.5 coal emissions (tons)",main="Evolution of total pm2.5 coal emissions \n in the US between 1999 and 2008",xlab="year")
  #assignment asks for "CHANGE", not total emissions only? mean and var seem interesting too?
  boxplot(log(Emissions)~year,data=nei,ylab="log(pm2.5 coal emissions in tons)",main="Mean and variation of pm2.5 coal emissions \n in the US between 1999 and 2008",xlab="year")
  
  dev.off()
}