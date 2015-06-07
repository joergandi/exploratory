plot3 <- function(datadir="exdata-data-NEI_data") {
  
  library("ggplot2")
  library("reshape2") #for melt
  
  nei<-readRDS(paste(datadir,"/","summarySCC_PM25.rds",sep=""))
  #subset to location baltimore
  nei<-subset(nei,fips == "24510")
  #use year and type as factors
  nei<-transform(nei,year=as.factor(year))
  nei<-transform(nei,type=as.factor(type))
  #sum emissions over each (year, type) pair
  totalperyearpertype<-tapply(nei$Emissions,list(nei$year,nei$type),sum)
  #convert to dataframe with year column
  totals<-data.frame(totalperyearpertype)
  totals$years=rownames(totals)
  #convert to tall frame with type as variable instead of column
  t2<-melt(totals,id.vars="years",variable.name="type")
  t2<-transform(t2,years=as.integer(years))
  #assignment asks for total emissions per type of source, in ggplot
  g<-qplot(x=years,y=value,data = t2,geom=c("point","line"),col=type,xlim=c(1999,2008),ylab="total pm2.5 emissions (tons)",main = "Evolution of total pm2.5 emissions \n in Baltimore per source type")
  
  png("plot3.png",width=480,height=480)
  print(g)
  dev.off()
}