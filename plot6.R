plot6 <- function(datadir="exdata-data-NEI_data") {
  
  library(ggplot2)
  library(grid)
  library(gridExtra)
  #helper function to subset to location and sourcetype and sum total emissions
  subset_by_fips_and_scc<-function(nei, fipsstr,sccs) {
    nei_s<-subset(nei,fips==fipsstr)  
    nei_s<-subset(nei_s,SCC %in% sccs) 
    #nei_s<-transform(nei_s,year=as.factor(year))
    totalperyear<-tapply(nei_s$Emissions,nei_s$year,sum)
    years<-as.numeric(rownames(totalperyear))
    totals<-data.frame(totalperyear,years)
    return(totals)
  }
  
  nei<-readRDS(paste(datadir,"/","summarySCC_PM25.rds",sep=""))
  scc<-readRDS(paste(datadir,"/","Source_Classification_Code.rds",sep=""))
  
  veh = grep("Highway Vehicles",scc$SCC.Level.Two); 
  vehsccs<-scc$SCC[veh]
  
  totals_ba<-subset_by_fips_and_scc(nei, "24510",vehsccs)  #baltimore, cars
  totals_la<-subset_by_fips_and_scc(nei, "06037",vehsccs)  #la county, cars
  
  totals_ba$city <- rep("Baltimore City",nrow(totals_ba))
  totals_la$city <- rep("Los Angeles County",nrow(totals_la))
  
  t<-rbind(totals_ba,totals_la)
  t<-transform(t,city<-as.factor(city))
  g<-qplot(x=years,y=totalperyear,data=t,geom=c("point","line"),col=city,ylab="total pm2.5 motor vehicle emissions (tons)",main = "Evolution of total pm2.5 motor vehicle emissions")

  #assignment asks for "CHANGE", not just total emissions? mean and var also interesting to assess change?
  nei_ba<-subset(nei,fips=="24510")  
  nei_ba<-subset(nei_ba,SCC %in% vehsccs) 
  nei_ba$city<-rep("Baltimore City",nrow(nei_ba))
  nei_la<-subset(nei,fips=="06037")  
  nei_la<-subset(nei_la,SCC %in% vehsccs) 
  nei_la$city<-rep("Los Angeles County",nrow(nei_la))
  t2<-rbind(nei_ba,nei_la)
  t2<-transform(t2,city = as.factor(city))
  t2<-transform(t2,year = as.factor(year))
  
  g2<-ggplot(t2,aes(year,log(Emissions))) + 
    geom_jitter(aes(color=city)) + 
    geom_boxplot(aes(color=city)) + 
    labs(title="Mean and variation of pm2.5 motor vehice emissions",y="log(pm2.5 motor vehicle emissions in tons)")
  
  png("plot6.png",width=1280,height=480)
  #print(g)
  # multiplot with ggplot doesnt work via par mfrow
  grid.arrange(g, g2, ncol = 2, main = "pm2.5 motor vehicle emissions")
  
  dev.off()
}

