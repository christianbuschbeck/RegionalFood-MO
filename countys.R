folder <- dirname(rstudioapi::getActiveDocumentContext()$path)

bev <- raster(paste(folder,"/GIS_data/Bevoelkerungszahl.tif",sep=""))
load(paste(folder,"/GIS_data/landclasses.Rdata",sep=""))
load(paste(folder,"/GIS_data/pastures.Rdata",sep=""))
rgbz <- st_read(paste(folder,"/GIS_data/VG1000_RBZ.shp",sep=""))
krs <- st_read(paste(folder,"/GIS_data/VG1000_KRS.shp",sep=""))
lan <- st_read(paste(folder,"/GIS_data/VG1000_LAN.shp",sep=""))

bawue <- subset(lan,GEN=="Baden-Württemberg")[1,]


krsinbw <- st_intersection(bawue,krs)
krsinbw <- subset(krs, is.element(krs$GEN,krsinbw$GEN.1))
n <- nrow(krsinbw)
krsinbw <- st_transform(krsinbw,4326)

krsinbw_spat <- as_Spatial(krsinbw)
plot(krsinbw_spat)
lab<-polygonsLabel(krsinbw_spat,labels=c(1:n),method="inpolygon",doPlot=F)
shadowtext(lab,labels=c(1:n),col="green",cex=0.8,bg="white")

bawue_proj <-st_transform(bawue,4326)
plot(st_geometry(bawue_proj),add=T,border="red")
notin <- c(6,7,3,4,5,1,2,56,57,55,58,54,53,52,65,60,61,62,59,64,66,63)

krsinbw <- krsinbw[-notin,]
krs <- st_transform(krs,4326)

freiburg <- st_transform(subset(rgbz,GEN =="Freiburg"),4326)
karlsruhe <- st_transform(subset(rgbz,GEN =="Karlsruhe"),4326)
stuttgart <- st_transform(subset(rgbz,GEN =="Stuttgart"),4326)
tuebingen <- st_transform(subset(rgbz,GEN =="Tübingen"),4326)

#######################
## Regierunsbezirk ####
#######################

dm <- demands[,c(1,2)]


#Freiburg
pop <- freiburg$EWZ
totalarea <- gettotalarea(st_intersection(landclasses,freiburg))
pa <- as.numeric(sum(st_area(st_intersection(pastures,freiburg)))/10000)

perfect <- limitless_sma(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
up <- perfect$prodquantity + 0.0001
up[c(8,16)]<- sum(up[c(8,16)])
up[c(22,28)]<- sum(up[c(22,28)])
up[c(21,23,25,27)] <- up[c(22,24,26,28)]
up[29:32]<- perfect$td_all[12,2] + 0.0001 

low <- rep(0.000001,32)

mat <- matrix(ncol=32,nrow=50)

#Applying the semi mechanistic approach to retrieve starting populations
for(k in 1:50){
  c <- sma_c(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
  mat[k,] <- c$prodquantity
}
suggestedSol <- mat

#Running the ga function to optimize LSS
GA_freiburg <-ga(type="real-valued", fitness= fit_LSS,lower=low,upper= up,maxiter=200,popSize = 50,pmutation=0.2,suggestions = suggestedSol)
save(GA_freiburg,file=paste(folder,"/Rdata/GA_freiburg.Rdata",sep=""))


#karlsruhe
pop <- karlsruhe$EWZ
totalarea <- gettotalarea(st_intersection(landclasses,karlsruhe))
pa <- as.numeric(sum(st_area(st_intersection(pastures,karlsruhe)))/10000)

perfect <- limitless_sma(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
up <- perfect$prodquantity + 0.0001
up[c(8,16)]<- sum(up[c(8,16)])
up[c(22,28)]<- sum(up[c(22,28)])
up[c(21,23,25,27)] <- up[c(22,24,26,28)]
up[29:32]<- perfect$td_all[12,2] + 0.0001 

low <- rep(0.000001,32)

mat <- matrix(ncol=32,nrow=50)

#Applying the semi mechanistic approach to retrieve starting populations
for(k in 1:50){
  c <- sma_c(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
  mat[k,] <- c$prodquantity
}
suggestedSol <- mat

#Running the ga function to optimize LSS
GA_karlsruhe <-ga(type="real-valued", fitness= fit_LSS,lower=low,upper= up,maxiter=200,popSize = 50,pmutation=0.2,suggestions = suggestedSol)
save(GA_karlsruhe,file=paste(folder,"/Rdata/GA_karlsruhe.Rdata",sep=""))



#stuttgart
pop <- stuttgart$EWZ
totalarea <- gettotalarea(st_intersection(landclasses,stuttgart))
pa <- as.numeric(sum(st_area(st_intersection(pastures,stuttgart)))/10000)

perfect <- limitless_sma(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
up <- perfect$prodquantity + 0.0001
up[c(8,16)]<- sum(up[c(8,16)])
up[c(22,28)]<- sum(up[c(22,28)])
up[c(21,23,25,27)] <- up[c(22,24,26,28)]
up[29:32]<- perfect$td_all[12,2] + 0.0001 

low <- rep(0.000001,32)

mat <- matrix(ncol=32,nrow=50)


#Applying the semi mechanistic approach to retrieve starting populations
for(k in 1:50){
  c <- sma_c(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
  mat[k,] <- c$prodquantity
}
suggestedSol <- mat

#Running the ga function to optimize LSS
GA_stuttgart <-ga(type="real-valued", fitness= fit_LSS,lower=low,upper= up,maxiter=200,popSize = 50,pmutation=0.2,suggestions = suggestedSol)
save(GA_stuttgart,file=paste(folder,"/Rdata/GA_stuttgart.Rdata",sep=""))




#Tuebingen
pop <- tuebingen$EWZ
totalarea <- gettotalarea(st_intersection(landclasses,tuebingen))
pa <- as.numeric(sum(st_area(st_intersection(pastures,tuebingen)))/10000)

perfect <- limitless_sma(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
up <- perfect$prodquantity + 0.0001
up[c(8,16)]<- sum(up[c(8,16)])
up[c(22,28)]<- sum(up[c(22,28)])
up[c(21,23,25,27)] <- up[c(22,24,26,28)]
up[29:32]<- perfect$td_all[12,2] + 0.0001 

low <- rep(0.000001,32)

mat <- matrix(ncol=32,nrow=50)

#Applying the semi mechanistic approach to retrieve starting populations
for(k in 1:50){
  c <- sma_c(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
  mat[k,] <- c$prodquantity
}
suggestedSol <- mat

#Running the ga function to optimize LSS
GA_tuebingen <-ga(type="real-valued", fitness= fit_LSS,lower=low,upper= up,maxiter=200,popSize = 50,pmutation=0.2,suggestions = suggestedSol)
save(GA_tuebingen,file=paste(folder,"/Rdata/GA_tuebingen.Rdata",sep=""))

load(paste(folder,"/Rdata/GA_stuttgart.Rdata",sep=""))
load(paste(folder,"/Rdata/GA_karlsruhe.Rdata",sep=""))
load(paste(folder,"/Rdata/GA_freiburg.Rdata",sep=""))
load(paste(folder,"/Rdata/GA_tuebingen.Rdata",sep=""))

setwd(folder)

png("districts.png",width = 500, height=600)
plot(st_transform(bawue$geometry,4326),main = "LSS-values for districts within BW [%]")
plot(as_Spatial(stuttgart),add=T)
polygonsLabel(as_Spatial(stuttgart),labels=round(summary(GA_stuttgart)$fitness*100),col="red",cex=1.5)
plot(as_Spatial(karlsruhe),add=T)
polygonsLabel(as_Spatial(karlsruhe),labels=round(summary(GA_karlsruhe)$fitness*100),col="red",cex=1.5)
plot(as_Spatial(freiburg),add=T)
polygonsLabel(as_Spatial(freiburg),labels=round(summary(GA_freiburg)$fitness*100),col="red",cex=1.5)
plot(as_Spatial(tuebingen),add=T)
polygonsLabel(as_Spatial(tuebingen),labels=round(summary(GA_tuebingen)$fitness*100),col="red",cex=1.5)
dev.off()



########################
####   Kreise    #######
########################

Kreis <- paste(krsinbw$BEZ,krsinbw$GEN,sep =" ")
LSS <- numeric(length(Kreis))
krs_LSS <- data.frame(Kreis,LSS)

for(i in 1:nrow(krsinbw)){
  
  kreis <- krsinbw[i,]
  
  #Retrieving population size
 
  pop <-kreis$EWZ
  
  int_p  <- st_intersection(pastures,kreis)
  int_lc <- st_intersection(landclasses,kreis)
  
  #Calculating arable land and pasture
  pa <- as.numeric(sum(st_area(int_p))/10000)
  totalarea <-gettotalarea(int_lc = int_lc)
  
  perfect <- limitless_sma(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
  up <- perfect$prodquantity + 0.0001
  up[c(8,16)]<- sum(up[c(8,16)])
  up[c(22,28)]<- sum(up[c(22,28)])
  up[c(21,23,25,27)] <- up[c(22,24,26,28)]
  up[29:32]<- perfect$td_all[12,2] + 0.0001 
  
  low <- rep(0.000001,32)
  
  suggestedSol <- matrix(ncol=32,nrow=50)
  
  #Applying the semi mechanistic approach to retrieve starting populations
  for(k in 1:50){
    c <- sma_c(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
    suggestedSol[k,] <- c$prodquantity
  }
  
  #Running the ga function to optimize LSS
  GA_kreis <-ga(type="real-valued", fitness= fit_LSS,lower=low,upper= up,maxiter=100,popSize = 50,pmutation=0.2,suggestions = suggestedSol)
  krs_LSS[i,2] <-summary(GA_kreis)$fitness
  
}
save(krs_LSS,file=paste(folder,"/Rdata/kreise_LSS.Rdata",sep=""))

#load(paste(folder,"/Rdata/kreise_LSS.Rdata",sep=""))

krsinbw_spat <- as_Spatial(krsinbw)

par(mfrow=c(1,1))
png("countys.png",width = 500, height=600)

plot(krsinbw_spat,main = "LSS-values for counties within BW [%]")
lab<-polygonsLabel(krsinbw_spat,labels=round(krs_LSS$LSS*100),method="inpolygon",doPlot=F)
shadowtext(lab,labels=round(krs_LSS$LSS*100),col="red",cex=1.2,bg="white")
dev.off()

fortextable <- krs_LSS
fortextable$LSS <- round(fortextable$LSS*100)
require(xtable)
xtable(fortextable,digits = 0,row.names)


