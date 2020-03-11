source("./packages.r")
source("./model-functions.r")
source("./optimization-functions.r")

load("./Rdata/processed_data.Rdata")


pop <- 10458365
totalarea <- totalarea_bw
pa <- pa_bw

dm <- demands[,c(1,2)]
emoa_base <- EMOA(MU=1500, LAMBDA= 200, MAX.ITER = 10,start=400)
save(emoa_base,file = "./Rdata/emoa_base.Rdata")

emoa_base <- EMOA(MU=1500, LAMBDA= 200, MAX.ITER = 3000,POPULATION = emoa_base$population)
save(emoa_base,file = "./Rdata/emoa_base2.Rdata")


#----------------

dm <- demands[,c(1,3)]
emoa_vegi <- EMOA(MU=1500, LAMBDA= 200, MAX.ITER = 10,start=400)
save(emoa_vegi,file = "./Rdata/emoa_vegi.Rdata")

emoa_vegi <- EMOA(MU=1500, LAMBDA= 200, MAX.ITER = 3000,POPULATION = emoa_vegi$population)
save(emoa_vegi,file = "./Rdata/emoa_vegi2.Rdata")


#--------------------
dm <- demands[,c(1,4)]
emoa_vegan <- EMOA(MU=1500, LAMBDA= 200, MAX.ITER = 10,start=400)
save(emoa_vegan,file = "./Rdata/emoa_vegan.Rdata")

emoa_vegan <- EMOA(MU=1500, LAMBDA= 200, MAX.ITER = 3000,POPULATION = emoa_vegan$population)
save(emoa_vegan,file = "./Rdata/emoa_vegan2.Rdata")

load("./Rdata/emoa_base2.Rdata")
base <- emoa_base
load("./Rdata/emoa_vegi2.Rdata")
vegi <- emoa_vegi
load("./Rdata/emoa_vegan2.Rdata")
vegan <- emoa_vegan


setwd(folder)

impbase<-levelplot(population=base$population ,ft =base$fitness, des= desirability,plotname="impacts_base",tit="Levelplots for base scenario")
impvegi<-levelplot(population=vegi$population ,ft =vegi$fitness, des= desirability,plotname="impacts_vegi",tit="Levelplots for vegi scenario")
impvegan<-levelplot(population=vegan$population ,ft =vegan$fitness, des= desirability,plotname="impacts_vegan",tit="Levelplots for vegan scenario")


impbase<-levelplot(population=emoa_base$population ,ft =emoa_base$fitness, des= desirability,plotname="impacts_base",tit="Levelplots for base scenario")
impvegi<-levelplot(population=emoa_vegi$population ,ft =emoa_vegi$fitness, des= desirability,plotname="impacts_vegi",tit="Levelplots for vegi scenario")
impvegan<-levelplot(population=emoa_vegan$population ,ft =emoa_vegan$fitness, des= desirability,plotname="impacts_vegan",tit="Levelplots for vegan scenario")

#IMPACTS

impbase$max_impacts
impvegi$max_impacts
impvegan$max_impacts

impbase$opt_impacts
impvegi$opt_impacts
impvegan$opt_impacts

#SHARE ORGANIC

print(paste("organicshare:",round(sum(impbase$opt_pars[[1]][seq(from=1,to = 31 , by =2)])/sum(impbase$opt_pars[[1]]),2)))
print(paste("organicshare:",round(sum(impvegi$opt_pars[[1]][seq(from=1,to = 31 , by =2)])/sum(impvegi$opt_pars[[1]]),2)))
print(paste("organicshare:",round(sum(impvegan$opt_pars[[1]][seq(from=1,to = 31 , by =2)])/sum(impvegan$opt_pars[[1]]),2)))

# AREA USED MAX

round(1- (area(impbase$max_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/sum(totalarea_bw)),2)
round(1-area(impvegi$max_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/sum(totalarea_bw),2)
round(1-area(impvegan$max_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/sum(totalarea_bw),2)

#PASTURE USED MAX 

round(1-(pasture(impbase$max_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/pa),2)
round(1-(pasture(impvegi$max_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/pa),2)
round(1-(pasture(impvegan$max_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/pa),2)

#AREA USED OPT

round(1-(area(impbase$opt_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/sum(totalarea_bw)),2)
round(1-(area(impvegi$opt_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/sum(totalarea_bw)),2)
round(1-(area(impvegan$opt_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/sum(totalarea_bw)),2)

#PASTURE USED OPT

round(1-(pasture(impbase$opt_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/pa),2)
round(1-(pasture(impvegi$opt_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/pa),2)
round(1-(pasture(impvegan$opt_pars[[1]],AREA=totalarea_bw,PASTURE = pa)/pa),2)

#RADRACHART

png("./radarchart.png",height=500, width = 1000)
par(mar=c(0,0,3,0))
par(mfrow=c(1,3))

toradar <- impbase$opt_impacts
toradar <- rbind(impbase$max_impacts,rep(0,5),impbase$opt_impacts,impbase$max_impacts)
colnames(toradar) <- c("CC","ME","TAC","TET","LSS")
radarchart(as.data.frame(toradar), title="base demand", seg=5,vlcex=2,cex.main = 3,plwd=2,cglwd=1.5)

toradar <- impvegi$opt_impacts
toradar <- rbind(impvegi$max_impacts,rep(0,5),impvegi$opt_impacts,impvegi$max_impacts)
colnames(toradar) <- c("CC","ME","TAC","TET","LSS")
radarchart(as.data.frame(toradar),title = "vegetarian demand", seg=5,vlcex=2,cex.main = 3,plwd=2,cglwd=1.5)
legend("bottom",c("max LSS","MO optimum"), col=c("red","black"),lty = 1,cex=2)

toradar <- impvegan$opt_impacts
toradar <- rbind(impvegan$max_impacts,rep(0,5),impvegan$opt_impacts,impvegan$max_impacts)
colnames(toradar) <- c("CC","ME","TAC","TET","LSS")
radarchart(as.data.frame(toradar), title = "vegan demand",seg=5,vlcex=2,cex.main = 3,plwd=2,cglwd=1.5)

dev.off()


