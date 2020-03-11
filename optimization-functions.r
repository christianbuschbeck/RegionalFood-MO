#Multiobjective Optimization
#''''''''''''''''''''

#This function is the fitness function for the multiobjective optimization. It is a combination of 4 impact categories and missing LSS values
multifit <-function(prodquantity){
  
  #The function LCA is called to retrieve impact values
  res <- as.numeric(LCA(prodquantity)) 
  #Missing LSS values are calculated
  missLSS <- 100 - (fit_LSS(prodquantity)*100)
  
  return(c(res,missLSS))
  
}

#The evolutionary multi objective algorithm (EMOA) function can be used to perform the MO.
#MU is the population size. LAMBDA is the number of offspring to create. MAX.ITER is the maximum iteration number.
#P.mut and p.recomb are the probabilities to mutate and recombinate. For POPULATION a starting population can be given.
#start defines how many starting populations are created with the semi mechanistic approach
EMOA <- function(MU = 1000,LAMBDA = 20, MAX.ITER = 1000,p.mut=0.2,p.recomb=0.7,POPULATION=0,start=50){
  
  #First the boundaries are calculated
  perfect <- limitless_sma(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
  up <- perfect$prodquantity
  up[c(8,16)]<- sum(up[c(8,16)])
  up[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)] <- up[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)]
  up[29:32]<- perfect$td_all[12,2] 
  up <- up + 0.0001
  low <- rep(0.000001,32)
  
  #The objective function is declared
  fn = multifit
  
  #The control object contains relevant parameters for the optimization
  control = initECRControl(fitness.fun=fn,n.objectives = 5,minimize = T)
  control = registerECROperator(control, "mutate", mutGauss, sdev = 5, lower = low, upper = up) # Mutation function is selected
  control = registerECROperator(control,"recombine",recIntermediate) # Recombination function is selected
  
  control = registerECROperator(control, "selectForSurvival", selNondom) # Selection function for survival is selected
  control = registerECROperator(control, "selectForMating", selNondom) # Selection function for mating is selected
  
  #In case POPULATION is not given the first population will partly be created with the semi mechanistic approach
  if(class(POPULATION)=="numeric"){
    
    #First a random population is determined
    population = genReal(n=MU, n.dim=32, lower=low, upper=up)
    
    #Then the semi mechanistic approach for conventional production and the semi mechanistic approach for organic production
    #both genereate some individuals for the starting population
    for(k in 1:start){
      print(paste("iteration c",k))
      c <- sma_c(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
      population[[k]] <- c$prodquantity+0.00001
    }
    
    for(k in 1:start){
      print(paste("iteration o",k))
      o <- sma_o(pop = pop,PRINT=F,DEMAND=dm,AREA=totalarea,PASTURE=as.numeric(pa))
      population[[k+start]] <- o$prodquantity+0.00001
    }
    
    
    
    
  }
  
  
  #The fitness of the population is calculated
  if(class(POPULATION) =="list"){population <-POPULATION}
  fitness = evaluateFitness(control, population)
  
  #Here the iteration starts
  for (i in seq_len(MAX.ITER)) {
    print(paste("iter:",i))
    
    # generate offspring by mutation and recombination and evaluate their fitness
    offspring=generateOffspring(control, population, fitness, lambda=LAMBDA, p.recomb = p.recomb,p.mut = p.mut)
    fitness.o = evaluateFitness(control, offspring)
    
    # now select the best out of the union of population and offspring
    sel = replaceMuPlusLambda(control=control, population=population, offspring=offspring, fitness=fitness, fitness.offspring =fitness.o)
    
    population = sel$population
    fitness = sel$fitness
  }
  
  
  l <- list()
  l[["population"]] <- population
  l[["fitness"]] <- fitness
  
  return(l)
}

#The checkdesire function is called for levelplots. It transforms desirabilities into a score
checkdesire<- function(ft){
  
  vec <- numeric(5)
  for(i in 1:5){
    if(ft[i] < desirability[i,2]) {vec[i] <- 0
    next}
    if(ft[i] < desirability[i,3]) {vec[i] <- 1
    next}
    if(ft[i] < desirability[i,4]) {vec[i] <- 6
    next}
    if(ft[i] < desirability[i,5]) {vec[i] <- 31
    next}
    if(ft[i] >= desirability[i,5]) {vec[i] <- 156
    next}
  }
  return(vec)
}

#levelplots are created with the following funciton. It needs a desirabilitymatrix (des), a fitness matrix (ft) a list with
#points of the pareto set (population),the number of points (n) and plotname and title
levelplot <- function(des,ft,n=1500,plotname,tit,population){
  fitness <<- ft
  fitness[c(1:4),] <- fitness[c(1:4),]/1000000
  desirability <<- des
  
  #Maximum and minimum values are calculated for the normalization
  
  jM <- apply(fitness,1,max)
  jm <- apply(fitness,1,min)
  
  norm <- numeric(n)
  
  #all points in the pareto set are normalized
  for(i in 1:n){
    
    ji <- fitness[,i]
    jiq<-((ji - jm)/(jM-jm))
    norm[i] <- max(jiq)
  }
  
  #all points are ordered according to their norm values
  ord <- order(norm,decreasing = F)
  fitnesssort <- fitness[,ord]
  
  
  normsort <- norm[ord]
  populationsort <- population[ord]
  
  #scoring
  fitness_score<-apply(fitnesssort,2,checkdesire)
  score<<-apply(fitness_score,2,sum)
  
  #define plot settings
  cols <- topo.colors(max(score))
  mains <- c("climate change", "marine eutrophication","terrestrial acidification","terrestrial ecotoxicity","level of self sufficiency")
  xlb <- c("kt CO2-eq.","kt N-eq.","kt SO2-eq.","kt 1,4-DCB-eq."," %")
  heat <- rev(heat.colors(n=5,alpha=0.2))
  
  #identifying the best point
  bescht <- which(normsort == min(normsort[order(score)[1:10]]))
  bescht <- bescht[1]
  
  maxLSS <- which(fitnesssort[5,] == min(fitnesssort[5,]))
  maxLSS <- maxLSS[1]
  
  png(paste(plotname,".png",sep=""),width = 1200, height=800)
  
  #plotting
  par(mfrow=c(2,3),cex=1.3,mar=c(5, 4, 5, 2) + 0.1)
  xlimitslow <- c(500,3,0,0,0)
  xlimitsup <- c(6000,35,95,450,100)
  
  fitnesssort[5,] <- 100 - fitnesssort[5,]
  desirability[5,] <- 100-  desirability[5,]
  for(i in 1:5){
    plot(normsort~fitnesssort[i,],pch=16,col=cols[score],xlab=xlb[i],ylab="inf.-norm",main=mains[i],xlim = c(xlimitslow[i],xlimitsup[i]))
    rect(desirability[i,1],-1,desirability[i,2],2,col=heat[1],border = "transparent")
    rect(desirability[i,2],-1,desirability[i,3],2,col=heat[2],border = "transparent")
    rect(desirability[i,3],-1,desirability[i,4],2,col=heat[3],border = "transparent")
    rect(desirability[i,4],-1,desirability[i,5],2,col=heat[4],border = "transparent")
    rect(desirability[i,5],-1,desirability[i,6],2,col=heat[5],border = "transparent")
    points(normsort~fitnesssort[i,],pch=16,col=cols[score],xlab=xlb[i],ylab="inf.-norm",main=mains[i],xlim = c(xlimitslow[i],xlimitsup[i]))
    abline(v = desirability[i,],lty=3,col="grey")
    points(normsort[bescht]~fitnesssort[i,bescht],col="red",pch=17,cex=1.8)
    points(normsort[maxLSS]~fitnesssort[i,maxLSS],col="red",pch=15,cex=1.8)
  }
  
  plot.new()
  
  legend("topleft",legend=c("HD","D","T","UD","HUD"),fill=heat,title="Desirability",cex=1.4,bty="n")
  legend.col(col=cols,xs=0.2)
  dev.off()
  
  bestlist <- list()
  bestlist[["opt_impacts"]] <- round(fitnesssort[,bescht])
  bestlist[["opt_pars"]] <- populationsort[bescht]
  bestlist[["max_impacts"]] <- round(fitnesssort[,maxLSS])
  bestlist[["max_pars"]] <- populationsort[maxLSS]
  
  
  desirability[5,] <- 100-  desirability[5,]
  
  return(bestlist)
}

#Legend col is a helper function for the levelplot function
legend.col <- function(col,xs=0,ys=0){
  
  
  n <- length(col)
  
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)-0.6
  xx[c(3,4)] <- xx[c(3,4)]+0.1
  xx <- xx+xs
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- (c(box.cy[1] + (box.sy * (i - 1)),
             box.cy[1] + (box.sy * (i)),
             box.cy[1] + (box.sy * (i)),
             box.cy[1] + (box.sy * (i - 1)))*0.7)
    yy <- yy+ 0.2+ys
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  text(c(0.5,0.63,0.65)+xs,c(1,0.19,0.95)+ys,c("score",paste("-",min(score),sep=" "),paste("-",max(score),sep=" ")),cex=1.2)
  
}
