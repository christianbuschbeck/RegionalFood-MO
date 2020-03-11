##############################
###     Functions          ###
##############################


#Agricultural allocation model:
#'''''''''''''''''''''''''''''
#This function is the imlementation of the agricultural allocation model
aam <- function(prodquantity,PRINT=F,AREA,PASTURE){
  
  #Area available and pasture available represent the initial area for the respective agricultural land
  areaavailable <- AREA
  pastureavailable <- PASTURE
  
  #The data frame prodquantity_dir is the direct product quantity
  prodquantity_dir <- as.data.frame(prod,stringsAsFactors = F)   
  prodquantity_dir["quant"] <- prodquantity   
  
  #The data frame prodquantity_ind is the indirect product quantity and is first created here as the vegetal quantity (first 20 elements)
  prodquantity_ind <- prodquantity_dir[1:20,]
  
  #The feeframe represents the fodder quantities,i.e. how much of each fodder is needed.
  #It is created from the alo_feedstuff data frame here because it already has the desired product column
  feedframe <- alo_feedstuff
  colnames(feedframe) <- c("prod","quant")
  feedframe[,2]<-0
  
  for(i in 21:32){
    feedframe[,2] <-feedframe[,2]+ prodquantity_dir[i,2]*as.numeric(feedstuff.demand[,i-20+1])
    
  }
  
  #The fodder quantities are combined with the vegetal quantities. Together they form the indirect product quantity.
  prodquantity_ind<- rbind(prodquantity_ind,feedframe)
  
  ### Crop rotation add up
  #Because some crops (e.g. wheat) exist as food crop and as fodder crop they are combine here to count as one when it comes to crop rotation
  prodquantity_ind[c(7,8,11,12,15,16,19,20),2]<- prodquantity_ind[c(7,8,11,12,15,16,19,20),2] + prodquantity_ind[c(39,26,41,28,43,30,40,27),2]
  prodquantity_ind[c(39,26,41,28,43,30,40,27),2] <- 0
  
  #The final need is later stored in the stillneeded vector
  stillneeded <- numeric(nrow(prodquantity_ind))
  
  #The products are ordered according to their maximum demand fullfilment rate
  rightorder <- c(11,12,15,16,7,8,19,20,1,2,3,4,5,6,9,10,13,14,17,18,33,22,34,23,35,29,36,24,37,25,39,26,40,27,43,30,44,31,21, 32, 38, 42,41,28)
  
  #The vector bef (before) is a container for the area that is reserved by organic cultivation 
  #It is as long as landclasses exist
  bef <-numeric(138)
  
  for(i in rightorder){
    
    #The need is the area which is needed to grow the current crop
    need <- prodquantity_ind[i,2]*alo[i,2]
    CR <- cr[which(cr[,2]==prodquantity_ind[i,1]),1][1]/100 # Crop rotation factor
    
    
    prodname <- str_split(prodquantity_ind[i,1],"_")[[1]][1]
    #The variable ocf describes whether the current product is organic or conventional
    #For organic products (uneven) it's 1, for conventional (even) it is zero
    ocf <- i%%2
    
    #If the current product is pasture the area available for pasture is diminished by the need
    #For all other products all land classes where the product can be grown are iterated
    if(prodname=="pasture"){diff <- pastureavailable-need
    pastureavailable <- ifelse(diff>0,diff,0)
    need <- ifelse(diff<0,abs(diff),0) }else{
      for(c in rev(lc[[prodname]])){
        
        areaavailable_cr <- min((AREA[c]-bef[c])*CR,areaavailable[c]) # The area available under crop rotation constraint is calculated
        bef[c] <- (areaavailable_cr/CR)*ocf #The area reserved for organic production is stored in bef if the product is organic (ocf=1) 
        need <- need-areaavailable_cr # The area available under crop rotation constraints is subtracted from the need and from the area available
        
        areaavailable[c] <- areaavailable[c] - areaavailable_cr    
        
        #If the need is smaller than zero the difference is added back to the area available.
        #Also the area occupied by organic production is added back to the bef vector
        if(need<0){areaavailable[c] <- areaavailable[c] + abs(need)
        bef[c] <- ((areaavailable_cr-abs(need))/CR)*ocf
        need <-0
        break}
        
        
      }}
    
    stillneeded[i] <- need
    if(PRINT==T){
      print(paste("AREA:",prodquantity_ind[i,1]))
      print(areaavailable)
      
    }
  }
  
  
  res <- stillneeded
  return(res)
}

#Demand fulfillment models
#''''''''''''''''''''''''
#The demand fulfillment models all work the same. They only differ in their output. Production yields
#what is produced. dfm yields demand fulfillment rates. dfm diff yields the difference between totaldemand and produced food
#Quantities are summed up according to their category and co products are produced
production <- function(prodquantity){
  
  prodquantity_dir <- as.data.frame(prod,stringsAsFactors = F)   
  prodquantity_dir["quant"] <- prodquantity   
  
  ## CO PRODS
  
  prodquantity_res <- demands[,c(1,2)]
  prodquantity_res[,2] <- 0
  
  for(i in 1:nrow(prodquantity_dir)){
    prodname <- str_split(prodquantity_dir[i,1],"_")[[1]][1]
    
    prodquantity_res[which(prodquantity_res$X==prodname),2] <- prodquantity_res[which(prodquantity_res$X==prodname),2] + prodquantity_dir[i,2]
    
    #Different names
    if(prodname=="rapeseed" | prodname=="sunflower"){prodquantity_res[which(prodquantity_res$X=="oil"),2] <- prodquantity_res[which(prodquantity_res$X=="oil"),2] + prodquantity_dir[i,2]} 
    if(prodname=="turkey" | prodname=="broiler"){prodquantity_res[which(prodquantity_res$X=="poultry"),2] <- prodquantity_res[which(prodquantity_res$X=="poultry"),2] + prodquantity_dir[i,2]} 
    if(prodname=="pig"){prodquantity_res[which(prodquantity_res$X=="pork"),2] <- prodquantity_res[which(prodquantity_res$X=="pork"),2] + prodquantity_dir[i,2]} 
    
    #Co products
    if(str_detect(prodname,"milk")){prodquantity_res[which(prodquantity_res$X=="beef"),2] <- prodquantity_res[which(prodquantity_res$X=="beef"),2] + prodquantity_dir[i,2]*milkco[i-28]} 
    if(str_detect(prodname,"egg")){prodquantity_res[which(prodquantity_res$X=="poultry"),2] <- prodquantity_res[which(prodquantity_res$X=="poultry"),2] + prodquantity_dir[i,2]*eggco[i-22]} 
    
  }
  res <-  prodquantity_res[,2]
  return(res)
}
dfm <- function(prodquantity,pop,DEMAND){
  
  #determine totaldemand 
  totaldemand <- DEMAND 
  totaldemand[,2] <- totaldemand[,2] *pop # 10.9 mio einwohner 
  totaldemand[,2] <- totaldemand[,2] / 1000000 # In kiloton 
  
  prodquantity_dir <- as.data.frame(prod,stringsAsFactors = F)   
  prodquantity_dir["quant"] <- prodquantity   
  
  ## CO PRODS
  
  prodquantity_res <- totaldemand
  prodquantity_res[,2] <- 0
  
  for(i in 1:nrow(prodquantity_dir)){
    prodname <- str_split(prodquantity_dir[i,1],"_")[[1]][1]
    
    prodquantity_res[which(prodquantity_res$X==prodname),2] <- prodquantity_res[which(prodquantity_res$X==prodname),2] + prodquantity_dir[i,2]
    
    #Different names
    if(prodname=="rapeseed" | prodname=="sunflower"){prodquantity_res[which(prodquantity_res$X=="oil"),2] <- prodquantity_res[which(prodquantity_res$X=="oil"),2] + prodquantity_dir[i,2]} 
    if(prodname=="turkey" | prodname=="broiler"){prodquantity_res[which(prodquantity_res$X=="poultry"),2] <- prodquantity_res[which(prodquantity_res$X=="poultry"),2] + prodquantity_dir[i,2]} 
    if(prodname=="pig"){prodquantity_res[which(prodquantity_res$X=="pork"),2] <- prodquantity_res[which(prodquantity_res$X=="pork"),2] + prodquantity_dir[i,2]} 
    
    #Co products
    if(str_detect(prodname,"milk")){prodquantity_res[which(prodquantity_res$X=="beef"),2] <- prodquantity_res[which(prodquantity_res$X=="beef"),2] + prodquantity_dir[i,2]*milkco[i-28]} 
    if(str_detect(prodname,"egg")){prodquantity_res[which(prodquantity_res$X=="poultry"),2] <- prodquantity_res[which(prodquantity_res$X=="poultry"),2] + prodquantity_dir[i,2]*eggco[i-22]} 
    #if(prodname=="pig"){prodquantity_res[which(prodquantity_res$X=="pork"),2] <- prodquantity_res[which(prodquantity_res$X=="pork"),2] + prodquantity_dir[i,2]} * pigco[i-24]
    
    
    
    
  }
  
  totaldemand[,2] <- (prodquantity_res[,2]/totaldemand[,2]) *100
  
  res <- totaldemand[,2]
  return(res)
}
dfm_diff <- function(prodquantity,pop,DEMAND){
  
  #totaldemand is determined
  totaldemand <- DEMAND 
  totaldemand[,2] <- totaldemand[,2] *pop # 10.9 mio einwohner 
  totaldemand[,2] <- totaldemand[,2] / 1000000 # In kiloton 
  
  prodquantity_dir <- as.data.frame(prod,stringsAsFactors = F)   
  prodquantity_dir["quant"] <- prodquantity   
  
  ## CO PRODS
  
  prodquantity_res <- totaldemand
  prodquantity_res[,2] <- 0
  
  
  
  for(i in 1:nrow(prodquantity_dir)){
    prodname <- str_split(prodquantity_dir[i,1],"_")[[1]][1]
    
    prodquantity_res[which(prodquantity_res$X==prodname),2] <- prodquantity_res[which(prodquantity_res$X==prodname),2] + prodquantity_dir[i,2]
    
    #Different names
    if(prodname=="rapeseed" | prodname=="sunflower"){prodquantity_res[which(prodquantity_res$X=="oil"),2] <- prodquantity_res[which(prodquantity_res$X=="oil"),2] + prodquantity_dir[i,2]} 
    if(prodname=="turkey" | prodname=="broiler"){prodquantity_res[which(prodquantity_res$X=="poultry"),2] <- prodquantity_res[which(prodquantity_res$X=="poultry"),2] + prodquantity_dir[i,2]} 
    if(prodname=="pig"){prodquantity_res[which(prodquantity_res$X=="pork"),2] <- prodquantity_res[which(prodquantity_res$X=="pork"),2] + prodquantity_dir[i,2]} 
    
    #Co products
    if(str_detect(prodname,"milk")){prodquantity_res[which(prodquantity_res$X=="beef"),2] <- prodquantity_res[which(prodquantity_res$X=="beef"),2] + prodquantity_dir[i,2]*milkco[i-28]} 
    if(str_detect(prodname,"egg")){prodquantity_res[which(prodquantity_res$X=="poultry"),2] <- prodquantity_res[which(prodquantity_res$X=="poultry"),2] + prodquantity_dir[i,2]*eggco[i-22]} 
    #if(prodname=="pig"){prodquantity_res[which(prodquantity_res$X=="pork"),2] <- prodquantity_res[which(prodquantity_res$X=="pork"),2] + prodquantity_dir[i,2]} * pigco[i-24]
    
    
    
    
  }
  
  totaldemand[,2] <- totaldemand[,2] - prodquantity_res[,2]
  
  res <- totaldemand[,2]
  return(res)
}

# Semi mechanistic applications
#''''''''''''''''''''''''''''''
# The semi mechanistic approach for conventional products is used for LSS and LCSS optimization
 
sma_c<-function(pop,PRINT=F,DEMAND,AREA,PASTURE,acc=0.01,stop=101){
  
  
  q <- rep(0,32) #The vector q contains all product quantities and every element is gradually increased
  count<-0  
  status <- "good"
  dl <- DEMAND # dl stands for demand left and assures later that no overproduction occurs
  totaldemand <- DEMAND # The totaldemand contains the totaldemand of each product category
  totaldemand[,2] <- totaldemand[,2]*pop / 1000000
  nofurther <- "beef" # Beef doesn't need to be produced extra and is therefore the first element in the nofurther vector.
  #if(sample(c(1,2),1) == 1)nofurther <- c(nofurther,"soybean")                    # All products that are in this vector will not increase further
  
  #Here the ratios for oil poultry and milk are created randomly.
  #Each ratiovec vector contains the probability for each product to be chosen in form of numbers of elements
  #Later from these vectors a sample will be drawn for each production step, defining which product to produce
  ratio <- sample(1:100,1)
  
  ratiovec_oil <- c(rep(1,ratio),rep(2,100-ratio))
  
  ratio <- sample(1:100,1)
  ratiovec_poultry <- c(rep(1,ratio),rep(2,100-ratio))
  
  ratio <- sample(1:100,1)
  ratiovec_milk <- c(rep(2,ratio),rep(4,100-ratio))
  
  #milk_inc gives sets how fast milk will increase. If it would exceed areal limitations with 5 percent,
  #it is set to one percent to maintain the same precision as other products
  milk_inc <- sample(c(0,10,100),1)
  
  for(i in 1:milk_inc){
    q.old <- q
    coin <- sample(ratiovec_milk,1)
    q[which(str_detect(prod,"milk"))[coin]] <- q[which(str_detect(prod,"milk"))[coin]]+totaldemand[which(totaldemand[,1]=="milk"),2]*acc
    if(any(aam(q,AREA=AREA,PASTURE=PASTURE)>0)){q <- q.old;break}
  }
  
  nofurther <- c(nofurther,"milk")
  #This while loop has 100 iterations because count increases with every one.
  #Because demand fulfillment rates increase by one percent they can reach exactly 100 %
  
  while(count < 100){
    
    if(count == stop)break
    q.old <- q # q.old is set to the current q. In case the area is exceeded q is set back to q.old and againg it is tried to increas demand fulfillment rates
    count <- count+1
    dl <- cbind(demands[,1],dfm_diff(q,pop=pop,DEMAND=DEMAND)) # The demand left (dfm_diff) is stored in dl
    
    #With every iteration all products are decided to need more and later those are excluded that exceed the area or have 100% demand fulfillment rate
    needmore <- dl[,1]
    nofurther <- unique(c(nofurther,dl[which(as.numeric(dl[,2])<0.01),1])) # Those that have almost 100 % dfr are declared to not increase further
    if(any(as.numeric(dl[-c(10,14),2])>1)==F)break # If the demand left is smaller than 1kt for each product full self sufficiency is reached and the loop breaks
    
    # Those products that should not be produced more are excluded from the needmore vector
    # If needmore is empty the limit is reached
    if(length(nofurther) > 0) needmore <- needmore[-which(is.element(needmore,nofurther))]
    if(length(needmore)==0){status <- "limit reached"
    break}
    
    if(PRINT==T)print(dl)
    
    #Now the needmore vector is iterated to increase the quantity q of those products that are to be increased by one
    #percent of the totaldemand of the corresponding category
    for(i in needmore){
      
      #For all vegetal crops except oil seeds this can be done without any decision
      if(is.element(i,cropnames[-c(4,8)])){q[which(str_detect(prod,i))][2] <- q[which(str_detect(prod,i))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      #the variable coin determines whether sunflower or rapeseed is produced
      if(i == "oil"){coin <- sample(ratiovec_oil,1)
      if(coin==1){q[which(str_detect(prod,"sunflower"))][2] <- q[which(str_detect(prod,"sunflower"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(coin==2){q[which(str_detect(prod,"rapeseed"))][2] <- q[which(str_detect(prod,"rapeseed"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      }
      
      #the variable coin determines whether broiler or turkey  is produced
      if(i == "poultry"){coin <- sample(ratiovec_poultry,1)
      if(coin==1){q[which(str_detect(prod,"broiler"))][2] <- q[which(str_detect(prod,"broiler"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(coin==2){q[which(str_detect(prod,"turkey"))][2] <- q[which(str_detect(prod,"turkey"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      }
      
      if(i =="pork"){q[which(str_detect(prod,"pig"))][2] <- q[which(str_detect(prod,"pig"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      if(i=="egg"){q[which(str_detect(prod,"egg"))][2] <- q[which(str_detect(prod,"egg"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      
      #the variable coin determines whether pasture fed or arab fed milk is produced
      
      if(any(aam(q,AREA=AREA,PASTURE=PASTURE)>0)){nofurther <- unique(c(nofurther,i))
      q <- q.old
      count <- count - 1
      break}
    }
    print(count)
    
  }
  l <- list()
  l[["prodquantity"]] <- q
  l[["produced"]] <- production(q)
  l[["td_all"]] <- totaldemand
  l[["td_tot"]] <- sum(totaldemand[,2])
  l[["share"]] <- l[["produced"]] /totaldemand[,2]
  l[["left"]] <- c(area(q,AREA=AREA,PASTURE=PASTURE),pasture(q,PASTURE=PASTURE))
  l[["wblss"]] <- sum(l[["produced"]])/sum(totaldemand[,2])
  print(status)
  return(l)
}
sma_o<-function(pop,PRINT=F,DEMAND,AREA,PASTURE,acc=0.01,stop=101){
  
  q <- rep(0,32)
  count<-0  
  status <- "good"
  dl <- DEMAND
  totaldemand <- DEMAND
  totaldemand[,2] <- totaldemand[,2]*pop / 1000000
  nofurther <- "beef"
  ratio <- sample(1:100,1)
  
  ratiovec_oil <- c(rep(1,ratio),rep(2,100-ratio))
  
  ratio <- sample(1:100,1)
  ratiovec_poultry <- c(rep(1,ratio),rep(2,100-ratio))
  
  ratio <- sample(1:100,1)
  ratiovec_milk <- c(rep(1,ratio),rep(3,100-ratio))
  
  milk_inc <- sample(c(0,10,100),1)
  
  for(i in 1:milk_inc){
    q.old <- q
    coin <- sample(ratiovec_milk,1)
    q[which(str_detect(prod,"milk"))[coin]] <- q[which(str_detect(prod,"milk"))[coin]]+totaldemand[which(totaldemand[,1]=="milk"),2]*acc
    if(any(aam(q,AREA=AREA,PASTURE=PASTURE)>0)){q <- q.old;break}
  }
  
  nofurther <- c(nofurther,"milk")
  
  while(count < 100){
    
    if(count == stop)break
    q.old <- q
    count <- count+1
    dl <- cbind(demands[,1],dfm_diff(q,pop=pop,DEMAND=DEMAND))
    
    needmore <- dl[,1]
    nofurther <- unique(c(nofurther,dl[which(as.numeric(dl[,2])<0.01),1]))
    if(any(as.numeric(dl[-c(10,14),2])>1)==F)break
    
    if(length(nofurther) > 0) needmore <- needmore[-which(is.element(needmore,nofurther))]
    if(length(needmore)==0){status <- "limit reached"
    break}
    
    if(PRINT==T)print(dl)
    
    for(i in needmore){
      
      if(is.element(i,cropnames[-c(4,8)])){q[which(str_detect(prod,i))][1] <- q[which(str_detect(prod,i))][1]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(i == "oil"){coin <- sample(ratiovec_oil,1)
      
      if(coin==1){q[which(str_detect(prod,"sunflower"))][1] <- q[which(str_detect(prod,"sunflower"))][1]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(coin==2){q[which(str_detect(prod,"rapeseed"))][1] <- q[which(str_detect(prod,"rapeseed"))][1]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      }
      
      if(i == "poultry"){coin <- sample(ratiovec_poultry,1)
      
      if(coin==1){q[which(str_detect(prod,"broiler"))][1] <- q[which(str_detect(prod,"broiler"))][1]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(coin==2){q[which(str_detect(prod,"turkey"))][1] <- q[which(str_detect(prod,"turkey"))][1]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      }
      
      if(i =="pork"){q[which(str_detect(prod,"pig"))][1] <- q[which(str_detect(prod,"pig"))][1]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      if(i=="egg"){q[which(str_detect(prod,"egg"))][1] <- q[which(str_detect(prod,"egg"))][1]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      
      if(any(aam(q,AREA=AREA,PASTURE=PASTURE)>0)){nofurther <- unique(c(nofurther,i))
      q <- q.old
      count <- count - 1
      break}
    }
    print(count)
    
  }
  l <- list()
  l[["prodquantity"]] <- q
  l[["produced"]] <- production(q)
  l[["td_all"]] <- totaldemand
  l[["td_tot"]] <- sum(totaldemand[,2])
  l[["share"]] <- l[["produced"]] /totaldemand[,2]
  l[["left"]] <- c(area(q,AREA=AREA,PASTURE=PASTURE),pasture(q,PASTURE=PASTURE))
  l[["wblss"]] <- sum(l[["produced"]])/sum(totaldemand[,2])
  print(status)
  return(l)
}

#The limitless semi mechanistic approach is always run with so much area and pasture as input that it is impossible not to reach full self sufficiency
limitless_sma<-function(pop,PRINT=F,DEMAND,AREA,PASTURE,acc=0.01){
  
  q <- rep(0,32)
  count<-0  
  status <- "good"
  dl <- DEMAND
  totaldemand <- DEMAND
  totaldemand[,2] <- totaldemand[,2]*pop / 1000000
  nofurther <- "beef"
  
  AREA <- AREA*1000
  PASTURE <- PASTURE*1000
  
  while(count<100){
    
    q.old <- q
    count <- count+1
    dl <- cbind(demands[,1],dfm_diff(q,pop=pop,DEMAND=DEMAND))
    
    needmore <- dl[,1]
    nofurther <- unique(c(nofurther,dl[which(as.numeric(dl[,2])<0.01),1]))
    if(any(as.numeric(dl[-c(10,14),2])>1)==F)break
    
    if(length(nofurther) > 0) needmore <- needmore[-which(is.element(needmore,nofurther))]
    if(length(needmore)==0){status <- "limit reached"
    break}
    
    if(PRINT==T)print(dl)
    
    for(i in needmore){
      
      if(is.element(i,cropnames[-c(4,8)])){q[which(str_detect(prod,i))][2] <- q[which(str_detect(prod,i))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(i == "oil"){coin <- sample(c(1,2),1)
      
      if(coin==1){q[which(str_detect(prod,"sunflower"))][2] <- q[which(str_detect(prod,"sunflower"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(coin==2){q[which(str_detect(prod,"rapeseed"))][2] <- q[which(str_detect(prod,"rapeseed"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      }
      
      if(i == "poultry"){coin <- sample(c(1,2),1)
      
      if(coin==1){q[which(str_detect(prod,"broiler"))][2] <- q[which(str_detect(prod,"broiler"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      if(coin==2){q[which(str_detect(prod,"turkey"))][2] <- q[which(str_detect(prod,"turkey"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      }
      
      if(i =="pork"){q[which(str_detect(prod,"pig"))][2] <- q[which(str_detect(prod,"pig"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      if(i=="egg"){q[which(str_detect(prod,"egg"))][2] <- q[which(str_detect(prod,"egg"))][2]+totaldemand[which(totaldemand[,1]==i),2]*acc }
      
      
      if(i=="milk"){
        
        coin <- sample(c(1,2),1)*2
        q[which(str_detect(prod,"milk"))[coin]] <- q[which(str_detect(prod,"milk"))[coin]]+totaldemand[which(totaldemand[,1]==i),2]*acc}
      
      
    }
    print(count)
    
  }
  l <- list()
  l[["prodquantity"]] <- q
  l[["produced"]] <- production(q)
  l[["td_all"]] <- totaldemand
  l[["td_tot"]] <- sum(totaldemand[,2])
  l[["share"]] <- l[["produced"]] /totaldemand[,2]
  l[["left"]] <- c(area(q,AREA=AREA,PASTURE=PASTURE),pasture(q,PASTURE=PASTURE))
  l[["wblss"]] <- sum(l[["produced"]])/sum(totaldemand[,2])
  print(status)
  return(l)
}


# Fitness functions
#''''''''''''''''''
#The fitness function for LSS optimization makes mirrored estimation about the "missing area"
fit_LSS <- function(q){
  q <- q+0.0000001 # no zero values are allowed for any element of q 
  over <- dfm(q,DEMAND=dm,pop=pop) # First demand fulfillment rates are calculated
  over[which(is.finite(over)==F)] <- 100                # In case of vegan and vegetarian demand some dfr are infinity. They need to be set to 100 (no additional area needed)
  o <- ifelse(any(over[-10]>101),sum(over[which(over>101)],na.rm=T),1) # o is a penalty factor for overproduction
  
  #In the vector NEED the area used for each category seperately is stored. Beef needs nor additional area
  NEED <- c(sum(need(c(q[1:2],rep(0,30)))[,2]), 
            sum(need(c(rep(0,2),q[3:4],rep(0,28)))[,2]),
            sum(need(c(rep(0,6),q[7:8],rep(0,6),q[15:16],rep(0,16)))[,2]),
            sum(need(c(rep(0,4),q[5:6],rep(0,26)))[,2]),
            sum(need(c(rep(0,8),q[9:10],rep(0,22)))[,2]),
            sum(need(c(rep(0,10),q[11:12],rep(0,20)))[,2]),
            sum(need(c(rep(0,12),q[13:14],rep(0,18)))[,2]),
            sum(need(c(rep(0,16),q[17:18],rep(0,14)))[,2]),
            sum(need(c(rep(0,18),q[19:20],rep(0,12)))[,2]),
            0,
            sum(need(c(rep(0,22),q[23:24],rep(0,8)))[,2]),
            sum(need(c(rep(0,28),q[29:32]))[,2]),
            sum(need(c(rep(0,24),q[25:26],rep(0,6)))[,2]),
            sum(need(c(rep(0,20),q[21:22],rep(0,4),q[27:28],rep(0,4)))[,2])
  )
  
  #Demand fulfillmentrate for poultry if totaldemand of eggs would be met
  if(dm[14,2]>0 & over[14] > 1) over[14] <- (((over[14]/100*(dm[14,2]*pop/ 1000000)) - (over[11]/100 * (dm[11,2]*pop/ 1000000)*0.066))/((dm[14,2]*pop/ 1000000) - ((dm[11,2]*pop/ 1000000)*0.066)))*100
  
  # Calculating the multiplicator that estimates missing food
  mult <-(100-over)/over
  
  # The area needed if the demand would be met is the actual area used (NEED) and the sum of each category's need multiplied with the respective multiplication factor
  perfectarea <- sum(NEED)+ sum(mult*NEED)
  
  #Applying the overproduction- and area- penalty factor
  p <- sum(need(q)[,2])/perfectarea
  a <- sum(aam(q,PRINT=F,AREA=totalarea,PASTURE=as.numeric(pa)))
  if(a < 1) a <- 1
  
  res <- p/(a*o)
  return(res)
  
}


#Help functions
#''''''''''''''

#The need function is called by the fit_LSS function and calculates the area needed for each crop, given a certain quantity
need<- function(prodquantity){
  
  feedframe <- alo_feedstuff
  colnames(feedframe) <- c("prod","quant")
  feedframe[,2]<-0
  
  
  prodquantity_dir <- as.data.frame(prod,stringsAsFactors = F)   
  prodquantity_dir["quant"] <- prodquantity   
  
  
  prodquantity_ind <- prodquantity_dir[1:20,]
  feedframe[,2]<-0
  for(i in 21:32){
    feedframe[,2] <-feedframe[,2]+ prodquantity_dir[i,2]*as.numeric(feedstuff.demand[,i-20+1])
    
  }
  
  prodquantity_ind<- rbind(prodquantity_ind,feedframe)
  
  #Each product is multiplied with it's ALO value 
  res <- prodquantity_ind
  res[,2] <- res[,2]*alo[,2]
  return(res)
}

#The LCA function takes environmental impacts (per kiloton) ,multiplies them with corresponding quantities and sums it up
LCA <- function(prodquantity){
  res <- impacts
  for(i in 1:length(prodquantity)){
    res[,i]<- impacts[,i] * prodquantity[i]
  }
  
  impacts <- apply(res,1,sum)
  
  return(impacts)
  
}

#The gettotalara function is used to calculated the arable land for a given intersection with the landclass shapefile (int_lc)
gettotalarea <- function(int_lc){
  
  kl <- numeric()
  for(i in 2:138){
    
    kl <- c(kl,sum(st_area(subset(int_lc,klasse_ver==i)))/10000)
  }
  #klorder is the order of landclasses defined by bitterich (it is the order of landclasses for BW)
  return(kl[klorder])
}

#The pasture function gives the unused pasture for given quantities, arable land and pasture
pasture <- function(prodquantity,AREA,PASTURE){
  feedframe <- alo_feedstuff
  colnames(feedframe) <- c("prod","quant")
  feedframe[,2]<-0
  
  
  pastureavailable <- PASTURE
  
  prodquantity_dir <- as.data.frame(prod,stringsAsFactors = F)   
  prodquantity_dir["quant"] <- prodquantity   
  
  
  prodquantity_ind <- prodquantity_dir[1:20,]
  feedframe[,2]<-0
  for(i in 21:32){
    feedframe[,2] <-feedframe[,2]+ prodquantity_dir[i,2]*as.numeric(feedstuff.demand[,i-20+1])
    
  }
  
  prodquantity_ind<- rbind(prodquantity_ind,feedframe)
  
  #Area ############################################################
  
  for(i in 1:nrow(prodquantity_ind)){
    
    
    need <- prodquantity_ind[i,2]*alo[i,2]
    
    prodname <- str_split(prodquantity_ind[i,1],"_")[[1]][1]
    
    if(prodname=="pasture"){diff <- pastureavailable-need
    pastureavailable <- ifelse(diff>0,diff,0)
    need <- ifelse(diff<0,abs(diff),0) }
    
    
  }
  return(pastureavailable)
}  
#The area function gives the unused arable land for given quantities, arable land and pasture
area <- function(prodquantity,PRINT=F,AREA,PASTURE,summe=T){
  feedframe <- alo_feedstuff
  colnames(feedframe) <- c("prod","quant")
  feedframe[,2]<-0
  
  
  areaavailable <- AREA
  pastureavailable <- PASTURE
  
  prodquantity_dir <- as.data.frame(prod,stringsAsFactors = F)   
  prodquantity_dir["quant"] <- prodquantity   
  
  
  prodquantity_ind <- prodquantity_dir[1:20,]
  feedframe[,2]<-0
  for(i in 21:32){
    feedframe[,2] <-feedframe[,2]+ prodquantity_dir[i,2]*as.numeric(feedstuff.demand[,i-20+1])
    
  }
  
  prodquantity_ind<- rbind(prodquantity_ind,feedframe)
  
  ### Crop rotation add up
  
  prodquantity_ind[c(7,8,11,12,15,16,19,20),2]<- prodquantity_ind[c(7,8,11,12,15,16,19,20),2] + prodquantity_ind[c(39,26,41,28,43,30,40,27),2]
  prodquantity_ind[c(39,26,41,28,43,30,40,27),2] <- 0
  
  #Area ############################################################
  
  rightorder <- c(11,12,15,16,7,8,19,20,1,2,3,4,5,6,9,10,13,14,17,18,33,22,34,23,35,29,36,24,37,25,39,26,40,27,43,30,44,31,21, 32, 38, 42,41,28)
  
  #The vector bef (before) is a container for the area that is reserved by organic cultivation 
  #It is as long as landclasses exist
  bef <-numeric(138)
  
  for(i in rightorder){
    
    #The need is the area which is needed to grow the current crop
    need <- prodquantity_ind[i,2]*alo[i,2]
    CR <- cr[which(cr[,2]==prodquantity_ind[i,1]),1][1]/100 # Crop rotation factor
    
    
    prodname <- str_split(prodquantity_ind[i,1],"_")[[1]][1]
    #The variable ocf describes whether the current product is organic or conventional
    #For organic products (uneven) it's 1, for conventional (even) it is zero
    ocf <- i%%2
    
    #If the current product is pasture the area available for pasture is diminished by the need
    #For all other products all land classes where the product can be grown are iterated
    if(prodname=="pasture"){diff <- pastureavailable-need
    pastureavailable <- ifelse(diff>0,diff,0)
    need <- ifelse(diff<0,abs(diff),0) }else{
      for(c in rev(lc[[prodname]])){
        
        areaavailable_cr <- min((AREA[c]-bef[c])*CR,areaavailable[c]) # The area available under crop rotation constraint is calculated
        bef[c] <- (areaavailable_cr/CR)*ocf #The area reserved for organic production is stored in bef if the product is organic (ocf=1) 
        need <- need-areaavailable_cr # The area available under crop rotation constraints is subtracted from the need and from the area available
        
        areaavailable[c] <- areaavailable[c] - areaavailable_cr    
        
        #If the need is smaller than zero the difference is added back to the area available.
        #Also the area occupied by organic production is added back to the bef vector
        if(need<0){areaavailable[c] <- areaavailable[c] + abs(need)
        bef[c] <- ((areaavailable_cr-abs(need))/CR)*ocf
        need <-0
        break}
        
        
      }}
    
    if(PRINT==T){
      print(paste("AREA:",prodquantity_ind[i,1]))
      print(areaavailable)
      
      
    }
  }
  a <- areaavailable
  if(summe ==T) a <- sum(a)
  return(a)
}  

