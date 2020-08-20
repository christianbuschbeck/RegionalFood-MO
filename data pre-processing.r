##############################
####    Load Data         ####
##############################

#Tis character string specifies the folder where this scrip is in
folder <- dirname(rstudioapi::getActiveDocumentContext()$path)

## Model data

alo<-read.table(paste(folder,"/Model_data/ALO",sep=""),stringsAsFactors=F)
alo_feedstuff<-read.table(paste(folder,"/Model_data/ALO_feedstuff",sep=""),stringsAsFactors=F)
prod<-as.character(read.table(paste(folder,"/Model_data/products",sep=""))[,1])
feedstuff.demand<-read.table(paste(folder,"/Model_data/feedstuff_demand",sep=""),stringsAsFactors=F)
cr<-read.table(paste(folder,"/Model_data/cr",sep=""),stringsAsFactors=F)
demands <-  read.csv(paste(folder,"/Model_data/demands",sep=""),stringsAsFactors=F)
milkco <- t(read.table(paste(folder,"/Model_data/milkco",sep="")))
eggco <- t(read.table(paste(folder,"/Model_data/eggkco",sep="")))
load(paste(folder,"/Model_data/lc.Rdata",sep=""))
cropnames <-c("carrots" ,  "lettuce",   "potato"  ,  "rapeseed" , "rye" ,      "soybean"    ,   "sugarbeet" ,"sunflower", "tomatoes" ,"wheat")
load(paste(folder,"/Model_data/lc.Rdata",sep=""))
impacts <-read.table(paste(folder,"/Model_data/impacts",sep=""))
desirability <- read.table(paste(folder,"/Model_data/desirability",sep=""))

## GIS data

bev <- raster(paste(folder,"/GIS_data/Bevoelkerungszahl.tif",sep=""))
load(paste(folder,"/GIS_data/landclasses.Rdata",sep=""))
load(paste(folder,"/GIS_data/pastures.Rdata",sep=""))
rgbz <- st_read(paste(folder,"/GIS_data/VG1000_RBZ.shp",sep=""))

##############################
### GIS Transformations   ####
##############################

#Borders of BW and Freiburg are retrieved from the shapefile containing district borders and transformed 
bawue <- subset(rgbz,GEN =="Freiburg"|GEN =="Karlsruhe"|GEN =="Stuttgart"|GEN =="T?bingen")
freiburg <- subset(rgbz,GEN =="Freiburg")
bawue <- st_transform(bawue,4326) 
freiburg <- st_transform(freiburg,4326)

#Intersection

int_p_bawue  <- st_intersection(pastures,bawue)

fakegroup_pa <- rep(1,nrow(int_p_bawue))
int_p_bawue  <- cbind(int_p_bawue,fakegroup_pa)

fakegroup_lc <- rep(1,nrow(landclasses))
int_lc_bawue <- cbind(landclasses,fakegroup_lc)

#klorder is the orders of land classes. It is needed for the gettotalarea function to calculate the area of each landclass for a polygon
klorder <- numeric()
for(i in 2:138){
  
  klorder <- c(klorder,sum(st_area(subset(int_lc_bawue,klasse_ver==i)))/10000)
  
}

totalarea_bw <- klorder[order(klorder,decreasing=T)]
pa_bw <- round(as.numeric(sum(st_area(st_intersection(pastures,bawue)))/10000))

klorder <- order(klorder,decreasing=T)

#The population density raster is clipped masked and trimmed to fit BW
crs.bev <- as.character(crs(bev))
bw <-st_transform(bawue,crs.bev)
bw <- st_union(bw)
bw <- as_Spatial(bw)
bev.bw <- mask(bev,bw)
bev.bw <- trim(bev.bw)

#All relevant cities of BW get coordinates
name <- c("s","ka","ma","fr","hd","ul","hb","pf","re","og","vs","si","ra","loe","lu")
staedte <- as.data.frame(name)
staedte["lon"] <- c(9.1829321,8.403653,8.4660395,7.842104,8.6724335,9.9876076,9.210879,8.6946286,9.2038043,7.949802,8.460140,8.834670,9.612130,7.658900,9.192220)
staedte["lat"] <- c(48.7758459,49.006889,49.4874592,47.999008,49.3987524,48.4010822,49.1426929,48.8921862,48.5069389,48.473450,48.055408,47.761791,47.778271,47.611700,48.896140)

save.image(paste(folder,"/Rdata/processed_data.Rdata",sep=""))

