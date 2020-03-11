##############################
####    packages         ####
##############################
 
rm(list=ls(all=TRUE))

list.of.packages = c("sf","stringr","lwgeom","ecr","raster","rgdal","rgeos","GA",
                     "rstudioapi","VIMGUI","maptools","lattice","sp","spsurvey","data.table","fmsb","TeachingDemos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {install.packages(new.packages)}
lapply(list.of.packages, require, character.only=T)

