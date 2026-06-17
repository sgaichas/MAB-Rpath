## Script name: MAB_landings.R
##
## Purpose of script: Convert commercial landings from Sean Lucey to correct
##                    units and format for use in RPath.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Email: brandon.beltz@stonybrook.edu


# Fri Dec  8 17:01:12 2023

## Load libraries, packages and functions ---------------------------------------------------
library(here);library(tidyr);library(dplyr);library(survdat)

## Load landings, species codes, survdat and basic inputs, 
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/dd034d1573f79ce011c01054bdc017e241e7857e/data/mean_landings_mab_80_85.RData?raw=true'))
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/dd034d1573f79ce011c01054bdc017e241e7857e/data/Species_codes.RData?raw=true'))
load(url("https://github.com/NOAA-EDAB/Rpathdata/blob/dd034d1573f79ce011c01054bdc017e241e7857e/data/Survdat.RData?raw=true"))
source(here("MAB_basic_inputs.R"))


## Change "HMS" to "HMS Fleet"
mean.land[FLEET == "HMS",FLEET := "HMS FLEET"]

## Calculate area within MAB
# strata<-readOGR('data/strata','strata')
# strat.area<-getarea(strata, 'STRATA')
# setnames(strat.area,'STRATA','STRATUM')
# MAB.strat.area<-strat.area[STRATUM %in% MAB.strata,sum(Area)]

## Filter species codes for NESPP3 and RPATH
spp<-select(spp, one_of(c("NESPP3","RPATH")))
spp<-unique(na.exclude(spp))
spp<-distinct(spp,NESPP3,.keep_all=TRUE)

## Merge landings data and clean up species code columns
mean.land<-left_join(mean.land,spp,by = "NESPP3")
mean.land<-na.exclude(mean.land[,-1])

## Sum landings for functional groups
mean.land<-mean.land %>% group_by(FLEET,RPATH) %>% summarise(SPPLIVMT=sum(SPPLIVMT))

## Convert to t/km^2 and remove SPPLIVMT
mean.land$landings<-mean.land$SPPLIVMT/MAB.area
mean.land<-mean.land[,-3]

## Organize fleets into data frames
fixed<-filter(mean.land,FLEET == "Fixed Gear")
lg_mesh<-filter(mean.land,FLEET == "LG Mesh")
other<-filter(mean.land,FLEET == "Other")
sm_mesh<-filter(mean.land,FLEET == "SM Mesh")
scallop<-filter(mean.land,FLEET == "Scallop Dredge")
trap<-filter(mean.land,FLEET == "Trap")
hms<-filter(mean.land,FLEET == "HMS Fleet")
pelagic<-filter(mean.land,FLEET == "Pelagic")
other_dredge<-filter(mean.land,FLEET == "Other Dredge")
clam<-filter(mean.land,FLEET == "Clam Dredge")

## Combine groups and fill in gaps
## Fixed gear
fixed<-left_join(MAB.groups,fixed,by="RPATH")
fixed$FLEET<-"Fixed Gear"
fixed[is.na(fixed)]<-0

#Large mesh
lg_mesh<-left_join(MAB.groups,lg_mesh,by="RPATH")
lg_mesh$FLEET<-"LG Mesh"
lg_mesh[is.na(lg_mesh)]<-0

#Other
other<-left_join(MAB.groups,other,by="RPATH")
other$FLEET<-"Other"
other[is.na(other)]<-0

#Small mesh
sm_mesh<-left_join(MAB.groups,sm_mesh,by="RPATH")
sm_mesh$FLEET<-"SM Mesh"
sm_mesh[is.na(sm_mesh)]<-0

#Scallop
scallop<-left_join(MAB.groups,scallop,by="RPATH")
scallop$FLEET<-"Scallop"
scallop[is.na(scallop)]<-0

#Trap
trap<-left_join(MAB.groups,trap,by="RPATH")
trap$FLEET<-"Trap"
trap[is.na(trap)]<-0

#HMS
hms<-left_join(MAB.groups,hms,by="RPATH")
hms$FLEET<-"HMS Fleet"
hms[is.na(hms)]<-0

#Pelagic
pelagic<-left_join(MAB.groups,pelagic,by="RPATH")
pelagic$FLEET<-"Pelagic"
pelagic[is.na(pelagic)]<-0

#Other dredge
other_dredge<-left_join(MAB.groups,other_dredge,by="RPATH")
other_dredge$FLEET<-"Other Dredge"
other_dredge[is.na(other_dredge)]<-0

#Clam
clam<-left_join(MAB.groups,clam,by="RPATH")
clam$FLEET<-"Clam Dredge"
clam[is.na(clam)]<-0

