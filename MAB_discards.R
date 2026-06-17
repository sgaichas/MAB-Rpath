## ---------------------------
## Script name: MAB_discards.R
##
## Purpose of script: Estimate discards from NOAA observer data for MAB Rpath
##                    functional groups.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Last modified: 02 Sep 2021
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: This script follows the method established by Weisberg.

# Fri Dec  8 17:00:31 2023 ------------------------------


## Load libraries, packages and functions
library(here);library(tidyr);library(data.table)

## Load observer data and landings
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/3c1362b6f1517618147becc2b6c96c6058867941/data/observer_data.RData?raw=true'))
source(here("MAB_landings.R"))

## Rename HMS fleet
ob.all[FLEET == "HMS",FLEET := "HMS Fleet"]

## Subset discards data for MAB
MAB.ob<-filter(ob.all, EPU == "MAB")
MAB.ob<-MAB.ob[,-2]

## Add RPath species codes
MAB.ob$NESPP3<-as.numeric(MAB.ob$NESPP3)
MAB.ob<-left_join(MAB.ob,spp,by = "NESPP3")
MAB.ob<-MAB.ob[,-3]

## Remove NAs
MAB.ob<-na.exclude(MAB.ob)

## Average multispecies functional groups over multiple NESPP3 codes
MAB.ob<-MAB.ob %>%
  group_by(YEAR,FLEET,RPATH) %>%
  summarise(DK = mean(DK))

## Calculate first available DK ratios
first.dk<-MAB.ob %>%
  group_by(FLEET,RPATH) %>%
  mutate(first_YEAR = min(YEAR)) %>%
  ungroup() %>%
  filter(YEAR == first_YEAR)
first.dk<-first.dk[,-1]
names(first.dk)[3]<-"First_DK"
first.dk<-unique(first.dk)

## Merge with landings
first.dk<-merge(first.dk,mean.land,by = c("RPATH","FLEET"))

## Multiply landings by DK
first.dk$Discards<-first.dk$landings*first.dk$First_DK

## Remove negligible discards (<10^-5 per Weisberg)
first.dk$Discards<-as.numeric(first.dk$Discards)
first.dk<-subset(first.dk, Discards > 1e-05)

## Merge discards with DK ratios
discards.top<-merge(first.dk,MAB.ob,by = c("RPATH","FLEET"))

## Average DK over 5 years for each group and gear
temp<-c()
for (i in 1:length(first.dk$Discards)){
  temp<-subset(discards.top,first.dk$RPATH[i] == discards.top$RPATH & first.dk$FLEET[i] == discards.top$FLEET)
  temp<-subset(temp,YEAR<=first_YEAR+4)
  first.dk$five_means[i]<-mean(temp$DK)
  first.dk$cv[i]<-mean(temp$DK)/sd(temp$DK)
}

## Create subsets for high and low variability
low.var<-subset(first.dk,cv<=1 | is.na(cv)==T)
low.var<-low.var %>%
  select(RPATH,FLEET,five_means)
names(low.var)[3]<-'DK'
high.var<-subset(first.dk,cv>1)

## Average high.var over 10 years instead of 5
temp<-c()
for (i in 1:length(high.var$Discards)){
  temp<-subset(discards.top,high.var$RPATH[i] == discards.top$RPATH & high.var$FLEET[i] == discards.top$FLEET)
  temp<-subset(temp,YEAR<=first_YEAR+9)
  high.var$ten_means[i]<-mean(temp$DK)
}

## Subset high variability
high.var<-high.var %>%
  select(RPATH,FLEET,ten_means)
names(high.var)[3]<-'DK'

## Merge high and low variability results
MAB.discards<-first.dk %>%
  select(RPATH,FLEET,landings)
MAB.discards<-left_join(MAB.discards,low.var,by = c("RPATH","FLEET"))
MAB.discards<-left_join(MAB.discards,high.var,by = c("RPATH","FLEET"))
MAB.discards[is.na(MAB.discards)]<-0
MAB.discards$DK<-MAB.discards$DK.x+MAB.discards$DK.y
MAB.discards<-MAB.discards[,-c(4,5)]
MAB.discards$discards<-MAB.discards$landings*MAB.discards$DK
MAB.discards<-MAB.discards[,-c(3,4)]

## Reorganize fleets into data frames
fixed.d<-filter(MAB.discards,FLEET == "Fixed Gear")
lg_mesh.d<-filter(MAB.discards,FLEET == "LG Mesh")
other.d<-filter(MAB.discards,FLEET == "Other")
sm_mesh.d<-filter(MAB.discards,FLEET == "SM Mesh")
scallop.d<-filter(MAB.discards,FLEET == "Scallop Dredge")
trap.d<-filter(MAB.discards,FLEET == "Trap")
hms.d<-filter(MAB.discards,FLEET == "HMS Fleet")
pelagic.d<-filter(MAB.discards,FLEET == "Pelagic")
other_dredge.d<-filter(MAB.discards,FLEET == "Other Dredge")

## Combine with groups
## Fixed gear
fixed.d<-left_join(MAB.groups,fixed.d,by="RPATH")
fixed.d$FLEET<-"Fixed Gear"
fixed.d[is.na(fixed.d)]<-0

## Large mesh
lg_mesh.d<-left_join(MAB.groups,lg_mesh.d,by="RPATH")
lg_mesh.d$FLEET<-"LG Mesh"
lg_mesh.d[is.na(lg_mesh.d)]<-0

## Other
other.d<-left_join(MAB.groups,other.d,by="RPATH")
other.d$FLEET<-"Other"
other.d[is.na(other.d)]<-0

## Small mesh
sm_mesh.d<-left_join(MAB.groups,sm_mesh.d,by="RPATH")
sm_mesh.d$FLEET<-"SM Mesh"
sm_mesh.d[is.na(sm_mesh.d)]<-0

## Scallop
scallop.d<-left_join(MAB.groups,scallop.d,by="RPATH")
scallop.d$FLEET<-"Scallop"
scallop.d[is.na(scallop.d)]<-0

## Trap
trap.d<-left_join(MAB.groups,trap.d,by="RPATH")
trap.d$FLEET<-"Trap"
trap.d[is.na(trap.d)]<-0

## HMS
hms.d<-left_join(MAB.groups,hms.d,by="RPATH")
hms.d$FLEET<-"HMS Fleet"
hms.d[is.na(hms.d)]<-0

## Pelagic
pelagic.d<-left_join(MAB.groups,pelagic.d,by="RPATH")
pelagic.d$FLEET<-"Pelagic"
pelagic.d[is.na(pelagic.d)]<-0

## Other dredge
other_dredge.d<-left_join(MAB.groups,other_dredge.d,by="RPATH")
other_dredge.d$FLEET<-"Other Dredge"
other_dredge.d[is.na(other_dredge.d)]<-0
