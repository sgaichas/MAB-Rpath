## ---------------------------
## Script name: MAB_fleets.R
##
## Purpose of script: Create a data table of groups and fleets for use in the
##                    final MAB Rpath model.
##                    
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
##
## Email: brandon.beltz@stonybrook.edu

# Tue Apr 16 12:05:00 2024 ------------------------------


## Load libraries, packages and functions
library(data.table)

## Load commercial landings from Sean and MAB basic inputs
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/dd034d1573f79ce011c01054bdc017e241e7857e/data/mean_landings_mab_80_85.RData?raw=true'))
source(here("MAB_basic_inputs.R"))

## Rename HMS to HMS Fleet
mean.land[FLEET == "HMS",FLEET := "HMS Fleet"]

## Add columns for discards and detritus
dis_det_cols<-as.data.table(rbind("Detritus","Discards"))
colnames(dis_det_cols)<-"RPATH"
groups_fleets<-rbind(MAB.groups,dis_det_cols)

## Pull unique fleets
MAB.fleets<-as.data.table(unique(mean.land$FLEET))
colnames(MAB.fleets)<-"RPATH"

## Add rec & purse seine fleets
rec_men<-data.frame(rbind("Recreational","PurseSeine"))
names(rec_men)<-c("RPATH")
groups_fleets<-rbind(groups_fleets,MAB.fleets,rec_men)
