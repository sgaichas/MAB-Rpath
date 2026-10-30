## ---------------------------
## Script name: MAB_rec_catch.R
##
## Purpose of script: Pull recreational catch for species and groups in the MAB
##                    RPath model.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg and Sarah Gaichas
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------

# Fri Dec 29 15:16:51 2023 ------------------------------
# 29 October 2026 SKG update

## Load libraries, packages and functions
library(here);library(tidyr);library(data.table);library(survdat); library(dplyr)

## Load MRIP species list, RPath species list and MAB groups
load(here("data/Species_codes.RData"))
load(here("data/mrip_species.RData"))
# load(here("data/mrip_data.RData")) #SKG: this is MRIP before 2026 recalibration

# SKG: this is MRIP after 2026 calibration, assembled from mrip_catch_bywave_YYYY.csv files here 
# https://apps-st.fisheries.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads/
MAB.mrip <- readRDS(gzcon(url("https://github.com/MAFMC/indicators-project-data/raw/refs/heads/main/data-raw/MRIP/allbywave19812025.rds")))
MAB.mrip$SP_CODE <- as.numeric(MAB.mrip$SP_CODE)

source(here("MAB_basic_inputs.R"))

## Merge MRIP and RPath species lists by SCINAME
colnames(MAB.mrip_species)[3]<-"SCINAME"
species_key<-merge(MAB.mrip_species,spp,by = "SCINAME")
species_key<-species_key[,c(2,20)]
species_key<-merge(MAB.groups,species_key,by = "RPATH")
colnames(species_key)[2]<-"SP_CODE"

## Subset MRIP data by state and area
## ----------------added by SKG
## MRIP State codes variable ST
## Atlantic and Gulf States: 23= ME, 33= NH, 25= MA, 44= RI, 9= CT, 
## 36= NY, 34= NJ, 10= DE, 24= MD, 51= VA, 37= NC, 45= SC, 
## 13= GA, 12= FL (121= East FL, 122= West FL), 1= AL,
## 28= MS, and 22= LA.
## MRIP Mid Atlantic= c(10, 24, 34, 36, 51) #DE, MD, NJ, NY, VA
## MRIP North Atlantic= c(9, 23, 25, 33, 44) #CT, ME, MA, NH, RI
##
## MAB model as defined here includes CT, DE, MD, NJ, NY, NC, RI, VA, different from MRIP regions
##
## MRIP area codes variable AREA_X
## Collapsed area of fishing 
## 1 = Ocean <= 3 mi (all but WFL) 
## 2 = Ocean > 3 mi (all but WFL) 
## 3 = Ocean <= 10 mi (WFL only) 
## 4 = Ocean > 10 mi (WFL only) 
## 5 = Inland
## 
## MAB model as defined here excludes inland waters and ocean waters > 10 mi off WFL
## The WFL designations should not matter to us but for safety I'm going to change this
## to exclude only inland waters: AREA_X < 5
## ----------------
#MAB.mrip<-subset(MAB.mrip,MAB.mrip$ST == c(9,10,24,34,36,37,44,51) | MAB.mrip$AREA_X < 4, select=c(SP_CODE,WGT_AB1))
#SW changed
#MAB.mrip_filter<- MAB.mrip %>% filter(ST %in% c(9,10,24,34,36,37,44,51) & AREA_X < 4) %>%
#  select(SP_CODE,WGT_AB1,YEAR)

# SKG changed AREA_X but it shouldn't matter in the Mid
MAB.mrip_filter<- MAB.mrip %>% filter(ST %in% c(9,10,24,34,36,37,44,51) & AREA_X < 5) %>%
  select(SP_CODE,WGT_AB1,YEAR)


#filter for years 1980-1985 (NOTE only 1981 on in MRIP data)
#SW addition
MAB.mrip_filter <-MAB.mrip_filter %>% filter(YEAR <= 1985)


## Merge with RPath names
MAB.mrip_filter<-merge(MAB.mrip_filter,species_key,by = "SP_CODE")
#MAB.mrip_filter<-MAB.mrip_filter[,-1]

## Sum recreational catch for each RPath group by year
MAB.mrip_summary<-MAB.mrip_filter %>%
  group_by(RPATH,YEAR) %>%
  summarise(WGT_AB1 = sum(WGT_AB1))

#average
#SW addition
MAB.mrip_summary <- MAB.mrip_summary %>% group_by(RPATH) %>% summarise(catch = mean(WGT_AB1))

## Divide by MAB area to get kg/km^2
#Calculate total MAB area
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
MAB.area<-subset(area, area$STRATUM %in% MAB.strata)
MAB.area<-sum(MAB.area$Area)
rm(area)

MAB.mrip_summary$Per_Area<-(MAB.mrip_summary$catch/1000)/MAB.area
