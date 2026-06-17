## Script name: MAB_biomass_estimates.R
##
## Purpose of script: Calculate biomass estimates for single-species and 
##                    multi-species functional groups using survey data and 
##                    EMAX estimates, respectively.
##
## Author: Brandon Beltz
## edited by: M.T. Grezlik
##
## Date Created: 2021-06-15
##
## Email: mgrezlik@umassd.edu
##
## Notes: This version is specific to the MABRpath model.
##


## Load libraries, packages and functions

library(data.table)
library(here)
# remotes::install_github('NOAA-EDAB/survdat')
library(survdat)
library(lwgeom)
library(dplyr)
library(units)
library(readr)
'%notin%' <-Negate('%in%')

## Load Survdat, species list and strata from Rpathdata repo
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/f34255ee1afea1bdc51e90166f79df9b273de6cd/data/Survdat.RData?raw=true'))
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/f34255ee1afea1bdc51e90166f79df9b273de6cd/data/Species_codes.RData?raw=true'))
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/f34255ee1afea1bdc51e90166f79df9b273de6cd/data/survdatClams.RData?raw=true'))
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/f34255ee1afea1bdc51e90166f79df9b273de6cd/data/survdatScallops.RData?raw=true'))

source(here('MAB_basic_inputs.R'))

#Calculate total MAB area ------------------------------------------
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
MAB.area<-subset(area, area$STRATUM %in% MAB.strata)
MAB.area<-sum(MAB.area$Area)
rm(area)

# strata<-readOGR('data/strata','strata')
# 
# ## Generate area table
# strat.area<-getarea(strata, 'STRATA')
# setnames(strat.area,'STRATA','STRATUM')

## Load basic inputs
source(here('MAB_basic_inputs.R'))

## Aggregate low biomass species
spp <- spp[!duplicated(spp$SVSPP),]
spp <- spp[RPATH == 'AtlHerring', RPATH := 'SmPelagics']
# spp <- spp[RPATH == 'Clams', RPATH := 'Megabenthos']
spp <- spp[RPATH == 'Haddock', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'LargePelagics', RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'OffHake', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'OtherFlatfish', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Pollock', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Rays', RPATH := 'OtherSkates']
spp <- spp[RPATH == 'RedCrab', RPATH := 'Megabenthos']
spp <- spp[RPATH == 'AmShad', RPATH := 'RiverHerring']
spp <- spp[RPATH == 'Tilefish', RPATH := 'SouthernDemersals']
spp <- spp[RPATH == 'WitchFlounder', RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'WhiteHake', RPATH := 'OtherDemersals']
#spp <- spp[SCINAME == 'BREVOORTIA', RPATH := 'AtlMenhaden']

#Calculate swept area biomass ------------------------------------------
#Fall season only
swept<-calc_swept_area(surveyData=survdat, areaPolygon = 'NEFSC strata', areaDescription = 'STRATA', 
                       filterByArea = MAB.strata, filterBySeason= "FALL", 
                       groupDescription = "SVSPP", filterByGroup = "all", mergesexFlag = T,tidy = F, q = NULL, a = 0.0384)

# ## Subset by Fall and MAB strata
# MAB.fall<-survdat[SEASON == 'FALL' & STRATUM %in% MAB.strata,]
# 
# ## Run stratification prep
# MAB.prep<-stratprep(MAB.fall,strat.area,strat.col = 'STRATUM',area.col = 'Area')

## Merge with RPATH names
spp <- spp[!duplicated(spp$SVSPP),]

## Calculate stratified means
#mean.biomass<-stratmean(MAB.prep,group.col = 'SVSPP',strat.col = 'STRATUM')

## Merge with RPATH names
#mean.biomass<-merge(swept,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

## Calculate total biomass from swept area
#total.biomass<-sweptarea(MAB.prep, mean.biomass, strat.col = 'STRATUM', area.col = 'Area')

## Merge with RPATH names
total.biomass<-merge(swept,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')

## Calculate total area
#MAB.strat.area<-strat.area[STRATUM %in% MAB.strata,sum(Area)]

## Convert to b/a in mt/km^2
total.biomass <- total.biomass[, biomass.t_area :=(tot.biomass*.001)/(Fall.q*MAB.area)]

#Sum by RPATH names
#SW added
setkey(total.biomass,RPATH,YEAR)
summary.biomass <- total.biomass[, sum(biomass.t_area), by = key(total.biomass)]
setnames(summary.biomass, 'V1','Biomass')
MAB.biomass.80s<-summary.biomass[YEAR %in% 1980:1985, mean(Biomass), by=RPATH]
setnames(MAB.biomass.80s, 'V1','Biomass')

#Replace Biomass for Clams and Scallops with appropriate surveys --------

##Clams ----------------------------------------------
#Use GB clam region to calculate biomass
MAB.clam.index <- clams$data |> 
                    filter(clam.region != 'MAB') |>
                    filter(!is.na(SVSPP))
clam.index <- MAB.clam.index |>
                  group_by(YEAR, SVSPP) |>
                  mutate(B = mean(BIOMASS.MW,narm = T)) |>
                  ungroup()

#Need to expand from kg/tow to mt/km^2
# Clam tows can vary greatly by I'll use an example tow as the expansion
# 0.0039624 (dredge width in km) * 0.374(tow length in km) = 0.00148 
#kg to mt is 0.001
# so conversion is 0.001 / 0.00148 or 0.6757
# clam.index[, B := B * 0.6757]
# clam.index[, Units := 'mt km^-2']
# clam.index$RPATH <- ifelse(clam.index$SVSPP == 409, 'OceanQuahog','SurfClam')
#clam.index[, RPATH := 'Clams']

clam.index <- clam.index |>
                mutate(B = B * 0.6757) |>
                mutate(Units = 'mt km^-2') |>
                mutate(RPATH = ifelse(SVSPP == 409, 
                                      'OceanQuahog','SurfClam')) |> 
                ungroup()


#Input biomass
# clam.input <- clam.index[YEAR %in% 1981:1985, .(B = mean(B, na.rm = T)),
#                          by = RPATH]

clam.input <- clam.index |>
                group_by(RPATH) |>
                filter(YEAR %in% 1981:1985)|>
                mutate(Biomass = mean(B, na.rm = T)) |> 
                select(RPATH, Biomass) |>
                distinct()

#remove old biomass estimates for clams
MAB.biomass.80s <- MAB.biomass.80s[RPATH != 'OceanQuahog',]
MAB.biomass.80s <- MAB.biomass.80s[RPATH != 'SurfClam',]
#Add clam biomass 
MAB.biomass.80s$Biomass <- drop_units(MAB.biomass.80s$Biomass)
MAB.biomass.80s <- rbindlist(list(MAB.biomass.80s, clam.input))

##Scallops ----------------------------------------------
#Scallops and clam survey not included in ms-keyrun data set as they are not 
#used in the other models
library(DBI); library(sf); library(survdat)

#Connect to the database
# channel <- dbutils::connect_to_database('sole', 'slucey')

#scall <- survdat::get_survdat_scallop_data(channel, getWeightLength = T)
# load(here::here('data', 'survdatScallops.RData'))

#Scallop survey did not record weight prior to 2001 (FSCS) so need to manually
#calculate catch weights
scalldat <- scallops$survdat[, BIOMASS := sum(WGTLEN), by = c('YEAR', 'STATION')]

#Calculate scallop index
#use poststrat to assign to EPU
epu <- sf::st_read(dsn = here::here('data','gis', 'EPU_extended.shp'))


scall.mean <- survdat::calc_stratified_mean(scalldat, areaPolygon = epu,
                                            areaDescription = 'EPU',
                                            filterByArea = 'MAB',
                                            filterBySeason = 'SUMMER', tidy = T)

scall.index <- scall.mean[variable == 'strat.biomass', .(Biomass = value), by = YEAR]

#Need to expand from kg/tow to mt/km^2
#A tow is approximately 0.0045 km^2 
# 0.001317 (dredge width in nautical miles) * 1.852(convert naut mi to km)
# 1.0 (tow length in nautical miles) * 1.852(convert naut mi to km)
#kg to mt is 0.001
# so conversion is 0.001 / 0.0045 or 0.222
scall.index[, B := Biomass * 0.222]
scall.index[, Biomass := NULL]
scall.index[, Units := 'mt km^-2']
scall.index[, RPATH := 'AtlScallop']

#Input biomass
scall.input <- scall.index[YEAR %in% 1981:1985, .(Biomass = mean(B, na.rm = T)),
                           by = RPATH]

#remove bottom trawl biomass estimate for scallop
MAB.biomass.80s <- MAB.biomass.80s[RPATH != 'AtlScallop',]
#Add scallop biomass estimate from scallop survey
MAB.biomass.80s <- rbindlist(list(MAB.biomass.80s, scall.input))



# Add EMAX groups ------------------------------------------
load(url('https://github.com/NOAA-EDAB/Rpathdata/blob/cb7f1b381a79c927e842335408072293c087a8bf/data/MAB_EMAX_params.RData?raw=true'))
MAB.EMAX<-as.data.table(EMAX.params)

## Merge groups with biomass estimates
MAB.groups<-merge(MAB.biomass.80s, MAB.groups, by = 'RPATH', all.y=TRUE)
setnames(MAB.groups,'V1','Biomass', skip_absent = TRUE)

## Subset EMAX model for group and biomass
setnames(MAB.EMAX,'Group','RPATH')
setnames(MAB.EMAX,'Biomass','Biomass')
MAB.EMAX<-MAB.EMAX[, c('RPATH','Biomass')]

## Match EMAX names with RPATH groups
MAB.EMAX<-MAB.EMAX[RPATH == 'Small copepods', RPATH := 'SmCopepods',]
MAB.EMAX<-MAB.EMAX[RPATH == 'Large Copepods', RPATH := 'LgCopepods',]
MAB.EMAX<-MAB.EMAX[RPATH == 'Sea Birds', RPATH := 'SeaBirds',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Macro', RPATH:='Macrobenthos',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Baleen', RPATH := 'BaleenWhales',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Gel',RPATH := 'GelZooplankton',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Phyto', RPATH :='Phytoplankton',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Megabenthos',RPATH := 'Megabenthos',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Pelagics',RPATH := 'Pelagics',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Demersals',RPATH := 'Demersals',]
MAB.EMAX<-MAB.EMAX[RPATH %like% 'Sharks', RPATH :='Sharks']

## Aggregate EMAX sharks
SharksBiomass<-MAB.EMAX[RPATH == 'Sharks', sum(Biomass),]
MAB.EMAX<-MAB.EMAX[RPATH == 'Sharks', Biomass:=SharksBiomass,]

## Aggregate EMAX macrobenthos
BenthosBiomass<-MAB.EMAX[RPATH == 'Macrobenthos', sum(Biomass),]
MAB.EMAX<-MAB.EMAX[RPATH == 'Macrobenthos', Biomass:=BenthosBiomass,]

## Aggregate EMAX megabenthos
MegaBiomass<-MAB.EMAX[RPATH == 'Megabenthos', sum(Biomass),]
MAB.EMAX<-MAB.EMAX[RPATH == 'Megabenthos', Biomass:=MegaBiomass,]

##Assign a portion of micronekton biomass to krill
#SW added
krill_prop<-0.15
KrillBiomass<-MAB.EMAX[RPATH == 'Micronekton', Biomass]*krill_prop
MAB.EMAX<-MAB.EMAX[RPATH == 'Micronekton', Biomass := Biomass*(1-krill_prop)]
MAB.EMAX<-rbind(MAB.EMAX,list("Krill",KrillBiomass))

## Remove unused groups
MAB.EMAX<-MAB.EMAX[RPATH %notin% c('Larval-juv fish- all','Shrimp et al.','Pelagics','Demersals','Discard','Detritus-POC','Fishery'),]
MAB.EMAX<-unique(MAB.EMAX)

## Combine survey and EMAX biomass estimates
MAB.biomass.80s<-merge(MAB.groups,MAB.EMAX,by='RPATH', all=TRUE)
MAB.biomass.80s<-MAB.biomass.80s[is.na(Biomass.y) , Biomass.y := Biomass.x]
MAB.biomass.80s<-MAB.biomass.80s[,Biomass.x :=NULL]
setnames(MAB.biomass.80s,"Biomass.y","Biomass")

##Manually add menhaden biomass - based on Chagaris et al. (2020)
##Assuming 10% of menhaden biomass is in relevant MAB area
MAB.biomass.80s<-MAB.biomass.80s[RPATH == "AtlMenhaden", Biomass :=1.775190819]


#Save output ------------------------------------------
save(MAB.biomass.80s, file = 'data/MAB_biomass_fall_80s.RData')
