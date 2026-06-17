## Info ---------------------------
## Script name: MAB_diet.R
##
## Purpose of script: Generate diet matrix for model functional groups
##                    using reference to diet and stomach data. Diet
##                    data is pulled from other sources where appropriate.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg, further updated by M.T. Grezlik
##
## Notes: This version is specific to the MABRpath model.
##
# Fri Dec 29 13:57:41 2023 





# Load packages and data -------------------------------------
library(readr) 
library(data.table)
#library(rgdal)
library(here)

## Load prey data and stomach data
# prey <- as.data.table(read_csv("data/SASPREY12B.csv"))
prey <- read_csv(url('https://raw.githubusercontent.com/NOAA-EDAB/Rpathdata/main/data/SASPREY12B.csv'))
# load("data/MAB_foodhabits.RData")
MAB_foodhabits_url <- "https://github.com/NOAA-EDAB/Rpathdata/blob/41be5bbe5d46f59468c580c6ab9d7564502c6d8a/data/MAB_foodhabits.RData?raw=true"
load(url(MAB_foodhabits_url))

## Run MAB_biomass_estimates.R
source(here('MAB_biomass_estimates.R'))

## Run MAB_biomass_accumulation.R)
#source('MAB_biomass_accumulation.R')

## Match species names ------------------------------
## Species to species
prey <- as.data.table(prey)
prey[PYCOMNAM == 'ATLANTIC HERRING',        RPATH := 'SmPelagics']
prey[PYCOMNAM == 'ATLANTIC MACKEREL',       RPATH := 'AtlMackerel']
prey[PYCOMNAM == 'BUTTERFISH',              RPATH := 'Butterfish']
prey[PYCOMNAM == 'BUTTERFISH OTOLITHS',     RPATH := 'Butterfish']
prey[PYCOMNAM == 'ATLANTIC COD',            RPATH := 'Cod']
prey[PYCOMNAM == 'HADDOCK',                 RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'GOOSEFISH',               RPATH := 'Goosefish']
prey[PYCOMNAM == 'OFFSHORE HAKE',           RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'SILVER HAKE',             RPATH := 'SilverHake']
prey[PYCOMNAM == 'SILVER HAKE OTOLITHS',    RPATH := 'SilverHake']
prey[PYCOMNAM == 'RED HAKE',                RPATH := 'RedHake']
prey[PYCOMNAM == 'WHITE HAKE',              RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'ACADIAN REDFISH',         RPATH := 'Redfish']
prey[PYCOMNAM == 'POLLOCK',                 RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'OCEAN POUT',              RPATH := 'OceanPout']
prey[PYCOMNAM == 'BLACK SEA BASS',          RPATH := 'BlackSeaBass']
prey[PYCOMNAM == 'BLUEFISH',                RPATH := 'Bluefish']
prey[PYCOMNAM == 'SCUP',                    RPATH := 'Scup']
prey[PYCOMNAM == 'FOURSPOT FLOUNDER',       RPATH := 'Fourspot']
prey[PYCOMNAM == 'SUMMER FLOUNDER',         RPATH := 'SummerFlounder']
prey[PYCOMNAM == 'AMERICAN PLAICE',         RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'WINDOWPANE',              RPATH := 'Windowpane']
prey[PYCOMNAM == 'WINTER FLOUNDER',         RPATH := 'WinterFlounder']
prey[PYCOMNAM == 'WITCH FLOUNDER',          RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'YELLOWTAIL FLOUNDER',     RPATH := 'YTFlounder']
prey[PYCOMNAM == 'SPINY DOGFISH',           RPATH := 'SpinyDogfish']
prey[PYCOMNAM == 'SMOOTH DOGFISH',          RPATH := 'SmoothDogfish']
prey[PYCOMNAM == 'LITTLE SKATE',            RPATH := 'LittleSkate']
prey[PYCOMNAM == 'NORTHERN SHORTFIN SQUID', RPATH := 'Illex']
prey[PYNAM    == 'ILLEX SP',                RPATH := 'Illex']
prey[PYCOMNAM == 'AMERICAN LOBSTER',        RPATH := 'AmLobster']
prey[PYCOMNAM == 'KRILL',                   RPATH := 'Krill'] 
prey[PYCOMNAM == 'EMPTY STOMACH',           RPATH := 'Empty']
prey[PYCOMNAM == 'BLOWN STOMACH',           RPATH := 'Blown']
prey[PYCOMNAM == 'NORTHERN SHRIMP',         RPATH := 'OtherShrimps']
prey[PYCOMNAM == 'HORSESHOE CRAB',          RPATH := 'Megabenthos']
prey[PYCOMNAM == 'RED DEEPSEA CRAB',        RPATH := 'Megabenthos']
prey[PYCOMNAM == 'TILEFISH',                RPATH := 'SouthernDemersals']
prey[PYCOMNAM == 'ALEWIFE',                 RPATH := 'RiverHerring']
prey[PYNAM == 'SELENE SETAPINNIS',          RPATH := 'SmPelagics']
prey[PYNAM == 'EPIGONUS PANDIONIS',         RPATH := 'OtherDemersals']
prey[PYCOMNAM == 'AMERICAN SHAD',              RPATH := 'RiverHerring']
prey[PYCOMNAM == 'ATLANTIC CROAKER',           RPATH := 'AtlCroaker']
prey[PYCOMNAM == 'WEAKFISH',                   RPATH := 'Weakfish']
prey[PYCOMNAM == 'ATLANTIC MENHADEN',                   RPATH := 'AtlMenhaden']


## Species to groups -----------------------------------------------
prey[PYCOMNAM %in% c('SEA SCALLOP', 'SEA SCALLOP VISCERA','SCALLOPS',
                     'SCALLOP VISCERA', 'SCALLOP SHELL'), RPATH := 'AtlScallop']
prey[PYCOMNAM %in% c('ATLANTIC SURFCLAM', 'SURFCLAM VISCERA'), RPATH := 'SurfClam']
prey[PYCOMNAM %in% c('OCEAN QUAHOG', 'OCEAN QUAHOG VISCERA', 'OCEAN QUAHOG SHELL'), 
     RPATH := 'OceanQuahog']
prey[PYCOMNAM %in% c('LONGFIN SQUID', 'LOLIGO SP PEN'), RPATH := 'Loligo']
prey[PYNAM    %in% c('LOLIGO SP', 'LOLIGO SP BEAKS'),   RPATH := 'Loligo']

## Megabenthos species
prey[is.na(RPATH) & AnalCom == 'STARFISH', RPATH := 'Megabenthos']
prey[is.na(RPATH) & AnalCom == 'MANTIS SHRIMPS', RPATH := 'Megabenthos']
prey[is.na(RPATH) & Collcom %in% c('CANCER CRABS', 'DECAPODA CRAB', 'DECAPODA', 
                                   'DECAPODA LARVAE', 'LOBSTER', 'BLUE CRAB', 
                                   'SLIPPER LOBSTERS'), RPATH := 'Megabenthos']

#GelZooplankton/Cephalopods/Shrimp
prey[is.na(RPATH) & AnalCom == 'SQUIDS, OCTOPUS', RPATH := 'OtherCephalopods']
prey[is.na(RPATH) & Collcom %in% c('JELLYFISH', 'CNIDARIA', 'HYDROZOA'), RPATH := 'GelZooplankton']       
prey[is.na(RPATH) & Collcom %in% c('PANDALIDAE', 'PENAEIDAE', 'DECAPODA SHRIMP', 'CRAGONID SHRIMP'), RPATH := 'OtherShrimps']

#Macrobenthos
prey[is.na(RPATH) & MODCAT == 'BENINV', RPATH := 'Macrobenthos']

#MODCAT PELINV
prey[is.na(RPATH) & Collcom == 'COMB JELLIES', RPATH := 'GelZooplankton']
prey[PYCOMNAM == 'ROTIFERS', RPATH := 'Microzooplankton']
prey[is.na(RPATH) & Collcom == 'KRILL', RPATH := 'Krill'] 
prey[is.na(RPATH) & Collcom == 'COPEPODA', RPATH := 'LgCopepods']
prey[is.na(RPATH) & MODCAT == 'PELINV', RPATH := 'Micronekton']

#MODCAT LDEM
prey[is.na(RPATH) & ANALCAT %in% c('BOTFAM', 'SOLFAM'), RPATH := 'SmFlatfishes']
prey[is.na(RPATH) & ANALCAT == 'RAJORD', RPATH := 'OtherSkates']
prey[is.na(RPATH) & ANALCAT %in% c('LUTFAM', 'SCAFAM', 'SCIFAM', 'SPAFAM', 'SERFA3'), RPATH := 'SouthernDemersals']
prey[is.na(RPATH) & ANALCAT == 'SHARK', RPATH := 'Sharks']
prey[is.na(RPATH) & ANALCAT == 'MACFAM', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & ANALCAT %in% c('PLEFAM', 'PLEORD'), RPATH := 'OtherDemersals']
prey[is.na(RPATH) & MODCAT == 'LDEM', RPATH := 'OtherDemersals']

#MODCAT LPEL
prey[PYCOMNAM %in% c('BOA DRAGONFISH', 'VIPERFISH'), RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'LPEL' & ANALCAT %in% c('CARFAM', 'POMFAM', 'SALSAL','SCOFAM', 'OTHFIS'), RPATH := 'OtherPelagics']
prey[is.na(RPATH) & MODCAT == 'LPEL', RPATH := 'SmPelagics']

#MODCAT SDEM
prey[PYCOMNAM == 'DRAGONET FISH', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & AnalCom == 'GREENEYES', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'SDEM', RPATH := 'OtherDemersals']

#MODCAT SPEL
prey[is.na(RPATH) & MODCAT == 'SPEL' & AnalCom == 'LANTERNFISHES', RPATH := 'Mesopelagics']
prey[PYABBR == 'MAUWEI', RPATH := 'Mesopelagics']
prey[is.na(RPATH) & MODCAT == 'SPEL' & AnalCom == 'HERRINGS', RPATH := 'SmPelagics']
prey[is.na(RPATH) & MODCAT == 'SPEL', RPATH := 'SmPelagics']

#Fish Larvae
prey[is.na(RPATH) & MODCAT == 'FISLAR', RPATH := 'Micronekton']

#Ignoring eggs for now
prey[is.na(RPATH) & MODCAT == 'FISEGG', RPATH := 'NotUsed']

#Miscellaneous - Mostly trash (plastic, twine, rubber)
prey[PYABBR %in% c('POLLAR', 'AMPTUB'), RPATH := 'Macrobenthos']
prey[is.na(RPATH) & MODCAT == 'MISC', RPATH := 'NotUsed']

#Other
prey[PYABBR %in% c('INVERT', 'ARTHRO', 'CRUSTA', 'CRUEGG', 'INSECT', 'UROCHO'),
     RPATH := 'Macrobenthos']
prey[PYABBR %in% c('MARMAM', 'MARMA2', 'DELDEL', 'GLOBSP'), RPATH := 'Odontocetes']
prey[PYABBR %in% c('AVES', 'AVEFEA'), RPATH := 'Seabirds']
prey[PYABBR %in% c('PLANKT', 'DIATOM'), RPATH := 'Phytoplankton']
prey[is.na(RPATH) & MODCAT == 'OTHER', RPATH := 'NotUsed'] #Plants and Parasites

#Leftovers
prey[AnalCom == 'SAND LANCES', RPATH := 'SmPelagics']
prey[PYABBR %in% c('PERORD', 'MYOOCT'), RPATH := 'OtherDemersals']
prey[PYABBR %in% c('CLUSCA', 'CLUHA2'), RPATH := 'AtlHerring']

#Unidentified Stuff
prey[MODCAT == 'AR', RPATH := 'AR']
prey[is.na(RPATH) & AnalCom == 'OTHER FISH', RPATH := 'UNKFish']
prey[PYABBR == 'FISSCA', RPATH := 'UNKFish']
prey[PYABBR == 'CHONDR', RPATH := 'UNKSkate']
prey[PYABBR == 'PRESER', RPATH := 'NotUsed']

## Reassign groups -----------------------------------------------
spp <- spp[!duplicated(spp$SVSPP),]
spp <- spp[RPATH == 'AtlHerring',     RPATH := 'SmPelagics']
# spp <- spp[RPATH == 'Clams',          RPATH := 'Megabenthos']
spp <- spp[RPATH == 'Haddock',        RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'LargePelagics',  RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'OffHake',        RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'OtherFlatfish',  RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Pollock',        RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Rays',           RPATH := 'OtherSkates']
spp <- spp[RPATH == 'RedCrab',        RPATH := 'Megabenthos']
spp <- spp[RPATH == 'AmShad',         RPATH := 'RiverHerring']
spp <- spp[RPATH == 'Tilefish',       RPATH := 'SouthernDemersals']
spp <- spp[RPATH == 'WitchFlounder',  RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Barndoor',       RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'RedDrum',        RPATH := 'SouthernDemersals']
spp <- spp[RPATH == 'RedFish',        RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'Redfish',        RPATH := 'OtherDemersals']
spp <- spp[RPATH == 'StripedBass',    RPATH := 'OtherPelagics']
spp <- spp[RPATH == 'Sturgeon',       RPATH := 'OtherDemersals']

#Assign Rpath codes to predators
MAB.fh <- merge(MAB.fh, spp, by = 'SVSPP', all.x = T)
setnames(MAB.fh, 'RPATH', 'Rpred')

#Assign Rpath codes to prey
MAB.fh <- merge(MAB.fh, prey[, list(PYNAM, RPATH)], by = 'PYNAM', all.x = T)
setnames(MAB.fh, 'RPATH', 'Rprey')

#Remove NotUsed, AR, UNKFish and UNKSkate
MAB.fh <- MAB.fh[!Rprey %in% c('NotUsed', 'AR', 'UNKFish', 'UNKSkate'), ]

#Remove Freshwater as predator
MAB.fh <- MAB.fh[!Rpred %in% 'Freshwater', ]

#Deal with prey items that were missing from prey data table
MAB.fh <- MAB.fh[PYNAM == 'SYACIUM PAPILLOSUM',   Rprey := 'OtherDemersals']
MAB.fh <- MAB.fh[PYNAM == 'DACTYLOPTERUS VOLITANS',     Rprey := 'OtherPelagics']
MAB.fh <- MAB.fh[PYNAM == 'SCYLIORHINUS RETIFER', Rprey := 'Sharks']
MAB.fh <- MAB.fh[PYNAM == 'ANTIGONIA CAPROS',     Rprey := 'OtherDemersals']
MAB.fh <- MAB.fh[PYNAM == 'ANTHIAS NICHOLSI',     Rprey := 'SouthernDemersals']
MAB.fh <- MAB.fh[PYNAM == 'SELENE SETAPINNIS',    Rprey := 'SmPelagics']
MAB.fh <- MAB.fh[PYNAM == 'OGCOCEPHALIDAE',    Rprey := 'OtherDemersals']
MAB.fh <- MAB.fh[PYNAM == 'STERNOPTYCHIDAE',    Rprey := 'Mesopelagics']
MAB.fh <- MAB.fh[PYNAM == 'SYNAGROPS BELLUS',    Rprey := 'Mesopelagics']
MAB.fh <- MAB.fh[PYNAM == 'PRIACANTHUS CRUENTATUS',    Rprey := 'SouthernDemersals']
MAB.fh <- MAB.fh[PYNAM == 'PARASUDIS TRUCULENTA',    Rprey := 'Mesopelagics']
MAB.fh <- MAB.fh[PYNAM == 'MONOLENE SESSILICAUDA',   Rprey := 'OtherDemersals']
MAB.fh <- MAB.fh[PYNAM == 'PRIONOTUS ALATUS',    Rprey := 'SouthernDemersals']


#Merge prey items
setkey(MAB.fh, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW, Rpred, PDID, Rprey)
MAB.fh2 <- MAB.fh[, sum(PYAMTW), by = key(MAB.fh)]
setnames(MAB.fh2, 'V1', 'PYAMTW')

#Calculate Percent weight using a cluster sampling design (Nelson 2014)
cluster <- c('CRUISE6', 'STRATUM', 'STATION', 'TOW', 'Rpred')

#Calculate numbers of fish per cluster
MAB.pred <- unique(MAB.fh2, by = c(cluster, 'PDID'))
MAB.pred[, Mi := length(PDID), by = cluster]
MAB.pred <- unique(MAB.pred[, list(CRUISE6, STRATUM, STATION, TOW, Rpred, Mi)])
MAB.pred[, sumMi := sum(Mi), by = Rpred]
MAB.fh2 <- merge(MAB.fh2, MAB.pred, by = cluster)

#Sum prey weight per stomach
MAB.fh2[, yij := sum(PYAMTW), by = c(cluster, 'Rprey')]
MAB.fh2[, mu := yij / Mi]
MAB.cluster <- unique(MAB.fh2, by = c(cluster, 'Rprey'))
MAB.cluster[, c('PYAMTW', 'yij') := NULL]

#Calculate weighted contribution
MAB.cluster[, Miu := Mi * mu]
MAB.cluster[, rhat := Miu / sumMi]
MAB.cluster[, sum.rhat := sum(rhat), by = .(Rpred, Rprey)]

#Grab unique rows
MAB.diet.survey <- unique(MAB.cluster[, list(Rpred, Rprey, sum.rhat)], by = c('Rpred', 'Rprey'))

MAB.diet.survey <- na.omit(MAB.diet.survey)

#Convert to percentages
MAB.diet.survey[, tot.preyw := sum(sum.rhat), by = Rpred]
MAB.diet.survey[, preyper := sum.rhat / tot.preyw]
MAB.diet.survey[, c('sum.rhat', 'tot.preyw') := NULL]
setkey(MAB.diet.survey, Rpred, preyper)

#Remove OtherDemersals, Sharks, SmPelagics, Illex and Loligo
#Will use EMAX diet estimates for these groups
MAB.diet.survey <- MAB.diet.survey[!Rpred %in% c('OtherDemersals','Sharks','SmPelagics','Loligo','Illex')]

#Load in params table with biomass as previously calculated
all.groups <- MAB.biomass.80s

# EMAX group diets ------------------------------------------------
## Add EMAX classifications to RPATH groups -------------------------
convert.groups<-data.table(RPATH = c('AmLobster','RiverHerring','AtlCroaker','AtlMackerel','AtlScallop',
                                     'Bacteria','BaleenWhales','BlackSeaBass','Bluefish','Butterfish',
                                     'Cod','Fourspot','GelZooplankton','Goosefish','HMS','Illex',
                                     'LgCopepods','LittleSkate','Loligo','Macrobenthos','Megabenthos',
                                     'Mesopelagics','Micronekton','Microzooplankton','OceanPout',
                                     'Odontocetes','OtherCephalopods','OtherDemersals','OtherPelagics',
                                     'OtherShrimps','OtherSkates','Phytoplankton','RedHake','Scup',
                                     'SeaBirds','Sharks','Sharks','SilverHake','SmCopepods','SmFlatfishes',
                                     'SmoothDogfish','SmPelagics','SouthernDemersals','SpinyDogfish',
                                     'SummerFlounder','Weakfish','Windowpane',
                                     'WinterFlounder','WinterSkate','YTFlounder','Krill','AtlMenhaden','Discards','Detritus'),
                           
                           EMAX = c('Megabenthos- other','Small Pelagics- commercial','Demersals- benthivores',
                                    'Small Pelagics- commercial','Megabenthos- filterers','Bacteria',
                                    'Baleen Whales','Demersals- omnivores','Medium Pelagics- (piscivores & other)',
                                    'Small Pelagics- commercial','Demersals- piscivores','Demersals- benthivores',
                                    'Gelatinous Zooplankton','Demersals- piscivores','HMS','Small Pelagics- squid',
                                    'Large Copepods','Demersals- omnivores','Small Pelagics- squid','Macrobenthos',
                                    'Megabenthos','Mesopelagics','Micronekton','Microzooplankton','Demersals- benthivores',
                                    'Odontocetes','Small Pelagics- squid','Demersals- benthivores','Medium Pelagics- (piscivores & other)',
                                    'Shrimp et al.','Demersals- omnivores','Phytoplankton- Primary Producers','Demersals- benthivores',
                                    'Demersals- benthivores','Sea Birds','Sharks- pelagics','Sharks- coastal','Demersals- piscivores','Small copepods',
                                    'Demersals- benthivores','Demersals- piscivores','Small Pelagics- other','Demersals- benthivores',
                                    'Demersals- piscivores','Demersals- piscivores','Demersals- benthivores','Demersals- benthivores',
                                    'Demersals- benthivores','Demersals- omnivores','Demersals- benthivores','Micronekton','Small Pelagics- other','Discard','Detritus-POC'))
all.groups<-merge(all.groups,convert.groups,by = 'RPATH')

#Remove EMAX:RPATH many:1s
all.groups <- all.groups[!RPATH %in% c('Megabenthos','Macrobenthos'),]

#Change Discard to Detritus ---------------------------------
all.groups[which(EMAX == "Discard")]$RPATH <- "Detritus"

#Calculate proportionality for EMAX:RPATH many:1s
load("data/EMAX_params.RData")
EMAX.params<-as.data.table(EMAX.params)

#Megabenthos groups --------------------------------------------
#Calculate biomass remaining in Megabenthos- filterers after removing scallops
Megabenthos.filterers <- EMAX.params[Group == 'Megabenthos- filterers',Biomass] - all.groups[RPATH == 'AtlScallop',Biomass]
Megabenthos.filterers <- as.data.table(cbind('Megabenthos',Megabenthos.filterers,'Megabenthos- filterers'))
setnames(Megabenthos.filterers, c("RPATH","Biomass","EMAX"))
#Calculate biomass remaining in Megabenthos- other after removing lobster
Megabenthos.other <-EMAX.params[Group == 'Megabenthos- other',Biomass] - all.groups[RPATH == 'AmLobster',Biomass]
Megabenthos.other <- as.data.table(cbind('Megabenthos',Megabenthos.other,'Megabenthos- other'))
setnames(Megabenthos.other, c("RPATH","Biomass","EMAX"))
# #Calculate biomass remaining in Megabenthos- other after removing Quahog
# Megabenthos.other <-EMAX.params[Group == 'Megabenthos- other',Biomass] - all.groups[RPATH == 'OceanQuahog',Biomass]
# Megabenthos.other <- as.data.table(cbind('Megabenthos',Megabenthos.other,'Megabenthos- other'))
# setnames(Megabenthos.other, c("RPATH","Biomass","EMAX"))
# #Calculate biomass remaining in Megabenthos- other after removing Surfclam
# Megabenthos.other <-EMAX.params[Group == 'Megabenthos- other',Biomass] - all.groups[RPATH == 'SurfClam',Biomass]
# Megabenthos.other <- as.data.table(cbind('Megabenthos',Megabenthos.other,'Megabenthos- other'))
# setnames(Megabenthos.other, c("RPATH","Biomass","EMAX"))

#Macrobenthos groups ----------------------------------------------
Macrobenthos <- as.data.table(EMAX.params[Group %like% 'Macrobenthos',c(1,3)])
Macrobenthos <- cbind(Macrobenthos,"Macrobenthos")
setnames(Macrobenthos,c('EMAX','Biomass','RPATH'))

#Micronekton groups ------------------------------------------
Micronekton <- as.data.table(EMAX.params[Group %in% c('Micronekton','Larval-juv fish- all'),c(1,3)])
Micronekton <- cbind(Micronekton,"Micronekton")
setnames(Micronekton,c('EMAX','Biomass','RPATH'))

#SmPelagics groups -------------------------------------------
SmPelagics <- as.data.table(EMAX.params[Group =='Small Pelagics- anadromous', c(1,3)])
SmPelagics <- cbind(SmPelagics,"SmPelagics")
setnames(SmPelagics,c('EMAX','Biomass','RPATH'))

#Discard group ------------------------------------------------
EMAX.discards <- as.data.table(EMAX.params[Group =='Discard', c(1,3)])
EMAX.discards <- cbind(EMAX.discards,"Detritus")
setnames(EMAX.discards,c('EMAX','Biomass','RPATH'))

#Detritus group -----------------------------------------------
EMAX.detritus <- as.data.table(EMAX.params[Group =='Detritus-POC', c(1,3)])
EMAX.detritus <- cbind(EMAX.detritus,"Detritus")
setnames(EMAX.detritus,c('EMAX','Biomass','RPATH'))

#Merge all groups
all.groups <- rbind(all.groups,Megabenthos.filterers,Megabenthos.other,Macrobenthos,Micronekton,SmPelagics,EMAX.discards,EMAX.detritus, fill=TRUE)
all.groups <- all.groups[,Biomass := as.numeric(Biomass)]
all.groups[, EMAX.tot := sum(Biomass), by = EMAX]
all.groups[, Rpath.prop := Biomass / EMAX.tot]

#Whale diet from Laurel - used in Sarah Gaichas's GOM  ----------------
balwhale <- data.table(EMAX = c('Large Copepods', 'Micronekton', 'Small Pelagics- commercial',
                                'Small Pelagics- other', 'Small Pelagics- squid',
                                'Demersals- benthivores', 'Demersals- omnivores',
                                'Demersals- piscivores'),
                       DC = c(0.15392800, 0.50248550, 0.15509720, 0.08101361, 
                              0.03142953, 0.01532696, 0.01102390, 0.04969525))
balwhale <- merge(balwhale, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
balwhale[, preyper := DC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
balwhale <- balwhale[, sum(preyper), by = RPATH]
balwhale[, Rpred := 'BaleenWhales']
setnames(balwhale, c('RPATH', 'V1'), c('Rprey', 'preyper'))

#Odontocetes ----------------------------------------------
tooth <- EMAX.params[, list(diet.Odontocetes,diet.Group)]
setnames(tooth,'diet.Group','EMAX')
tooth <- merge(tooth, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
tooth[, preyper := diet.Odontocetes * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
tooth <- tooth[, sum(preyper), by = RPATH]
tooth[, Rpred := 'Odontocetes']
setnames(tooth, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(balwhale,tooth))

#SeaBirds -------------------------------------------------
birds <- EMAX.params[, list(diet.Sea.Birds,diet.Group)]
setnames(birds,'diet.Group','EMAX')
birds <- merge(birds, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
birds[, preyper := diet.Sea.Birds * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
birds <- birds[, sum(preyper), by = RPATH]
birds[, Rpred := 'SeaBirds']
setnames(birds, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,birds))

#HMS ------------------------------------------------------
hms <- EMAX.params[, list(diet.HMS,diet.Group)]
setnames(hms,'diet.Group','EMAX')
hms <- merge(hms, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
hms[, preyper := diet.HMS * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
hms <- hms[, sum(preyper), by = RPATH]
hms[, Rpred := 'HMS']
setnames(hms, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,hms))

#Sharks -----------------------------------------------------
sharks <- EMAX.params[, list(diet.Sharks..pelagics,diet.Group)]
setnames(sharks,'diet.Group','EMAX')
sharks <- merge(sharks, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
sharks[, preyper := diet.Sharks..pelagics * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
sharks <- sharks[, sum(preyper), by = RPATH]
sharks[, Rpred := 'Sharks']
setnames(sharks, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,sharks))

#OtherShrimp ----------------------------------------------
shrimp <- EMAX.params[, list(diet.Shrimp.et.al.,diet.Group)]
setnames(shrimp,'diet.Group','EMAX')
shrimp <- merge(shrimp, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
#shrimp <- shrimp[,RPATH.sum := sum(diet.Shrimp.et.al.), by = RPATH]
shrimp[, preyper := diet.Shrimp.et.al. * Rpath.prop]
#shrimp <- unique(shrimp[,c('RPATH','preyper')])
#Need to sum many:1 EMAX:Rpath
shrimp <- shrimp[, sum(preyper), by = RPATH]
others <- copy(shrimp[, Rpred := 'OtherShrimps'])
nshrimp<-copy(shrimp[,Rpred := 'NShrimp'])
setnames(others, c('RPATH', 'V1'), c('Rprey', 'preyper'))
setnames(nshrimp, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,others))

#GelZooplankton ----------------------------------------
jelly <- EMAX.params[, list(diet.Gelatinous.Zooplankton,diet.Group)]
setnames(jelly,'diet.Group','EMAX')
jelly <- merge(jelly, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
jelly[, preyper := diet.Gelatinous.Zooplankton * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
jelly <- jelly[, sum(preyper), by = RPATH]
jelly[, Rpred := 'GelZooplankton']
setnames(jelly, c('RPATH', 'V1'), c('Rprey', 'preyper'))
#doesn't quite sum to 1, make adjustments
jelly$preyper <- jelly$preyper/sum(jelly$preyper)

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,jelly))

#Microzooplankton -------------------------------------------
micro <- EMAX.params[, list(diet.Microzooplankton,diet.Group)]
setnames(micro,'diet.Group','EMAX')
micro <- merge(micro, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
micro[, preyper := diet.Microzooplankton * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
micro <- micro[, sum(preyper), by = RPATH]
micro[, Rpred := 'Microzooplankton']
setnames(micro, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,micro))

#Micronekton ---------------------------------------
micronekton <- EMAX.params[, list(diet.Micronekton,diet.Group)]
setnames(micronekton,'diet.Group','EMAX')
micronekton <- merge(micronekton, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
micronekton[, preyper := diet.Micronekton * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
micronekton <- micronekton[, sum(preyper), by = RPATH]
micronekton[, Rpred := 'Micronekton']
setnames(micronekton, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,micronekton))

#Krill --------------------------------------------------
#Use micronekton diet from EMAX
krill <- EMAX.params[, list(diet.Micronekton,diet.Group)]
setnames(krill,'diet.Group','EMAX')
krill <- merge(krill, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
krill[, preyper := diet.Micronekton * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
krill <- krill[, sum(preyper), by = RPATH]
krill[, Rpred := 'Krill']
setnames(krill, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,krill))

#OtherPelagics
# otherpel <- EMAX.params[, list(diet.Medium.Pelagics...piscivores...other.,diet.Group)]
# setnames(otherpel,'diet.Group','EMAX')
# otherpel <- merge(otherpel, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
# otherpel[, preyper := diet.Medium.Pelagics...piscivores...other. * Rpath.prop]
# #Need to sum many:1 EMAX:Rpath
# otherpel <- otherpel[, sum(preyper), by = RPATH]
# otherpel[, Rpred := 'OtherPelagics']
# setnames(otherpel, c('RPATH', 'V1'), c('Rprey', 'preyper'))
# 
# MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,otherpel))

#OtherDemersals --------------------------------------------------
otherdem <- EMAX.params[, list(diet.Demersals..benthivores,diet.Group)]
setnames(otherdem,'diet.Group','EMAX')
otherdem <- merge(otherdem, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
otherdem[, preyper := diet.Demersals..benthivores * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
otherdem <- otherdem[, sum(preyper), by = RPATH]
otherdem[, Rpred := 'OtherDemersals']
setnames(otherdem, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,otherdem))

#Lobster - use Megabenthos- other diet ------------------------
lobster <- EMAX.params[, list(diet.Megabenthos..other,diet.Group)]
setnames(lobster,'diet.Group','EMAX')
lobster <- merge(lobster, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
lobster[, preyper := diet.Megabenthos..other * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
lobster <- lobster[, sum(preyper), by = RPATH]
lobster[, Rpred := 'AmLobster']
setnames(lobster, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,lobster))

#AtlScallop  -------------------------------------------------------------------------
#- use Megabenthos- filterers diet
scallop <- EMAX.params[, list(diet.Megabenthos..filterers,diet.Group)]
setnames(scallop,'diet.Group','EMAX')
scallop <- merge(scallop, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
scallop[, preyper := diet.Megabenthos..filterers * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
scallop <- scallop[, sum(preyper), by = RPATH]
scallop[, Rpred := 'AtlScallop']
setnames(scallop, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,scallop))

#OceanQuahog -------------------------------------------------------------------------
#- use Megabenthos- filterers diet
quahog <- EMAX.params[, list(diet.Megabenthos..filterers,diet.Group)]
setnames(quahog,'diet.Group','EMAX')
quahog <- merge(quahog, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
quahog[, preyper := diet.Megabenthos..filterers * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
quahog <- quahog[, sum(preyper), by = RPATH]
quahog[, Rpred := 'OceanQuahog']
setnames(quahog, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,quahog))

#SurfClam -------------------------------------------------------------------------
#- use Megabenthos- filterers diet
surfclam <- EMAX.params[, list(diet.Megabenthos..filterers,diet.Group)]
setnames(surfclam,'diet.Group','EMAX')
surfclam <- merge(surfclam, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
surfclam[, preyper := diet.Megabenthos..filterers * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
surfclam <- surfclam[, sum(preyper), by = RPATH]
surfclam[, Rpred := 'SurfClam']
setnames(surfclam, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,surfclam))

#OtherCephalopods -------------------------------------------------------------------
ceph <- EMAX.params[, list(diet.Small.Pelagics..squid,diet.Group)]
setnames(ceph,'diet.Group','EMAX')
ceph <- merge(ceph, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
ceph[, preyper := diet.Small.Pelagics..squid * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
ceph <- ceph[, sum(preyper), by = RPATH]
ceph[,Rpred := 'OtherCephalopods']
setnames(ceph, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,ceph))

#Illex ---------------------------------------------------------- 
# use EMAX squid diet 
illex <- EMAX.params[, list(diet.Small.Pelagics..squid,diet.Group)]
setnames(illex,'diet.Group','EMAX')
illex <- merge(illex, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
illex[, preyper := diet.Small.Pelagics..squid * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
illex <- illex[, sum(preyper), by = RPATH]
illex[,Rpred := 'Illex']
setnames(illex, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,illex))

#Loligo ---------------------------------------------------------- 
# use EMAX squid diet
loligo <- EMAX.params[, list(diet.Small.Pelagics..squid,diet.Group)]
setnames(loligo,'diet.Group','EMAX')
loligo <- merge(loligo, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
loligo[, preyper := diet.Small.Pelagics..squid * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
loligo <- loligo[, sum(preyper), by = RPATH]
loligo[,Rpred := 'Loligo']
setnames(loligo, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,loligo))

#SmCopepods -----------------------------------------------------
smcope <- EMAX.params[, list(diet.Small.copepods,diet.Group)]
setnames(smcope,'diet.Group','EMAX')
smcope <- merge(smcope, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
smcope[, preyper := diet.Small.copepods * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
smcope <- smcope[, sum(preyper), by = RPATH]
smcope[,Rpred := 'SmCopepods']
setnames(smcope, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,smcope))

#LgCopepods -----------------------------------------------------
lgcope <- EMAX.params[, list(diet.Large.Copepods,diet.Group)]
setnames(lgcope,'diet.Group','EMAX')
lgcope <- merge(lgcope, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
lgcope[, preyper := diet.Large.Copepods * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
lgcope <- lgcope[, sum(preyper), by = RPATH]
lgcope[,Rpred := 'LgCopepods']
setnames(lgcope, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,lgcope))

#SmPelagics ----------------------------------------------------- 
# - use diet from Small Pelagics- other
smpel <- EMAX.params[, list(diet.Small.Pelagics..other,diet.Group)]
setnames(smpel,'diet.Group','EMAX')
smpel <- merge(smpel, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
smpel[, preyper := diet.Small.Pelagics..other * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
smpel <- smpel[, sum(preyper), by = RPATH]
smpel[,Rpred := 'SmPelagics']
setnames(smpel, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,smpel))

#Menhaden ------------------------------------------------------- 
# use diet from Small Pelagics- other
men <- EMAX.params[, list(diet.Small.Pelagics..other,diet.Group)]
setnames(men,'diet.Group','EMAX')
men <- merge(men, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
men[, preyper := diet.Small.Pelagics..other * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
men <- men[, sum(preyper), by = RPATH]
men[,Rpred := 'AtlMenhaden']
setnames(men, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,men))

#Bacteria -------------------------------------------------------
bacteria <- EMAX.params[, list(diet.Bacteria,diet.Group)]
setnames(bacteria,'diet.Group','EMAX')
bacteria <- merge(bacteria, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
bacteria[, preyper := diet.Bacteria * Rpath.prop]
bacteria <- bacteria[, sum(preyper), by = RPATH]
bacteria[,Rpred := 'Bacteria']
setnames(bacteria, c('RPATH', 'V1'), c('Rprey', 'preyper'))

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,bacteria))

#Macrobenthos --------------------------------------------------------- 
# need to merge multiple EMAX groups
Macrobenthos<-Macrobenthos[,macro.prop := Biomass/sum(Biomass)]
poly<-EMAX.params[, list(diet.Macrobenthos..polychaetes,diet.Group)]
setnames(poly, 'diet.Macrobenthos..polychaetes','DC')
poly<-poly[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- polychaetes',macro.prop]]
crus<-EMAX.params[, list(diet.Macrobenthos..crustaceans,diet.Group)]
setnames(crus, 'diet.Macrobenthos..crustaceans','DC')
crus<-crus[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- crustaceans',macro.prop]]
moll<-EMAX.params[, list(diet.Macrobenthos..molluscs,diet.Group)]
setnames(moll, 'diet.Macrobenthos..molluscs','DC')
moll<-moll[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- molluscs',macro.prop]]
oth<-EMAX.params[, list(diet.Macrobenthos..other,diet.Group)]
setnames(oth, 'diet.Macrobenthos..other','DC')
oth<-oth[,macro.prop := Macrobenthos[EMAX == 'Macrobenthos- other',macro.prop]]

#Combine all EMAX groups into one --------------------------------------
combo <- rbindlist(list(poly, crus, moll, oth))
combo[, newDC := DC * macro.prop]
combo<-(combo[,sumnewDC := sum(newDC), by = 'diet.Group'])
combo<-unique(combo[,c('diet.Group','sumnewDC')])
setnames(combo, 'diet.Group','EMAX')
combo <- merge(combo, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
combo[, preyper := sumnewDC * Rpath.prop]
#Need to sum many:1 EMAX:Rpath
combo <- combo[, sum(preyper), by = RPATH]
setnames(combo, c('RPATH','V1'),c('Rprey','preyper'))
combo[, Rpred := 'Macrobenthos']

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,combo))

#Megabenthos ---------------------------------------------------------
# need to merge multiple EMAX groups
Megabenthos<-all.groups[RPATH == 'Megabenthos',]
Megabenthos<-Megabenthos[,mega.prop := Biomass/sum(Biomass)]
filter<- EMAX.params[, list(diet.Megabenthos..filterers,diet.Group)]
setnames(filter, 'diet.Megabenthos..filterers','DC')
filter<-filter[,mega.prop := Megabenthos[EMAX == 'Megabenthos- filterers',mega.prop]]
othermega<-EMAX.params[, list(diet.Megabenthos..other,diet.Group)]
setnames(othermega, 'diet.Megabenthos..other','DC')
othermega<-othermega[,mega.prop := Megabenthos[EMAX == 'Megabenthos- other',mega.prop]]

#Combine all EMAX groups into one
combomega <- rbindlist(list(filter, othermega))
combomega[, newDC := DC * mega.prop]
combomega<-(combomega[,sumnewDC := sum(newDC), by = 'diet.Group'])
combomega<-unique(combomega[,c('diet.Group','sumnewDC')])
setnames(combomega, 'diet.Group','EMAX')
combomega <- merge(combomega, all.groups[, list(RPATH, EMAX, Rpath.prop)], by = 'EMAX')
combomega[, preyper := sumnewDC * Rpath.prop]

#Need to sum many:1 EMAX:Rpath
combomega <- combomega[, sum(preyper), by = RPATH]
setnames(combomega, c('RPATH','V1'),c('Rprey','preyper'))
combomega[, Rpred := 'Megabenthos']

MAB.diet.EMAX<-rbindlist(list(MAB.diet.EMAX,combomega))

#Merge diet.survey with diet.EMAX -------------------------------------
MAB.diet <- rbindlist(list(MAB.diet.survey, MAB.diet.EMAX), use.names = T)


# Check diet -----------------------------------------------------------

MAB.diet.check <- MAB.diet |> 
  dplyr::group_by(Rpred) |> 
  dplyr::summarise(dietper = sum(preyper)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(dietper = round(dietper, 4))

# #Clean diet matrix
# MAB.diet<-na.omit(MAB.diet)
# MAB.diet<-MAB.diet[-148:-157]

#Import diet table for testing webplot shifts
#MAB.diet<-read.csv(file = "MAB_diet_adjusted.csv", header = TRUE, stringsAsFactors = FALSE)

#Output results
save(MAB.diet, file = 'data/MAB_diet.RData')

