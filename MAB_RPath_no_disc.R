
## Script name: MAB_RPath_no_disc.R
##
## Purpose of script: Compile all data to create functional RPath model.
##                    Balance model. Eliminate discards for purposes of ENA analyses.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg, updated by M.T.Grezlik
##
##
## Email: sarah.j.weisberg@stonybrook.edu
#

# Thu May 16 10:02:08 2024 ------------------------------


## Load libraries, packages and functions
# remotes::install_github('NOAA-EDAB/Rpath')
library(Rpath); library(data.table); library(dplyr); library(here)

# Model Setup -------------------------------------------------------------

## Add functional groups to model and generate rpath params
source(here("MAB_fleets.R"))
#remove Discards from groups list
groups_fleets <- groups_fleets %>% filter(RPATH != "Discards")
groups<-as.vector(groups_fleets$RPATH)
types<-c(rep(0,32),1,rep(0,20),2,rep(3,12))
MAB.rpath.params<-create.rpath.params(group = groups, type = types)

#count numbers of each group type
nliving <- nrow(MAB.rpath.params$model[Type <  2, ])
ndead   <- nrow(MAB.rpath.params$model[Type == 2, ])
nfleets <- nrow(MAB.rpath.params$model[Type == 3, ])

## Add biomass estimates
source(here("MAB_biomass_estimates.R"))
biomass<-left_join(groups_fleets,MAB.biomass.80s,by = "RPATH")

#Remove Discards 
biomass<-biomass %>% filter(RPATH !="Discards")
#and set Detritus biomass to NA
biomass[which(RPATH == "Detritus")]$Biomass<-NA
#Convert biomass to vector
biomass<-as.vector(biomass$Biomass)
MAB.rpath.params$model[,Biomass:=biomass]

## Add biological parameters
source(here("MAB_params.R"))
params<-left_join(groups_fleets,params,by = "RPATH")
## Add PB parameters
pb<-as.vector(params$PB)
MAB.rpath.params$model[,PB:=pb]

## Add QB parameters
qb<-as.vector(params$QB)
MAB.rpath.params$model[,QB:=qb]

## Add biomass accumulation
ba<-as.vector(params$BA)
ba[is.na(ba)]<-0
#manually add ba term for cod
ba[11]<-(-0.05)
ba[55:66]<-NA 
MAB.rpath.params$model[,BioAcc:=ba]

# Add unassimilated consumption
MAB.rpath.params$model[Type == 0, Unassim := 0.2]
MAB.rpath.params$model[Type > 0 & Type < 3, Unassim := 0]
# MAB.rpath.params$model[, Unassim := c(rep(0.2,52),rep(NA,12))]
#Increase unassim to 0.4 for zooplankton
MAB.rpath.params$model[Group %in% c('Microzooplankton', 'SmCopepods', 'LgCopepods'), Unassim := 0.4]

#Increase unassim to 0.3 for other detritavores
MAB.rpath.params$model[Group %in% c('AmLobster', 'Macrobenthos', 'Megabenthos', 
                                 'AtlScallop', 'OtherShrimps'), Unassim := 0.3]

## Add detrital fate and set discards to 0
MAB.rpath.params$model[,Detritus:=c(rep(1,nliving),rep(0,ndead),rep(1,nfleets))]
#MAB.rpath.params$model[,Discards:=c(rep(0,51),rep(1,11))]

## Add landings by gear type
source("MAB_discards.R")

## Fixed Gear
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
# fixed[52]<-0
MAB.rpath.params$model[,"Fixed Gear":=fixed]
MAB.rpath.params$model[Group == 'Detritus', 'Fixed Gear' := 0]

## Large Mesh
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
# lg_mesh[52]<-0
MAB.rpath.params$model[, "LG Mesh" := lg_mesh]
MAB.rpath.params$model[Group == 'Detritus', 'LG Mesh' := 0]

## Other
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
# other[52]<-0
MAB.rpath.params$model[, "Other" := other]
MAB.rpath.params$model[Group == 'Detritus', 'Other' := 0]

## Small Mesh
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
# sm_mesh[52]<-0
MAB.rpath.params$model[, "SM Mesh" := sm_mesh]
MAB.rpath.params$model[Group == 'Detritus', 'SM Mesh' := 0]

## Scallop Dredge
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
# scallop[52]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop]
MAB.rpath.params$model[Group == 'Detritus', 'Scallop Dredge' := 0]

## Trap
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
# trap[52]<-0
MAB.rpath.params$model[, "Trap" := trap]
MAB.rpath.params$model[Group == 'Detritus', 'Trap' := 0]

## HMS Fleet
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
# hms[52]<-0
MAB.rpath.params$model[, "HMS Fleet" := hms]
MAB.rpath.params$model[Group == 'Detritus', 'HMS Fleet' := 0]

## Pelagic
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
# pelagic[52]<-0
MAB.rpath.params$model[, "Pelagic" := pelagic]
MAB.rpath.params$model[Group == 'Detritus', 'Pelagic' := 0]

## Other Dredge
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
# other_dredge[52]<-0
MAB.rpath.params$model[, "Other Dredge" := other_dredge]
MAB.rpath.params$model[Group == 'Detritus', 'Other Dredge' := 0]

## Clam
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
# clam[52]<-0
MAB.rpath.params$model[, "Clam Dredge" := clam]
MAB.rpath.params$model[Group == 'Detritus', 'Clam Dredge' := 0]

## Recreational
source("MAB_rec_catch.R")
rec_catch<-left_join(groups_fleets,MAB.mrip_summary,by="RPATH")
rec_catch<-as.vector(rec_catch$Per_Area)
rec_catch[is.na(rec_catch)]<-0
# rec_catch[52]<-0
rec_catch[55:66]<-NA
MAB.rpath.params$model[,"Recreational":=rec_catch]
#MAB.rpath.params[["model"]][["Recreational"]][51:61] <-NA

#Manually add menhaden catch (SW)
purse_catch<-c(rep(0,52),0.35,0,rep(NA,12))
MAB.rpath.params$model[,"PurseSeine":=purse_catch]

#add discards
### Fixed Gear -----------------------------------------------------
fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
fixed.d<-as.vector(fixed.d$discards)
# fixed.d[50:51]<-0
MAB.rpath.params$model[, "Fixed Gear.disc" := fixed.d]
MAB.rpath.params$model[Group == 'Detritus', 'Fixed Gear.disc' := 0]

### Lg Mesh -------------------------------------------------------
lg_mesh.d<-left_join(groups_fleets,lg_mesh.d,by="RPATH")
lg_mesh.d<-as.vector(lg_mesh.d$discards)
# lg_mesh.d[50:51]<-0
MAB.rpath.params$model[, "LG Mesh.disc" := lg_mesh.d]

### Other ---------------------------------------------------------
other.d<-left_join(groups_fleets,other.d,by="RPATH")
other.d<-as.vector(other.d$discards)
# other.d[50:51]<-0
MAB.rpath.params$model[, "Other.disc" := other.d]
MAB.rpath.params$model[Group == 'Detritus', "Other.disc" := 0]
### SM Mesh -------------------------------------------------------
sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
sm_mesh.d<-as.vector(sm_mesh.d$discards)
# sm_mesh.d[50:51]<-0
MAB.rpath.params$model[, "SM Mesh.disc" := sm_mesh.d]
MAB.rpath.params$model[Group == 'Detritus', "SM Mesh.disc" := 0]

### Scallop Dredge -------------------------------------------------
scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
scallop.d<-as.vector(scallop.d$discards)
# scallop.d[50:51]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop.d]
MAB.rpath.params$model[Group == 'Detritus', "Scallop Dredge.disc" := 0]

### Trap ----------------------------------------------------------
trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
trap.d<-as.vector(trap.d$discards)
# trap.d[50:51]<-0
MAB.rpath.params$model[, "Trap.disc" := trap.d]
MAB.rpath.params$model[Group == 'Detritus', "Trap.disc" := 0]

## HMS Fleet -------------------------------------------------------
hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
hms.d<-as.vector(hms.d$discards)
# hms.d[50:51]<-0
MAB.rpath.params$model[, "HMS Fleet.disc" := hms.d]
MAB.rpath.params$model[Group == 'Detritus', "HMS Fleet.disc" := 0]

### Pelagic -------------------------------------------------------
pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
pelagic.d<-as.vector(pelagic.d$discards)
# pelagic.d[50:51]<-0
MAB.rpath.params$model[, "Pelagic.disc" := pelagic.d]
MAB.rpath.params$model[Group == 'Detritus', "Pelagic.disc" := 0]

### Other Dredge --------------------------------------------------
other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
other_dredge.d<-as.vector(other_dredge.d$discards)
# other_dredge.d[50:51]<-0
MAB.rpath.params$model[, "Other Dredge.disc" := other_dredge.d]
MAB.rpath.params$model[Group == 'Detritus', "Other Dredge.disc" := 0]

### Clam Dredge ---------------------------------------------------
clam.d<-c(rep(0,(nliving+ndead)),rep(NA,nfleets))
MAB.rpath.params$model[, "Clam Dredge.disc" := clam.d]

### Clam Dredge ---------------------------------------------------
rec.d<-c(rep(0,(nliving+ndead)),rep(NA,nfleets))
MAB.rpath.params$model[, "Recreational.disc" := rec.d]



## Run diet matrix
source(here("MAB_diet.R"))

## Fill Rpath parameter file with diet
source(here("MAB_diet_fill.R"))

#adjust copepod groups
source(here("Sarah_R/redo_copes_start.R"))

## Run model
MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath

# Balancing changes -------------------
## Biomass changes ---------------------------------------------------------

### OtherPelagics -----------------------------------------------------------
## Increase biomass by 200x (Lucey, Link, Buccheister et al.)
#MAB.rpath.params$model$Biomass[29]<-MAB.rpath.params$model$Biomass[29]*200
#OR set EE to 0.85 like Sean
# MAB.rpath.params$model$Biomass[29]<-NA
# MAB.rpath.params$model$EE[29]<-0.85
MAB.rpath.params$model[Group == 'OtherPelagics', Biomass := NA]
MAB.rpath.params$model[Group == 'OtherPelagics', EE := 0.85]

### OtherCephalopods --------------------------------------------------------
## Increase biomass by 1000x (Prebal diagnostics)
#MAB.rpath.params$model$Biomass[27]<-MAB.rpath.params$model$Biomass[27]*1000
#OR set EE to 0.85 like Sean
# MAB.rpath.params$model$Biomass[27]<-NA
# MAB.rpath.params$model$EE[27]<-0.85
MAB.rpath.params$model[Group == 'OtherCephalopods', Biomass := NA]
MAB.rpath.params$model[Group == 'OtherCephalopods', EE := 0.85]

### SpinyDogfish ------------------------------------------------------------
## Decrease biomass by 0.7x (Lucey, Buccheister et al.)
# MAB.rpath.params$model$Biomass[43]<-MAB.rpath.params$model$Biomass[43]*0.7
MAB.rpath.params$model[Group == 'SpinyDogfish', Biomass := Biomass * 0.7]

### OtherDemersals ----------------------------------------------------------
## Increase biomass by 200x
#MAB.rpath.params$model$Biomass[28]<-MAB.rpath.params$model$Biomass[28]*200
#OR set EE to 0.9 like Sean
# MAB.rpath.params$model$Biomass[28]<-NA
# MAB.rpath.params$model$EE[28]<-0.9
MAB.rpath.params$model[Group == 'OtherDemersals', Biomass := NA]
MAB.rpath.params$model[Group == 'OtherDemersals', EE := 0.9]

### Sharks ------------------------------------------------------------------
## Increase biomass by 3.15x
# MAB.rpath.params$model$Biomass[36]<-MAB.rpath.params$model$Biomass[36]*3.15
MAB.rpath.params$model[Group == 'Sharks', Biomass := Biomass * 3.15]

### BlackSeaBass ------------------------------------------------------------
## Increase biomass by 7.5x 
# MAB.rpath.params$model$Biomass[8]<-MAB.rpath.params$model$Biomass[8]*7.5
MAB.rpath.params$model[Group == 'BlackSeaBass', Biomass := Biomass * 7.5]

### OtherShrimps ------------------------------------------------------------
## Increase biomass by 220x (Buccheister et al., EMAX; Prebal diagnostics)
#MAB.rpath.params$model$Biomass[30]<-MAB.rpath.params$model$Biomass[30]*220
#OR set EE to 0.85 like Sean
# MAB.rpath.params$model$Biomass[30]<-NA
# MAB.rpath.params$model$EE[30]<-0.85
MAB.rpath.params$model[Group == 'OtherShrimps', Biomass := NA]
MAB.rpath.params$model[Group == 'OtherShrimps', EE := 0.85]

### Bluefish ---------------------------------------------------------------
## Increase biomass by 4x
# MAB.rpath.params$model$Biomass[9]<-MAB.rpath.params$model$Biomass[9]*4
MAB.rpath.params$model[Group == 'Bluefish', Biomass := Biomass * 4]

### SmFlatfishes -----------------------------------------------------------
## Increase biomass by 25
#MAB.rpath.params$model$Biomass[39]<-MAB.rpath.params$model$Biomass[39]*25
#OR set EE to 0.85 like Sean
# MAB.rpath.params$model$Biomass[39]<-NA
# MAB.rpath.params$model$EE[39]<-0.85
MAB.rpath.params$model[Group == 'SmFlatfishes', Biomass := NA]
MAB.rpath.params$model[Group == 'SmFlatfishes', EE := 0.85]

### AtlMackerel ------------------------------------------------------------
## Increase biomass by 16x (Lucey, Buccheister et al.)
# MAB.rpath.params$model$Biomass[4]<-MAB.rpath.params$model$Biomass[4]*16
MAB.rpath.params$model[Group == 'AtlMackerel', Biomass := Biomass * 16]

### GelZooplankton ---------------------------------------------------------
## Increase biomass by 2x (Prebal diagnostics)
# MAB.rpath.params$model$Biomass[13]<-MAB.rpath.params$model$Biomass[13]*2
MAB.rpath.params$model[Group == 'GelZooplankton', Biomass := Biomass * 2]

### SummerFlounder ---------------------------------------------------------
## Increase biomass by 2.75x (In line with assessment value)
# MAB.rpath.params$model$Biomass[44]<-MAB.rpath.params$model$Biomass[44]*2.75
MAB.rpath.params$model[Group == 'SummerFlounder', Biomass := Biomass * 2.75]

### OceanPout --------------------------------------------------------------
## Increase biomass by 5x 
# MAB.rpath.params$model$Biomass[25]<-MAB.rpath.params$model$Biomass[25]*5
MAB.rpath.params$model[Group == 'OceanPout', Biomass := Biomass * 5]

### WinterSkate ------------------------------------------------------------
## Increase biomass by 3x 
# MAB.rpath.params$model$Biomass[48]<-MAB.rpath.params$model$Biomass[48]*3
MAB.rpath.params$model[Group == 'WinterSkate', Biomass := Biomass * 3]

### Windowpane -------------------------------------------------------------
## Increase biomass by 3x
# MAB.rpath.params$model$Biomass[46]<-MAB.rpath.params$model$Biomass[46]*3
MAB.rpath.params$model[Group == 'Windowpane', Biomass := Biomass * 3]

### SmPelagics -------------------------------------------------------------
## Increase biomass by 18x (Buchheister)
#MAB.rpath.params$model$Biomass[41]<-MAB.rpath.params$model$Biomass[41]*18
#OR set EE to 0.85 like Sean
# MAB.rpath.params$model$Biomass[41]<-NA
# MAB.rpath.params$model$EE[41]<-0.9
MAB.rpath.params$model[Group == 'SmPelagics', Biomass := NA]
MAB.rpath.params$model[Group == 'SmPelagics', EE := 0.9]

### LittleSkate ------------------------------------------------------------
## Increase biomass by 2x (Lucey)
# MAB.rpath.params$model$Biomass[18]<-MAB.rpath.params$model$Biomass[18]*2
MAB.rpath.params$model[Group == 'LittleSkate', Biomass := Biomass * 2]

### Scup -------------------------------------------------------------------
## Increase biomass by 5x (Lucey)
# MAB.rpath.params$model$Biomass[34]<-MAB.rpath.params$model$Biomass[34]*4
MAB.rpath.params$model[Group == 'Scup', Biomass := Biomass * 4]

### WinterFlounder ---------------------------------------------------------
## Increase biomass by 2x (Lucey)
# MAB.rpath.params$model$Biomass[47]<-MAB.rpath.params$model$Biomass[47]*2
MAB.rpath.params$model[Group == 'WinterFlounder', Biomass := Biomass * 2]

### Mesopelagics -----------------------------------------------------------
## Increase biomass by 2.5x (Prebal diagnostics)
#MAB.rpath.params$model$Biomass[22]<-MAB.rpath.params$model$Biomass[22]*2.5
#OR set EE to 0.95
# MAB.rpath.params$model$Biomass[22]<-NA
# MAB.rpath.params$model$EE[22]<-0.95
MAB.rpath.params$model[Group == 'Mesopelagics', Biomass := NA]
MAB.rpath.params$model[Group == 'Mesopelagics', EE := 0.95]

### AtlScallop -------------------------------------------------------------
## Increase biomass by 3.5x (Lucey)
# MAB.rpath.params$model$Biomass[5]<-MAB.rpath.params$model$Biomass[5]*3.5
MAB.rpath.params$model[Group == 'AtlScallop', Biomass := Biomass * 3.5]

### OtherSkates ------------------------------------------------------------
## Increase biomass by 1.7x
#MAB.rpath.params$model$Biomass[31]<-MAB.rpath.params$model$Biomass[31]*1.7
#Or set EE to 0.85 like Sean
# MAB.rpath.params$model$Biomass[31]<-NA
# MAB.rpath.params$model$EE[31]<-0.85
MAB.rpath.params$model[Group == 'OtherSkates', Biomass := NA]
MAB.rpath.params$model[Group == 'OtherSkates', EE := 0.85]

### Odontocetes ------------------------------------------------------------
## Increase biomass by 1.5x (Lucey)
# MAB.rpath.params$model$Biomass[26]<-MAB.rpath.params$model$Biomass[26]*1.5
MAB.rpath.params$model[Group == 'Odontocetes', Biomass := Biomass * 1.5]

### SilverHake -------------------------------------------------------------
## Increase biomass by 2x (Lucey)
# MAB.rpath.params$model$Biomass[37]<-MAB.rpath.params$model$Biomass[37]*2
MAB.rpath.params$model[Group == 'SilverHake', Biomass := Biomass * 2]

### Goosefish --------------------------------------------------------------
## Increase biomass by 2.5x (Okey)
# MAB.rpath.params$model$Biomass[14]<-MAB.rpath.params$model$Biomass[14]*2.5
MAB.rpath.params$model[Group == 'Goosefish', Biomass := Biomass * 2.5]

### RiverHerring -----------------------------------------------------------
## Increase biomass by 4x 
# MAB.rpath.params$model$Biomass[2]<-MAB.rpath.params$model$Biomass[2]*4
MAB.rpath.params$model[Group == 'RiverHerring', Biomass := Biomass * 4]

### Fourspot ---------------------------------------------------------------
## Increase biomass by 1.5x (Lucey)
# MAB.rpath.params$model$Biomass[12]<-MAB.rpath.params$model$Biomass[12]*1.5
MAB.rpath.params$model[Group == 'Fourspot', Biomass := Biomass * 1.5]

### Weakfish ---------------------------------------------------------------
##Increase biomass by 1.5x (Assessment)
# MAB.rpath.params$model$Biomass[45]<-MAB.rpath.params$model$Biomass[45]*1.5
MAB.rpath.params$model[Group == 'Weakfish', Biomass := Biomass * 1.5]

### Macrobenthos -----------------------------------------------------------
#Decrease Macrobenthos biomass
MAB.rpath.params$model[Group=="Macrobenthos",Biomass := 45]

### LgCopepods -------------------------------------------------------------
#Increase LgCopepods biomass
MAB.rpath.params$model[Group=="LgCopepods",Biomass := Biomass*1.3]

### Micronekton ------------------------------------------------------------
#Increase Micronekton biomass slightly
MAB.rpath.params$model[Group=="Micronekton",Biomass := Biomass*1.2]

### YTFlounder -------------------------------------------------------------
#Increase YTFlounder biomass 
MAB.rpath.params$model[Group=="YTFlounder",Biomass := Biomass*1.75]

## PB changes --------------------------------------------------------------

### Bluefish --------------------------------------------------------------
MAB.rpath.params$model[Group == "Bluefish",PB := 0.6]

### AtlMackerel -----------------------------------------------------------
## Decrease PB by 4x
MAB.rpath.params$model[Group=="AtlMackerel",PB := PB/4]

### GelZooplankton --------------------------------------------------------
## Increase PB to 35 (Lucey)
MAB.rpath.params$model[Group == "GelZooplankton",PB := 35]

### Megabenthos -----------------------------------------------------------
## Decrease PB by 2x (Lucey, Link et al.)
MAB.rpath.params$model[Group=="Megabenthos",PB := 2.5]


### SilverHake ------------------------------------------------------------
## Decrease PB to 0.6
MAB.rpath.params$model[Group=="SilverHake",PB := 0.6]

### SmoothDogfish ---------------------------------------------------------
## Decrease PB to 0.5
MAB.rpath.params$model[Group=="SmoothDogfish",PB:= 0.5]

### WinterFlounder --------------------------------------------------------
## Increase PB to 0.57
MAB.rpath.params$model[Group=="WinterFlounder",PB:= 0.57]

### WinterSkate -----------------------------------------------------------
## Decrease PB by 0.6x
MAB.rpath.params$model[Group=="WinterSkate", PB := PB*0.6]

#SW changes from here
### LgCopepods ------------------------------------------------------------
#decrease LgCopepods PB so Resp > 0
MAB.rpath.params$model[Group=="LgCopepods", PB := 61]

### Longevity estimates ---------------------------------------------------
#Changes below based on longevity estimates
MAB.rpath.params$model[Group=="Cod", PB:= 0.4]
MAB.rpath.params$model[Group=="OtherSkates", PB := 0.55]
MAB.rpath.params$model[Group=="BlackSeaBass", PB := 0.5]
MAB.rpath.params$model[Group=="OceanPout", PB := 0.57]
MAB.rpath.params$model[Group=="YTFlounder", PB := 0.9]
MAB.rpath.params$model[Group=="SummerFlounder", PB := 0.7]
MAB.rpath.params$model[Group=="OtherDemersals", PB := 0.65]
MAB.rpath.params$model[Group=="SouthernDemersals", PB := 1.14]
MAB.rpath.params$model[Group=="SmFlatfishes",PB := 1.64]
MAB.rpath.params$model[Group=="Sharks",PB := 0.16]
MAB.rpath.params$model[Group=="LittleSkate",PB := 0.5]
MAB.rpath.params$model[Group=="RedHake",PB := 0.45]
MAB.rpath.params$model[Group=="OtherPelagics",PB:=1.2]

### RiverHerring ----------------------------------------------------------
#Adjust RiverHerring based on Dias et al 2019
MAB.rpath.params$model[Group=="RiverHerring",PB := 1.3]

### Inverts ---------------------------------------------------------------
#Changes below are to make invert groups more realistic
MAB.rpath.params$model[Group=="OtherShrimps", PB := 2]
MAB.rpath.params$model[Group=="Illex", PB := 3]
MAB.rpath.params$model[Group=="Loligo", PB := 3]
MAB.rpath.params$model[Group=="OtherCephalopods", PB := 3]
MAB.rpath.params$model[Group=="Macrobenthos", PB := 2.5]
MAB.rpath.params$model[Group=="AmLobster", PB := 1.5]
MAB.rpath.params$model[Group=="OceanQuahog", PB := 0.05]

## QB changes --------------------------------------------------------------

### HMS  -------------------------------------------------------------
## Decrease QB by 3x
MAB.rpath.params$model[Group=="HMS", QB :=QB/3]

### Seabirds -----------------------------------------------------------
## Set to 76.2 based on Heymans (2001)
MAB.rpath.params$model[Group == "SeaBirds", QB:= 76.2]

### Odontocetes --------------------------------------------------------
## Decrease QB
MAB.rpath.params$model[Group=="Odontocetes",QB := QB*0.65]

### RedHake ----------------------------------------------------------
## Decrease QB by 4x (Lucey)
MAB.rpath.params$model$QB[33]<-MAB.rpath.params$model$QB[33]/4

### GE adjust -----------------------------------------------------------
#Changes below made to keep GEs between 0.3 and 0.1 for fishes
#SW
MAB.rpath.params$model[Group=="SmPelagics",QB:=5.39]
MAB.rpath.params$model[Group=="WinterFlounder",QB := 2.024]
MAB.rpath.params$model[Group=="BlackSeaBass",QB := 1.66]
MAB.rpath.params$model[Group=="Scup",QB := 1.66]
MAB.rpath.params$model[Group=="Fourspot",QB := 1.61]
MAB.rpath.params$model[Group=="AtlMenhaden",QB:=5.39]
MAB.rpath.params$model[Group=="SpinyDogfish",QB:=1.16]
MAB.rpath.params$model[Group=="SmoothDogfish",QB:=2.44]
MAB.rpath.params$model[Group=="OceanPout", QB:=2]
MAB.rpath.params$model[Group=="LittleSkate",QB:=1.4]
MAB.rpath.params$model[Group=="Bluefish",QB:=3.5]
MAB.rpath.params$model[Group=="Goosefish",QB:=1.22]
MAB.rpath.params$model[Group=="RedHake",QB:=0.94]
MAB.rpath.params$model[Group=="OtherPelagics",QB:=4]
MAB.rpath.params$model[Group=="OtherSkates",QB:=1.1]
MAB.rpath.params$model[Group=="Cod",QB:=1.2]
MAB.rpath.params$model[Group=="RiverHerring",QB:=9.4]
MAB.rpath.params$model[Group=="OceanQuahog", QB := 0.3]

### Micronekton --------------------------------------------------------
#Adjusting Micronekton QB - not sure why it was so high in EMAX
MAB.rpath.params$model[Group=="Micronekton",QB:=QB/2]

### LgCopepods ---------------------------------------------------------
#Adjusting LgCopepods QB to be a bit higher in MAB relative to other regions
MAB.rpath.params$model[Group=="LgCopepods",QB:=148]

#MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
##GE

## Fishing Changes ---------------------------------------------------------
# 
# ## Fishing changes
### Sharks ---------------------------------------------------------------
# ## Reduce trap fishing
# MAB.rpath.params$model$Trap[36]<-MAB.rpath.params$model$Trap[36]*0
MAB.rpath.params$model[Group == 'Sharks', Trap := Trap * 0]
# ## Reduce rec fishing
# MAB.rpath.params$model$Recreational[36]<-MAB.rpath.params$model$Recreational[36]*0.85
MAB.rpath.params$model[Group == 'Sharks', Recreational := Recreational * 0.85]

### OtherPelagics --------------------------------------------------------
# ## Reduce trap fishing
# MAB.rpath.params$model$Trap[29]<-MAB.rpath.params$model$Trap[29]*0
MAB.rpath.params$model[Group == 'OtherPelagics', Trap := Trap * 0]

### SouthernDemersals ----------------------------------------------------
# ## Reduce trap fishing
# MAB.rpath.params$model$Trap[42]<-MAB.rpath.params$model$Trap[42]*0
MAB.rpath.params$model[Group == 'SouthernDemersals', Trap := Trap * 0]

### OtherDemersals -------------------------------------------------------
# ## Reduce trap fishing
# MAB.rpath.params$model$Trap[28]<-MAB.rpath.params$model$Trap[28]*.01
MAB.rpath.params$model[Group == 'OtherDemersals', Trap := Trap * 0.01]

### Mesopelagics ---------------------------------------------------------
# ## Reduce trap fishing
# MAB.rpath.params$model$Trap[22]<-MAB.rpath.params$model$Trap[22]*0
MAB.rpath.params$model[Group == 'Mesopelagics', Trap := Trap * 0]

### OceanPout ------------------------------------------------------------
## Reduce trap fishing
# MAB.rpath.params$model$Trap[25]<-MAB.rpath.params$model$Trap[25]*.01
MAB.rpath.params$model[Group == 'OceanPout', Trap := Trap * 0.01]

## Diet changes ------------------------------------------------------------

## OtherCephalapods ---------------------
# Relieve predation pressure
## SummerFlounder: OtherCephalopods -10%, Illex +10%
# MAB.rpath.params$diet[27,45]<-MAB.rpath.params$diet[27,45]-0.10
MAB.rpath.params$diet[Group == 'OtherCephalopods', SummerFlounder := SummerFlounder - 0.10]
# MAB.rpath.params$diet[16,45]<-MAB.rpath.params$diet[16,45]+0.10
MAB.rpath.params$diet[Group == 'Illex', SummerFlounder := SummerFlounder + 0.10]

## SpinyDogfish: OtherCephalopods -3%, Loligo +3%
# MAB.rpath.params$diet[27,44]<-MAB.rpath.params$diet[27,44]-0.03
# MAB.rpath.params$diet[19,44]<-MAB.rpath.params$diet[19,44]+0.03
MAB.rpath.params$diet[Group == 'OtherCephalopods', SpinyDogfish := SpinyDogfish - 0.03]
MAB.rpath.params$diet[Group == 'Loligo', SpinyDogfish := SpinyDogfish + 0.03]

## SilverHake: OtherCephalopods -4%, Loligo +4%
# MAB.rpath.params$diet[27,38]<-MAB.rpath.params$diet[27,38]-0.04
# MAB.rpath.params$diet[19,38]<-MAB.rpath.params$diet[19,38]+0.04
MAB.rpath.params$diet[Group == 'OtherCephalopods', SilverHake := SilverHake - 0.04]
MAB.rpath.params$diet[Group == 'Loligo', SilverHake := SilverHake + 0.04]

## Bluefish: OtherCephalopods -5%, Loligo +5%
# MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.05
# MAB.rpath.params$diet[19,10]<-MAB.rpath.params$diet[19,10]+0.05
MAB.rpath.params$diet[Group == 'OtherCephalopods', Bluefish := Bluefish - 0.05]
MAB.rpath.params$diet[Group == 'Loligo', Bluefish := Bluefish + 0.05]

## Butterfish: OtherCephalopods -.5%, Loligo +.5%
# MAB.rpath.params$diet[27,11]<-MAB.rpath.params$diet[27,11]-0.005
# MAB.rpath.params$diet[19,11]<-MAB.rpath.params$diet[19,11]+0.005
MAB.rpath.params$diet[Group == 'OtherCephalopods', Butterfish := Butterfish - 0.005]
MAB.rpath.params$diet[Group == 'Loligo', Butterfish := Butterfish + 0.005]

## Fourspot: OtherCephalopods -18%, Loligo +18%
# MAB.rpath.params$diet[27,13]<-MAB.rpath.params$diet[27,13]-0.18
# MAB.rpath.params$diet[19,13]<-MAB.rpath.params$diet[19,13]+0.18
MAB.rpath.params$diet[Group == 'OtherCephalopods', Fourspot := Fourspot - 0.18]
MAB.rpath.params$diet[Group == 'Loligo', Fourspot := Fourspot + 0.18]

## RedHake: OtherCephalopods -4%, Loligo +4%
# MAB.rpath.params$diet[27,34]<-MAB.rpath.params$diet[27,34]-0.04
# MAB.rpath.params$diet[19,34]<-MAB.rpath.params$diet[19,34]+0.04
MAB.rpath.params$diet[Group == 'OtherCephalopods', RedHake := RedHake - 0.04]
MAB.rpath.params$diet[Group == 'Loligo', RedHake := RedHake + 0.04]

## Goosefish: OtherCephalopods -4%, Loligo +4%
MAB.rpath.params$diet[Group=="OtherCephalopods",Goosefish := Goosefish - 0.04]
MAB.rpath.params$diet[Group=="Loligo",Goosefish := Goosefish + 0.04]

## Scup: OtherCephalopods -0.5%, Loligo +0.5%
MAB.rpath.params$diet[Group=="OtherCephalopods",Scup := Scup - 0.005]
MAB.rpath.params$diet[Group=="Loligo",Scup := Scup + 0.005]

### OtherPelagics ---------------------
## Relieve predation pressure on OtherPelagics
## SpinyDogfish: OtherPelagics -5%, Illex +5%
# MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.05
# MAB.rpath.params$diet[16,44]<-MAB.rpath.params$diet[16,44]+0.05
MAB.rpath.params$diet[Group == 'OtherPelagics', SpinyDogfish := SpinyDogfish - 0.05]
MAB.rpath.params$diet[Group == 'Illex', SpinyDogfish := SpinyDogfish + 0.05]

## SpinyDogfish: OtherPelagics -5%, Macrobenthos +5%
# MAB.rpath.params$diet[29,44]<-MAB.rpath.params$diet[29,44]-0.05
# MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.05
MAB.rpath.params$diet[Group == 'OtherPelagics', SpinyDogfish := SpinyDogfish - 0.05]
MAB.rpath.params$diet[Group == 'Macrobenthos', SpinyDogfish := SpinyDogfish + 0.05]

### AtlMackerel ---------------------
## Relieve predation pressure on AtlMackerel
## SpinyDogfish: AtlMackerel -7%, Macrobenthos +7% 
# MAB.rpath.params$diet[4,44]<-MAB.rpath.params$diet[4,44]-0.07
# MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.07
MAB.rpath.params$diet[Group == 'AtlMackerel', SpinyDogfish := SpinyDogfish - 0.07]
MAB.rpath.params$diet[Group == 'Macrobenthos', SpinyDogfish := SpinyDogfish + 0.07]

## Goosefish: AtlMackerel -4%, Macrobenthos +4% 
# MAB.rpath.params$diet[4,15]<-MAB.rpath.params$diet[4,15]-0.04
# MAB.rpath.params$diet[20,15]<-MAB.rpath.params$diet[20,15]+0.04
MAB.rpath.params$diet[Group == 'AtlMackerel', Goosefish := Goosefish - 0.04]
MAB.rpath.params$diet[Group == 'Macrobenthos', Goosefish := Goosefish + 0.04]

## SilverHake: AtlMackerel -4%, Macrobenthos +4% 
# MAB.rpath.params$diet[4,38]<-MAB.rpath.params$diet[4,38]-0.04
# MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.04
MAB.rpath.params$diet[Group == 'AtlMackerel', SilverHake := SilverHake - 0.04]
MAB.rpath.params$diet[Group == 'Macrobenthos', SilverHake := SilverHake + 0.04]

## SummerFlounder: AtlMackerel -4%, Macrobenthos +4% 
# MAB.rpath.params$diet[4,45]<-MAB.rpath.params$diet[4,45]-0.04
# MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.04
MAB.rpath.params$diet[Group == 'AtlMackerel', SummerFlounder := SummerFlounder - 0.04]
MAB.rpath.params$diet[Group == 'Macrobenthos', SummerFlounder := SummerFlounder + 0.04]

## OtherPelagics: AtlMackerel -3%, SmPelagics +3% 
MAB.rpath.params$diet[Group=="AtlMackerel", OtherPelagics := OtherPelagics - 0.03]
MAB.rpath.params$diet[Group=="SmPelagics", OtherPelagics := OtherPelagics + 0.03]

## Bluefish: AtlMackerel -0.05%, SmPelagics +0.05% 
MAB.rpath.params$diet[Group=="AtlMackerel", Bluefish := Bluefish - 0.005]
MAB.rpath.params$diet[Group=="SmPelagics", Bluefish := Bluefish + 0.005]

### SmFlatfishes ---------------------
## Relieve predation pressure on SmFlatfishes
## SpinyDogfish: SmFlatfishes -2.2%, Macrobenthos +2.2%
# SW changed, previously had been leading to negative diet 
# MAB.rpath.params$diet[39,44]<-MAB.rpath.params$diet[39,44]-0.023
# MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.023
MAB.rpath.params$diet[Group == 'SmFlatfishes', SpinyDogfish := SpinyDogfish - 0.023]
MAB.rpath.params$diet[Group == 'Macrobenthos', SpinyDogfish := SpinyDogfish + 0.023]

## Goosefish: SmFlatfishes -1.2%, Macrobenthos +1.2%
# MAB.rpath.params$diet[39,15]<-MAB.rpath.params$diet[39,15]-0.012-0.001
# MAB.rpath.params$diet[20,15]<-MAB.rpath.params$diet[20,15]+0.012+0.001
MAB.rpath.params$diet[Group == 'SmFlatfishes', Goosefish := Goosefish - 0.013]
MAB.rpath.params$diet[Group == 'Macrobenthos', Goosefish := Goosefish + 0.013]

## OtherSkates: SmFlatfishes -2.5%, Macrobenthos +2.5%
# MAB.rpath.params$diet[39,32]<-MAB.rpath.params$diet[39,32]-0.025
# MAB.rpath.params$diet[20,32]<-MAB.rpath.params$diet[20,32]+0.025
MAB.rpath.params$diet[Group == 'SmFlatfishes', OtherSkates := OtherSkates - 0.025]
MAB.rpath.params$diet[Group == 'Macrobenthos', OtherSkates := OtherSkates + 0.025]

## Cod: SmFlatfishes -.5%, Macrobenthos +.5%
# MAB.rpath.params$diet[39,12]<-MAB.rpath.params$diet[39,12]-0.005
# MAB.rpath.params$diet[20,12]<-MAB.rpath.params$diet[20,12]+0.005
MAB.rpath.params$diet[Group == 'SmFlatfishes', Cod := Cod - 0.005]
MAB.rpath.params$diet[Group == 'Macrobenthos', Cod := Cod + 0.005]

## LittleSkate: SmFlatfishes -.5%, Macrobenthos +.5%
# MAB.rpath.params$diet[39,19]<-MAB.rpath.params$diet[39,19]-0.005
# MAB.rpath.params$diet[20,19]<-MAB.rpath.params$diet[20,19]+0.005
MAB.rpath.params$diet[Group == 'SmFlatfishes', LittleSkate := LittleSkate - 0.005]
MAB.rpath.params$diet[Group == 'Macrobenthos', LittleSkate := LittleSkate + 0.005]

## RedHake: SmFlatfishes -2.5%, Macrobenthos +2.5%
# MAB.rpath.params$diet[39,34]<-MAB.rpath.params$diet[39,34]-0.025
# MAB.rpath.params$diet[20,34]<-MAB.rpath.params$diet[20,34]+0.025
MAB.rpath.params$diet[Group == 'SmFlatfishes', RedHake := RedHake - 0.025]
MAB.rpath.params$diet[Group == 'Macrobenthos', RedHake := RedHake + 0.025]

## SilverHake: SmFlatfishes -.3%, Macrobenthos +.3%
# MAB.rpath.params$diet[39,38]<-MAB.rpath.params$diet[39,38]-0.003
# MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.003
MAB.rpath.params$diet[Group == 'SmFlatfishes', SilverHake := SilverHake - 0.003]
MAB.rpath.params$diet[Group == 'Macrobenthos', SilverHake := SilverHake + 0.003]

## SummerFlounder: SmFlatfishes -.6%, Macrobenthos +.6%
# MAB.rpath.params$diet[39,45]<-MAB.rpath.params$diet[39,45]-0.006
# MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.006
MAB.rpath.params$diet[Group == 'SmFlatfishes', SummerFlounder := SummerFlounder - 0.006]
MAB.rpath.params$diet[Group == 'Macrobenthos', SummerFlounder := SummerFlounder + 0.006]

## Windowpane: SmFlatfishes -2.4%, Macrobenthos +2.4%
# MAB.rpath.params$diet[39,47]<-MAB.rpath.params$diet[39,47]-0.024
# MAB.rpath.params$diet[20,47]<-MAB.rpath.params$diet[20,47]+0.024
MAB.rpath.params$diet[Group == 'SmFlatfishes', Windowpane := Windowpane - 0.024]
MAB.rpath.params$diet[Group == 'Macrobenthos', Windowpane := Windowpane + 0.024]

## WinterSkate: SmFlatfishes -2.3%, Macrobenthos +2.3%
# MAB.rpath.params$diet[39,49]<-MAB.rpath.params$diet[39,49]-0.023
# MAB.rpath.params$diet[20,49]<-MAB.rpath.params$diet[20,49]+0.023
MAB.rpath.params$diet[Group == 'SmFlatfishes', WinterSkate := WinterSkate - 0.023]
MAB.rpath.params$diet[Group == 'Macrobenthos', WinterSkate := WinterSkate + 0.023]

### OtherDemersals ---------------------
## Relieve predation pressure on OtherDemersals
## SummerFlounder: OtherDemersals -5%, Macrobenthos +5%
# MAB.rpath.params$diet[28,45]<-MAB.rpath.params$diet[28,45]-0.05
# MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.05
MAB.rpath.params$diet[Group == 'OtherDemersals', SummerFlounder := SummerFlounder - 0.05]
MAB.rpath.params$diet[Group == 'Macrobenthos', SummerFlounder := SummerFlounder + 0.05]

## Goosefish: OtherDemersals -5%, Goosefish +5%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",Goosefish := Goosefish - 0.05]
MAB.rpath.params$diet[Group=="Goosefish",Goosefish := Goosefish + 0.05]

## Goosefish: OtherDemersals -2.5%, SmPelagics +2.5%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",Goosefish := Goosefish - 0.025]
MAB.rpath.params$diet[Group=="SmPelagics",Goosefish := Goosefish + 0.025]

## SpinyDogfish: OtherDemersals -3%, Macrobenthos +3%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",SpinyDogfish := SpinyDogfish - 0.03]
MAB.rpath.params$diet[Group=="Macrobenthos",SpinyDogfish := SpinyDogfish + 0.03]

## Bluefish: OtherDemersals -1.5%, AtlMenhaden +1.5%
#SW added
MAB.rpath.params$diet[Group=="OtherDemersals",Bluefish := Bluefish - 0.015]
MAB.rpath.params$diet[Group=="AtlMenhaden",Bluefish := Bluefish + 0.015]

### SilverHake ---------------------
## Relieve predation pressure on SilverHake
## SilverHake: SilverHake -10%, Macrobenthos +10%
# MAB.rpath.params$diet[37,38]<-MAB.rpath.params$diet[37,38]-0.1
# MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.1
MAB.rpath.params$diet[Group == 'SilverHake', SilverHake := SilverHake - 0.1]
MAB.rpath.params$diet[Group == 'Macrobenthos', SilverHake := SilverHake + 0.1]

## Goosefish: SilverHake -7%, Macrobenthos +7%
# MAB.rpath.params$diet[37,15]<-MAB.rpath.params$diet[37,15]-0.07
# MAB.rpath.params$diet[20,15]<-MAB.rpath.params$diet[20,15]+0.07
MAB.rpath.params$diet[Group == 'SilverHake', Goosefish := Goosefish - 0.07]
MAB.rpath.params$diet[Group == 'Macrobenthos', Goosefish := Goosefish + 0.07]

## SpinyDogfish: SilverHake -1.5%, Macrobenthos +1.5%
# MAB.rpath.params$diet[37,44]<-MAB.rpath.params$diet[37,44]-0.015
# MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.015
MAB.rpath.params$diet[Group == 'SilverHake', SpinyDogfish := SpinyDogfish - 0.015]
MAB.rpath.params$diet[Group == 'Macrobenthos', SpinyDogfish := SpinyDogfish + 0.015]


### RedHake ---------------------
## Relieve predation pressure on RedHake

# ## OtherDemersals: RedHake -.5%, Macrobenthos +.5%
# MAB.rpath.params$diet[33,29]<-MAB.rpath.params$diet[33,29]-0.005
# MAB.rpath.params$diet[20,29]<-MAB.rpath.params$diet[20,29]+0.005
MAB.rpath.params$diet[Group == 'RedHake', OtherDemersals := OtherDemersals - 0.01]
MAB.rpath.params$diet[Group == 'Macrobenthos', OtherDemersals := OtherDemersals + 0.01]

### SmPelagics ---------------------
## Relieve predation pressure on SmPelagics
## SpinyDogfish: SmPelagics -10%, Macrobenthos +10%
# MAB.rpath.params$diet[41,44]<-MAB.rpath.params$diet[41,44]-0.1
# MAB.rpath.params$diet[20,44]<-MAB.rpath.params$diet[20,44]+0.1
MAB.rpath.params$diet[Group == 'SmPelagics', SpinyDogfish := SpinyDogfish - 0.1]
MAB.rpath.params$diet[Group == 'Macrobenthos', SpinyDogfish := SpinyDogfish + 0.1]

## Bluefish: SmPelagics -10%, Macrobenthos +10%
# MAB.rpath.params$diet[41,10]<-MAB.rpath.params$diet[41,10]-0.1
# MAB.rpath.params$diet[20,10]<-MAB.rpath.params$diet[20,10]+0.1
MAB.rpath.params$diet[Group == 'SmPelagics', Bluefish := Bluefish - 0.1]
MAB.rpath.params$diet[Group == 'Macrobenthos', Bluefish := Bluefish + 0.1]

## SilverHake: SmPelagics -10%, Macrobenthos +10%
# MAB.rpath.params$diet[41,38]<-MAB.rpath.params$diet[41,38]-0.1
# MAB.rpath.params$diet[20,38]<-MAB.rpath.params$diet[20,38]+0.1
MAB.rpath.params$diet[Group == 'SmPelagics', SilverHake := SilverHake - 0.1]
MAB.rpath.params$diet[Group == 'Macrobenthos', SilverHake := SilverHake + 0.1]

## Weakfish: SmPelagics -20%, Macrobenthos +20%
# MAB.rpath.params$diet[41,46]<-MAB.rpath.params$diet[41,46]-0.2
# MAB.rpath.params$diet[20,46]<-MAB.rpath.params$diet[20,46]+0.2
MAB.rpath.params$diet[Group == 'SmPelagics', Weakfish := Weakfish - 0.2]
MAB.rpath.params$diet[Group == 'Macrobenthos', Weakfish := Weakfish + 0.2]

## SummerFlounder: SmPelagics -10%, Macrobenthos +10%
# MAB.rpath.params$diet[41,45]<-MAB.rpath.params$diet[41,45]-0.1
# MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.1
MAB.rpath.params$diet[Group == 'SmPelagics', SummerFlounder := SummerFlounder - 0.1]
MAB.rpath.params$diet[Group == 'Macrobenthos', SummerFlounder := SummerFlounder + 0.1]

## AtlCroaker: SmPelagics -10%, Macrobenthos +10%
# MAB.rpath.params$diet[41,4]<-MAB.rpath.params$diet[41,4]-0.1
# MAB.rpath.params$diet[20,4]<-MAB.rpath.params$diet[20,4]+0.1
MAB.rpath.params$diet[Group == 'SmPelagics', AtlCroaker := AtlCroaker - 0.1]
MAB.rpath.params$diet[Group == 'Macrobenthos', AtlCroaker := AtlCroaker + 0.1]

## Cod: SmPelagics -10%, Macrobenthos +10%
# MAB.rpath.params$diet[41,12]<-MAB.rpath.params$diet[41,12]-0.1
# MAB.rpath.params$diet[20,12]<-MAB.rpath.params$diet[20,12]+0.1
MAB.rpath.params$diet[Group == 'SmPelagics', Cod := Cod - 0.1]
MAB.rpath.params$diet[Group == 'Macrobenthos', Cod := Cod + 0.1]

### OtherShrimps ---------------------
## Relieve predation pressure on OtherShrimps
## Loligo: OtherShrimps -2%, Macrobenthos +2%
# MAB.rpath.params$diet[30,20]<-MAB.rpath.params$diet[30,20]-0.02
# MAB.rpath.params$diet[20,20]<-MAB.rpath.params$diet[20,20]+0.02
MAB.rpath.params$diet[Group == 'OtherShrimps', Loligo := Loligo - 0.02]
MAB.rpath.params$diet[Group == 'Macrobenthos', Loligo := Loligo + 0.02]

### Windowpane
## Relieve predation pressure on Windowpane
## Bluefish: Windowpane -1%, Macrobenthos +1%
# MAB.rpath.params$diet[46,10]<-MAB.rpath.params$diet[46,10]-0.01
# MAB.rpath.params$diet[20,10]<-MAB.rpath.params$diet[20,10]+0.01
MAB.rpath.params$diet[Group == 'Windowpane', Bluefish := Bluefish - 0.01]
MAB.rpath.params$diet[Group == 'Macrobenthos', Bluefish := Bluefish + 0.01]

## Loligo: Butterfish -1.5%, Macrobenthos +1.5%
# MAB.rpath.params$diet[10,20]<-MAB.rpath.params$diet[10,20]-0.015
# MAB.rpath.params$diet[20,20]<-MAB.rpath.params$diet[20,20]+0.015
MAB.rpath.params$diet[Group == 'Butterfish', Loligo := Loligo - 0.015]
MAB.rpath.params$diet[Group == 'Macrobenthos', Loligo := Loligo + 0.015]

### Weakfish ---------------------
## Relieve predation pressure on Weakfish
## SW added
## SouthernDemersals: Weakfish -4%, Macrobenthos +4%
# MAB.rpath.params$diet[45,43]<-MAB.rpath.params$diet[45,43]-0.04
# MAB.rpath.params$diet[20,43]<-MAB.rpath.params$diet[20,43]+0.04
MAB.rpath.params$diet[Group == 'Weakfish', SouthernDemersals := SouthernDemersals - 0.04]
MAB.rpath.params$diet[Group == 'Macrobenthos', SouthernDemersals := SouthernDemersals + 0.04]

### OtherSkates ---------------------
#Relieve predation on OtherSkates
#Goosefish: OtherSkates -3%, LittleSkate +3%
MAB.rpath.params$diet[Group=="OtherSkates",Goosefish := Goosefish - 0.03]
MAB.rpath.params$diet[Group=="LittleSkate",Goosefish := Goosefish + 0.03]

#Macrobenthos: OtherSkates -0.004%, Megabenthos +0.004%
MAB.rpath.params$diet[Group=="OtherSkates",Macrobenthos := Macrobenthos-0.00004]
MAB.rpath.params$diet[Group=="Megabenthos",Macrobenthos := Macrobenthos+0.00004]

#Relieve predation on OceanPout
#Bluefish: OceanPout -2.5%, AtlMenhaden +2.5%
MAB.rpath.params$diet[Group=="OceanPout",Bluefish := Bluefish - 0.025]
MAB.rpath.params$diet[Group=="AtlMenhaden",Bluefish := Bluefish + 0.025]

#SpinyDogfish: OceanPout -0.4%, SmPelagics +0.4%
MAB.rpath.params$diet[Group=="OceanPout",SpinyDogfish := SpinyDogfish - 0.004]
MAB.rpath.params$diet[Group=="SmPelagics",SpinyDogfish := SpinyDogfish + 0.004]

## Relieve predation pressure on Mesopelagics
# SW added
#Illex: Mesopelagics -0.9%, Butterfish +0.9%
MAB.rpath.params$diet[Group=="Mesopelagics", Illex := Illex - 0.009]
MAB.rpath.params$diet[Group=="Butterfish",Illex := Illex + 0.009]

#Loligo: Mesopelagics -0.9%, Butterfish +0.9%
MAB.rpath.params$diet[Group=="Mesopelagics", Loligo := Loligo - 0.009]
MAB.rpath.params$diet[Group=="Butterfish",Loligo := Loligo + 0.009]

#OtherCephalopods: Mesopelagics -0.9%, Butterfish +0.9%
MAB.rpath.params$diet[Group=="Mesopelagics", OtherCephalopods := OtherCephalopods - 0.009]
MAB.rpath.params$diet[Group=="Butterfish",OtherCephalopods := OtherCephalopods + 0.009]

# Check for balance -------------------------------------------------------
#add data pedigree
source("Sarah_R/data_pedigree.R")

#Load Sean's prebal functions
source(url("https://github.com/NOAA-EDAB/GBRpath/blob/master/R/PreBal.R?raw=true"))

check.rpath.params(MAB.rpath.params)

MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
check.ee(MAB.rpath)

#webplot(MAB.rpath,labels = T)
#Save files
save(MAB.rpath, file = "outputs/MAB_Rpath_no_disc.RData")
save(MAB.rpath.params,file = "outputs/MAB_params_Rpath_no_disc.RData")
