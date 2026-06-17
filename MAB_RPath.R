
## Script name: MAB_RPath.R
##
## Purpose of script: Compile all data to create functional RPath model.
##                    
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg, updated by M.T. Grezlik
##
##
## Email: sarah.j.weisberg@stonybrook.edu
#

# Fri Dec  8 16:54:53 2023 ------------------------------

## Load libraries, packages and functions
library(Rpath); library(data.table); library(dplyr); library(here)

# Model Setup -------------------------------------------------------------

## Add functional groups to model and generate rpath params ------------
source(here("MAB_fleets.R"))
groups<-as.vector(groups_fleets$RPATH)
types<-c(rep(0,32),1,rep(0,20),rep(2,2),rep(3,12))
MAB.rpath.params<-create.rpath.params(group = groups, type = types)

## Add biomass estimates -------------------------------------------
source(here("MAB_biomass_estimates.R"))
biomass<-left_join(groups_fleets,MAB.biomass.80s,by = "RPATH")
biomass<-as.vector(biomass$Biomass)
MAB.rpath.params$model[,Biomass:=biomass]

## Add PB parameters ----------------------------------------------
source(here("MAB_params.R"))
pb<-params |> select(RPATH,PB)
pb<-left_join(groups_fleets,pb,by = "RPATH")
pb<-as.vector(pb$PB)
MAB.rpath.params$model[,PB:=pb]

## Add QB parameters ----------------------------------------------
qb<-params |> select(RPATH,QB)
qb<-left_join(groups_fleets,qb,by = "RPATH")
qb<-as.vector(qb$QB)
MAB.rpath.params$model[,QB:=qb]

## Add biomass accumulation ----------------------------------------
ba<-params |> select(RPATH,BA)
ba<-left_join(groups_fleets,ba,by = "RPATH")
ba<-as.vector(ba$BA)
MAB.rpath.params$model[,BioAcc:=ba]

## Add unassimilated consumption -----------------------------------
MAB.rpath.params$model[Type == 0, Unassim := 0.2]
MAB.rpath.params$model[Type > 0 & Type < 3, Unassim := 0]

# Using GB method for setting values as I didn't know why these were
# set this way - Max
# MAB.rpath.params$model[, Unassim := c(rep(0.2,5),0.4,rep(0.2,6),0.4,rep(0.2,3),0.4,rep(0.2,6),0.4,rep(0.2,7),0,rep(0.2,5),0.4,rep(0.2,11),rep(0,2),rep(NA,11))]

## Add detrital fate and set discards to 0 --------------------------
MAB.rpath.params$model[,Detritus:=c(rep(1,53),rep(0,14))]
MAB.rpath.params$model[,Discards:=c(rep(0,55),rep(1,12))]

## Add landings by gear type ---------------------------------------
source("MAB_discards.R")

### Fixed Gear -----------------------------------------------------
fixed<-left_join(groups_fleets,fixed,by="RPATH")
fixed<-as.vector(fixed$landings)
# fixed[50:51]<-0
MAB.rpath.params$model[,"Fixed Gear":=fixed]

### Large Mesh -----------------------------------------------------
lg_mesh<-left_join(groups_fleets,lg_mesh,by="RPATH")
lg_mesh<-as.vector(lg_mesh$landings)
# lg_mesh[50:51]<-0
MAB.rpath.params$model[, "LG Mesh" := lg_mesh]

### Other ----------------------------------------------------------
other<-left_join(groups_fleets,other,by="RPATH")
other<-as.vector(other$landings)
# other[50:51]<-0
MAB.rpath.params$model[, "Other" := other]

### Small Mesh -----------------------------------------------------
sm_mesh<-left_join(groups_fleets,sm_mesh,by="RPATH")
sm_mesh<-as.vector(sm_mesh$landings)
# sm_mesh[50:51]<-0
MAB.rpath.params$model[, "SM Mesh" := sm_mesh]

### Scallop Dredge -------------------------------------------------
scallop<-left_join(groups_fleets,scallop,by="RPATH")
scallop<-as.vector(scallop$landings)
# scallop[50:51]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop]

### Trap ----------------------------------------------------------
trap<-left_join(groups_fleets,trap,by="RPATH")
trap<-as.vector(trap$landings)
# trap[50:51]<-0
MAB.rpath.params$model[, "Trap" := trap]

### HMS Fleet -----------------------------------------------------
hms<-left_join(groups_fleets,hms,by="RPATH")
hms<-as.vector(hms$landings)
# hms[50:51]<-0
MAB.rpath.params$model[, "HMS Fleet" := hms]

### Pelagic -------------------------------------------------------
pelagic<-left_join(groups_fleets,pelagic,by="RPATH")
pelagic<-as.vector(pelagic$landings)
# pelagic[50:51]<-0
MAB.rpath.params$model[, "Pelagic" := pelagic]

### Other Dredge --------------------------------------------------
other_dredge<-left_join(groups_fleets,other_dredge,by="RPATH")
other_dredge<-as.vector(other_dredge$landings)
# other_dredge[50:51]<-0
MAB.rpath.params$model[, "Other Dredge" := other_dredge]

### Clam Dredge ---------------------------------------------------
clam<-left_join(groups_fleets,clam,by="RPATH")
clam<-as.vector(clam$landings)
# clam[50:51]<-0
MAB.rpath.params$model[, "Clam Dredge" := clam]

### Recreational --------------------------------------------------
source("MAB_rec_catch.R")
rec_catch<-left_join(groups_fleets,MAB.mrip_summary,by="RPATH")
rec_catch<-as.vector(rec_catch$Per_Area)
rec_catch[is.na(rec_catch)]<-0
# rec_catch[50:51]<-0
MAB.rpath.params$model[,"Recreational":=rec_catch]

## Add discards by gear type ---------------------------------------
### Fixed Gear -----------------------------------------------------
fixed.d<-left_join(groups_fleets,fixed.d,by="RPATH")
fixed.d<-as.vector(fixed.d$discards)
# fixed.d[50:51]<-0
MAB.rpath.params$model[, "Fixed Gear.disc" := fixed.d]

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

### SM Mesh -------------------------------------------------------
sm_mesh.d<-left_join(groups_fleets,sm_mesh.d,by="RPATH")
sm_mesh.d<-as.vector(sm_mesh.d$discards)
# sm_mesh.d[50:51]<-0
MAB.rpath.params$model[, "SM Mesh.disc" := sm_mesh.d]

### Scallop Dredge -------------------------------------------------
scallop.d<-left_join(groups_fleets,scallop.d,by="RPATH")
scallop.d<-as.vector(scallop.d$discards)
# scallop.d[50:51]<-0
MAB.rpath.params$model[, "Scallop Dredge" := scallop.d]

### Trap ----------------------------------------------------------
trap.d<-left_join(groups_fleets,trap.d,by="RPATH")
trap.d<-as.vector(trap.d$discards)
# trap.d[50:51]<-0
MAB.rpath.params$model[, "Trap.disc" := trap.d]

## HMS Fleet -------------------------------------------------------
hms.d<-left_join(groups_fleets,hms.d,by="RPATH")
hms.d<-as.vector(hms.d$discards)
# hms.d[50:51]<-0
MAB.rpath.params$model[, "HMS Fleet.disc" := hms.d]

### Pelagic -------------------------------------------------------
pelagic.d<-left_join(groups_fleets,pelagic.d,by="RPATH")
pelagic.d<-as.vector(pelagic.d$discards)
# pelagic.d[50:51]<-0
MAB.rpath.params$model[, "Pelagic.disc" := pelagic.d]

### Other Dredge --------------------------------------------------
other_dredge.d<-left_join(groups_fleets,other_dredge.d,by="RPATH")
other_dredge.d<-as.vector(other_dredge.d$discards)
# other_dredge.d[50:51]<-0
MAB.rpath.params$model[, "Other Dredge.disc" := other_dredge.d]

### Clam Dredge ---------------------------------------------------
clam.d<-c(rep(0,53),rep(NA,14))
MAB.rpath.params$model[, "Clam Dredge.disc" := clam.d]

## Add diet matrix ---------------------------------------------------
### Run diet script ---------------------------------------------------
source(here("MAB_diet.R"))

### Fill Rpath parameter file with diet matrix ------------------------
source(here("MAB_diet_fill.R"))


# Sarah_changes -----------------------------------------------------------

source(here("Sarah_R/rebalance.R"))


## Run model
MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath

## Create webplot
webplot(MAB.rpath, labels = T)

## Save .csv file with model information
# write.Rpath(MAB.rpath, file="MAB_model.csv")
# write.csv(MAB.rpath.params$model, file="MAB_prebalance_model.csv")
# write.csv(MAB.rpath.params$diet, file = "MAB_prebalance_diet.csv")

# ## Balance adjustments
# MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
# MAB.rpath
# source(here("MAB_prebal.R"))
# EE<-MAB.rpath$EE
# EE[order(EE)]
# write.Rpath(MAB.rpath,morts=T,file="MAB.rpath_morts.csv")
# #write.Rpath(MAB.rpath, file="MAB_model.csv")

# Balancing changes -------------------
# M.T.G. edited 07/12/2024 to move from referencing group number to group name
# leaving original script commented out in case we need to revert

## OtherPelagics ---------------------------
## Increase biomass by 200x (Lucey, Link, Buccheister et al.)
# MAB.rpath.params$model$Biomass[29]<-MAB.rpath.params$model$Biomass[29]*200
MAB.rpath.params$model[Group == 'OtherPelagics', Biomass := Biomass * 200]

## OtherCephalopods ------------------------
## Increase biomass by 600x (Prebal diagnostics)
# MAB.rpath.params$model$Biomass[27]<-MAB.rpath.params$model$Biomass[27]*600
MAB.rpath.params$model[Group == 'OtherCephalopods', Biomass := Biomass * 600]

## SpinyDogfish -----------------------------
## Decrease biomass by 0.8x (Lucey, Buccheister et al.)
# MAB.rpath.params$model$Biomass[43]<-MAB.rpath.params$model$Biomass[43]*.8
MAB.rpath.params$model[Group == 'SpinyDogfish', Biomass := Biomass * 0.8]

## OtherDemersals ---------------------------
## Increase biomass by 25x
# MAB.rpath.params$model$Biomass[28]<-MAB.rpath.params$model$Biomass[28]*200
MAB.rpath.params$model[Group == 'OtherDemersals', Biomass := Biomass * 200]

## Sharks ---------------------------------
## Increase biomass by 1.5x
# MAB.rpath.params$model$Biomass[36]<-MAB.rpath.params$model$Biomass[36]*1.5
MAB.rpath.params$model[Group == 'Sharks', Biomass := Biomass * 1.5]

## BlackSeaBass ----------------------------
## Increase biomass by 11x (Lucey)
# MAB.rpath.params$model$Biomass[8]<-MAB.rpath.params$model$Biomass[8]*11
MAB.rpath.params$model[Group == 'BlackSeaBass', Biomass := Biomass * 11]

## SouthernDemersals -----------------------
## Increase biomass by 10x (Prebal diagnostics)
# MAB.rpath.params$model$Biomass[42]<-MAB.rpath.params$model$Biomass[42]*10
MAB.rpath.params$model[Group == 'SouthernDemersals', Biomass := Biomass * 10]

## Cod ------------------------------------
## Increase biomass by 5x (Lucey, Okey, Buccheister et al.)
# MAB.rpath.params$model$Biomass[11]<-MAB.rpath.params$model$Biomass[11]*10
MAB.rpath.params$model[Group == 'Cod', Biomass := Biomass * 5]

## OtherShrimps ----------------------------
## Increase biomass by 100x (Buccheister et al., EMAX; Prebal diagnostics)
# MAB.rpath.params$model$Biomass[30]<-MAB.rpath.params$model$Biomass[30]*100
MAB.rpath.params$model[Group == 'OtherShrimps', Biomass := Biomass * 100]

## Megabenthos ----------------------------
## Increase biomass by 50x (Prebal diagnostics)
# MAB.rpath.params$model$Biomass[21]<-MAB.rpath.params$model$Biomass[21]*50
MAB.rpath.params$model[Group == 'Megabenthos', Biomass := Biomass * 50]

## Bluefish -------------------------------
## Increase biomass by 2x (Buccheister et al.)
# MAB.rpath.params$model$Biomass[9]<-MAB.rpath.params$model$Biomass[9]*2
MAB.rpath.params$model[Group == 'Bluefish', Biomass := Biomass * 2]

## SmFlatfishes ---------------------------
## Increase biomass by 14.5x (Lucey)
## SW altered (increased to 14.5 from 14)
# MAB.rpath.params$model$Biomass[39]<-MAB.rpath.params$model$Biomass[39]*14.5
MAB.rpath.params$model[Group == 'SmFlatfishes', Biomass := Biomass * 14.5]

## AtlMackerel ---------------------------
## Increase biomass by 16x (Lucey, Buccheister et al.)
# MAB.rpath.params$model$Biomass[4]<-MAB.rpath.params$model$Biomass[4]*16
MAB.rpath.params$model[Group == 'AtlMackerel', Biomass := Biomass * 16]

## GelZooplankton ------------------------
## Increase biomass by 2x (Prebal diagnostics)
# MAB.rpath.params$model$Biomass[13]<-MAB.rpath.params$model$Biomass[13]*2
MAB.rpath.params$model[Group == 'GelZooplankton', Biomass := Biomass * 2]

## SummerFlounder ------------------------
# This was commented out for balanced model before addition of 
# SurfClam and OceanQuahog. Leaving commented out for now -M.T.G.
## Increase biomass by 10x (Lucey, Buccheister et al.)
#MAB.rpath.params$model$Biomass[44]<-MAB.rpath.params$model$Biomass[44]*10

## HMS -----------------------------------
## Decrease biomass by 0.75x
# MAB.rpath.params$model$Biomass[15]<-MAB.rpath.params$model$Biomass[15]*.75
MAB.rpath.params$model[Group == 'HMS', Biomass := Biomass * 0.75]

## OceanPout -----------------------------
## Increase biomass by 10x (Lucey)
# MAB.rpath.params$model$Biomass[25]<-MAB.rpath.params$model$Biomass[25]*10
MAB.rpath.params$model[Group == 'OceanPout', Biomass := Biomass * 10]

## WinterSkate ---------------------------
## Increase biomass by 5x (Lucey)
# MAB.rpath.params$model$Biomass[48]<-MAB.rpath.params$model$Biomass[48]*5
MAB.rpath.params$model[Group == 'WinterSkate', Biomass := Biomass * 5]

## Windowpane ----------------------------
## Increase biomass by 8x (Lucey)
# MAB.rpath.params$model$Biomass[46]<-MAB.rpath.params$model$Biomass[46]*8
MAB.rpath.params$model[Group == 'Windowpane', Biomass := Biomass * 8]

## SmPelagics ----------------------------
## Increase biomass by 18x (Buchheister)
# MAB.rpath.params$model$Biomass[41]<-MAB.rpath.params$model$Biomass[41]*18
MAB.rpath.params$model[Group == 'SmPelagics', Biomass := Biomass * 18]

## LittleSkate ---------------------------
## Increase biomass by 5x (Lucey)
# MAB.rpath.params$model$Biomass[18]<-MAB.rpath.params$model$Biomass[18]*5
MAB.rpath.params$model[Group == 'LittleSkate', Biomass := Biomass * 5]

## Scup --------------------------------
## Increase biomass by 5x (Lucey)
# MAB.rpath.params$model$Biomass[34]<-MAB.rpath.params$model$Biomass[34]*5
MAB.rpath.params$model[Group == 'Scup', Biomass := Biomass * 5]

## WinterFlounder -----------------------
## Increase biomass by 8x (Lucey)
# MAB.rpath.params$model$Biomass[47]<-MAB.rpath.params$model$Biomass[47]*8
MAB.rpath.params$model[Group == 'WinterFlounder', Biomass := Biomass * 8]

## Mesopelagics -------------------------
## Increase biomass by 2.5x (Prebal diagnostics)
# MAB.rpath.params$model$Biomass[22]<-MAB.rpath.params$model$Biomass[22]*2.5
MAB.rpath.params$model[Group == 'Mesopelagics', Biomass := Biomass * 2.5]

## AtlScallop ---------------------------
## Increase biomass by 5x (Lucey)
# MAB.rpath.params$model$Biomass[5]<-MAB.rpath.params$model$Biomass[5]*5
MAB.rpath.params$model[Group == 'AtlScallop', Biomass := Biomass * 5]

## OtherSkates --------------------------
## Increase biomass by 3x (Lucey)
# MAB.rpath.params$model$Biomass[31]<-MAB.rpath.params$model$Biomass[31]*3
MAB.rpath.params$model[Group == 'OtherSkates', Biomass := Biomass * 3]

## Odontocetes --------------------------
## Increase biomass by 1.5x (Lucey)
# MAB.rpath.params$model$Biomass[26]<-MAB.rpath.params$model$Biomass[26]*2
MAB.rpath.params$model[Group == 'Odontocetes', Biomass := Biomass * 2]

## SilverHake ---------------------------
## Increase biomass by 2x (Lucey)
# MAB.rpath.params$model$Biomass[37]<-MAB.rpath.params$model$Biomass[37]*2
MAB.rpath.params$model[Group == 'SilverHake', Biomass := Biomass * 2]

## Goosefish ----------------------------
## Increase biomass by 2.5x (Okey)
# MAB.rpath.params$model$Biomass[14]<-MAB.rpath.params$model$Biomass[14]*2.5
MAB.rpath.params$model[Group == 'Goosefish', Biomass := Biomass * 2.5]

## AmShad -------------------------------
## Increase biomass by 2x (Prebal diagnostics)
# MAB.rpath.params$model$Biomass[2]<-MAB.rpath.params$model$Biomass[2]*2
MAB.rpath.params$model[Group == 'AmShad', Biomass := Biomass * 2]

## Fourspot -----------------------------
## Increase biomass by 1.5x (Lucey)
# MAB.rpath.params$model$Biomass[12]<-MAB.rpath.params$model$Biomass[12]*1.5
MAB.rpath.params$model[Group == 'Fourspot', Biomass := Biomass * 1.5]

##Weakfish ------------------------------
##Increase biomass by 1.5x (Assessment)
# MAB.rpath.params$model$Biomass[45]<-MAB.rpath.params$model$Biomass[45]*1.5
MAB.rpath.params$model[Group == 'Weakfish', Biomass := Biomass * 1.5]

# PB changes ---------------------------
## Bluefish -----------------------------
## Increase PB by 2x (Buccheister et al.)
# MAB.rpath.params$model$PB[9]<-MAB.rpath.params$model$PB[9]*2
MAB.rpath.params$model[Group == 'Bluefish', PB := PB * 2]

## HMS --------------------------------
## Decrease PB by 2x
# MAB.rpath.params$model$PB[15]<-MAB.rpath.params$model$PB[15]/2
MAB.rpath.params$model[Group == 'HMS', PB := PB / 2]

## AtlMackerel -------------------------
## Decrease PB by 2x
# MAB.rpath.params$model$PB[4]<-MAB.rpath.params$model$PB[4]/2
MAB.rpath.params$model[Group == 'AtlMackerel', PB := PB / 2]

## GelZooplankton ----------------------
## Increase PB by 10x (Lucey)
# MAB.rpath.params$model$PB[13]<-MAB.rpath.params$model$PB[13]*10
MAB.rpath.params$model[Group == 'GelZooplankton', PB := PB * 10]

## Megabenthos -------------------------
## Decrease PB by 2x (Lucey, Link et al.)
# MAB.rpath.params$model$PB[21]<-MAB.rpath.params$model$PB[21]/2
MAB.rpath.params$model[Group == 'Megabenthos', PB := PB / 2]

## OtherShrimps ------------------------
## Decrease PB by 0.6x (Lucey)
# MAB.rpath.params$model$PB[30]<-MAB.rpath.params$model$PB[30]*0.6
MAB.rpath.params$model[Group == 'OtherShrimps', PB := PB * 0.6]

## RedHake -----------------------------
## Decrease PB by 2x
# MAB.rpath.params$model$PB[33]<-MAB.rpath.params$model$PB[33]/2
MAB.rpath.params$model[Group == 'RedHake', PB := PB / 2]

## SilverHake --------------------------
## Decrease PB by 2x
# MAB.rpath.params$model$PB[37]<-MAB.rpath.params$model$PB[37]/2
MAB.rpath.params$model[Group == 'SilverHake', PB := PB / 2]

## SmoothDogfish -----------------------
## Decrease PB by 4x
# MAB.rpath.params$model$PB[40]<-MAB.rpath.params$model$PB[40]/4
MAB.rpath.params$model[Group == 'SmoothDogfish', PB := PB / 4]

## SmPelagics --------------------------
## Decrease PB by 2x
# MAB.rpath.params$model$PB[41]<-MAB.rpath.params$model$PB[41]/2
MAB.rpath.params$model[Group == 'SmPelagics', PB := PB / 2]

## Windowpane --------------------------
## Decrease PB by 0.6x
# MAB.rpath.params$model$PB[46]<-MAB.rpath.params$model$PB[46]*0.6
MAB.rpath.params$model[Group == 'Windowpane', PB := PB * 0.6]

## WinterFlounder -----------------------
## Decrease PB by 0.6x
# MAB.rpath.params$model$PB[47]<-MAB.rpath.params$model$PB[47]*0.6
MAB.rpath.params$model[Group == 'WinterFlounder', PB := PB * 0.6]

## WinterSkate --------------------------
## Decrease PB by 0.6x
# MAB.rpath.params$model$PB[48]<-MAB.rpath.params$model$PB[48]*0.6
MAB.rpath.params$model[Group == 'WinterSkate', PB := PB * 0.6]


# QB changes --------------------------------
## AtlMackerel -------------------------
## Increase QB to bring GE < 1
# MAB.rpath.params$model$QB[4]<-MAB.rpath.params$model$QB[4]*1.5
MAB.rpath.params$model[Group == 'AtlMackerel', QB := QB * 1.5]

## OtherShrimps ------------------------
## Increase QB to bring GE < 1
# MAB.rpath.params$model$QB[30]<-MAB.rpath.params$model$QB[30]*1.75
MAB.rpath.params$model[Group == 'OtherShrimps', QB := QB * 1.75]

## HMS --------------------------------
## Decrease QB by 2x
# MAB.rpath.params$model$QB[15]<-MAB.rpath.params$model$QB[15]/2
MAB.rpath.params$model[Group == 'HMS', QB := QB / 2]

## Seabirds ----------------------------
## Decrease QB by 2x
# MAB.rpath.params$model$QB[35]<-MAB.rpath.params$model$QB[35]/2
MAB.rpath.params$model[Group == 'Seabirds', QB := QB / 2]

## GelZooplankton ----------------------
## Decrease QB by 0.75x (Lucey)
# MAB.rpath.params$model$QB[13]<-MAB.rpath.params$model$QB[13]*0.75
MAB.rpath.params$model[Group == 'GelZooplankton', QB := QB * 0.75]

## SummerFlounder -----------------------
## Decrease QB by 0.75x (Lucey)
# MAB.rpath.params$model$QB[44]<-MAB.rpath.params$model$QB[44]*0.75
MAB.rpath.params$model[Group == 'SummerFlounder', QB := QB * 0.75]

## Odontocetes -------------------------
## Decrease QB by 2x (Lucey)
# MAB.rpath.params$model$QB[26]<-MAB.rpath.params$model$QB[26]/2
MAB.rpath.params$model[Group == 'Odontocetes', QB := QB / 2]

## RedHake -----------------------------
## Decrease QB by 4x (Lucey)
# MAB.rpath.params$model$QB[33]<-MAB.rpath.params$model$QB[33]/4
MAB.rpath.params$model[Group == 'RedHake', QB := QB / 4]

# Fishing changes -------------------------
## Sharks --------------------------------
### Reduce recreational landings ------------------------------
# MAB.rpath.params$model$Recreational[36]<-MAB.rpath.params$model$Recreational[36]*0
MAB.rpath.params$model[Group == 'Sharks', Recreational := Recreational * 0]

### Reduce trap landings ------------------------------
# MAB.rpath.params$model$Trap[36]<-MAB.rpath.params$model$Trap[36]*0
MAB.rpath.params$model[Group == 'Sharks', Trap := Trap * 0]

## OtherPelagics ------------------------
### Reduce trap landings -------------------------
# MAB.rpath.params$model$Trap[29]<-MAB.rpath.params$model$Trap[29]*0
MAB.rpath.params$model[Group == 'OtherPelagics', Trap := Trap * 0]

### Reduce recreational landings by 0.01x ---------------------
#MAB.rpath.params$model$Recreational[29]<-MAB.rpath.params$model$Recreational[29]*0.01
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

### Reduce pelagic landings by 0.1x -------------------------
# MAB.rpath.params$model$Pelagic[29]<-MAB.rpath.params$model$Pelagic[29]*0.1
MAB.rpath.params$model[Group == 'OtherPelagics', Pelagic := Pelagic * 0.1]

## Bluefish -----------------------------
### Reduce recreational landings by 0.01x ---------------------
# MAB.rpath.params$model$Recreational[9]<-MAB.rpath.params$model$Recreational[9]*.01
MAB.rpath.params$model[Group == 'Bluefish', Recreational := Recreational * 0.01]

## Scup --------------------------------
## Reduce recreational fishing
## SW addition
#MAB.rpath.params$model$Recreational[34]<-MAB.rpath.params$model$Recreational[34]*0.05
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

## SouthernDemersals -------------------
### Reduce recreational landings ---------------------
#MAB.rpath.params$model$Recreational[42]<-MAB.rpath.params$model$Recreational[42]*.01
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

### Reduce trap landings ---------------------
# MAB.rpath.params$model$Trap[42]<-MAB.rpath.params$model$Trap[42]*.01
MAB.rpath.params$model[Group == 'SouthernDemersals', Trap := Trap * 0.01]

## BlackSeaBass -------------------------
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[8]<-MAB.rpath.params$model$Recreational[8]*.01
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

## Cod --------------------------------
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[11]<-MAB.rpath.params$model$Recreational[11]*.1
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

## AtlMackerel -------------------------
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[4]<-MAB.rpath.params$model$Recreational[4]*.01
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

## OtherDemersals -----------------------
### Reduce trap landings ---------------------
# MAB.rpath.params$model$Trap[28]<-MAB.rpath.params$model$Trap[28]*.01
MAB.rpath.params$model[Group == 'OtherDemersals', Trap := Trap * 0.01]

## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[28]<-MAB.rpath.params$model$Recreational[28]*.01
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

## SilverHake --------------------------
### Reduce SM Mesh landings ---------------------
# MAB.rpath.params$model$`SM Mesh`[37]<-MAB.rpath.params$model$`SM Mesh`[37]*.1
MAB.rpath.params$model[Group == 'SilverHake', 'SM Mesh' := `SM Mesh` * 0.1]

## Weakfish ----------------------------
### Reduce recreational landings --------------------- 
## SW added
# MAB.rpath.params$model$Recreational[45]<-MAB.rpath.params$model$Recreational[45]*.5
MAB.rpath.params$model[Group == 'Weakfish', Recreational := Recreational * 0.5]

## RedHake
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[33]<-MAB.rpath.params$model$Recreational[33]*.01
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

## SummerFlounder
## Reduce recreational fishing
#MAB.rpath.params$model$Recreational[44]<-MAB.rpath.params$model$Recreational[44]*.1
# This was commented out in balanced model before addition of SurfClam and OceanQuahog
# Leaving commented out for now -M.T.G.

## Mesopelagics -------------------------
### Reduce trap landings --------------------- 
# MAB.rpath.params$model$Trap[22]<-MAB.rpath.params$model$Trap[22]*.1
MAB.rpath.params$model[Group == 'Mesopelagics', Trap := Trap * 0.1]

# Diet changes ---------------------------

## OtherCephalapods ---------------------
# Relieve predation pressure
## SummerFlounder: OtherCephalopods -10%, Illex +10%
# MAB.rpath.params$diet[27,45]<-MAB.rpath.params$diet[27,45]-0.10
MAB.rpath.params$diet[Group == 'OtherCephalopods', SummerFlounder := SummerFlounder - 0.10]
# MAB.rpath.params$diet[16,45]<-MAB.rpath.params$diet[16,45]+0.10
MAB.rpath.params$diet[Group == 'Illex', SummerFlounder := SummerFlounder + 0.10]

## SpinyDogfish: OtherCephalopods -3%, Illex +3%
# MAB.rpath.params$diet[27,44]<-MAB.rpath.params$diet[27,44]-0.03
# MAB.rpath.params$diet[16,44]<-MAB.rpath.params$diet[16,44]+0.03
MAB.rpath.params$diet[Group == 'OtherCephalopods', SpinyDogfish := SpinyDogfish - 0.03]
MAB.rpath.params$diet[Group == 'Illex', SpinyDogfish := SpinyDogfish + 0.03]

## SilverHake: OtherCephalopods -4%, Illex +4%
# MAB.rpath.params$diet[27,38]<-MAB.rpath.params$diet[27,38]-0.04
# MAB.rpath.params$diet[16,38]<-MAB.rpath.params$diet[16,38]+0.04
MAB.rpath.params$diet[Group == 'OtherCephalopods', SilverHake := SilverHake - 0.04]
MAB.rpath.params$diet[Group == 'Illex', SilverHake := SilverHake + 0.04]

## Bluefish: OtherCephalopods -5%, Illex +5%
# MAB.rpath.params$diet[27,10]<-MAB.rpath.params$diet[27,10]-0.05
# MAB.rpath.params$diet[16,10]<-MAB.rpath.params$diet[16,10]+0.05
MAB.rpath.params$diet[Group == 'OtherCephalopods', Bluefish := Bluefish - 0.05]
MAB.rpath.params$diet[Group == 'Illex', Bluefish := Bluefish + 0.05]

## Butterfish: OtherCephalopods -.5%, Illex +.5%
# MAB.rpath.params$diet[27,11]<-MAB.rpath.params$diet[27,11]-0.005
# MAB.rpath.params$diet[16,11]<-MAB.rpath.params$diet[16,11]+0.005
MAB.rpath.params$diet[Group == 'OtherCephalopods', Butterfish := Butterfish - 0.005]
MAB.rpath.params$diet[Group == 'Illex', Butterfish := Butterfish + 0.005]

## Fourspot: OtherCephalopods -10%, Illex +10%
# MAB.rpath.params$diet[27,13]<-MAB.rpath.params$diet[27,13]-0.10
# MAB.rpath.params$diet[16,13]<-MAB.rpath.params$diet[16,13]+0.10
MAB.rpath.params$diet[Group == 'OtherCephalopods', Fourspot := Fourspot - 0.10]
MAB.rpath.params$diet[Group == 'Illex', Fourspot := Fourspot + 0.10]

## RedHake: OtherCephalopods -4%, Illex +4%
# MAB.rpath.params$diet[27,34]<-MAB.rpath.params$diet[27,34]-0.04
# MAB.rpath.params$diet[16,34]<-MAB.rpath.params$diet[16,34]+0.04
MAB.rpath.params$diet[Group == 'OtherCephalopods', RedHake := RedHake - 0.04]
MAB.rpath.params$diet[Group == 'Illex', RedHake := RedHake + 0.04]

## OtherPelagics ------------------------
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

## AtlMackerel -------------------------
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

## SmFlatfishes --------------------------
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
MAB.rpath.params$diet[Group == 'SmFlatfishes', Goosefish := Goosefish - 0.012]
MAB.rpath.params$diet[Group == 'Macrobenthos', Goosefish := Goosefish + 0.012]

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

## OtherDemersals -----------------------
## Relieve predation pressure on OtherDemersals

## SummerFlounder: OtherDemersals -5%, Macrobenthos +5%
# MAB.rpath.params$diet[28,45]<-MAB.rpath.params$diet[28,45]-0.05
# MAB.rpath.params$diet[20,45]<-MAB.rpath.params$diet[20,45]+0.05
MAB.rpath.params$diet[Group == 'OtherDemersals', SummerFlounder := SummerFlounder - 0.05]
MAB.rpath.params$diet[Group == 'Macrobenthos', SummerFlounder := SummerFlounder + 0.05]

## Megabenthos --------------------------
## Relieve predation pressure on Megabenthos

## Macrobenthos: Megabenthos -1%, Macrobenthos +1%
# MAB.rpath.params$diet[21,21]<-MAB.rpath.params$diet[21,21]-0.01
# MAB.rpath.params$diet[20,21]<-MAB.rpath.params$diet[20,21]+0.01
MAB.rpath.params$diet[Group == 'Megabenthos', Macrobenthos := Macrobenthos - 0.01]
MAB.rpath.params$diet[Group == 'Macrobenthos', Macrobenthos := Macrobenthos + 0.01]

## SilverHake --------------------------
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

## WinterSkate --------------------------
## Relieve predation pressure on WinterSkate

## Macrobenthos: WinterSkate -0.003%, Macrobenthos +0.003%
# MAB.rpath.params$diet[48,21]<-MAB.rpath.params$diet[48,21]-0.000037
# MAB.rpath.params$diet[20,21]<-MAB.rpath.params$diet[20,21]+0.000037
MAB.rpath.params$diet[Group == 'WinterSkate', Macrobenthos := Macrobenthos - 0.000037]
MAB.rpath.params$diet[Group == 'Macrobenthos', Macrobenthos := Macrobenthos + 0.000037]

## RedHake -----------------------------
## Relieve predation pressure on RedHake

## Macrobenthos: RedHake -0.004%, Macrobenthos +0.004%
# MAB.rpath.params$diet[33,21]<-MAB.rpath.params$diet[33,21]-0.000042
# MAB.rpath.params$diet[20,21]<-MAB.rpath.params$diet[20,21]+0.000042
MAB.rpath.params$diet[Group == 'RedHake', Macrobenthos := Macrobenthos - 0.000042]
MAB.rpath.params$diet[Group == 'Macrobenthos', Macrobenthos := Macrobenthos + 0.000042]

## OtherDemersals: RedHake -1.5%, Macrobenthos +1.5%
# MAB.rpath.params$diet[33,29]<-MAB.rpath.params$diet[33,29]-0.015
# MAB.rpath.params$diet[20,29]<-MAB.rpath.params$diet[20,29]+0.015
MAB.rpath.params$diet[Group == 'RedHake', OtherDemersals := OtherDemersals - 0.015]
MAB.rpath.params$diet[Group == 'Macrobenthos', OtherDemersals := OtherDemersals + 0.015]

## SmPelagics --------------------------
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

## OtherShrimps ------------------------
## Relieve predation pressure on OtherShrimps

## Loligo: OtherShrimps -2%, Macrobenthos +2%
# MAB.rpath.params$diet[30,20]<-MAB.rpath.params$diet[30,20]-0.02
# MAB.rpath.params$diet[20,20]<-MAB.rpath.params$diet[20,20]+0.02
MAB.rpath.params$diet[Group == 'OtherShrimps', Loligo := Loligo - 0.02]
MAB.rpath.params$diet[Group == 'Macrobenthos', Loligo := Loligo + 0.02]

## Windowpane --------------------------
## Relieve predation pressure on Windowpane

## Bluefish: Windowpane -1%, Macrobenthos +1%
# MAB.rpath.params$diet[46,10]<-MAB.rpath.params$diet[46,10]-0.01
# MAB.rpath.params$diet[20,10]<-MAB.rpath.params$diet[20,10]+0.01
MAB.rpath.params$diet[Group == 'Windowpane', Bluefish := Bluefish - 0.01]
MAB.rpath.params$diet[Group == 'Macrobenthos', Bluefish := Bluefish + 0.01]

## Butterfish ---------------------------
## Relieve predation pressure on Butterfish

## OtherPelagics: Butterfish -10%, Macrobenthos +10%
## SW removed -- this led to negative diet & was not needed to balance
# MAB.rpath.params$diet[10,30]<-MAB.rpath.params$diet[10,30]-0.1
# MAB.rpath.params$diet[20,30]<-MAB.rpath.params$diet[20,30]+0.1

## Loligo: Butterfish -1.5%, Macrobenthos +1.5%
# MAB.rpath.params$diet[10,20]<-MAB.rpath.params$diet[10,20]-0.015
# MAB.rpath.params$diet[20,20]<-MAB.rpath.params$diet[20,20]+0.015
MAB.rpath.params$diet[Group == 'Butterfish', Loligo := Loligo - 0.015]
MAB.rpath.params$diet[Group == 'Macrobenthos', Loligo := Loligo + 0.015]

## Weakfish ----------------------------
## Relieve predation pressure on Weakfish
## SW added
## SouthernDemersals: Weakfish -4%, Macrobenthos +4%
# MAB.rpath.params$diet[45,43]<-MAB.rpath.params$diet[45,43]-0.04
# MAB.rpath.params$diet[20,43]<-MAB.rpath.params$diet[20,43]+0.04
MAB.rpath.params$diet[Group == 'Weakfish', SouthernDemersals := SouthernDemersals - 0.04]
MAB.rpath.params$diet[Group == 'Macrobenthos', SouthernDemersals := SouthernDemersals + 0.04]

#add data pedigree
source("Sarah_R/data_pedigree.R")

MAB.rpath<-rpath(MAB.rpath.params,eco.name='Mid-Atlantic Bight')
MAB.rpath
# source("MAB_prebal.R")
EE<-MAB.rpath$EE
EE[order(EE)]
#write.Rpath(MAB.rpath,morts=T,file="MAB.rpath_morts.csv")
#write.Rpath(MAB.rpath, file="MAB_model.csv")
#write.csv(MAB.rpath.params$diet, file = "MAB_diet.csv")


#Save files
save(MAB.rpath, file = "outputs/MAB_Rpath.RData")
save(MAB.rpath.params,file = "outputs/MAB_params_Rpath.RData")
