## Script name: MAB_params.R
##
## Purpose of script: Reformat parameters for compatibility with final RPath
##                    model.
##                    
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Email: brandon.beltz@stonybrook.edu

# Fri Dec  8 16:59:07 2023

## Load libraries, packages and functions --------------------------
library(here);library(data.table);library(dplyr);library(tidyverse)

## Load basic inputs and initial parameter set--------------------------
load("data/MAB_params.RData")
source(here('MAB_basic_inputs.R'))

## Remove unnecessary annotations from MAB_params.RData--------------------
params<-MAB.Params[,c("RPATH","PB","QB","BA")]

##Manually add parameter values for krill, menhaden-----------------------
krill_men<-cbind(c("Krill","AtlMenhaden"),c(14.25,1.45),c(36.5,3.804),c(0,0))
colnames(krill_men)<-c("RPATH","PB","QB","BA")
params<-rbind(params,krill_men)
params <- params %>% mutate(PB=as.numeric(PB),QB=as.numeric(QB),BA=as.numeric(BA),RPATH = as.character(RPATH))

# Manually add parameter values for SurfClam, OceanQuahog -----------------------
# duplicating values for AtlScallop
# except biomass accumulation which is 0 for both species but not Scallop
params <- as.data.table(params)
surf<- params[RPATH=="AtlScallop",]
surf[,RPATH := 'SurfClam']
surf[,BA := 0]
ocean<- params[RPATH=="AtlScallop",]
ocean[,RPATH := 'OceanQuahog']
ocean[,BA := 0]
params<-rbind(params,surf)
params<-rbind(params,ocean)


#Change AmShad to RiverHerring
 params <- params %>% mutate(RPATH = replace(RPATH,RPATH == "AmShad","RiverHerring"))

# ## Convert PB and QB values into vectors
# MAB.PB<-params[,"PB"]
# MAB.QB<-params[,"QB"]
# MAB.BA<-params[,"BA"]
