#Code to rebalance 1980-85 Mid-Atlantic Bight Rpath model
#Goal is to align starting model with Gulf of Maine, Georges Bank models

#Author: Sarah J. Weisberg
# Updated by M.T. Grezlik to reference groups rather than column numbers
# helped when splitting or collapsing groups

# Tue Dec  5 12:54:24 2023 ------------------------------

#load packages
library(here)
library(Rpath)
library(dplyr)


#correct detritus, discards BA, should be 0
#had been NA
# MAB.rpath.params[["model"]][["BioAcc"]][50:51] <- 0
MAB.rpath.params$model[Group == 'Detritus', BioAcc := 0]
MAB.rpath.params$model[Group == 'Discards', BioAcc := 0]

#correct fishing matrix
#fleets are catching other fleets
#issue is with rec fishery
# MAB.rpath.params[["model"]][["Recreational"]][52:62] <-NA
inorganic <-  c("Detritus", "Discards","Fixed Gear","LG Mesh",       
                "HMS Fleet","Scallop Dredge", "Trap","Other Dredge",  
                "Other","Clam Dredge","Pelagic","SM Mesh",       
                "Recreational","PurseSeine" )
MAB.rpath.params$model[Group %in% inorganic, Recreational := NA]

#decrease LgCopepods so Resp > 0
# MAB.rpath$PB[17]<- 61
MAB.rpath.params$model[Group == 'LgCopepods', PB := 61]

#redo copepods
source(here("Sarah_R/redo_copes_start.R"))



