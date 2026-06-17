#Code to reorganize copepod groups so that large copepod group
# contains only Calanus finmarchicus

#Author: Sarah J. Weisberg
# Updated by M.T. Grezlik to reference Group name rather than column number
# this was done when splitting groups changed column numbers
# Thu Dec 14 12:51:29 2023 ------------------------------


# Load packages -----------------------------------------------------------

library(sf) #r spatial package
library(ggplot2) #plotting
library(rnaturalearth) #simple map 
library(lubridate) #dates
library(spatialEco) #more advanced GIS
library(dplyr) 
library(tidyverse)
library(here)


# Biomass adjustments -----------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

#load data
plankton <- read.csv(here("data/EcoMon_Plankton_Data_v3_7-Data.csv"))
#convert data and create year, month, day columns: 
plankton$date <- as.Date(plankton$date, format =  "%d-%b-%y")
plankton$month <- month(plankton$date)
plankton$year <- year(plankton$date)
plankton$day <- day(plankton$date)

#filter for MAB, SNE only
#combine with the strata dataset 
#load strata
strata <- read_sf(here("data/EcomonStrata_v4b.shp"))
#set coordinate system
st_crs(strata) = 4326 #this is WGS 1984

#make ecomon spatial with the same defined coordinate system
plankton_sp <- st_as_sf(x = plankton, 
                      coords = c("lon", "lat"),
                      crs = 4326)

#extract stratum values to points
#get a weird error - need to follow these steps: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf::sf_use_s2(FALSE)
plankton_extract <- st_intersection(plankton_sp, strata)
plankton_extract <- as.data.frame(plankton_extract)
plankton <- plankton_extract

#filter for MAB and SNE only
plankton_extract <- plankton_extract %>% filter(Region %in% c("MAB","SNE"))
plankton <- plankton_extract

#create season column based on 2 month period
plankton$season <- ifelse(plankton$month == 1 | plankton$month == 2, "JanFeb", ifelse(plankton$month == 3 | plankton$month == 4, "MarApril",
                  ifelse(plankton$month == 5 | plankton$month == 6, "MayJun",ifelse(plankton$month == 7 | plankton$month == 8, "JulAug",ifelse(plankton$month == 9 | plankton$month == 10, "SepOct",ifelse(plankton$month == 11 | plankton$month == 12, "NovDec", NA))))))

#filter for copepods of interest
#tidy
copes_y<-plankton %>% select(ctyp_10m2,calfin_10m2,mlucens_10m2)
copes_x<-as.data.frame(cbind(plankton$year, plankton$season))
colnames(copes_x)<-c("Year","Season")
copes_y[is.na(copes_y)] <- 0 #NAs should be true 0s
copes <- cbind(copes_x, copes_y)

#pull out species names
copes_ynames <- colnames(copes_y)

#make long dataframe
copes_long <- copes %>% pivot_longer(all_of(copes_ynames), names_to = "spp")

#rename spp
copes_long<- copes_long %>% 
  mutate(spp_long=if_else(spp=="ctyp_10m2","CENTROPAGES_TYPICUS",
  if_else(spp=="calfin_10m2","CALANUS_FINMARCHICUS", if_else(spp=="mlucens_10m2","METRIDIA_LUCENS",NA))))
  
#filter for 1980-85
copes_long <- copes_long %>% filter(Year >= 1980 & Year <=1985)

#look at temporal coverage
cruises<-copes_long %>% select(Year,Season) %>% distinct()

#average abundance per cruise
copes_cruise<-copes_long %>% group_by(Year,Season,spp_long) %>% dplyr::summarise(abd = mean(value))
#annual average (across cruises)
copes_annual<-copes_cruise %>% group_by(Year,spp_long) %>% dplyr::summarise(abd=mean(abd))
#average across 1980-85
copes_avg<-copes_annual %>% group_by(spp_long) %>% dplyr::summarise(abd=mean(abd))

#convert to biomass
sizes<-read.csv(here("data/EcoMon_Copepod size.csv"))
sizes<-sizes[-1,]
sizes<-sizes %>% select(c("X","Literature"))
colnames(sizes)<-c("spp_long","length")
sizes$length<-as.numeric(sizes$length)
#use l-w conversion from EMAX (wet weight)
sizes$weight<-0.0881*sizes$length^2.8514

#join back with abd density estimates
#join back with abd estimates
copes_avg<-left_join(copes_avg,sizes,by="spp_long")
#multiply ind * mg/ind
copes_avg$biomass<-copes_avg$abd*copes_avg$weight
#pull out calanus
calfin<-copes_avg %>% filter(spp_long=="CALANUS_FINMARCHICUS")
copes_ratio<- calfin$biomass/sum(copes_avg$biomass)

#adjust starting biomasses
#SmCopepods
sm_new<-MAB.rpath.params$model$Biomass[which(MAB.rpath.params[["model"]][["Group"]] == "SmCopepods")] + MAB.rpath.params$model$Biomass[which(MAB.rpath.params[["model"]][["Group"]] == "LgCopepods")] * (1-copes_ratio)
MAB.rpath.params$model$Biomass[which(MAB.rpath.params[["model"]][["Group"]] == "SmCopepods")] <- sm_new
#LgCopepods
lg_new<-MAB.rpath.params$model$Biomass[which(MAB.rpath.params[["model"]][["Group"]] == "LgCopepods")] * copes_ratio
MAB.rpath.params$model$Biomass[which(MAB.rpath.params[["model"]][["Group"]] == "LgCopepods")] <- lg_new

# Diet adjustments --------------------------------------------------------
#get rid of NAs first
MAB.rpath.params$diet<-replace(MAB.rpath.params$diet,is.na(MAB.rpath.params$diet),0)

## AmLobster -----------------------------------------------------------
#Shift 0.5% from Macrobenthos[20] to LgCopepods[17]

# MAB.rpath.params$diet[20,2]<-MAB.rpath.params$diet[20,2]-0.005
MAB.rpath.params$diet[Group == 'Macrobenthos', AmLobster := AmLobster - 0.005]
# MAB.rpath.params$diet[17,2]<-MAB.rpath.params$diet[17,2]+0.005
MAB.rpath.params$diet[Group == 'LgCopepods', AmLobster := AmLobster + 0.005]

#Shift 0.5% from Macrobenthos[20]] to SmCopepods[38]

# MAB.rpath.params$diet[20,2]<-MAB.rpath.params$diet[20,2]-0.005
MAB.rpath.params$diet[Group == 'Macrobenthos', AmLobster := AmLobster - 0.005]
# MAB.rpath.params$diet[38,2]<-MAB.rpath.params$diet[38,2]+0.005
MAB.rpath.params$diet[Group == 'SmCopepods', AmLobster := AmLobster + 0.005]

##AtlMackerel -----------------------------------------------------------
#Remove 5% from Macrobenthos[20]
#Remove 8% from Micronekton[23]
#Remove 7% from Krill
#Remove 10% from SmPelagics[41]
#Remove 30% from LgCopepods[17]
#Move 60% to SmCopepods[38]
# MAB.rpath.params$diet[20,5]<-MAB.rpath.params$diet[20,5]-0.05
MAB.rpath.params$diet[Group == 'Macrobenthos', AtlMackerel := AtlMackerel - 0.05]
# MAB.rpath.params$diet[23,5]<-MAB.rpath.params$diet[23,5]-0.08
MAB.rpath.params$diet[Group == 'Micronekton', AtlMackerel := AtlMackerel - 0.08]
# MAB.rpath.params$diet[50,5]<-MAB.rpath.params$diet[50,5]-0.07
MAB.rpath.params$diet[Group == 'Krill', AtlMackerel := AtlMackerel - 0.07]
# MAB.rpath.params$diet[41,5]<-MAB.rpath.params$diet[41,5]-0.1
MAB.rpath.params$diet[Group =='SmPelagics', AtlMackerel := AtlMackerel - 0.1]
# MAB.rpath.params$diet[17,5]<-MAB.rpath.params$diet[17,5]-0.3
MAB.rpath.params$diet[Group == 'LgCopepods', AtlMackerel := AtlMackerel - 0.3]
# MAB.rpath.params$diet[38,5]<-MAB.rpath.params$diet[38,5]+0.6
MAB.rpath.params$diet[Group == 'SmCopepods', AtlMackerel := AtlMackerel + 0.6]

##SmCopepods -----------------------------------------------------------
#Move 0.25% from SmCopepods[38] to LgCopepods[17]
# MAB.rpath.params$diet[38,39]<-MAB.rpath.params$diet[38,39]-0.0025
MAB.rpath.params$diet[Group == 'SmCopepods', SmCopepods := SmCopepods - 0.0025]
# MAB.rpath.params$diet[17,39]<-MAB.rpath.params$diet[17,39]+0.0025
MAB.rpath.params$diet[Group == 'LgCopepods', SmCopepods := SmCopepods + 0.0025]


## For rest of groups, add portion to SmCope consumption ----------------
# MAB.rpath.params$diet[38,c(3:4,5:38,40:50)]<-
#   MAB.rpath.params$diet[38,c(3:4,5:38,40:50)]+
#   MAB.rpath.params$diet[17,c(3:4,5:38,40:50)]*(1-copes_ratio)
# 
# MAB.rpath.params$diet[17,c(3:4,5:38,40:50)]<-
#   MAB.rpath.params$diet[17,c(3:4,5:38,40:50)]*copes_ratio

# find species that prey on both small and large copepods
SmCopes.preds <- MAB.rpath.params$diet[Group == "SmCopepods",] |> 
                        pivot_longer(!Group, names_to = 'Rpred', values_to ='preyper') |> 
                        filter(preyper > 0) |> 
                        select(Rpred)
LgCopes.preds <- MAB.rpath.params$diet[Group == "LgCopepods",] |> 
                        pivot_longer(!Group, names_to = 'Rpred', values_to ='preyper') |> 
                        filter(preyper > 0) |>
                        select(Rpred)
# find overlapping Rpred
Copes.preds <- intersect(SmCopes.preds, LgCopes.preds)

### AmLobster -----------------------------------------------------------
AmLobster.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', AmLobster]
MAB.rpath.params$diet[Group == 'SmCopepods', AmLobster := AmLobster + (AmLobster.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', AmLobster := AmLobster * copes_ratio]

### GelZooplankton -----------------------------------------------------------
GelZooplankton.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', GelZooplankton]
MAB.rpath.params$diet[Group == 'SmCopepods', GelZooplankton := GelZooplankton + (GelZooplankton.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', GelZooplankton := GelZooplankton * copes_ratio]

### LgCopepods -----------------------------------------------------------
LgCopepods.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', LgCopepods]
MAB.rpath.params$diet[Group == 'SmCopepods', LgCopepods := LgCopepods + (LgCopepods.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', LgCopepods := LgCopepods * copes_ratio]

### Macrobenthos -----------------------------------------------------------
Macrobenthos.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', Macrobenthos]
MAB.rpath.params$diet[Group == 'SmCopepods', Macrobenthos := Macrobenthos + (Macrobenthos.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', Macrobenthos := Macrobenthos * copes_ratio]

### Micronekton -----------------------------------------------------------
Micronekton.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', Micronekton]
MAB.rpath.params$diet[Group == 'SmCopepods', Micronekton := Micronekton + (Micronekton.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', Micronekton := Micronekton * copes_ratio]

### SmCopepods -----------------------------------------------------------
SmCopepods.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', SmCopepods]
MAB.rpath.params$diet[Group == 'SmCopepods', SmCopepods := SmCopepods + (SmCopepods.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', SmCopepods := SmCopepods * copes_ratio]

### SmPelagics -----------------------------------------------------------
SmPelagics.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', SmPelagics]
MAB.rpath.params$diet[Group == 'SmCopepods', SmPelagics := SmPelagics + (SmPelagics.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', SmPelagics := SmPelagics * copes_ratio]

### Krill -----------------------------------------------------------
Krill.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', Krill]
MAB.rpath.params$diet[Group == 'SmCopepods', Krill := Krill + (Krill.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', Krill := Krill * copes_ratio]

### AtlMenhaden -----------------------------------------------------------
AtlMenhaden.from.large <- MAB.rpath.params$diet[Group == 'LgCopepods', AtlMenhaden]
MAB.rpath.params$diet[Group == 'SmCopepods', AtlMenhaden := AtlMenhaden + (AtlMenhaden.from.large * (1-copes_ratio))]
MAB.rpath.params$diet[Group == 'LgCopepods', AtlMenhaden := AtlMenhaden * copes_ratio]

#find the sum of each column
diet.check <-  MAB.rpath.params$diet |> 
                pivot_longer(!Group, names_to = 'Rpred', values_to ='preyper') |> 
                group_by(Rpred) |> 
                summarise(sum = sum(preyper))
