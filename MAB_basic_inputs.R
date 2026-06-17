## ---------------------------
## Script name: MAB_basic_inputs.R
##
## Purpose of script: User enters basic model data for reference in future
##                    scripts.
##
## Author: Brandon Beltz, updated by Sarah J. Weisberg
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: This version is specific to the MABRpath model.

# Thu May  9 15:15:29 2024 ------------------------------


## Load libraries

library(data.table);library(here);library(survdat)

## ---------------------------

MAB.groups<-as.data.table(c('AmLobster','RiverHerring','AtlCroaker','AtlMackerel',
                            'AtlScallop','Bacteria','BaleenWhales','BlackSeaBass',
                            'Bluefish','Butterfish','Cod','Fourspot','GelZooplankton',
                            'Goosefish','HMS','Illex','LgCopepods','LittleSkate','Loligo',
                            'Macrobenthos','Megabenthos','Mesopelagics','Micronekton',
                            'Microzooplankton','OceanPout','OceanQuahog','Odontocetes',
                            'OtherCephalopods','OtherDemersals','OtherPelagics','OtherShrimps',
                            'OtherSkates','Phytoplankton','RedHake','Scup','SeaBirds','Sharks',
                            'SilverHake','SmCopepods','SmFlatfishes','SmoothDogfish',
                            'SmPelagics','SouthernDemersals','SpinyDogfish',
                            'SummerFlounder','SurfClam', 'Weakfish','Windowpane',
                            'WinterFlounder','WinterSkate','YTFlounder','Krill','AtlMenhaden'))
setnames(MAB.groups,'V1','RPATH')
MAB.strata<-c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
MAB.stareas<-c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632)

#Calculate total MAB area ------------------------------------------
area<-sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
area<-get_area(areaPolygon = area, areaDescription="STRATA")
MAB.area<-subset(area, area$STRATUM %in% MAB.strata)
MAB.area<-sum(MAB.area$Area)
rm(area)