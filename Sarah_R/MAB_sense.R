#Title: GOM Ecosense

# Purpose: This script uses Ecosense simplified Bayesian synthesis to
#           generate plausible versions of the MAB food web (1980-85)
#           These plausible webs incorporate parameter uncertainty, as
#           determined by data pedigree.

# DataFiles:"GOM_params_Rpath.RData"; "GOM_Rpath.RData"

# Author: S. Weisberg
# Contact details: sarah.j.weisberg@stonybrook.edu

# Sat Jan 27 12:29:25 2024 ------------------------------


#Required packages--------------------------------------------------------------
library(here); library(data.table); library(Rpath);library(tidyr); library(dgof)

#Source code from sense_beta branch of Rpath Repo
library(devtools)
source_url('https://raw.githubusercontent.com/NOAA-EDAB/Rpath/sense_beta/R/ecosense.R')

#Load balanced model
load(here("outputs/MAB_params_Rpath_no_disc.RData"))
load(here("outputs/MAB_Rpath_no_disc.RData"))

#Set up model with group names and types
groups<-as.vector(MAB.rpath$Group)
#Count number of each group type
#ngroups <- nrow(MAB.rpath.params)
nliving <- nrow(MAB.rpath.params$model[Type <  2, ])
ndead   <- nrow(MAB.rpath.params$model[Type == 2, ])
#Identify index of pp
pp<-which(groups=="Phytoplankton")

#Fix PB/QB pedigree values so that Respiration cannot <0
Unassim<-MAB.rpath$Unassim[1:(nliving+ndead)]
QB<-MAB.rpath$QB[1:(nliving+ndead)]
PB<-MAB.rpath$PB[1:(nliving+ndead)]
#Identify which groups violate this rule
fixers<-which((1-Unassim)*QB*(1-MAB.rpath.params$pedigree[,QB]) < (1+MAB.rpath.params$pedigree[, PB])*PB)
#exclude Phytoplankton & Detritus
fixers <- fixers[!fixers %in% c(pp,(nliving+ndead))]

#adjust QB and PB pedigree values where needed
for (i in 1:length(fixers)){
  to_fix<-fixers[i]
  Resp_edge<-PB[to_fix]/((1-Unassim[to_fix])*QB[to_fix])
  QB_ped<-MAB.rpath.params$pedigree[to_fix,QB]
  PB_ped<-MAB.rpath.params$pedigree[to_fix,PB]
  limit<-(1-QB_ped)/(1+PB_ped)
  while(Resp_edge>limit){
    if(QB_ped >= PB_ped){
      QB_ped <- QB_ped - 0.1
    }
    else{
      PB_ped <- PB_ped - 0.1
    }
    limit<-(1-QB_ped)/(1+PB_ped)
  }
  MAB.rpath.params$pedigree[to_fix,PB := PB_ped]
  MAB.rpath.params$pedigree[to_fix,QB := QB_ped]
}


#Fix PB/QB pedigree values so that GE cannot >1
# GE<-MAB.rpath$GE
# limits<-(1-MAB.rpath.params$pedigree[,QB])/(1+MAB.rpath.params$pedigree[, PB])
# #Identify which groups violate this rule
# fixers<-which(limits<GE[1:length(limits)])
# #exclude Phytoplankton (19)
# fixers <- fixers[-19]
# 
# #adjust QB and PB values where needed
# for (i in 1:length(fixers)){
#   to_fix<-fixers[i]
#   GE_group<-GE[to_fix]
#   QB_ped<-MAB.rpath.params$pedigree[to_fix,QB]
#   PB_ped<-MAB.rpath.params$pedigree[to_fix,PB]
#   limit<-(1-QB_ped)/(1+PB_ped)
#   while(GE_group>limit){
#     if(QB_ped >= PB_ped){
#       QB_ped <- QB_ped - 0.1
#     }
#     else{
#       PB_ped <- PB_ped - 0.1
#     }
#     limit<-(1-QB_ped)/(1+PB_ped)
#   }
#   MAB.rpath.params$pedigree[to_fix,PB := PB_ped]
#   MAB.rpath.params$pedigree[to_fix,QB := QB_ped]
# }


#Set up sense runs
all_years <- 1:50
scene <- rsim.scenario(MAB.rpath, MAB.rpath.params, years = all_years)
orig.biomass<-scene$start_state$Biomass

# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- nkept
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

#fail_groups<-c()
set.seed(19)
for (irun in 1:NUM_RUNS){
  MABsense <- copy(scene)
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- MABsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(MABsense, MAB.rpath.params)	# Replace the base params with Ecosense params  
  #MABsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef #took out this line on May 2, 2022
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  MABsense$params <- parlist[[irun]]
  MABtest <- rsim.run(MABsense, method = "RK4", years = all_years)
  # failList <- which((is.na(MABtest$end_state$Biomass) | MABtest$end_state$Biomass/orig.biomass > 1000 | MABtest$end_state$Biomass/orig.biomass < 1/1000))
  # {if (length(failList)>0)
  # {cat(irun,": fail in year ",MABtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
  #   else
  #   {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  # # failList<-as.data.frame(failList)
  # # # fail_groups<-rbind(fail_groups,failList)
  parlist[[irun]]$BURN_YEARS <- 1
}
       
# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
MAB_sense <- parlist[KEPT]

MAB_sense_unbound <- parlist

fail_groups <- fail_groups %>% group_by(failList) %>% tally()
colnames(fail_groups)<-c("group_num","total")
groups_num<- as.data.frame(cbind(groups[1:nliving],seq(2,(nliving+1))))
colnames(groups_num) <-c("name","group_num")
groups_num$group_num<-as.numeric(groups_num$group_num)
fail_groups<-left_join(fail_groups,groups_num,by="group_num")

save(MAB_sense, file = "outputs/MAB_sense_Rpath_50k_2024.RData")
save(MAB_sense_unbound, file = "outputs/MAB_sense_unbound.RData")
# ----- Examine results relative to starting model ----- #######################################
biomass_sense<-c()
#biomass distributions
for (i in 1:length(MAB_sense)){
  model<-model<-MAB_sense[[i]]
  biomass<-cbind(groups[1:(nliving+ndead)],model$B_BaseRef[2:(nliving+ndead+1)],rep(i,(nliving+ndead)))
  biomass_sense<-rbind(biomass_sense,biomass)
}
biomass_sense<-as.data.frame(biomass_sense)
colnames(biomass_sense)<-c("Group","Biomass","Model")
biomass_sense$Biomass<-as.numeric(biomass_sense$Biomass)

ggplot(data=biomass_sense,aes(x=Biomass))+
  geom_histogram(alpha = 0.4)+
  facet_wrap(vars(Group),nrow = 6,scales = "free")

ggplot(data=biomass_group,aes(x=Biomass))+
  geom_density(alpha = 0.4,fill="orange")

biomass_sense_wide <-pivot_wider(biomass_sense,names_from="Group",values_from="Biomass") %>%
  dplyr::select(-Model) 

biomass_cor<-cor(biomass_sense_wide)
library(corrplot)
corrplot(biomass_cor,method = "color",diag=F,type="lower",sig.level = 0.05)

#which biomass distributions come out as NOT uniform
ks<-c()
for(i in 1:(nliving+ndead)){
  group<-groups[i]
  biomass_group<-biomass_sense %>% filter(Group==group)
  test<-ks.test(biomass_group$Biomass,"punif",min(biomass_group$Biomass),max(biomass_group$Biomass))
  ks<-rbind(ks,c(group,test$p.value))
}
ks<-as.data.frame(ks)
colnames(ks)<-c("Group","p")
ks$p<-as.numeric(ks$p)

#which flow distributions are not the same
ks_flow<-c()
for(i in 1:(nliving+ndead)){
  bound<-flow_T %>% filter(group == groups[i])
  unbound<-flow_unbound %>% filter(group == groups[i])
  test<-ks.test(bound$flow,unbound$flow)
  ks_flow<-rbind(ks_flow,c(groups[i],test$p.value))
}
ks_flow<-as.data.frame(ks_flow)
colnames(ks_flow)<-c("Group","p")
ks_flow$p<-as.numeric(ks_flow$p)

#compare to control
ks_flow<-left_join(ks_flow,sc,by="Group")
ks_flow$sc<-as.numeric(ks_flow$sc)

#visualize results
ks_flow<- ks_flow %>% mutate(quant = ifelse(p<quantile(p,0.1),10,
                                            ifelse(p<quantile(p,0.2),20,
                                            ifelse(p<quantile(p,0.3),30,
                                            ifelse(p<quantile(p,0.4),40,
                                            ifelse(p<quantile(p,0.5),50,
                                            ifelse(p<quantile(p,0.6),60,
                                            ifelse(p<quantile(p,0.7),70,
                                            ifelse(p<quantile(p,0.8),80,
                                            ifelse(p<quantile(p,0.9),90,100))))))))))
ks_vis<-ks_flow %>% mutate(sign = ifelse(sc>0,"pos","neg")) %>% group_by(quant) %>%tally(sign=="pos") %>%
  mutate(prop=n/5)

