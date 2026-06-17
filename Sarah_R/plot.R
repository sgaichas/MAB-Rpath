#code to visualize MAB and GOM food webs
#uses network model

# Tue Mar  5 13:29:10 2024 ------------------------------

#load required packages
library(igraph)
library(ggplot2)
library(ggnetwork)
library(viridis)

#first run enaR_sense.R to get orig.network
# MAB model to visual network ----------------------------------------------------
#pull network structure
A<-enaStructure(orig.network_MAB)$A
g<-igraph::graph_from_adjacency_matrix(A)

#get x and y coordinates
#derived from the plotfw function (https://rfrelat.github.io/BalticFoodWeb.html)
nylevel<-7 #determines number of levels along y-axis
n <- vcount(g) #number of vertices
tl<-MAB.rpath$TL[1:(nliving+ndead)] #pull trophic level calculations from rpath
bks <- c(0.9, seq(1.9, max(tl), length.out = nylevel))
ynod <- cut(tl, breaks = bks, include.lowest = TRUE, 
            labels = 1:(length(bks)-1)) #assign group to a y-level
maxx <- max(table(ynod)) #looks for max # of groups at any y-level
xnod <- rep(0,n)
for (i in 1:nylevel){
  l <- sum(ynod==i)
  
  ltr <- (l/maxx)**(1/2)*maxx
  if (l>1) {
    xnod[ynod==i] <- seq(-ltr,ltr,length.out = l)
  } else {
    xnod[ynod==i] <- 0
  }
}

coo <- cbind(xnod,tl) #these inform x and y coordinates

#scootch Bacteria to the left to get more space in the bottom of the web
#original x and y
coo[2,1]<-(-14)

#also need to move megabenthos, microzooplankton,loligo, smoothdogfish
coo[24,1]<-6
coo[21,1]<-12
coo[19,1]<-(-2.4)
coo[40,1]<-6.5
coo[38,1]<-10

#use ggnetwork to create a network geometry
#storage and flows from original model
n_MAB<-ggnetwork(orig.network_MAB,layout=coo,weights="flow")
#bind with TL info
TL<-as.data.frame(cbind(MAB.rpath$TL,groups)) %>% rename(vertex.names=groups,TL=V1)
TL<-TL %>% mutate(TL = as.numeric(TL)) %>% mutate(TL = round(TL,2))
n_MAB<-left_join(n_MAB,TL,by="vertex.names")

# GOM model to visual network ---------------------------------------------
load("~/Desktop/GOM-Rpath/outputs/GOM_params_Rpath.RData")
load("~/Desktop/GOM-Rpath/outputs/GOM_Rpath.RData")
#Set up model with group names and types
groups<-as.vector(GOM$Group)

#Count number of each group type
#ngroups <- nrow(GOM.params)
nliving <- nrow(GOM.params$model[Type <  2, ])
ndead   <- nrow(GOM.params$model[Type == 2, ])

#Find index of pp groups
pp<- which(groups == "Phytoplankton")

# enaR on initial GOM model ------------------------------------------------------
#Calculate network analysis outputs for original model (balanced)
#Pull diet matrix
diet<-GOM$DC
#Get consumption values by DC*QB*Biomass
QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
for (j in 1:nliving){
  QQ[,j]<-diet[,j]*GOM$QB[j]*GOM$Biomass[j]
}
#Ignore Imports
QQ<-QQ[1:(nliving+ndead),]
colnames(QQ)<-groups[1:nliving]
rownames(QQ)<-groups[1:(nliving+ndead)]
#Sum discards
#Discards<-rowSums(GOM$Discards)
#Discards<-Discards[1:58]
#Calculate flow to detritus
M0<-GOM$PB*(1-GOM$EE)
Detritus<-(M0*GOM$Biomass+GOM$QB*GOM$Biomass*GOM$Unassim)*GOM$DetFate[,1]
#Detritus<-GOM$QB*GOM$Biomass*GOM$Unassim
Detritus<-Detritus[1:(nliving+ndead)]
#Deal with flow to detritus from discards
#Should be equal to all flow to discards minus consumption by SeaBirds(45)
#DetInDisc<-sum(Discards)
#Detritus[58]<-DetInDisc-QQ[58,45]
#Flow to detritus from detritus = 0
Detritus[(nliving+1)]<-0
#Bind diet matrix (QQ) with flow to detritus, discards
QQ<-cbind(QQ,Detritus)
#Calculate exports
#First sum catch
Catch<-rowSums(GOM$Landings)
#Add positive biomass accumulation terms
Export<-Catch+(ifelse(GOM$BA>0,GOM$BA,0))
Export<-Export[1:(nliving+ndead)]
for (i in 1:ndead){
  Export[nliving+i]<-GOM$PB[(nliving+i)]*GOM$Biomass[(nliving+i)]
}
#Calculate respiration
#Assume detritus, discards have 0 respiration
Resp<-((1-GOM$Unassim)*GOM$QB-GOM$PB)*GOM$Biomass
Resp<-ifelse(Resp>0,Resp,0)
Resp<-Resp[1:(nliving+ndead)]
Resp[(nliving+1):(nliving+ndead)]<-0
#Deal with Primary Production
#First, estimate GROSS production = Imports
#P/B in Ecopath model gives NET production
#Ratio of gross:net is going to be fixed based on EMAX
gross_net<-4101.9/3281.5
gross<-gross_net*GOM$PB[1]*GOM$Biomass[1]
Resp[1]<-gross-(GOM$PB[1]*GOM$Biomass[1])
#Calculate imports
#Negative biomass accumulation terms
#Gross primary production
Import<-abs(ifelse(GOM$BA<0,GOM$BA,0))
Import[1]<-gross
Import<-Import[1:(nliving+ndead)]
#Trim biomass
Biomass<-GOM$Biomass[1:(nliving+ndead)]
#Pack the model directly and store
orig.network_GOM<-enaR::pack(flow = QQ,
                         input = Import,
                         export = Export,
                         living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                         respiration = Resp,
                         storage = Biomass)


A<-enaStructure(orig.network_GOM)$A
g<-igraph::graph_from_adjacency_matrix(A)

#get x and y coordinates
#derived from the plotfw function (https://rfrelat.github.io/BalticFoodWeb.html)
nylevel<-7 #determines number of levels along y-axis
n <- vcount(g) #number of vertices
tl<-GOM$TL[1:(nliving+ndead)] #pull trophic level calculations from rpath
bks <- c(0.9, seq(1.9, max(tl), length.out = nylevel))
ynod <- cut(tl, breaks = bks, include.lowest = TRUE, 
            labels = 1:(length(bks)-1)) #assign group to a y-level
maxx <- max(table(ynod)) #looks for max # of groups at any y-level
xnod <- rep(0,n)
for (i in 1:nylevel){
  l <- sum(ynod==i)
  
  ltr <- (l/maxx)**(1/2)*maxx
  if (l>1) {
    xnod[ynod==i] <- seq(-ltr,ltr,length.out = l)
  } else {
    xnod[ynod==i] <- 0
  }
}

coo <- cbind(xnod,tl) #these inform x and y coordinates

#scootch Bacteria to the left to get more space in the bottom of the web
coo[2,1]<-(-10)
coo[31,1]<-7
coo[48,1]<-16

#use ggnetwork to create a network geometry
#storage and flows from original model
n_GOM<-ggnetwork(orig.network_GOM,layout=coo,weights="flow")
#bind with TL info
TL<-as.data.frame(cbind(GOM$TL,groups)) %>% rename(vertex.names=groups,TL=V1)
TL<-TL %>% mutate(TL = as.numeric(TL)) %>% mutate(TL = round(TL,2))
n_GOM<-left_join(n_GOM,TL,by="vertex.names")



# plotting ----------------------------------------------------------------
#Need to think about scaling the models logically -- want to facilitate reasonable side by side view
#set minimum size
min_biomass<-min(GOM$Biomass[1:57])
det_MAB<-max(MAB.rpath$Biomass)
det_GOM<-max(GOM$Biomass)
n_MAB<-n_MAB %>% mutate(storage_adjust = ifelse(storage == det_MAB,100,storage))
#adjust Detritus storage so it doesn't swamp everything
n_MAB <- n_MAB %>% mutate(storage_adjust = ifelse(storage < 0.05,0.05,storage_adjust))
n_GOM <- n_GOM %>% mutate(storage_adjust = ifelse(storage < 0.05,0.05,storage))
n_GOM <- n_GOM %>% mutate(storage_adjust = ifelse(storage == det_GOM,100,storage))

GOM_web<-ggplot(n_GOM,aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(7, "pt"), type = "open"),
             curvature = 0.15,position="jitter",
             aes(color=TL,linewidth=flow)) +
  scale_color_gradientn(colors = turbo(6))+
  scale_linewidth(range = c(0.15,9))+
  geom_nodelabel(aes(label=vertex.names,size=((storage_adjust))),show.legend = F) +
  scale_size(range=c(2.5,10))+
  guides(linewidth=F)+
  annotate("text",x=0.025,y=0.975,label="GOM",size=9)+
  theme_blank(legend.position="none")+
  theme_blank(legend.position=c(0.8,0.2))+
  theme(panel.background = element_rect(fill="#EEEEEEFF"),
        plot.background = element_rect(fill="#EEEEEEFF"),
        legend.background=element_rect(fill="#EEEEEEFF"))

GOM_web

MAB_web<-ggplot(n_MAB,aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(7, "pt"), type = "open"),
             curvature = 0.15,position="jitter",
             aes(color=TL,linewidth=flow)) +
  #scale_color_viridis(option = "turbo")+
  scale_color_gradientn(colors = turbo(6))+
  scale_linewidth(range = c(0.15,9))+
  geom_nodelabel(aes(label=vertex.names,size=((storage_adjust))),show.legend = F) +
  scale_size(range=c(2.5,10))+
  guides(linewidth=F)+
  annotate("text",x=0.025,y=0.975,label="MAB",size=9)+
  theme_blank(legend.position=c(0.9,0.15))+
  theme(panel.background = element_rect(fill="#EEEEEEFF"),
        plot.background = element_rect(fill="#EEEEEEFF"),
        legend.background=element_rect(fill="#EEEEEEFF"))

MAB_web


ggsave(filename = "outputs/webplot_MAB.png",width = 35, height=25,units = c("cm"),plot=MAB_web,dpi=300)

ggsave(filename = "outputs/webplot_GOM.png",width = 35, height=25,units = c("cm"),plot=GOM_web,dpi=300)
