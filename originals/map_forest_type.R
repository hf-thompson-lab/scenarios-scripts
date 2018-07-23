#Monthly NEE spatial analysis in New England under climate change
#This is where forest type maps are built.
#Matthew Duveneck
library(raster);options(scipen=999)#  ;library(flux)
library(lattice)
library(grid) 
scen_rep<-20170110901#301601701
SCEN<-"GrOn"
climate<-"current"
scen_path<-paste0("Y:/LANDIS_OUTPUTS/PNET_178_scenario_",SCEN,"_",climate,"/replicate",scen_rep,"/output/agbiomass/")
setwd(scen_path)

eco_map<-raster("Y:/LANDIS_INPUTS/input_rasters/eco_11_17_2016B.img")
PRJ<-projection(eco_map)
EXT<-extent(eco_map)
eco1<-eco_map>0
years<-c(0,10,20,30,40,50)
species_file<-read.csv("Y:/LANDIS_INPUTS/eco_spp_inc/LANDIS_NE_SPECIES.csv")
species<-species_file$GENUSPEC
species_group<-as.data.frame(cbind(as.character(species),as.character(species_file$spp_group)))
colnames(species_group)<-c("SPCD","SPG")
USG1<-as.character(unique(species_group$SPG))#[-species_group$SPG=="southern_pine"]
USG<-USG1[USG1!="southern_pine"]
eco_df<-as.data.frame(eco1)

USG_short<-c("SF", "NH", "AB","TC","PE","OK","NA")
for (YR in years){
if(YR<100){  YR2010<-2010+YR}
if(YR==100){  YR2010<-2010+YR-10}
    print (YR2010)
    spg_matrix<-matrix(nrow=nrow(eco1)*ncol(eco1), ncol=length(USG)+1)
      for (SG in 1: length(USG)){#For each unique species group.
      sg<-as.character(USG[SG])  #unique species group name
      print (sg)
      spp<-as.character(subset(species_group,species_group$SPG%in%sg)$SPCD)#species in the group
    #  print(length(spp))
      print(spp)
      spp_stack<-sum(stack(paste0(spp,"/AGBiomass_",YR,".img")))  
      spg_matrix[,SG]<-as.data.frame(spp_stack)[,1]
      }#end of each species group loop
    spg_matrix[,SG+1]<-eco_df[,1]  #place ecoregion forested mask on end of matrix to make type "7" to be no data.
    FT_data_frame<-max.col(spg_matrix, ties.method = c("last"))#which forest type.
    FT_matrix<-matrix(FT_data_frame, nrow=nrow(eco_map),ncol=ncol(eco_map), byrow=T)
    FT_raster<-raster(FT_matrix)  
    projection(FT_raster)<-PRJ;extent(FT_raster)<-EXT
    #plot(FT_raster, main=YR2010, axes=F, breaks=seq(0.5,7.5,1))#, col=1:7)
    writeRaster(FT_raster, file=paste0("forest_type_yr",YR,".img"), overwrite=T)
    freq_ft<-table(FT_data_frame)#Frequency of each forest type
    names(freq_ft)<-USG_short
    barplot(freq_ft, main=YR2010, cex.names=1)
}