#2/2/2017 attempt to get at dissimilarity of rt maps at year 50.

library(raster);library(vegan);options(scipen=999);library(TeachingDemos)
eco_map<-raster("Y:/LANDIS_INPUTS/input_rasters/eco_11_17_2016B.img")
PRJ<-projection(eco_map);EXT<-extent(eco_map)

species_file<-read.csv("Y:/LANDIS_INPUTS/eco_spp_inc/LANDIS_NE_SPECIES.csv")
species<-species_file$GENUSPEC

scen_rep<-20170110901#301601701#1101701
RDA_REP<-20
climates<-c("current","HADGE")#,"CESM1","CCSM4","MPIMLR")#,"CESM1")#,"HADGE","CCSM4")
land_uses<-c("GrOw", "Dev", "Har", "HarDev")
scenarios<-c(paste(climates[1], land_uses, sep="_"),paste(climates[2], land_uses, sep="_"))[-1]
par(new=F)
BGC<-"white"
PLC<-"black"


#############################################################################################
#############################################################################################
#############################################################################################


cont_dir<-paste0("Y:/LANDIS_OUTPUTS/PNET_178_scenario_RT_current/replicate",scen_rep,"/")
spp_contol<-stack(paste0(cont_dir,RDA_REP,"SPP_agb_subtract_GrOw_yr",50,".img"))[[-33]]
sum_control<-sum(spp_contol)


SPP_1<-sum_control>0#bionary; active cells==1, all others==NA.
SPP_1df<-as.data.frame(SPP_1)
SPP_1df[is.na(SPP_1df)]<-0
SV<-SPP_1df[,1]==1#Active cell selection vector.
full_matrix<-matrix(nrow=ncell(SPP_1), ncol=1)#length(climates))

#dim(full_matrix)





sp_contdf<-as.data.frame(spp_contol)[SV,]
sp_contdf_relative<-sp_contdf/rowSums(sp_contdf)
sp_contdf_relative[is.na(sp_contdf_relative)]<-0


#colnames(BM)<-climates
for (CP in 1: length(scenarios)){
  clim_LU<-scenarios[CP]
  clim<-gsub( "_.*$", "", clim_LU )
  LU_scen<-gsub( ".*_", "", clim_LU )
  print (LU_scen)
  print(clim)
  clim_path<-paste0("Y:/LANDIS_OUTPUTS/PNET_178_scenario_RT_",clim,"/replicate",scen_rep,"/")
  SPP_SCEN<-stack(paste0(clim_path,RDA_REP,"SPP_agb_subtract_",LU_scen,"_yr50.img"))[[-33]]
  SPP_SCENdf<-as.data.frame(SPP_SCEN)[SV,] 
  SPP_SCENdf_relative<-SPP_SCENdf/rowSums(SPP_SCENdf)
  SPP_SCENdf_relative[is.na(SPP_SCENdf_relative)]<-0
  BM<-matrix(nrow=nrow(sp_contdf), ncol=1)#length(climates))
  for (cell in 1:nrow(sp_contdf)){#for every active cell.
    sp1<-t(sp_contdf_relative[cell,])
    sp2<-t(SPP_SCENdf_relative[cell,])
    community_matrix<-t(cbind(sp1,sp2))
    rownames(community_matrix)<-1:nrow(community_matrix)
    BC<-as.data.frame(as.matrix(vegdist(x=community_matrix, method="bray", diag=F)))[1,2]
    prop<-  cell/nrow(sp_contdf)*100
    print(c(CP,prop,cell,BC))
    BM[cell]<-BC      
  }#end of cell loop 
  save_file<-paste0("Y:/resistance_measurement/rda_files_feb_2017_dissimilarity/DISS",scen_rep,"_",RDA_REP,"_",clim_LU,"_4_2017_PNET_dissimilarity_relative_total_matrix.Rda")    
  save(BM, file=save_file)
}#end of scenarios
########################################################################################################33

#Build Rasters and plot maps

pal2 <- colorRampPalette(c("green", "red"))
numshades <- 10
colors2 <- pal2(numshades)
brks=seq(0,1,.1)
for (SCP1 in 1: length(scenarios)){
  # CL<-1
  SCP<-scenarios[SCP1]
  print(SCP)
  diss_out_path<-paste0("Y:/temporary_figures/diss_",SCP,"_2_3_2017_RT.jpg")
  jpeg(filename=diss_out_path, bg=BGC, width = 9, height = 12, units = "in", pointsize = 16, res= 1200) # open plot output to file
  par(mfrow=c(1,1))
  par(mar=c(0, 0, 2, 0))
  par(oma=c(0,0,0,0))
  in_file<-paste0("Y:/resistance_measurement/rda_files_feb_2017_dissimilarity/DISS",scen_rep,"_",RDA_REP,"_",SCP,"_4_2017_PNET_dissimilarity_relative_total_matrix.Rda")    
  load(in_file)
  head(BM)
  mean_diss <- round(mean(BM[!is.na(BM)]),5)
  mean_diss
  full_matrix[SV,]<-BM
  diss_rast<-raster(matrix(full_matrix, nrow=nrow(eco_map), ncol=ncol(eco_map), byrow=T))
  projection(diss_rast)<-PRJ;extent(diss_rast)<-EXT
  plot(diss_rast, main=paste(SCP, mean_diss),col=colors2, breaks=brks, axes=F)
  #freq(diss_rast)
  
  writeRaster(diss_rast, file=paste0("Y:/resistance_measurement/rda_files_feb_2017_dissimilarity/raster",RDA_REP,"_",scen_rep,SCP,"_RT_diss_relative.img"), overwrite=T)
  dev.off()
  }#end of climates






