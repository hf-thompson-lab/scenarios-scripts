#RCN scenario outputs for report
#Matthew Duveneck 10/12/2016
library(raster);library(vegan); library(RColorBrewer); library(utils)

#scen_rep<-"1002701"#number cooredponsing to date landis simulation started
scen_rep<-"20170110901"#  301601701"#1101701" 
rda_num<-20
options(scipen=999);rasterOptions(tmpdir="Y:/raster_package_temp_files/");removeTmpFiles() 
climates<-c("HADGE")#current and HADGE for RT, just hadge for RCN scenarios.
#climates<-c("current", "HADGE")
scenarios<-c("RT","YC","GG","GA","CC")
#scenarios<-c("YC","GG","GA")
#scenarios<-c("RT")
#scenarios<-c("YC")
#scenarios<-c("GA")
#scenarios<-c("CC")
#scenarios<-c("GG")

NE_states1<-raster("Y:/new_england_overview_gis/ne_states_raster.img") #New England states layer.
state_names<-c("ME", "VT", "NH", "MA", "CT", "RI")
eco_map<-raster("Y:/LANDIS_INPUTS/input_rasters/eco_11_17_2016B.img")
projection(eco_map)

NE_states2<-crop(NE_states1, eco_map)

landuses<-c("GrOw", "LU", "TH", "THLU")
eco_one<-eco_map>0
NE_states<-NE_states2*eco_one
CBSA1<-raster("Y:/land_use_dinamica_output/cbsa_raster2.img")
CBSA<-crop(CBSA1,eco_one)*eco_one

eco_df<-as.data.frame(eco_map)
eco_df[is.na(eco_df)]<-0
SV<-eco_df>0 #Active cell selection vector.
LU_temp_dir1<-"Y:/land_use_dinamica_output/September2016_250m/"
time_steps<-seq(0,50,10)
#time_steps<-50
SPP_file<-read.csv("Y:/LANDIS_INPUTS/eco_spp_inc/LANDIS_NE_SPECIES.CSV")
#SPP_file<-read.csv("Y:/LANDIS_INPUTS/disturbance/LANDIS_NE_SPECIES_Harvest.csv")
SPP<-as.character(SPP_file$GENUSPEC)#list of landis-ii species
USG<-as.character(unique(SPP_file$spp_group))
PRJ<-projection(NE_states);EXT<-extent(NE_states)
projection(CBSA)<-PRJ;extent(CBSA)<-EXT

usg_num<-0
AGBrow_number<-0
row_number<-0
diversity_row_number<-0
removed_row_number<-0
old_forest_row_number<-0
harvested_TABLE_matrix<-NULL#Harvest frequency table for Josh.
  for (SC in 1: length(scenarios)){
  SCEN<-scenarios[SC]
  print (SCEN)
  
  
  state_time_agb<-as.data.frame(matrix(nrow=length(time_steps)*length(climates), ncol=length(state_names)+5+32))
  colnames(state_time_agb)<-c("scen","clim","TS","landuse",state_names,"total", 1:32)
  state_time_diversity<-as.data.frame(matrix(nrow=length(time_steps)*length(climates), ncol=length(state_names)+5))
  colnames(state_time_diversity)<-c("scen","clim","TS","landuse",state_names,"total")
  state_time_spp_group<-as.data.frame(matrix(nrow=length(time_steps)*length(USG)*length(climates), ncol=length(state_names)+6))
  colnames(state_time_spp_group)<-c("scen","clim","TS","USG","landuse",state_names,"total")
  state_time_agb_remove<-as.data.frame(matrix(nrow=(length(time_steps)-1)*length(climates), ncol=length(state_names)+5))
  colnames(state_time_agb_remove)<-c("scen","clim","TS","landuse",state_names,"total")
  state_time_age200<-as.data.frame(matrix(nrow=length(time_steps)*length(climates), ncol=length(state_names)+5))
  colnames(state_time_age200)<-c("scen","clim","TS","landuse",state_names, "total")
  
  
  for (CL in 1: length (climates)){
    climate<-climates[CL]
    print(climate)
  print(climates)
  print(scenarios)
  

  
  scen_path<-paste0("Y:/LANDIS_OUTPUTS/PNET_178_scenario_",SCEN,"_",climate,"/replicate",scen_rep,"/")
  cum_dev_pre_DF<-matrix(nrow=ncell(eco_one), ncol=6)
  for (TS1 in 1: length (time_steps)){
  TS2<-TS1-1#use for land use change dinamica map conversions
 
  row_number<-row_number+1 
  diversity_row_number<-diversity_row_number+1
  AGBrow_number<-AGBrow_number+1
  removed_row_number<-removed_row_number+1
  old_forest_row_number<-old_forest_row_number+1
  TS<-time_steps[TS1]
  print(c(SCEN,climate,TS))
  setWindowTitle(paste(SCEN, climate, TS))
##  #  # AGB
  print ("Grow only agb")
  GrOw_dir<-paste0("Y:/LANDIS_OUTPUTS/PNET_178_scenario_GrOn_",climate,"/replicate",scen_rep,"/") #grow only scenario
  GrOw_spp_biom_layers1<-paste0(GrOw_dir,"output/agbiomass/",SPP,"/AGBiomass_",TS,".img")
  AGB_layer1<-sum(stack(GrOw_spp_biom_layers1))
  original_spp_stack1<-stack(GrOw_spp_biom_layers1)
  projection(original_spp_stack1)<-PRJ;extent(original_spp_stack1)<-EXT
  projection(AGB_layer1)<-PRJ;extent(AGB_layer1)<-EXT
  original_spp_stack<-original_spp_stack1*eco_one
  AGB_Original<-AGB_layer1*eco_one#TOTAL AGB
  #F2A_dyn250_GrOw<-raster(paste0(LU_temp_dir1,"GrOn","year",TS2,"cummulative_F2A_mean_agg50_resam_bilin_T_250_September26_2016.img"))
  #F2LD_dyn250_GrOw<-raster(paste0(LU_temp_dir1,"GrOn","year",TS2,"cummulative_F2LD_mean_agg50_resam_bilin_T_250_September26_2016.img"))
  #F2HD_dyn250_GrOw<-raster(paste0(LU_temp_dir1,"GrOn","year",TS2,"cummulative_F2HD_mean_agg50_resam_bilin_T_250_September26_2016.img"))
#CUM_DEV_GrOw<-(F2A_dyn250_GrOw+(F2HD_dyn250_GrOw*.94)+F2LD_dyn250_GrOw*.5)*eco_one  #Development map is =(1*agriculture)+(.8*residential) to account for street trees in residential development.
#  CUM_DEV_GrOw<-raster(paste0(LU_temp_dir1,"GrOnyear",TS2,"cumulative_development250_1_20_2017.img"))*eco_one
  eco_zero<-eco_one-1
    CUM_DEV_preNA<-(raster(paste0(LU_temp_dir1,SCEN,"year",TS2,"cumulative_development250_6_7_2017.img")))
  
  fun_NA_zero <- function(x) { x[is.na(x)] <- 0; return(x)}
  CUM_DEV_pre_zero <- calc(CUM_DEV_preNA, fun_NA_zero)
  CUM_DEV_pre<-CUM_DEV_pre_zero*eco_one
  cum_dev_df_singleyear<-as.data.frame(CUM_DEV_pre)[,1]
  cum_dev_pre_DF[,TS1]<-cum_dev_df_singleyear
  SV_active<-!is.na(cum_dev_pre_DF[,1])
  cum_dev_active<-  cum_dev_pre_DF[SV_active,]
  cum_dev_active_max<-apply(cum_dev_active, 1, function(x) max(x, na.rm = TRUE))
  cum_dev_max<-matrix(nrow=ncell(eco_one), ncol=1)
  cum_dev_max[SV_active,1]<-cum_dev_active_max
  cum_dev_max_mat<-matrix(cum_dev_max, nrow=nrow(eco_one), ncol=ncol(eco_one), byrow=T)
  cum_dev_max_rast<-raster(cum_dev_max_mat)#, nrows=nrow(eco_one), ncols=ncol(eco_one))
  projection(cum_dev_max_rast)<-PRJ;extent(cum_dev_max_rast)<-EXT
  #THIS DID NOT WORK #CDC<-get(paste0("CUM_DEV_pre_correct_",TS2))
  if(TS2==0){
    CUM_DEV<-cum_dev_max_rast
    CUM_DEV_GrOw<-CUM_DEV
  }
  if(TS2>0){
    prev_year<-TS2-1
    #THIS DID NOT WORK    #CDP<-get(paste0("CUM_DEV_pre_correct_",prev_year)) #THIS DID NOT WORK
    #THIS DID NOT WOR K# CUM_DEV<-overlay(CDC,CDP,fun=function(x,y)(ifelse(x<y,y,x)))  #THIS DID NOT WORK
    #THIS DID NOT WORK # CUM_DEV<-max(stack(list(CDC,CDP)), na.rm=T)#THIS DID NOT WORK
    CUM_DEV<-cum_dev_max_rast
  }
  #assign(paste0("cum_dev_post",TS2),CUM_DEV)
  AGB_GrOwn<-AGB_Original-(AGB_Original*CUM_DEV_GrOw)#biomass remaining after year zero dev, subtracted.
  SPP_GrOwn<-original_spp_stack-(original_spp_stack*CUM_DEV_GrOw)
writeRaster(SPP_GrOwn, file=paste0(scen_path,rda_num,"SPP_agb_subtract_GrOw_yr",TS,".img"), overwrite=T,  bandorder='BIL')  
  mean_landscape_agb<-cellStats(AGB_GrOwn,stat='mean', na.rm=T)
  state_zone_AGB<-as.data.frame(zonal(AGB_GrOwn,NE_states, fun="mean"), na.rm=T)
  cbsa_zonal<-as.data.frame(zonal(AGB_GrOwn,CBSA,fun="mean"), na.rm=T)
  state_time_agb[AGBrow_number,]<-c(SC,CL,TS,0,state_zone_AGB$mean,mean_landscape_agb,cbsa_zonal$mean)
writeRaster(AGB_GrOwn,paste0(scen_path,rda_num,"AGB_GrOw_subtract_yr", TS,".img"), overwrite=T)

print ("landuse substraction AGB")  
AGBrow_number<-AGBrow_number+1
AGB_develop<-AGB_Original-(AGB_Original*CUM_DEV)#Biomass remaining after development subtracted.
SPP_Dev<-original_spp_stack-(original_spp_stack*CUM_DEV)

writeRaster(SPP_Dev, file=paste0(scen_path,rda_num,"SPP_agb_subtract_Dev_yr",TS,".img"), overwrite=T,  bandorder='BIL') 

DEV_REMOVED_cumulative<-AGB_GrOwn-AGB_develop
#plot(DEV_REMOVED_cumulative, main=paste("Dev_removed",TS))  
writeRaster(DEV_REMOVED_cumulative, file=paste0(scen_path,"harvest/",rda_num,"removed_agb_by_develop_CUMULATIVE_TO_TS",TS,"_2_28_2017.img"),overwrite=T,NAflag=0,datatype='FLT8S')  

  LU_mean_landscape_agb<-cellStats(AGB_develop, stat='mean')
  LU_state_zone_remain_AGB<-as.data.frame(zonal(AGB_develop,NE_states, fun="mean"))
  LU_cbsa_zonal<-as.data.frame(zonal(AGB_develop,CBSA,fun="mean"), na.rm=T)
  state_time_agb[AGBrow_number,]<-c(SC,CL,TS,1,LU_state_zone_remain_AGB$mean,LU_mean_landscape_agb,LU_cbsa_zonal$mean )
writeRaster(AGB_develop,paste0(scen_path,rda_num,"AGB_Dev_subtract_yr", TS,".img"), overwrite=T)
  
print ("timber harvest substraction AGB")
  AGBrow_number<-AGBrow_number+1
  setwd(scen_path)
  TH_spp_biom_layers1<-paste0("output/agbiomass/",SPP,"/AGBiomass_",TS,".img")
  TH_AGB_layer1<-sum(stack(TH_spp_biom_layers1))
  original_TH_spp_stack1<-stack(TH_spp_biom_layers1)
  projection(original_TH_spp_stack1)<-PRJ;extent(original_TH_spp_stack1)<-EXT
  projection(TH_AGB_layer1)<-PRJ;extent(TH_AGB_layer1)<-EXT
  TH_AGB_layer<-TH_AGB_layer1*eco_one #TOTAL AGB
  original_TH_spp_stack<-original_TH_spp_stack1*eco_one
  TH_remaining_AGB<-TH_AGB_layer*(1-CUM_DEV_GrOw)
  SPP_Har<-original_TH_spp_stack-(original_TH_spp_stack*CUM_DEV_GrOw)
  
  writeRaster(SPP_Har, file=paste0(scen_path,rda_num,"SPP_agb_subtract_Har_yr",TS,".img"), overwrite=T,  bandorder='BIL') 
  
  TH_mean_landscape_agb<-cellStats(TH_remaining_AGB, stat='mean')
  TH_state_zone_remain_AGB<-as.data.frame(zonal(TH_remaining_AGB,NE_states, fun="mean"))
  TH_cbsa_zonal<-as.data.frame(zonal(TH_remaining_AGB,CBSA,fun="mean"), na.rm=T)
  state_time_agb[AGBrow_number,]<-c(SC,CL,TS,2,TH_state_zone_remain_AGB$mean,TH_mean_landscape_agb,TH_cbsa_zonal$mean)
  writeRaster(TH_remaining_AGB,paste0(scen_path,rda_num,"AGB_Har_subtract_yr", TS,".img"), overwrite=T)
  
print ("timber harvest & Land Use substraction AGB")
  AGBrow_number<-AGBrow_number+1
  THLU_remaining_AGB<-TH_AGB_layer*(1-CUM_DEV)
  SPP_HarDev<-original_TH_spp_stack-(original_TH_spp_stack*CUM_DEV)
  
  writeRaster(SPP_HarDev, file=paste0(scen_path,rda_num,"SPP_agb_subtract_HarDev_yr",TS,".img"), overwrite=T,  bandorder='BIL') 
  THLU_mean_landscape_agb<-cellStats(THLU_remaining_AGB, stat='mean')
  THLU_state_zone_remain_AGB<-as.data.frame(zonal(THLU_remaining_AGB,NE_states, fun="mean"))
  THLU_cbsa_zonal<-as.data.frame(zonal(THLU_remaining_AGB,CBSA,fun="mean"), na.rm=T)
  state_time_agb[AGBrow_number,]<-c(SC,CL,TS,3,THLU_state_zone_remain_AGB$mean,THLU_mean_landscape_agb,THLU_cbsa_zonal$mean)
  writeRaster(THLU_remaining_AGB,paste0(scen_path,rda_num,"AGB_HarDev_subtract_yr", TS,".img"), overwrite=T)

#   ##  #  # #Diversity
   print (c(SCEN,"start Grow Only Diversity"))
   GrOw_spp_biom_layers<-stack(GrOw_spp_biom_layers1)
   projection(GrOw_spp_biom_layers)<-PRJ;extent(GrOw_spp_biom_layers)<-EXT
  GrOw_spp_biom_df<-as.data.frame(GrOw_spp_biom_layers*(1-CUM_DEV_GrOw))
  GrOw_spp_biom_df_active<-GrOw_spp_biom_df[SV,]
  GrOw_shann_div<-diversity(GrOw_spp_biom_df_active,index="shannon", base=exp(1))
  GrOw_shann_div_df<-data.frame(matrix(nrow=nrow(GrOw_spp_biom_df), ncol=1))
  GrOw_shann_div_df[SV,]<-GrOw_shann_div
  GrOw_shann_div_df[is.na(GrOw_shann_div_df)]<-0
  GrOw_shann_div_rast1<-raster(nrows=nrow(AGB_Original),ncols=ncol(AGB_Original))
  extent(GrOw_shann_div_rast1)<-EXT;projection(GrOw_shann_div_rast1)<-PRJ
  GrOw_shann_div_rast<-setValues(GrOw_shann_div_rast1, GrOw_shann_div_df[,1])*eco_one
  writeRaster(GrOw_shann_div_rast, file=paste0(rda_num,SCEN,TS,"GrOw_shannon_div.img"), overwrite=T, datatype="FLT8S", NAflag=0)
  GrOw_state_zone_diversity<-as.data.frame(zonal(GrOw_shann_div_rast,NE_states, fun="mean"))
  GrOw_mean_landscape_diversity<-cellStats(GrOw_shann_div_rast,stat='mean', na.rm=T)
  state_time_diversity[diversity_row_number,]<-c(SC, CL,TS,0,GrOw_state_zone_diversity$mean,GrOw_mean_landscape_diversity)

print ("start LU Diversity")
  diversity_row_number<-diversity_row_number+1
  LU_spp_biom_layers<-GrOw_spp_biom_layers*(1-CUM_DEV)
  LU_spp_biom_df<-as.data.frame(LU_spp_biom_layers)
  LU_spp_biom_df_active<-LU_spp_biom_df[SV,]
  LU_shann_div<-diversity(LU_spp_biom_df_active,index="shannon", base=exp(1))
  LU_shann_div_df<-data.frame(matrix(nrow=nrow(LU_spp_biom_df), ncol=1))
  LU_shann_div_df[SV,]<-LU_shann_div
  LU_shann_div_df[is.na(LU_shann_div_df)]<-0
  LU_shann_div_rast1<-raster(nrows=nrow(AGB_Original),ncols=ncol(AGB_Original))
  extent(LU_shann_div_rast1)<-EXT;projection(LU_shann_div_rast1)<-PRJ
  LU_shann_div_rast<-setValues(LU_shann_div_rast1, LU_shann_div_df[,1])*eco_one
  writeRaster(LU_shann_div_rast, file=paste0(rda_num,SCEN,TS,"LU_shannon_div.img"), overwrite=T, datatype="FLT8S", NAflag=0)
  LU_state_zone_diversity<-as.data.frame(zonal(LU_shann_div_rast,NE_states, fun="mean"))
  LU_mean_landscape_diversity<-cellStats(LU_shann_div_rast,stat='mean', na.rm=T)
  state_time_diversity[diversity_row_number,]<-c(SC, CL,TS,1,LU_state_zone_diversity$mean,LU_mean_landscape_diversity)

print ("start TH Diversity")
  diversity_row_number<-diversity_row_number+1
  TH_spp_biom_layers2<-stack(TH_spp_biom_layers1)
  projection(TH_spp_biom_layers2)<-PRJ;extent(TH_spp_biom_layers2)<-EXT
  TH_spp_biom_layers<-TH_spp_biom_layers2*(1-CUM_DEV_GrOw)
  TH_spp_biom_df<-as.data.frame(TH_spp_biom_layers)
  TH_spp_biom_df_active<-TH_spp_biom_df[SV,]
  TH_shann_div<-diversity(TH_spp_biom_df_active,index="shannon", base=exp(1))
  TH_shann_div_df<-data.frame(matrix(nrow=nrow(TH_spp_biom_df), ncol=1))
  TH_shann_div_df[SV,]<-TH_shann_div
  TH_shann_div_df[is.na(TH_shann_div_df)]<-0
  TH_shann_div_rast1<-raster(nrows=nrow(AGB_Original),ncols=ncol(AGB_Original))
  extent(TH_shann_div_rast1)<-EXT;projection(TH_shann_div_rast1)<-PRJ
  TH_shann_div_rast<-setValues(TH_shann_div_rast1, TH_shann_div_df[,1])*eco_one
  writeRaster(TH_shann_div_rast, file=paste0(rda_num,SCEN,TS,"TH_shannon_div.img"), overwrite=T, datatype="FLT8S", NAflag=0)
  TH_state_zone_diversity<-as.data.frame(zonal(TH_shann_div_rast,NE_states, fun="mean"))
  TH_mean_landscape_diversity<-cellStats(TH_shann_div_rast,stat='mean', na.rm=T)
  state_time_diversity[diversity_row_number,]<-c(SC, CL,TS,2,TH_state_zone_diversity$mean,TH_mean_landscape_diversity)

print ("start TH and LU Diversity")
  diversity_row_number<-diversity_row_number+1
  THLU_spp_biom_layers<-TH_spp_biom_layers2*(1-CUM_DEV)
  THLU_spp_biom_df<-as.data.frame(THLU_spp_biom_layers)
  THLU_spp_biom_df_active<-THLU_spp_biom_df[SV,]
  THLU_shann_div<-diversity(THLU_spp_biom_df_active,index="shannon", base=exp(1))
  THLU_shann_div_df<-data.frame(matrix(nrow=nrow(THLU_spp_biom_df), ncol=1))
  THLU_shann_div_df[SV,]<-THLU_shann_div
  THLU_shann_div_df[is.na(THLU_shann_div_df)]<-0
  THLU_shann_div_rast1<-raster(nrows=nrow(AGB_Original),ncols=ncol(AGB_Original))
  extent(THLU_shann_div_rast1)<-EXT;projection(THLU_shann_div_rast1)<-PRJ
  THLU_shann_div_rast<-setValues(THLU_shann_div_rast1, THLU_shann_div_df[,1])*eco_one
  writeRaster(THLU_shann_div_rast, file=paste0(rda_num,SCEN,TS,"THLU_shannon_div.img"), overwrite=T, datatype="FLT8S", NAflag=0)
  THLU_state_zone_diversity<-as.data.frame(zonal(THLU_shann_div_rast,NE_states, fun="mean"))
  THLU_mean_landscape_diversity<-cellStats(THLU_shann_div_rast,stat='mean', na.rm=T)
  state_time_diversity[diversity_row_number,]<-c(SC, CL,TS,3,THLU_state_zone_diversity$mean,THLU_mean_landscape_diversity)


# # ####spp group
  print (c(SCEN, TS,"starting forest type"))
  for (LA1 in 1: length(landuses)){
    LA<-LA1-1
    landuse<-landuses[LA1]
  for (SG1 in 1: length(USG)){
    usg_num<-usg_num+1
    SG<-as.character(USG[SG1])
    print (c(TS,SG, landuses[LA1]))
    spp_in_group<-as.numeric(row.names(subset(SPP_file, SPP_file$spp_group==SG)))#$GENUSPEC)
    get_landuse_spp_biom_layers<-get(paste0(landuse,"_spp_biom_layers"))
       spp_group_layers<-subset(get_landuse_spp_biom_layers,spp_in_group)
  #  plot(spp_group_layers, main=landuse)

  if(nlayers(spp_group_layers)==1){group_AGB_layer<-as.data.frame(spp_group_layers)}
  if(nlayers(spp_group_layers)>1){group_AGB_layer<-as.data.frame(sum(stack(spp_group_layers)))}
    group_AGB_layer[is.na(group_AGB_layer)]<-0
    spp_group_df_active<-group_AGB_layer[SV,]
    LA_totalspp<-rowSums(as.data.frame(sum(stack(get_landuse_spp_biom_layers))))
    LA_totalspp_active<-LA_totalspp[SV]
    prop_spp_group<-spp_group_df_active/LA_totalspp_active
    all_cell_spp_group<-data.frame(matrix(nrow=nrow(group_AGB_layer), ncol=1))
    all_cell_spp_group[SV,]<-prop_spp_group
    all_cell_spp_group[is.na(all_cell_spp_group)]<-0
    spp_group_rast<-raster(nrows=nrow(AGB_Original),ncols=ncol(AGB_Original))
    extent(spp_group_rast)<-EXT;projection(spp_group_rast)<-PRJ
    spp_group_rast<-setValues(spp_group_rast, all_cell_spp_group[,1])*eco_one
  writeRaster(spp_group_rast, file=paste0(rda_num,SCEN,TS,SG, landuse,"proportion.img"), overwrite=T, datatype="FLT8S", NAflag=0)
    state_zone_spp_group<-as.data.frame(zonal(spp_group_rast,NE_states, fun="mean"))
    mean_spp_group_landscape<-cellStats( spp_group_rast, stat='mean')
    state_time_spp_group[usg_num,]<-c(SC, CL,TS,SG1,LA,state_zone_spp_group$mean,mean_spp_group_landscape)
    }#end of forest type loop
}#end of land use loop


   ###REMOVED BY HARVEST

  if (TS>5){
print (c("Grow Only  harvest removal (none)", TS))
    GrOw_state_zone_AGB_remove<-as.data.frame(rep(0, length(state_names)))
    colnames(GrOw_state_zone_AGB_remove)<-"mean"
    GrOw_mean_landscape_remove_AGB<-0
    state_time_agb_remove[removed_row_number,]<-c(SC, CL,TS,0,GrOw_state_zone_AGB_remove$mean,GrOw_mean_landscape_remove_AGB)
print ("LU harvest removal (none)")
    removed_row_number<-removed_row_number+1
    state_time_agb_remove[removed_row_number,]<-c(SC, CL,TS,1,GrOw_state_zone_AGB_remove$mean,GrOw_mean_landscape_remove_AGB)
print ("TH removal")
    removed_row_number<-removed_row_number+1
    setwd(scen_path)
    agb_removedT1<-raster(paste0("harvest/harvest_biomass_removed_",TS-5,".img")) #harvest is in five year increments
    agb_removedT2<-raster(paste0("harvest/harvest_biomass_removed_",TS,".img"))#so add mid-decade year to total.

    TH_agb_removed1<-(agb_removedT1+agb_removedT2)
    projection(TH_agb_removed1)<-PRJ;extent(TH_agb_removed1)<-EXT

    funNA_zero <- function(x) { x[is.na(x)] <- 0; return(x)}
    #TH_agb_removed2 <- calc(TH_agb_removed1, funNA_zero)



    TH_agb_removed3<-TH_agb_removed1*eco_one*(1-CUM_DEV_GrOw)
    TH_agb_removed<-calc(TH_agb_removed3, funNA_zero)
    TH_state_zone_AGB_remove<-as.data.frame(zonal(TH_agb_removed,NE_states, fun="mean"))
    TH_mean_landscape_remove_AGB<-cellStats(TH_agb_removed, stat = "mean")

    state_time_agb_remove[removed_row_number,]<-c(SC, CL,TS,2,TH_state_zone_AGB_remove$mean,TH_mean_landscape_remove_AGB)

print ("THLU removed biomass from")
  removed_row_number<-removed_row_number+1
  THLU_agb_removed3<-TH_agb_removed1*eco_one*(1-CUM_DEV)
  THLU_agb_removed<-calc(THLU_agb_removed3, fun=funNA_zero)
  THLU_removed<-THLU_agb_removed>0
  #test <- funNA_zero(THLU_agb_removed3)

  writeRaster(THLU_agb_removed, file=paste0("harvest/harvested_agb_timestep_",rda_num,TS,"_post_dev_subtractNoNA.img"),overwrite=T,datatype='FLT8S')
  THLU_state_zone_AGB_remove<-as.data.frame(zonal(THLU_agb_removed,NE_states, fun="mean"))
  THLU_mean_landscape_remove_AGB<-cellStats(THLU_agb_removed, stat = "mean")
  state_time_agb_remove[removed_row_number,]<-c(SC, CL,TS,3,THLU_state_zone_AGB_remove$mean,THLU_mean_landscape_remove_AGB)

  
  #frequency of forest for Josh.
  harvested_pixels<-as.data.frame(zonal(THLU_removed,NE_states, fun=sum))
  harvested_HA<-harvested_pixels$value*6.25
  harvested_TABLE<-cbind(SCEN,climate,TS,state_names,harvested_HA)
  harvested_TABLE_matrix<-rbind(harvested_TABLE_matrix,harvested_TABLE)

  }#End of if yr 5 or later


  #######AGE
  #
  print ("starting old forest")
  ####  ####  AGB_GrOwn<-AGB_Original-(AGB_Original*CUM_DEV_GrOw)#biomass remaining after year zero dev, subtracted.
    age_Grow_orig<-raster(paste0(GrOw_dir,"output/AgeDist/Age",TS,".img"))
    extent(age_Grow_orig)<-EXT;projection(age_Grow_orig)<-PRJ
    age_GrOw200_orig<-age_Grow_orig>=200*eco_one

    age_harv_orig<-raster(paste0(scen_path,"output/AgeDist/Age",TS,".img"))
    extent(age_harv_orig)<-EXT;projection(age_harv_orig)<-PRJ
    age_harv200_orig<-age_harv_orig>=200*eco_one

  print ("Grow only old forests")
     age_GrOw200<-age_GrOw200_orig-(age_GrOw200_orig*CUM_DEV_GrOw)#
     state_zone_age_GrOw200<-as.data.frame(zonal(age_GrOw200,NE_states, fun="sum"))*6.25
     sum_landscape_old_forest_GrOw<-cellStats(age_GrOw200*6.25,stat='sum')
     state_time_age200[old_forest_row_number,]<-c(SC, CL,TS,0,state_zone_age_GrOw200$sum,sum_landscape_old_forest_GrOw)
     old_forest_row_number<-old_forest_row_number+1
     writeRaster(age_GrOw200,paste0(scen_path,rda_num,"age_GrOw200_", TS,".img"), overwrite=T)
  print ("Land Use (development) old forests")
     age_DEV200<-age_GrOw200_orig-(age_GrOw200_orig*CUM_DEV)#
     state_zone_age_DEV200<-as.data.frame(zonal(age_DEV200,NE_states, fun="sum"))*6.25
     sum_landscape_old_forest_DEV<-cellStats(age_DEV200*6.25,stat='sum')
     state_time_age200[old_forest_row_number,]<-c(SC, CL,TS,1,state_zone_age_GrOw200$sum,sum_landscape_old_forest_GrOw)
     old_forest_row_number<-old_forest_row_number+1
     writeRaster(age_DEV200,paste0(scen_path,rda_num,"age_Dev200_", TS,".img"), overwrite=T)

   print ("timber harvest old forests")
    age_harv200<-age_harv200_orig-(age_harv200_orig*CUM_DEV_GrOw)#
    state_zone_age_harv200<-as.data.frame(zonal(age_harv200,NE_states, fun="sum"))*6.25
    sum_landscape_old_forest_harv<-cellStats(age_harv200*6.25,stat='sum')
    state_time_age200[old_forest_row_number,]<-c(SC, CL,TS,2,state_zone_age_harv200$sum,sum_landscape_old_forest_harv)
    old_forest_row_number<-old_forest_row_number+1
    writeRaster(age_harv200,paste0(scen_path,rda_num,"age_Har200_", TS,".img"), overwrite=T)

  print ("timber harvest and development old forests")
  age_harvdev200<-age_harv200_orig-(age_harv200_orig*CUM_DEV)#
  state_zone_age_harvdev200<-as.data.frame(zonal(age_harvdev200,NE_states, fun="sum"))*6.25
  sum_landscape_old_forest_harvdev<-cellStats(age_harvdev200*6.25,stat='sum')
  state_time_age200[old_forest_row_number,]<-c(SC, CL,TS,3,state_zone_age_harvdev200$sum,sum_landscape_old_forest_harvdev)
  writeRaster(age_harvdev200,paste0(scen_path,rda_num,"age_HarDev200_", TS,".img"), overwrite=T)
   
  }#END OF TIME STEP LOOP.
  }#climate loop.  

  
#rda_dir<-"Y:/Silviculture_new_england/RCN_scenarios/summary_rcn_scenario_rdata/"
rda_dir<-paste0("D:/Plisinski/Dinamica_Runs/SEQUENCE_",SCEN,"/data/Landis_Outputs/")
save(state_time_agb,file=paste0(rda_dir,SCEN, paste(climates, collapse=""),rda_num,"state_time_agb_",scen_rep,paste(time_steps,collapse=""),".rda" ))
save(state_time_agb_remove,file=paste0(rda_dir,SCEN,paste(climates, collapse=""),rda_num,"state_time_agb_remove_",scen_rep,paste(time_steps,collapse=""),".rda" ))
save(state_time_diversity,file=paste0(rda_dir,SCEN,paste(climates, collapse=""),rda_num,"state_time_diversity_",scen_rep,paste(time_steps,collapse=""),".rda" ))
save(state_time_age200,file=paste0(rda_dir,SCEN,paste(climates, collapse=""),rda_num,"state_time_age200_",scen_rep,paste(time_steps,collapse=""),".rda" ))
save(state_time_spp_group,file=paste0(rda_dir,SCEN,paste(climates, collapse=""),rda_num,"state_time_spp_group_",scen_rep,paste(time_steps,collapse=""),".rda" ))
print (c("End of scenario ",SCEN,date()))

}#END OF SCENARIO LOOP.
#write.csv(harvested_TABLE_matrix,file=paste0("D:/Plisinski/Dinamica_Runs/Master_RT_spill1/data/Landis_Outputs/",scen_rep, rda_num,"rcn_scenario_output_harvest_freq_table.csv"), row.names = F)

