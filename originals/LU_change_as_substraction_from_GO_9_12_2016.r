#process dynamica-landuse outputs as a subtraction of grow only. 
#run this in parellel for each of five scenarios.
#Matthew Duveneck, started 3/17/2016
#9/12/2016
#5/11/2017

library(raster);library(RColorBrewer);options(scipen=999);rasterOptions(tmpdir="Y:/raster_package_temp_files/")
PLC<-"black";BGC<-"white"
col_plot<-c(brewer.pal(9,"Blues"),PLC)


removeTmpFiles() 
#scenarios<-c("GrOn",RT","YC","GG","GA","CC")

#scenarios<-"GrOn"
scenarios<-c("RT","YC","CC","GA","GG")
#scenarios<-c("YC")#,"YC","GG","GA","CC")
#scenarios<-c("GG")#,"YC","GG","GA","CC")
#scenarios<-c("GA")#,"YC","GG","GA","CC")
#scenarios<-c("CC")#,"YC","GG","GA","CC")

#scen30_m_path_names<-c("_Grow_Only","_Recent_Trends","_Yankee_Cosmopolitan_Q1", "_Growing_Global_Q4","_Go_It_Alone_Q3","_Community_Connectedness_Q2")
#scen30_m_path_names<-c("_Grow_Only")
#scen30_m_path_names<-c("_Recent_Trends")
#scen30_m_path_names<-c("_Yankee_Cosmopolitan_Q1")
#scen30_m_path_names<-c("_Growing_Global_Q4")
#scen30_m_path_names<-c("_Go_It_Alone_Q3")
#scen30_m_path_names<-c("_Community_Connectedness_Q2")




#scen30_m_path_names<-c("RT_spill1","Q1_spill2","Q2_spill3","Q3_nospill","Q4_spill2")
#Q1 = Yankee Cosmopolitan
#Q2  = Community Connectedness
#Q3 = Go It Alone
#Q4 = Growing Global
#RT = Recent Trends

LU_temp_dir1<-"Y:/land_use_dinamica_output/September2016_250m/"
scen_rep<-"1101701"
species_file<-read.csv("Y:/LANDIS_INPUTS/eco_spp_inc/LANDIS_NE_SPECIES.csv")
species<-species_file$GENUSPEC
climates<-c("HADGE")
time_steps<-0:5#

####r50<-raster("Y:/land_use_dinamica_output/empty_50m.img")  #Empty raster to use in 50 meter raster template for resample below.

fun_turn_NA_to_zero <- function(x) { x[is.na(x)] <- 0; return(x)}
#SNE<-raster("Y:/LANDIS_INPUTS/input_rasters/zone3_250_eco11_5_2014.img")>0
#SNE1 <- calc(SNE, fun_turn_NA_to_zero)
#NE<-raster("Y:/LANDIS_INPUTS/input_rasters/pre_11_17_2016_files/june_2016_cli_soil25NC_trial1.img")>0
NE<-raster("Y:/LANDIS_INPUTS/input_rasters/eco_11_17_2016B.img")>0
NE1<-calc(NE, fun_turn_NA_to_zero)
#plot(NE1)
r50<-disaggregate(NE1,5)#50m raster to use in aggregate/resample functions.


#LU_matrix<-NULL
for (SC1 in 1: length (scenarios)){
  SC<-scenarios[SC1]
  print(SC)
  #scen30_m_path_name<-scen30_m_path_names[SC1]
  #print(scen30_m_path_name)
  #LU_temp_dir<-paste0("D:/Plisinski/Dinamica_Runs/FINAL_RT_RUN1",scen30_m_path_name,"/New_Patches_SAP_rates/")#
  #LU_temp_dir<-paste0("D:/Plisinski/Dinamica_Runs/NARRATIVES_",SC,"/data/New_Patches/")#newer but only ready for RT as of 1/18/2017
  #LU_temp_dir<-paste0("D:/Plisinski/Dinamica_Runs/SCENARIOS",scen30_m_path_name,"/New_Patches_spillover/")#,SC,"/data/New_Patches/")#newer but only ready for RT as of 1/18/2017, pre 5/2017
  LU_temp_dir<-paste0("D:/Plisinski/Dinamica_Runs/New_England_Landscape_Futures_Results/New_Patches_For_Landis/",SC)
  #LU_temp_dir<-paste0("D:/Plisinski/Dinamica_Runs/Master_",scen30_m_path_name,"/data/New_Patches/")#5/11/2017  
  setwd(LU_temp_dir)
  list.files()
  LU_zero_dir<-"D:/Plisinski/Dinamica_Runs/FINAL_RT_RUN1_Recent_Trends/Binary_Reclassed_Initial_Landscape/"#to be used with initial ts=0.
  EXT<-extent(raster("LD2HD_01.img"));PRJ<-projection(raster("LD2HD_01.img"))
  CUM_F2HD<-raster()#empty raster 
  extent(CUM_F2HD)<-EXT;projection(CUM_F2HD)<-PRJ
  res(CUM_F2HD)<-30
  values(CUM_F2HD)<-0
  CUM_F2LD<-raster()#empty raster 
  extent(CUM_F2LD)<-EXT;projection(CUM_F2LD)<-PRJ
  res(CUM_F2LD)<-30
  values(CUM_F2LD)<-0
  CUM_F2A<-raster()#empty raster 
  extent(CUM_F2A)<-EXT;projection(CUM_F2A)<-PRJ
  res(CUM_F2A)<-30
  values(CUM_F2A)<-0
for (year in time_steps){
  print (c("start 30m conversions",year))
  if (year==0|SC=="GrOn"){ #If year is zero or grow only scenario, any year.
  print("year is zero or grow only scenario")
  setwd(LU_zero_dir)
  F2HD_30<-raster("HD.img")
  F2LD_30<-raster("LD.img")
  F2A_30<-raster("AG.img")+raster("OT.img")#Other out of initial landscape. rocks, lakses, etc.
  #plot(F2LD_30)
  }
  
  if (year>0&SC!="GrOn"){#If year is greater than zero AND scenario not grow only.
 print ("year is >0 and scenario not grow only")
  setwd(LU_temp_dir)
  list.files()
  F2HD_30<-raster(paste0("F2HD_0",year,".img"))
  #plot(F2HD_30)
  #summary(F2HD_30)
  
  
  F2LD_30<-raster(paste0("F2LD_0",year,".img"))
  F2A_30<-raster(paste0("F2A_0", year, ".img"))+raster(paste0(LU_zero_dir,"OT.img"))#take out other every time step & scenario.
}
CUM_F2HD<-((CUM_F2HD+F2HD_30)>0)
CUM_F2LD<-((CUM_F2LD+F2LD_30)>0)
CUM_F2A<-((CUM_F2A+F2A_30)>0) 
  writeRaster(CUM_F2LD,filename=paste0(LU_temp_dir1,SC,year,"6_7_2017_30m_cum_FTLD.img"),overwrite=T, datatype="INT4S", NAflag=0)
  writeRaster(CUM_F2HD,filename=paste0(LU_temp_dir1,SC,year,"6_7_2017_30m_cum_FTHD.img"),overwrite=T, datatype="INT4S", NAflag=0)
  writeRaster(CUM_F2A,filename=paste0(LU_temp_dir1,SC,year,"6_7_2017_30m_cum_FTA.img"),overwrite=T, datatype="INT4S", NAflag=0)

  print (paste(year,"starting low den aggregate functions",date()))
    #resamp_F2LD_50<-resample(x=CUM_F2LD,y=r50, method="bilinear",template=r50)
    #F2LD_dyn250<-aggregate(resamp_F2LD_50, fact=5, fun=mean,expand=F,na.rm=T)
    #resamp_F2LD_50<-resample(x=CUM_F2LD,y=r50, method="bilinear",template=r50)
    F2LD_dyn250<-aggregate(resample(x=CUM_F2LD,y=r50, method="bilinear",template=r50), fact=5, fun=mean,expand=F,na.rm=T)
  writeRaster(F2LD_dyn250,filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2LD_mean_agg50_resam_bilin_250_6_7_2017.img"), overwrite=T) #resample to 30 to 250.
    print (paste(year,"starting hi den aggregate functions",date()))
    F2HD_dyn250<-aggregate(resample(x=CUM_F2HD,y=r50, method="bilinear",template=r50), fact=5, fun=mean,expand=F,na.rm=T) 
  writeRaster(F2HD_dyn250,filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2HD_mean_agg50_resam_bilin_250_6_7_2017.img"), overwrite=T) #resample to 30 to 250.
    print (paste(year,"starting Ag aggregate functions",date()))
    F2A_dyn250<-aggregate(resample(x=CUM_F2A,y=r50, method="bilinear",template=r50), fact=5, fun=mean,expand=F,na.rm=T) 
  writeRaster(F2A_dyn250,filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2A_mean_agg50_resam_bilin_250_6_7_2017.img"), overwrite=T) #resample to 30 to 250.
    print (paste(year,"ending aggregate functions", date()))
  F2A_dyn250<-raster(paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2A_mean_agg50_resam_bilin_250_6_7_2017.img"))
  F2LD_dyn250<-raster(paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2LD_mean_agg50_resam_bilin_250_6_7_2017.img"))
  F2HD_dyn250<-raster(paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2HD_mean_agg50_resam_bilin_250_6_7_2017.img"))
  CUM_DEV_pre<-(F2A_dyn250+(F2HD_dyn250*.94)+F2LD_dyn250*.5)#Development map is =(1*agriculture)+(.8*residential) to account for street trees in residential development.
  CUM_DEV<-CUM_DEV_pre
  
  #THIS DID NOT WORK for some reason, I couldn't get the CUM_DEV to calculate properly on rasters.
  #It seems I have fixed the problem in the next script, rcn_landis_scenario_report.r by turning rasters into dataframes.
  #It would make sense to move it over here but I have not.
  #assign(paste0("CUM_DEV_pre_correct_", year) ,CUM_DEV_pre)
  print(year)
  #THIS DID NOT WORK  prev_year<-year-1
  #THIS DID NOT WORK CDC<-get(paste0("CUM_DEV_pre_correct_",year))
  #THIS DID NOT WORKif(year==0){CUM_DEV<-CUM_DEV_pre}
  #THIS DID NOT WORK if(year>0){
  #THIS DID NOT WORK CDP<-get(paste0("CUM_DEV_pre_correct_",prev_year))
  #THIS DID NOT WORKCUM_DEV<-overlay(CDC,CDP,fun=function(x,y)(ifelse(x<y,y,x)))
  #THIS DID NOT WORK}

  #plot(F2A_dyn250, main=paste(year,"ag"))
  #plot(F2HD_dyn250, main=paste(year,"high density"))   
  #plot(F2LD_dyn250, main=paste(year,"low density dev"))
  out_path<-paste0("Y:/temporary_figures/",SC,year,"6_7cumul_development.jpg")
  jpeg(filename=out_path, bg="white", width = 6, height = 6, units = "in", pointsize = 16, res= 600) # open plot output to file
  plot(CUM_DEV*NE, main=paste(SC,year,"Cumulative Development"), axes=F, breaks=seq(0,1,.1), col=col_plot)
  dev.off()
  EXT250<-extent(CUM_DEV);PRJ250<-projection(CUM_DEV)
  writeRaster(CUM_DEV, filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_development250_6_7_2017.img"), overwrite=T)

  
  #test1<-raster(paste0(LU_temp_dir1,SC,"year",1,"cumulative_development250_1_20_2017.img"))
  #test0<-raster(paste0(LU_temp_dir1,SC,"year",0,"cumulative_development250_1_20_2017.img"))
 #plot(test1)
 #plot(test0)
#  diff_01<-(test1-test0)
 #plot(diff_01)
 #diff_01
 
 #zoom(diff_01, drawExtent())
 #  for (CL in 1: length(climates)){
 #    CLIM<-climates[CL]
 #    print(paste("starting climate loop",CLIM,year, date()))
 #    GO_dir<-(paste0("Y:/LANDIS_OUTPUTS/PNET_178_scenario_",SC,"_",CLIM,"/replicate",scen_rep,"/output/agbiomass/"))
 #    GO_spp_biom<-stack(paste0(GO_dir,species,"/AGBiomass_",year*10,".img"))#GO species biomass stacked layers.
 #    
 #    extent(GO_spp_biom)<-EXT250; projection(GO_spp_biom)<-PRJ250 #projection and extent objects.
 #    LU_spp_biom<-(GO_spp_biom-(GO_spp_biom*CUM_DEV)) #LU species biomass stacked layers.
 #    save(LU_spp_biom,file=paste0(GO_dir,CLIM,year,"stack_all_spp_LU.rda"))    #save LU biomass as rda object.
 #    LU_spp_sum<-sum(LU_spp_biom)
 #    LU_spp_sum1 <- calc(LU_spp_sum, fun_turn_NA_to_zero)
 #    writeRaster(LU_spp_sum1, filename=paste0(GO_dir,CLIM,year,"total_biomass_LU1.img"),overwrite=T, NAflag=0,dataType='INT4S')
 #    GO_spp_sum<-sum(GO_spp_biom)
 #    GO_spp_sum1 <- calc(GO_spp_sum, fun_turn_NA_to_zero)
 #    writeRaster(GO_spp_sum1, filename=paste0(GO_dir,CLIM,year,"total_biomass_GO1.img"),overwrite=T, NAflag=0,dataType='INT4S')
 #    GO_mean<-cellStats(GO_spp_sum1*NE1, stat='mean', na.rm=T, asSample=F)
 #    LU_mean<-cellStats(LU_spp_sum1*NE1, stat='mean', na.rm=T, asSample=F)
 #    #GO_mean_SNE<-cellStats(GO_spp_sum1*SNE1, stat='mean', na.rm=F, asSample=F)
 #    #LU_mean_SNE<-cellStats(LU_spp_sum1*SNE1, stat='mean', na.rm=F, asSample=F)
 #    
 #    
 #  LU_row<- cbind(year,SC, CL,GO_mean, LU_mean)#,GO_mean_SNE,LU_mean_SNE)
 # print(LU_row)
 # LU_matrix<-as.data.frame(rbind(LU_matrix, LU_row))
 # 
 #  }#end of inner climate loop
 
}#end of outer timestep loop.

  
  #PLC<-"black"

#curr_sub<-subset(LU_matrix, LU_matrix$CL==1)
#plot(curr_sub$year,curr_sub$GO_mean, col=PLC, type="l", ylim=c(2800,4000))
#lines(curr_sub$LU_mean, col="red")
#cli_chan_sub<-subset(LU_matrix, LU_matrix$CL==2)
#lines(cli_chan_sub$LU_mean, col="red", type="l", lty=2)
#lines(cli_chan_sub$GO_mean, col=PLC, type="l", lty=2)

}#SC
