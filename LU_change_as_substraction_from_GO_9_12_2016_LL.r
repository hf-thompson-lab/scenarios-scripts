#process dynamica-landuse outputs as a subtraction of grow only. 
#run this in parellel for each of five scenarios.
#Matthew Duveneck, started 3/17/2016
#9/12/2016
#5/11/2017

library(raster);library(RColorBrewer);options(scipen=999);rasterOptions(tmpdir="Y:/duveneck/raster_package_temp_files/")
library(doParallel)
library(foreach)

# Define number of cores to use, leaving one core available for overhead tasks
cores <- 5

# Register cores
cl <- makeCluster(cores)
registerDoParallel(cl)

# Define map colors
PLC<-"black";BGC<-"white"
col_plot<-c(brewer.pal(9,"Blues"),PLC)

#removeTmpFiles() 

#scenarios<-c("GrOn",RT","YC","GG","GA","CC")
#scenarios<-"GrOn"
scenarios<-c("RT","YC","CC","GA","GG")
#scenarios <- c("RT", "YC")

LU_temp_dir1<-"Y:/Lee/Jonah_scripts/outputs/"
scen_rep<-"1101701"
species_file<-read.csv("Y:/duveneck/LANDIS_INPUTS/eco_spp_inc/LANDIS_NE_SPECIES.csv",
                       stringsAsFactors = FALSE)
species<-species_file$GENUSPEC
climates<-c("HADGE")
time_steps<-0:5#
#time_steps <- 0:1

fun_turn_NA_to_zero <- function(x) { x[is.na(x)] <- 0; return(x)}
#SNE<-raster("Y:/LANDIS_INPUTS/input_rasters/zone3_250_eco11_5_2014.img")>0
#SNE1 <- calc(SNE, fun_turn_NA_to_zero)
#NE<-raster("Y:/LANDIS_INPUTS/input_rasters/pre_11_17_2016_files/june_2016_cli_soil25NC_trial1.img")>0
NE<-raster("Y:/duveneck/LANDIS_INPUTS/input_rasters/eco_11_17_2016B.img")>0
NE1<-calc(NE, fun_turn_NA_to_zero)
#plot(NE1)
r50<-disaggregate(NE1,5)#50m raster to use in aggregate/resample functions.
print(paste("r50 raster created", date()))


#LU_matrix<-NULL

# does this require more memory than using CUM_F2?
# or does it trade off in time b/c the rasters are only made once?
empty.raster <- function(){
  ref_img <- raster("Y:/Plisinski/Dinamica_Runs/New_England_Landscape_Futures_Results/New_Patches_For_Landis/CC/LD2HD_01.img")
  x <- raster(ext = extent(ref_img), crs = crs(ref_img), res = 30, vals = 0)
  return(x)
}

zero_ras <- empty.raster()

system.time(foreach(SC1 = 1: length (scenarios), .packages = c("raster", "RColorBrewer"))%dopar%{
  sink(file = "Y:/Lee/Jonah_scripts/log_06012018.txt", append = TRUE)
  rasterOptions(tmpdir="Y:/duveneck/raster_package_temp_files/")
  
  SC<-scenarios[SC1]
  print(paste("log begun for", SC))
  LU_temp_dir<-paste0("Y:/Plisinski/Dinamica_Runs/New_England_Landscape_Futures_Results/New_Patches_For_Landis/",SC)
  setwd(LU_temp_dir)
  #list.files()
  LU_zero_dir<-"Y:/Plisinski/Dinamica_Runs/FINAL_RT_RUN1_Recent_Trends/Binary_Reclassed_Initial_Landscape/"#to be used with initial ts=0.
  
  # CUM_F2HD <- empty.raster()
  # print(paste("high density empty raster created", SC, date()))
  # CUM_F2LD <- empty.raster()
  # print(paste("low density empty raster created", SC, date()))
  # CUM_F2A <- empty.raster()
  # print(paste("agriculture empty raster created", SC, date()))
  
  for (year in time_steps){
    print (c("start 30m conversions",year))
    if (year==0|SC=="GrOn"){ #If year is zero or grow only scenario, any year.
      print(paste("year is zero or grow only scenario", SC))
      setwd(LU_zero_dir)
      F2HD_30<-raster("HD.img")
      F2LD_30<-raster("LD.img")
      F2A_30<-raster("AG.img")+raster("OT.img")#Other out of initial landscape. rocks, lakses, etc.
      print(paste("30m rasters acquired for", SC, date()))
    }
    
    if (year>0&SC!="GrOn"){#If year is greater than zero AND scenario not grow only.
      print (paste("year is >0 and scenario not grow only", SC))
      setwd(LU_temp_dir)
      #list.files()
      F2HD_30<-raster(paste0("F2HD_0",year,".img"))
      F2LD_30<-raster(paste0("F2LD_0",year,".img"))
      F2A_30<-raster(paste0("F2A_0", year, ".img"))+raster(paste0(LU_zero_dir,"OT.img"))#take out other every time step & scenario.
      print(paste("30m rasters acquired for", SC, date()))
    }
    CUM_F2HD<-((zero_ras+F2HD_30)>0)
    print(paste("high density cumulative boolean raster created", SC, date()))
    CUM_F2LD<-((zero_ras+F2LD_30)>0)
    print(paste("low density cumulative boolean raster created", SC, date()))
    CUM_F2A<-((zero_ras+F2A_30)>0) 
    print(paste("agriculture cumulative boolean raster created", SC, date()))
    writeRaster(CUM_F2LD,filename=paste0(LU_temp_dir1,SC,year,"6_7_2017_30m_cum_FTLD_LL.img"),overwrite=T, datatype="INT4S", NAflag=0)
    print(paste("CUM_F2LD written", SC, year, date()))
    writeRaster(CUM_F2HD,filename=paste0(LU_temp_dir1,SC,year,"6_7_2017_30m_cum_FTHD_LL.img"),overwrite=T, datatype="INT4S", NAflag=0)
    print(paste("CUM_F2HD written", SC, year, date()))
    writeRaster(CUM_F2A,filename=paste0(LU_temp_dir1,SC,year,"6_7_2017_30m_cum_FTA_LL.img"),overwrite=T, datatype="INT4S", NAflag=0)
    print(paste("CUM_F2A written", SC, year, date()))
  
    print (paste(year,"starting low den aggregate functions", SC, year, date()))
      F2LD_dyn250<-aggregate(resample(x=CUM_F2LD,y=r50, method="bilinear",template=r50), fact=5, fun=mean,expand=F,na.rm=T)
      print (paste(year,"low den aggregate functions completed, beginning to write raster", SC, year, date()))
    writeRaster(F2LD_dyn250,filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2LD_mean_agg50_resam_bilin_250_5_22_2018.img"), overwrite=T) #resample to 30 to 250.
      print (paste(year,"starting hi den aggregate functions", SC, year, date()))
      F2HD_dyn250<-aggregate(resample(x=CUM_F2HD,y=r50, method="bilinear",template=r50), fact=5, fun=mean,expand=F,na.rm=T) 
      print (paste(year,"hi den aggregate functions completed, beginning to write raster", SC, year, date()))
    writeRaster(F2HD_dyn250,filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2HD_mean_agg50_resam_bilin_250_5_22_2018.img"), overwrite=T) #resample to 30 to 250.
      print (paste(year,"starting Ag aggregate functions", SC, year, date()))
      F2A_dyn250<-aggregate(resample(x=CUM_F2A,y=r50, method="bilinear",template=r50), fact=5, fun=mean,expand=F,na.rm=T) 
      print (paste(year,"ag aggregate functions completed, beginning to write raster", SC, year, date()))
    writeRaster(F2A_dyn250,filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2A_mean_agg50_resam_bilin_250_5_22_2018.img"), overwrite=T) #resample to 30 to 250.
      print (paste(year,"ending aggregate functions",  SC, year, date()))
    F2A_dyn250<-raster(paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2A_mean_agg50_resam_bilin_250_5_22_2018.img"))
    F2LD_dyn250<-raster(paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2LD_mean_agg50_resam_bilin_250_5_22_2018.img"))
    F2HD_dyn250<-raster(paste0(LU_temp_dir1,SC,"year",year,"cumulative_F2HD_mean_agg50_resam_bilin_250_5_22_2018.img"))
    print(paste("begin cum raster math",  SC, year, date()))
    CUM_DEV_pre<-(F2A_dyn250+(F2HD_dyn250*.94)+F2LD_dyn250*.5)#Development map is =(1*agriculture)+(.8*residential) to account for street trees in residential development.
    CUM_DEV<-CUM_DEV_pre
    #print(year)
    out_path<-paste0("Y:/duveneck/temporary_figures/",SC,year,"5_22cumul_development.jpg")
    jpeg(filename=out_path, bg="white", width = 6, height = 6, units = "in", pointsize = 16, res= 600) # open plot output to file
    plot(CUM_DEV*NE, main=paste(SC,year,"Cumulative Development"), axes=F, breaks=seq(0,1,.1), col=col_plot)
    dev.off()
    EXT250<-extent(CUM_DEV);PRJ250<-projection(CUM_DEV)
    print(paste("begin writing final raster", SC, year, date()))
    writeRaster(CUM_DEV, filename=paste0(LU_temp_dir1,SC,"year",year,"cumulative_development250_5_22_2018.img"), overwrite=T)
    print(paste("end writing final raster", SC, year, date()))
  }#end of outer timestep loop.
})#SC

# End cluster
stopCluster(cl)





