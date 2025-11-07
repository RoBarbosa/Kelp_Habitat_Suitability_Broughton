###==================================================================
###     Results from Species Distribution models SDMs                ###########
###                                                                  ###########
### 4- Model Ensemble                                                ###########
### a- Calculate ensemble model; 
### b- mask by substrate; 
### c- mask by bathymetry (15m = 0.95quantile bathy)
### d- Calculate AUC per model and per cluster
### 
### Author: Romina Barbosa                                           ###########
### Date: 16-Jul-2024                                                ###########
### Last edition: 26-Sep-2024
### Part of code from: https://rspatial.org/raster/sdm/5_sdm_models.html
###==================================================================

# Load packages
library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(flexsdm)

SDM_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs"
path_model_res<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs"
path_clusters<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Clustering/Last version"
mypath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs/occ_without_shorelineSurveys"
  
setwd(paste(path_model_res))
dir()

### Load SDMs outputs and Clusters raster ======================================
mean_maxent<- raster(paste(path_model_res, "maxent_pred_mean_11Vars_50.tif", sep="/"))

mean_raf<- raster(paste(path_model_res, "Sept2024/RAF_pred_mean_11Vars_50.tif", sep="/"))

clusters<- raster(paste(path_clusters, "6CLUSTERs_6Variables_07022024_3005XY_FINAL_alignedSDMs.tif", sep="/"))


### Load variables and Bathymetry ==============================================
stack_vars<-  terra::rast(paste(SDM_path, "variables_selection/stack_29FilteredVars_stratification_FINAL2.tif", sep="/"))
names(stack_vars)<-  c( "bathymetry"    ,     "DIF"   ,             "DIN"    ,            "eastherness" ,      
                        "janBSpd_min",        "janBT_ave",          "janSSpd_min",        "julBS_ave",
                        "julBSpd_max",        "julBT_ave",          "julBT_max",          "julBT_min",
                        "julSS_ave",          "julSSpd_ave",        "julSSpd_min",        "julST_ave",
                        "NO3_spring23_max",   "NO3_spring23_mean",  "NO3_spring23_min",   "NO3_spring23_sd",
                        "NO3_summ23_mean",    "NO3_summ23_sd",      "NO3_winter23_sd",    "northness",
                        "slope",              "tidal_cur",          "TPI",                "wind",
                        "stratification_ind")

bathy<- raster(stack_vars[[1]])

subset<- c(   "TPI",
              "eastherness",
              "janBT_ave",            
              "janSSpd_min", ###
              "julBS_ave", 
              "julBSpd_max",
              "julBT_min", 
              "julSSpd_min",
              "julST_ave",
              "slope", #
              "tidal_cur") 

# subset<- c(   "TPI",
#               "eastherness",
#               "janBT_ave", #
#               "julBS_ave", #
#               "julBSpd_max",
#               "julBT_ave",
#               "julBT_min", #
#               "julSSpd_min", #
#               "julST_ave", #
#               "slope", #
#               "tidal_cur")


test_data<- read.csv( paste(mypath, "testData_30percent2023_X_Cluster.csv", sep="/"))
test_data<- test_data[-1]
colnames(test_data)[3]<- "pr_ab"
test_data<- test_data[,1:7]
test_data_subset<- test_data[c(1:7,which(colnames(test_data) %in% subset)) ]


train_data<- read.csv(paste(SDM_path,
                            "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_train.csv",
                            sep="/"))
# train_data<- read.csv(paste(SDM_path, "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_28May2024_FINAL.csv", sep="/"), header=T)
train_data<- as.data.frame(train_data)
colnames(train_data)[3]<- "pr_ab"

filtered_vars001 <- raster::subset(stack_vars, subset)
length(names(filtered_vars001))# 11


# kelp_data<- read.csv(paste(SDM_path, "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_28May2024_FINAL.csv", sep="/"), header=T)


###=============================================================================
### Create ENSEMBLE MODEL of habitat suitability      ==========================
###=============================================================================

load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/50repMaxent_17_15_11_5variables.RData")
load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/SDM_GLM_GBM_RF_model_2025.RData")

# load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/50repMaxent_18_11_4variables.RData")
# load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/glm_gbm_raf_ensemble_models11Vars_50replicates.RData")

all_thresholds<- ensemble_models$mod_performance%>%
  # filter(threshold == "lpt")%>%
  group_by(model, threshold)%>%
  summarize(AUC_mean= mean(AUC_mean, na.rm= T))

# # model AUC_mean
# 1 gbm   lpt          0.577
# 2 glm   lpt          0.574
# 3 raf   lpt          0.640


# LAST VERSION
# model threshold AUC_mean
# 1 gbm   lpt          0.578
# 2 glm   lpt          0.559
# 3 raf   lpt          0.636

AUC_maxent= mean(model_replicates_002$mod_performance$AUC_presabs) #0.5961 ## 0.5942

raf_performance<- data.frame(matrix(ncol=3, nrow = 0))
colnames(raf_performance)<- c("replica", "AUC", "lpt_threshold")

raf_performance[1,"replica"]= 1
raf_performance[1,"AUC"]= round(ensemble_models$models_rf[[1]]$performance[which(ensemble_models$models_rf[[1]]$performance$threshold == "lpt"),"AUC_mean"],2)
raf_performance[1,"lpt_threshold"]= round(ensemble_models$models_rf[[1]]$performance[which(ensemble_models$models_rf[[1]]$performance$threshold == "lpt"),"thr_value"],2)

for (i in 3:51) {
  
  performance=  ensemble_models$models_rf[[i]]$performance
  raf_performance[i,"AUC"]= round(performance[which(ensemble_models$models_rf[[i]]$performance$threshold == "lpt"),"AUC_mean"],2)
  raf_performance[i,"replica"]= i
  raf_performance[i,"lpt_threshold"]= round(performance[which(ensemble_models$models_rf[[i]]$performance$threshold == "lpt"),"thr_value"],2)
  
  }


lpt_thr_raf= mean(raf_performance[-2,]$lpt_threshold) #0.4482 ##0.4448

AUC_raf= mean(raf_performance[-2,]$AUC) #0.6402  ##0.6356
w_maxent= AUC_maxent / AUC_raf          #0.9311  ##0.9348

models_stack<- stack(mean_maxent, mean_raf)
ensemble_m<- weighted.mean(models_stack, w= c(w_maxent, 1)) # it take 3 minutes to run
# writeRaster(ensemble_m, paste(path_model_res, "ensembleRAFMaxent_11Vars.tif", sep="/"), overwrite=TRUE)





###=============================================================================
### Create Binary map from ensemble MODEL of habitat suitability      ==========
###=============================================================================

ensemble_m<- raster(paste(path_model_res, "ensembleRAFMaxent_11Vars.tif", sep="/"))

models_stack<- stack(mean_maxent, mean_raf, ensemble_m)
names(models_stack)<- c("maxent_mean", "RAF_mean", "Ensemble")
model_cl_stack<- stack(models_stack, clusters)
names(model_cl_stack)[4]<- "Cluster"



###==========================================================================###
### Determine threshold of no omission for ensemble model threshold ============

# First, threshold of no omission for each model
raf_thres<- ensemble_models$mod_performance%>%
  group_by(model, threshold)%>%
  filter(model=="raf")%>%
  summarize(thr_mean= mean(thr_value, na.rm= T))

# model threshold       thr_mean
# <chr> <chr>              <dbl>
# 1 raf   equal_sens_spec    0.481
# 2 raf   lpt                0.447
# 3 raf   max_fpb            0.447
# 4 raf   max_jaccard        0.447
# 5 raf   max_sens_spec      0.666
# 6 raf   max_sorensen       0.447
# 7 raf   sensitivity        0.726

# Version March 2025
# model threshold       thr_mean
# <chr> <chr>              <dbl>
#   1 raf   equal_sens_spec    0.481
# 2 raf   lpt                0.445
# 3 raf   max_fpb            0.445
# 4 raf   max_jaccard        0.445
# 5 raf   max_sens_spec      0.669
# 6 raf   max_sorensen       0.445
# 7 raf   sensitivity        0.729

# thr_models<- ensemble_models$mod_performance%>%
#   group_by(model, threshold)%>%
#   summarize(thr_mean= mean(thr_value, na.action= remove))

raf_thres[which(raf_thres$threshold == "max_sens_spec" & raf_thres$model == "raf"), "thr_mean"]
# 0.666
# 0.669 # March 2025

thr_maxent<- model_replicates_002$mod_performance%>%
  summarize(thrSS_mean= mean(threshold.SS, na.rm= T),
            thrNoO_mean= mean(threshold.NoO, na.rm= T),
            thrESS_mean= mean(threshold.ESS, na.rm= T))

# # thrSS_mean  thrNoO_mean  thrESS_mean
# # 0.5685461432 0.0659800314 0.5460573016
 
# thrSS_mean  thrNoO_mean  thrESS_mean
# 1 0.527933876 0.0329032124 0.5039667712

# Calculate ensemble model threshold with weighted average
ensemble_threshold_NoO<- weighted.mean(c(thr_maxent$thrNoO_mean, 
                                     raf_thres[which(raf_thres$threshold == "lpt"), "thr_mean"]$thr_mean),
                                   w= c(w_maxent, 1))
ensemble_threshold_ESS<- weighted.mean(c(thr_maxent$thrESS_mean, 
                                     raf_thres[which(raf_thres$threshold == "equal_sens_spec"), "thr_mean"]$thr_mean),
                                   w= c(w_maxent, 1))

round(ensemble_threshold_NoO, 2) # 0.26 ## 0.25
round(ensemble_threshold_ESS,2)  # 0.51 ## 0.49

models_NoO_thr<- as.data.frame(matrix(nrow=3, ncol= 2))
colnames(models_NoO_thr)<- c("Model", "NoO_Threshold")
models_NoO_thr$Model<- c("Maxent", "RAF", "Ensemble")
models_NoO_thr$NoO_Threshold<- round(c(thr_maxent$thrNoO_mean, 
                                 raf_thres[which(raf_thres$threshold == "lpt"), "thr_mean"]$thr_mean,
                                 ensemble_threshold_NoO), 3)

# # Model NoO_Threshold
# # 1   Maxent         0.066
# # 2      RAF         0.447
# # 3 Ensemble         0.264
# 
# Model NoO_Threshold
# 1   Maxent         0.033
# 2      RAF         0.445
# 3 Ensemble         0.246


###==========================================================================###
### Apply thresholds to model predictions to create binary maps ================
# function edited from: https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/

sdm_threshold <- function(sdm= models_stack$maxent_mean, thresh= models_NoO_thr, 
                          model= "Maxent", binary = T){
    thresh_m <- thresh[which(thresh$Model == model), "NoO_Threshold"]
    
  if(binary == F){
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh_m] <- NA
    
    names(sdm_thresh)<- paste( model, "mask_thrNoO", sep="_")
    
    return(sdm_thresh)
    }
  
  
  if(binary == T){
    sdm_thresh_2 <- sdm
    sdm_thresh_2[sdm_thresh_2 < thresh_m] <- 0
    sdm_thresh_2[sdm_thresh_2 >= thresh_m] <- 1
    names(sdm_thresh_2)<- paste( model, "bynary", sep="_")
    return(sdm_thresh_2)
  }
  }


m_map_thresh<- sdm_threshold(sdm= models_stack$maxent_mean, thresh= models_NoO_thr, 
                             model= "Maxent", binary = T)

raf_map_thresh<- sdm_threshold(sdm= models_stack$RAF_mean, thresh= models_NoO_thr, 
                             model= "RAF", binary = T)

ens_map_thresh<- sdm_threshold(sdm= models_stack$Ensemble, thresh= models_NoO_thr, 
                               model= "Ensemble", binary = T)

plot(ens_map_thresh)
# writeRaster(m_map_thresh, paste(path_model_res, "maxent_model_binary_NoOmision.tif", sep="/"), overwrite=TRUE)
# writeRaster(raf_map_thresh, paste(path_model_res, "RF_model_binary_NoOmision.tif", sep="/"), overwrite=TRUE)
# writeRaster(ens_map_thresh, paste(path_model_res, "ENSEMBLE_model_binary_NoOmision.tif", sep="/"), overwrite=TRUE)


binary_stack<- stack(m_map_thresh, raf_map_thresh, ens_map_thresh)


mapview::mapview(binary_stack$Ensemble_bynary, legend = TRUE)
mapview::mapview(binary_stack$Maxent_bynary, legend = TRUE)
mapview::mapview(binary_stack$RAF_bynary, legend = TRUE)


# models_stack<- stack(models_stack)
model_cl_stack<- stack(model_cl_stack, binary_stack)
names(model_cl_stack)


###==========================================================================###
### Mask Suitbale area by SUbstrate
###==========================================================================###
### Convert raster stack into points dataframe to import in QGis and extract substrate type ====

# model_cl_stack_df<- as.data.frame(model_cl_stack, xy=T)
# model_cl_stack_df<- na.exclude(model_cl_stack_df)
# head(model_cl_stack_df)

# write.csv(model_cl_stack_df, paste(path_model_res,"/model_cl_stack_df.csv", sep="/"))

# model_cl_stack_df<- read.csv(paste(path_model_res,"model_cl_stack_df.csv", sep="/"))
# subst_df<- read.csv(paste(path_model_res,"model_cl_substrate_df.csv", sep="/"))
# head(model_cl_stack_df)
# head(subst_df)
# length(subst_df$fid)
# length(model_cl_stack_df$x)
# 
# model_cl_subst_df<- cbind(model_cl_stack_df, subst_df)
# colnames(model_cl_subst_df)

# subst_raster <- rasterFromXYZ(model_cl_subst_df[,c("x", "y", "BType1")])
# mapview::mapview(subst_raster, cex=2, zcol="BType1", col.regions= c("red", "blue", "yellow"))# a visual check

# writeRaster(subst_raster, paste(path_model_res, "substrate_raster.tif", sep="/"))

subst_raster<- raster(paste(path_model_res, "substrate_raster.tif", sep="/"))

suitable_subst_mask<- subst_raster
suitable_subst_mask[suitable_subst_mask == 3]<- 0
suitable_subst_mask[suitable_subst_mask != 0]<- 1
mapview::mapview(suitable_subst_mask, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check


# model_cl_subst_sf <- model_cl_subst_df %>%
#   st_as_sf(coords = c("x", "y"), crs = 3005)

names(model_cl_stack)
models_stack_masked<- model_cl_stack * suitable_subst_mask
names(models_stack_masked)<- names(model_cl_stack)
models_stack_masked<- stack(models_stack_masked, suitable_subst_mask)
names(models_stack_masked)[8]<- "substrate_binary"

plot(models_stack_masked$Ensemble_bynary)

###==========================================================================###
### Mask Suitable area by Bathymetry
###==========================================================================###
# 95% of training data are at bathymetry above 15m
bathy_15<- bathy
bathy_15[bathy_15<= 15]<- 1
bathy_15[bathy_15> 15]<- 0
plot(bathy_15)
plot(models_stack_masked$Ensemble_bynary )

ensemble_bathySubst<-  models_stack_masked$Ensemble_bynary * bathy_15
# writeRaster(ensemble_bathySubst, paste(path_model_res, "ENSEMBLE_masked_bathySubstrat.tif", sep="/"))

models_stack_maskedSub_Bathy<- models_stack_masked[[5:7]] * bathy_15
names(models_stack_maskedSub_Bathy)<- names(models_stack_masked[[5:7]])
names(models_stack_maskedSub_Bathy)<- c("Maxent_bynary_maskSubBathy", "RAF_bynary_maskSubBathy", "Ensemble_bynary_maskSubBathy")
 


# ###==========================================================================###
# ### MAsk suitable area by substrate   ===========================
# 
# subst_raster<- raster(paste(path_model_res, "substrate_raster.tif", sep="/"))
# 
# suitable_subst_mask<- subst_raster
# suitable_subst_mask[suitable_subst_mask == 3]<- 0
# suitable_subst_mask[suitable_subst_mask != 0]<- 1
# mapview::mapview(suitable_subst_mask, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check
# 
# 
# # model_cl_subst_sf <- model_cl_subst_df %>%
# #   st_as_sf(coords = c("x", "y"), crs = 3005)
# 
# names(model_cl_stack)
# models_stack_masked<- model_cl_stack * suitable_subst_mask
# names(models_stack_masked)<- names(model_cl_stack)
# names(models_stack_masked)<- c("maxent_mean_maskSub","RAF_mean_maskSub", "Ensemble_maskSub", "Cluster_maskSub",
#                                "Maxent_bynary_maskSub", "RAF_bynary_maskSub", "Ensemble_bynary_maskSub")
# 
# models_stack_masked<- stack(models_stack_masked, suitable_subst_mask)
# names(models_stack_masked)[8]<- "substrate_binary"
# 
# # writeRaster(models_stack_masked, paste(path_model_res, "models_stack_maskedSub.tif", sep="/"), overwrite=TRUE)
# # writeRaster(models_stack_masked$Ensemble_bynary_maskSub, paste(path_model_res, "Ensemble_bynary_maskSub.tif", sep="/"), overwrite=TRUE)
# # writeRaster(model_cl_stack$Ensemble_bynary, paste(path_model_res, "Ensemble_bynary.tif", sep="/"), overwrite=TRUE)
# 
# 
# 
# #### Mask model by bathymetry =================================================
# # 95% of training data are at bathymetry above 15m
# bathy_15<- bathy
# bathy_15[bathy_15<= 15]<- 1
# bathy_15[bathy_15> 15]<- 0
# plot(bathy_15)
# 
# names(models_stack_masked)
# models_stack_maskedSub_Bathy<- models_stack_masked[[5:7]] * bathy_15
# names(models_stack_maskedSub_Bathy)<- names(models_stack_masked[[5:7]])
# names(models_stack_maskedSub_Bathy)<- c("Maxent_bynary_maskSubBathy", "RAF_bynary_maskSubBathy", "Ensemble_bynary_maskSubBathy")
# 
# library(mapview)
# pal = mapviewPalette("mapviewTopoColors")
# pal = rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
#                                       space = "rgb")
# 
# mapview::mapview(models_stack_maskedSub_Bathy$Ensemble_bynary_maskSubBathy, 
#                  col.regions = rev(pal(100)), at = seq(0, 1, 0.1), legend = TRUE)
# 
# 
# 
# # writeRaster(models_stack_maskedSub_Bathy$Ensemble_bynary_maskSubBathy, paste(path_model_res, "models_stack_maskedSub_Bathy.tif", sep="/"), overwrite=TRUE)
# # writeRaster(models_stack_maskedSub_Bathy$Ensemble_bynary_maskSubBathy, paste(path_model_res, "Ensemble_bynary_maskSubBathy.tif", sep="/"), overwrite=TRUE)
# 

























# plot(models_stack_masked$Ensemble)
# 
# names(models_stack_masked)
# 
# pred_testdata<- sdm_extract(data= test_data, 
#                             x = "x",
#                             y = "y",
#                             env_layer = rast(models_stack_masked),
#                             variables = NULL,
#                             filter_na = FALSE)
# 
# 
# 
# # write.csv(pred_testdata, paste(path_model_res, "predict_test30_data.csv", sep="/"))
# 
# pred_testdata[which(is.na(pred_testdata$Ensemble_bynary)),"Ensemble_bynary"]
# 
# pred_testdata_sf <- pred_testdata %>%
#   st_as_sf(coords = c("x", "y"), crs = 3005)
# mapview::mapview(pred_testdata_sf, cex=2,  zcol="pr_ab", col.regions= c("red", "blue"))# a visual check
# mapview::mapview(pred_testdata_sf, cex=2,  zcol="Ensemble_bynary")# a visual check
# 
# 
# 
# 
# # suitable_subst_mask<- raster( paste(path_model_res, "9_variabls_FINAL/substrate_raster.tif", sep="/"))
# 
# 
# 
# pred_test_sat<- sdm_extract(data= pred_test_sat, 
#                             x = "lon",
#                             y = "lat",
#                             env_layer = rast(models_stack_masked$substrate_binary),
#                             variables = NULL,
#                             filter_na = FALSE)
# 
# 
# colnames(pred_test_sat)
# # write.csv(pred_test_sat, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/SDMs_validation/Sept2024/all_validation_pts_predictFINAL.csv")



