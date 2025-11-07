###==================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### 4- Maxent Limiting variables evaluation                     ################
### Author: Romina Barbosa                                      ################
### Date: 25-July-2024                                           ################
### Last edition: March-2025
###==================================================================
library(raster)
library(rmaxent)
library(dplyr)
library(ggplot2)
library(terra)
library(sf)
 
# # increase the RAM for Java
# library(dismo)
# library(rJava)
# options(java.parameters = "-Xmx8g" )
# suppressPackageStartupMessages(library(disdat))
# suppressPackageStartupMessages(library(dismo))
# suppressPackageStartupMessages(library(forcats))


mypath<- "E:/Kelp_postdoc/SDMs/csvs_to_SDMs"
mypath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/csvs_to_SDMs"

# layers_path<- "E:/Kelp_postdoc/SDMs/layers"
# plotspath<- "E:/Kelp_postdoc/SDMs/Plots"
# SDM_path<- "E:/Kelp_postdoc/SDMs"
SDM_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs"
# path_model_res<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs"
# path_clusters<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Clustering/Last version"
# mypath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs/occ_without_shorelineSurveys"


layers3005_path<- "E:/Kelp_postdoc/SDMs/new_layers_3005"
# maxent_outputs_path<- paste(SDM_path, "outputs_SDMs/Sept2024", sep="/")
maxent_outputs_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs"


setwd(paste(SDM_path))
dir()
load(paste(SDM_path, "outputs_SDMs/50repMaxent_17_15_11_5variables.RData", sep="/"))


### Load raster layers                                   #######################
###=============================================================================
stack_vars<-  rast(paste(SDM_path, "variables_selection/stack_29FilteredVars_stratification_FINAL2.tif", sep="/"))
names(stack_vars)

names(stack_vars)<-  c( "bathymetry"    ,     "DIF"   ,             "DIN"    ,            "eastherness" ,      
                        "janBSpd_min",        "janBT_ave",          "janSSpd_min",        "julBS_ave",
                        "julBSpd_max",        "julBT_ave",          "julBT_max",          "julBT_min",
                        "julSS_ave",          "julSSpd_ave",        "julSSpd_min",        "julST_ave",
                        "NO3_spring23_max",   "NO3_spring23_mean",  "NO3_spring23_min",   "NO3_spring23_sd",
                        "NO3_summ23_mean",    "NO3_summ23_sd",      "NO3_winter23_sd",    "northness",
                        "slope",              "tidal_cur",          "TPI",                "wind",
                        "stratification_ind")


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

filtered_vars001 <- raster::subset(stack_vars, subset)
length(names(filtered_vars001))# 11




library(rmaxent)
# To identify which variable is most responsible for decreasing suitability 
# in a given environment, we can use the limiting function. This is an R 
# implementation of an approach described previously and now incorporated 
# into Maxent. The limiting variable at a given location is identified by
# calculating the decrease in suitability, for each predictor in turn, relative
# to the suitability (logistic prediction) that would be achieved if that 
# predictor took the value equal to the mean at occurrence sites (median for
# categorical variables). The predictor associated with the largest decrease
# in suitability is the most limiting factor.



filtered_vars_rast<- stack(filtered_vars001)

lims <- limiting(filtered_vars_rast, model_replicates_003$models[[1]])
names(lims)<- paste("limiVars_maxent", 1, sep="_")

for (i in 2:50) {
  lim_i <- limiting(filtered_vars_rast, model_replicates_003$models[[i+1]])
  names(lim_i)<- paste("limiVars_maxent", i, sep="_")
  lims<- stack(lims, lim_i)
  time= Sys.time()
  print(paste(time, ", finish replicate",i, sep=" "))
  
}

lim_i@data@attributes
# ID   predictor
# 1   1 eastherness
# 2   2   janBT_ave
# 3   3 janSSpd_min
# 4   4   julBS_ave
# 5   5 julBSpd_max
# 6   6   julBT_min
# 7   7 julSSpd_min
# 8   8   julST_ave
# 9   9       slope
# 10 10   tidal_cur
# 11 11         TPI

# ==============================================================================
### Save results from limiting variables =======================================
limsA <- stackSave(lims, "Limiting_variables11_stack_March2025.stk")
# note that filename adds an extension .stk to a stackfile  
limsA <- stackOpen("Limiting_variables11_stackFINAL.stk")
names(limsA)

lims_modal<-  modal(lims)  
mapview::mapview(lims_modal, col.regions = rev(palSD(50)), at = seq(0, 11, 1), legend = TRUE)
# writeRaster(lims_modal, paste(SDM_path, "outputs_SDMs/Limiting_variables_/limiting_variables_11_maxentModal.tif", sep="/"))

# save.image(paste(SDM_path, "outputs_SDMs/Limiting_variables_/Limiting_variables11_FINAL.RData", sep="/"))


palSD = rgb.palette <- colorRampPalette(c("lightblue", "gold2", "grey", "green", "red", "blue"),#"purple",
                                        space = "rgb")
mapview::mapview(lims$limiVars_maxent_1, col.regions = rev(palSD(50)), at = seq(0, 17, 1), legend = TRUE)



