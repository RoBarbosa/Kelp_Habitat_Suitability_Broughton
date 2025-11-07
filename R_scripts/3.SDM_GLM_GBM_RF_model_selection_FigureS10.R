###==================================================================
### Presence absence data preparation for            ################
### Species Distribution models        SDMs          ################
###                                                  ################
###                                                  ################
### Author: Romina Barbosa                           ################
### Date: 05-Dec-2023                                ################
### Last edition: 16-Sep-2024                        ################
###==================================================================

library(raster)
library(rasterVis)
library(dplyr)
library(ggplot2)
library(terra)
library(sf)
library(flexsdm)

###==================================================================
# Set paths
options(digits=13)
mypath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs"
layers_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/layers"
plotspath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Plots"
SDM_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs"
layers3005_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/new_layers_3005"
maxent_outputs_path<- paste(SDM_path, "outputs_SDMs/maxent", sep="/")


### Load occurrences data                                #######################
###=============================================================================
# source("2.2_SDM_occ_prep_samplingBiasCorrect.R")

# test_data<- read.csv( paste(mypath, "occ_without_shorelineSurveys/testData_30percent2023_X_Cluster.csv", sep="/"))
test_data<- read.csv( paste(mypath, "occ_without_shorelineSurveys/testData_30percent2023_WVplusSalmonSites.csv", sep="/"))

test_data<- test_data[-1]
test_data<- test_data[-2]# remove pixel ID column ("X")
colnames(test_data)[2]<- "pr_ab"
test_data<- test_data[,c(1:6,65)]

# train_data<- read.csv(paste(SDM_path, "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_train.csv", sep="/"))
train_data<- read.csv(paste(SDM_path,
                            "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_train_withoutShorelineRecords.csv", sep="/"))

train_data<- as.data.frame(train_data)
colnames(train_data)[3]<- "pr_ab"


### Load raster layers                                   #######################
###=============================================================================
# stack_vars<-  rast(paste(SDM_path, "variables_selection/stack_28FilteredVars_FINAL.tif", sep="/"))
stack_vars<-  rast(paste(SDM_path, "variables_selection/stack_29FilteredVars_stratification_FINAL2.tif", sep="/"))
names(stack_vars)

names(stack_vars)<- c( "bathymetry"    ,     "DIF"   ,             "DIN"    ,            "eastherness" ,
                       "janBSpd_min",        "janBT_ave",          "janSSpd_min",        "julBS_ave",
                       "julBSpd_max",        "julBT_ave",          "julBT_max",          "julBT_min",
                       "julSS_ave",          "julSSpd_ave",        "julSSpd_min",        "julST_ave",
                       "NO3_spring23_max",   "NO3_spring23_mean",  "NO3_spring23_min",   "NO3_spring23_sd",
                       "NO3_summ23_mean",    "NO3_summ23_sd",      "NO3_winter23_sd",    "northness",
                       "slope",              "tidal_cur",          "TPI",                "wind",
                       "stratification_ind")


length(names(stack_vars)) # 29 variables


### Extract data from raster layers at each location of pres/abs  ##############
###=============================================================================
test_data<- sdm_extract( data= test_data[,1:4], 
                         x = "x",
                         y = "y",
                         env_layer = stack_vars,
                         variables = NULL,
                         filter_na = FALSE)
colnames(test_data)

train_data<- sdm_extract( data= train_data, 
                          x = "x",
                          y = "y",
                          env_layer = stack_vars,
                          variables = NULL,
                          filter_na = FALSE)
summary(train_data)


# subset<- c(   "TPI",
#               "eastherness",
#               "janBT_ave", #

#               "julBS_ave", #
#               "julBSpd_max",
#               "julBT_ave",######
#               "julBT_min", #
#               "julSSpd_min", #
#               "julST_ave", #
#               "slope", #
#               "tidal_cur") 

# Subset based on last selection with Maxent models
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


# subset001_miamax<-  names(stack_vars)[c(3,1,2,11,12,5,10)]# "~julBS.ave + DIN + janBT.ave + julST.ave + slope + julBT.ave + julSSpd.min

filtered_vars001 <- raster::subset(stack_vars, subset)
length(names(filtered_vars001))# 11

which(subset %in% names(filtered_vars001))
subset %in% names(filtered_vars001)


train_data_subset<- train_data[c(1:6,which(colnames(train_data) %in% subset))]
test_data_subset<- test_data[c(1:5,which(colnames(test_data) %in% subset)) ]


source("C:/Users/romi_/OneDrive/Documents/GitHub/SDMs_Broughton/glm_replicates_function.R")
ensemble_models<- asemble_model_replicates(replicates_number= 50,
                                           training_data= train_data_subset, 
                                           testing_data= test_data_subset,
                                           variables= filtered_vars001)


path_model_res<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs"
setwd(paste(path_model_res))
# save.image("SDM_GLM_GBM_RF_model_2025.RData")
load("SDM_GLM_GBM_RF_model_2025.RData")

## to calculate a mean and SD rasters
ave_ensamble<- calc(ensemble_models$preds_stack, fun = mean)
# writeRaster(ave_ensamble, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/ensemble_pred_mean_11Vars_50.tif")

sd_ensamble<- calc(ensemble_models$preds_stack, fun = sd)
# writeRaster(sd_ensamble, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/ensemble_pred_sd_11Vars_50.tif")



### Compare the performance of models with AUC and BOYCE =======================
m.a<- as.data.frame(ensemble_models$mod_performance)%>%
  ggplot(aes(x= model, y= AUC_mean))+
  geom_boxplot()+ labs(x= "Model", y= "AUC")+
  theme_bw()

m.b<- as.data.frame(ensemble_models$mod_performance)%>%
  ggplot(aes(x= model, y= BOYCE_mean))+
  geom_boxplot()+labs(x= "Model", y= "BOYCE Index")+
  theme_bw()



path_model_res<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs"
setwd(paste(path_model_res))
cowplot::plot_grid(m.a, m.b, labels = c("A)", "B)"))
# ggsave("AUC_BOYCE_presAbsence_11VarsDMs.png", units="cm", width=15, height= 7, dpi=300)



randomForest::varImpPlot(ensemble_models$models_rf[[22]]$model)



### Predict habitat suitability from Random Forest models ======================
i=1
x <- raster::stack()
names(x)

for (i in 1:50) {
  raf_i<- ensemble_models$models_rf[[i]]
  
  pr_i <- sdm_predict(
    models = raf_i,
    pred = filtered_vars001,
    thr = "max_sens_spec",
    con_thr = TRUE,
    predict_area = NULL
  )
  
  Loop.r<- raster(pr_i$raf$raf)
  plot(Loop.r)
  names(Loop.r)<- paste("mod_rep", i, sep="_")
  x <- stack(x, Loop.r)
  
}



ave_raf<- calc(x, fun = mean)
writeRaster(ave_raf, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/RAF_pred_mean_11Vars_50.tif")

sd_raf<- calc(x, fun = sd)
writeRaster(sd_raf, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/RAF_pred_sd_11Vars_50.tif")

# save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/glm_gbm_raf_ensemble_models11Vars_50replicates.RData")
load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/glm_gbm_raf_ensemble_models11Vars_50replicates.RData")



library(mapview)
pal = mapviewPalette("mapviewTopoColors")
pal = rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
                                      space = "rgb")
palSD = rgb.palette <- colorRampPalette(c("lightblue", "gold2", "purple"),
                                        space = "rgb")
pal_range= Lab.palette <- colorRampPalette(c("red", "orange", "blue"),
                                           space = "Lab")

mapview::mapview(ave_raf, col.regions = rev(pal(100)), at = seq(0, 1, 0.1), legend = TRUE)
mapview::mapview(sd_ensamble, col.regions = rev(pal(20)), at = seq(0, 1, 0.1), legend = TRUE)




# as.data.frame(ensemble_models$mod_performance)%>%
#   ggplot(aes(x= model, y= TSS_mean, color=threshold))+
#   geom_boxplot()+
#   theme_bw()
# 
# as.data.frame(ensemble_models$mod_performance)%>%
#   ggplot(aes(x= model, y= SORENSEN_mean, color=threshold))+
#   geom_boxplot()+
#   theme_bw()


# load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/July2024/14_variables_FINAL/ensemble_models_50replicates.RData")
ensemble_models$models_rf[1][[1]]$predictors
var_import_RAF<- as.data.frame(ensemble_models$models_rf[[1]]$model$importance)
str(var_import_RAF)
var_import_RAF$variable<- rownames(var_import_RAF)

as.data.frame(randomForest::varImpPlot(ensemble_models$models_rf[[1]]$model, 
                         # sort=FALSE, 
                         main="Variable Importance Plot"))



var_import_RAF
var_import_RAF$replicate<- 1

a=3
for (a in 3:51) {
  var_import_RAF_i<- as.data.frame(ensemble_models$models_rf[[3]]$model$importance)
  var_import_RAF_i$variable<- rownames(var_import_RAF_i)
  var_import_RAF_i$replicate<- a-1
  var_import_RAF<- rbind(var_import_RAF, var_import_RAF_i)
}

boxplot(var_import_RAF$MeanDecreaseGini)

var_import_RAF%>%
  mutate(variable = forcats::fct_reorder(variable, MeanDecreaseGini , .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, MeanDecreaseGini), x=MeanDecreaseGini)) + 
  # ggplot(aes(y= as.factor(variable), x=MeanDecreaseGini))+
    geom_point()+
    geom_boxplot()+
  labs(x= "Variable Importance (delta Gini))", y= "Variable")+
    # geom_col(width = 0.5)+
    theme_bw()



# Plots of Partial Dependence Plot for Temperature
library(pdp)

# Generate PDP for a selected feature
i=3

train_data_i<- train_data_subset%>% filter(rdomreplicate == i)
train_data_i$dataset<- "train"

pdp_temp <- partial(ensemble_models$models_rf[[3]]$model, pred.var = "slope", train = train_data_i)
pdp_temp <- partial(ensemble_models$models_rf[[3]]$model, pred.var = "TPI", train = train_data_i)
pdp_temp <- partial(ensemble_models$models_rf[[3]]$model, pred.var = "julST_ave", train = train_data_i)
pdp_temp <- partial(ensemble_models$models_rf[[3]]$model, pred.var = "julBT_ave", train = train_data_i)
pdp_temp <- partial(ensemble_models$models_rf[[3]]$model, pred.var = "tidal_cur", train = train_data_i)

# Plot PDP
plot(pdp_temp, main = "Partial Dependence Plot for Temperature")

