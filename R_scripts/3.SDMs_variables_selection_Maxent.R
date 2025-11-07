###==================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### 3- Modeling SDM without Nutrient variables from OceanLive   ################
### Author: Romina Barbosa                                      ################
### Date: 04-Jun-2024                                           ################
### Code edited from: https://sjevelazco.github.io/flexsdm/articles/v01_pre_modeling.html#data-species-occurrence-and-background-data ####
### Last edition: 18-Sep-2024
###==================================================================
library(raster)
library(dplyr)
library(ggplot2)
library(terra)
library(sf)
library(flexsdm)
library(tools)
# library(ENMeval)
# library(rasterVis)
library(viridis)
library(dplyr)
library(tidyr)
library(stringr)

# increase the RAM for Java
library(dismo)
library(rJava)
options(java.parameters = "-Xmx8g" )
suppressPackageStartupMessages(library(disdat))
suppressPackageStartupMessages(library(dismo))
suppressPackageStartupMessages(library(forcats)) # for handling factor variables
# lirmaxentbrary("remotes")
# remotes::install_github("johnbaums/rmaxent")
library(rmaxent)

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

test_data<- read.csv( paste(mypath, "occ_without_shorelineSurveys/testData_30percent2023_WVplusSalmonSites.csv", sep="/"))
                            # "csvs_to_SDMs/testData_30percent2023_X_Cluster.csv", sep="/"))
test_data<- test_data[-1]
test_data<- test_data[-2]# remove pixel ID column ("X")
colnames(test_data)[2]<- "pr_ab"
test_data<- test_data[,c(1:6,65)]

train_data<- read.csv(paste(SDM_path,
      "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_train_withoutShorelineRecords.csv",
      # csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_train.csv",
      sep="/"))
# train_data<- read.csv(paste(SDM_path, "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_28May2024_FINAL.csv", sep="/"), header=T)
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

stack_vars



### Extract data from raster layers at each location of pres/abs  ##############
###=============================================================================
test_data<- sdm_extract( data= test_data[,-c(5:6)], 
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

###=============================================================================
### MAXENT ; model without Bathy and ANY Nutrient variables                ###############
###=============================================================================
summary(train_data$bathymetry)

# Create subset of variables
subset00<- names(stack_vars)[c(4:9, 11:16, 24:28)]

# SUbset raster variables 
subset_ras00<- which(names(stack_vars) %in% subset00)
filtered_vars00 <- raster::subset(stack_vars, subset_ras00)

# Subset the variables in the training dataset
train_data_subsetVars00<- train_data[c(1:6,which(colnames(train_data) %in% subset00)) ]

# Subset the variables in the testing dataset
test_data_subsetVars00<- test_data[c(1:6,which(colnames(test_data) %in% subset00)) ]

### Run model replicates
source("C:/Users/romi_/OneDrive/Documents/GitHub/SDMs_Broughton/maxent_replicates_function.R")
mod_maxent00<- model_replicates_Varsfunction_v2(replicates_number= 50,
                                             training_data= train_data_subsetVars00,
                                             variables= filtered_vars00,
                                             n_rdompoints= 10000,
                                             selected_variables= 17,
                                             pred= T,
                                             testing_data= test_data_subsetVars00,
                                             bgd= "variable")

# colnames(mod_maxent00$mod_performance)[5:6]<- c("AUC_presabs","AUC_presbgd")
# save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/50repMaxent_17variables.RData")
load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/50repMaxent_17variables.RData")


###=============================================================================
### MAXENT ; model with second filtering of selected variables (>= 2% importance)###############
###=============================================================================
# source("C:/Users/romi_/OneDrive/Documents/GitHub/SDMs_Broughton/maxent_replicates_function.R")
variables_importance_00<- mod_maxent00$variables_importance
variables_importance_table00<- variables_importance_00%>%
  group_by(variable)%>%
  summarize(mean_var_contrib= mean(variable_contrib, na.rm = TRUE),
            sd_var_contrib= sd(variable_contrib),
            mean_var_permitImportance= mean(permutation_importance, na.rm = TRUE),
            sd_var_permitImportance= sd(permutation_importance))

variables_importance_table00[which(variables_importance_table00$mean_var_permitImportance< 2 | variables_importance_table00$mean_var_contrib< 2),"variable"]

selected_variable_names00<- variables_importance_table00[which(variables_importance_table00$mean_var_permitImportance>= 2 | variables_importance_table00$mean_var_contrib>= 2),"variable"]
selected_variables00<- variables_importance_table00[which(variables_importance_table00$mean_var_permitImportance>= 2 | variables_importance_table00$mean_var_contrib>= 2),]
selected_variables00<- selected_variables00$variable
subset001<- selected_variables00



stack_vars<-  rast(paste(SDM_path, "variables_selection/stack_29FilteredVars_stratification_FINAL2.tif", sep="/"))
names(stack_vars)<-  c( "bathymetry"    ,     "DIF"   ,             "DIN"    ,            "eastherness" ,      
"janBSpd_min",        "janBT_ave",          "janSSpd_min",        "julBS_ave",
"julBSpd_max",        "julBT_ave",          "julBT_max",          "julBT_min",
"julSS_ave",          "julSSpd_ave",        "julSSpd_min",        "julST_ave",
"NO3_spring23_max",   "NO3_spring23_mean",  "NO3_spring23_min",   "NO3_spring23_sd",
"NO3_summ23_mean",    "NO3_summ23_sd",      "NO3_winter23_sd",    "northness",
"slope",              "tidal_cur",          "TPI",                "wind",
"stratification_ind")


which(names(stack_vars) %in% subset001)
subset_ras001<- which(names(stack_vars) %in% subset001)
filtered_vars001 <- raster::subset(stack_vars, subset_ras001)#grep('_GAM', names(my_stack), value = T)
length(names(filtered_vars001))# 15 instead of 14

which(subset001 %in% names(filtered_vars001))

subset001 %in% names(filtered_vars001)
subset001 %in% names(filtered_vars00)

train_data_subsetVars001<- train_data[c(1:6,which(colnames(train_data)%in% subset001))]
test_data_subsetVars001<- test_data[c(1:6,which(colnames(test_data) %in% subset00)) ]


model_replicates_002<- model_replicates_Varsfunction_v2(replicates_number= 50, 
                                                         training_data= train_data_subsetVars001, 
                                                         variables= filtered_vars001, 
                                                         n_rdompoints= 10000,
                                                         selected_variables= 15,
                                                         testing_data= test_data_subsetVars001,
                                                         pred= F,
                                                         bgd= "variable")

# save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/50repMaxent_15variables2percent.RData")


performance_df_001<- model_replicates_001$mod_performance

model_replicates_001$mod_performance%>%
  summarise(mean_threshold.SS= mean(threshold.SS), sd_threshold.SS= sd(threshold.SS),
            mean_threshold.NoO= mean(threshold.NoO), sd_threshold.NoO= sd(threshold.NoO),
            mean_threshold.ESS= mean(threshold.ESS), sd_threshold.ESS= sd(threshold.ESS),
            mean_AUC_presbgd= mean(AUC_presbgd), sd_AUC_presbgd= sd(AUC_presbgd),
            mean_AUC_presabs= mean(AUC_presabs), sd_AUC_presabs= sd(AUC_presabs))


# ## to calculate a mean and SD rasters
# mean_maxent3001_v2<- calc(model_replicates_001$preds, fun = mean)
# # writeRaster(mean_maxent3001_v2, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/July2024/maxent_pred_mean_13Vars_50.tif")
# mapview::mapview(mean_maxent3001_v2, col.regions = rev(pal(10)), at = seq(0, 1, 0.1), legend = TRUE)
# 
# ### to calculate raster range
# sd_maxent001_v2<-  calc(model_replicates_001$preds, fun = sd)
# # writeRaster(sd_maxent001_v2, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/July2024/maxent_pred_SD_13Vars_50Rep.tif")
# # mapview::mapview(sd_maxent001_v2, col.regions = (palSD(10)), at = seq(0, 0.25, 0.04), legend = TRUE)


library(mapview)
pal = mapviewPalette("mapviewTopoColors")
pal = rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
                                      space = "rgb")
palSD = rgb.palette <- colorRampPalette(c("lightblue", "gold2", "purple"),
                                        space = "rgb")
pal_range= Lab.palette <- colorRampPalette(c("red", "orange", "blue"),
                                           space = "Lab")
# 
# # mapview::mapview(mean_maxent3001_v2, col.regions = rev(pal(100)), at = seq(0, 1, 0.2), legend = TRUE)
# # mapview::mapview(range_preds001_v2, col.regions = rev(pal(20)), at = seq(0, 1, 0.1), legend = TRUE)


###=============================================================================
### MAXENT ; model with third filtering of selected variables (>= 5% importance) ###############
###=============================================================================
variables_importance_table002<- model_replicates_002$variables_importance
variables_importance_table002<- variables_importance_table002%>%
  group_by(variable)%>%
  summarize(mean_var_contrib= mean(variable_contrib),
            sd_var_contrib= sd(variable_contrib),
            mean_var_permitImportance= mean(permutation_importance),
            sd_var_permitImportance= sd(permutation_importance))



selected_variable_names002<- variables_importance_table002[which(variables_importance_table002$mean_var_permitImportance>= 5 | variables_importance_table$mean_var_contrib>= 5),"variable"]
variables_importance_table[which(variables_importance_table002$mean_var_permitImportance>= 5),"variable"]
selected_variable_names002<- selected_variable_names002$variable
subset002<- selected_variable_names002


which(names(stack_vars) %in% subset002)
subset_ras002<- which(names(stack_vars) %in% subset002)
filtered_vars002 <- raster::subset(stack_vars, subset_ras002)#grep('_GAM', names(my_stack), value = T)
length(names(filtered_vars002))# 11

which(subset002 %in% names(filtered_vars002))
subset002 %in% names(filtered_vars002)


train_data_subsetVars002<- train_data[c(1:6, which(colnames(train_data) %in% subset002))]
test_data_subsetVars002<- test_data[c(1:6, which(colnames(test_data) %in% subset002)) ]

model_replicates_003<- model_replicates_Varsfunction_v2(replicates_number= 50, 
                                                        training_data= train_data_subsetVars002,
                                                        testing_data= test_data_subsetVars002,
                                                        variables= filtered_vars002, 
                                                        n_rdompoints= 10000,
                                                        selected_variables= 11,
                                                        pred= F,
                                                        bgd= "variable")

performance_df002<- model_replicates_002$mod_performance
# test_data_i_sf <- na.exclude(model_replicates_002$Test_data[,c("x", "y", "pr_ab")]) %>%
#   st_as_sf(coords = c("x", "y"), crs = "EPSG:3005")
# mapview::mapview(test_data_i_sf, cex=2, add=T,  zcol="pr_ab", col.regions= c("red", "blue"))# a visual check



# save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/50repMaxent_9variables5percent.RData")


x <- raster::stack()
for (i in 48:50){
  Loop.r <- predict(filtered_vars002, model_replicates_002$models[[1]], progress='text', na.rm=TRUE)
  Loop.r<- raster(Loop.r)
  # plot(Loop.r)
  names(Loop.r)<- paste("mod_rep", i, sep="_")
  x <- stack(x, Loop.r)
}

names(x)

## to calculate a mean and SD rasters
# mean_maxent002_<- calc(model_replicates_002$preds, fun = mean)
mean_maxent002_<- calc(x, fun = mean)
# writeRaster(mean_maxent002_, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/maxent_pred_mean_11Vars_50.tif")
mapview::mapview(mean_maxent002_, col.regions = rev(pal(10)), at = seq(0, 1, 0.1), legend = TRUE)

### to calculate raster range
# sd_maxent002<-  calc(model_replicates_002$preds, fun = sd)
sd_maxent002<-  calc(x, fun = sd)
writeRaster(sd_maxent002, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/maxent_pred_SD_11Vars_50.tif")
mapview::mapview(sd_maxent002, col.regions = (palSD(10)), at = seq(0, 0.25, 0.04), legend = TRUE)


### Calculate average thresholds ======
model_replicates_002$mod_performance%>%
  summarize(mean_threshold.SS= mean(threshold.SS), sd_threshold.SS= sd(threshold.SS), mean_threshold.NoO= mean(threshold.NoO), sd_threshold.NoO= sd(threshold.NoO), mean_threshold.ESS= mean(threshold.ESS),
            sd_threshold.ESS= sd(threshold.ESS), meanAUC=mean(AUC_presabs))


performance_df002<- model_replicates_002$mod_performance#%>%
unique(variables_importance_002$variable)



###=============================================================================
### MAXENT ; filtering of variables (>=7% importance for MAXENT) ###############
###=============================================================================
variables_importance_table002<- model_replicates_002$variables_importance%>%
  group_by(variable)%>%
  summarize(mean_var_contrib= mean(variable_contrib),
            sd_var_contrib= sd(variable_contrib),
            mean_var_permitImportance= mean(permutation_importance),
            sd_var_permitImportance= sd(permutation_importance))


selected_variable_names003<- variables_importance_table002[which(variables_importance_table002$mean_var_permitImportance>= 7 | variables_importance_table002$mean_var_contrib>= 7),"variable"]
variables_importance_table002[which(variables_importance_table002$mean_var_permitImportance>= 7),"variable"]
selected_variable_names003<- selected_variable_names003$variable
subset003<- selected_variable_names003

subset_ras003<- which(names(stack_vars) %in% subset003)
filtered_vars003 <- raster::subset(stack_vars, subset_ras003)
length(names(filtered_vars003))# 4--- 9


train_data_subsetVars003<- train_data[c(1:6,which(colnames(train_data)%in% subset003))]
test_data_subsetVars003<- test_data[c(1:6,which(colnames(test_data) %in% subset003))]


model_replicates_003<- model_replicates_Varsfunction_v2(replicates_number= 50,
                                                     training_data= train_data_subsetVars003, 
                                                     variables= filtered_vars003, 
                                                     n_rdompoints= 10000,
                                                     selected_variables= 9,
                                                     testing_data = test_data_subsetVars003,
                                                     pred= T,
                                                     bgd= "variable")

performance_df003<- model_replicates_003$mod_performance
# save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/50repMaxent_18_11_4variables.RData")
## save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sep2024/50repMaxent_7variables8percent.RData")

## to calculate a mean and SD rasters
mean_maxent003<- calc(model_replicates_003$preds, fun = mean)
# writeRaster(mean_maxent003, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/July2024/maxent_pred_mean_7Vars_50.tif")
mapview::mapview(mean_maxent003, col.regions = rev(pal(10)), at = seq(0, 1, 0.1), legend = TRUE)

### to calculate raster range
sd_maxent003<-  calc(model_replicates_002$preds, fun = sd)
# writeRaster(sd_maxent003, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/July2024/maxent_pred_SD_7Vars_50.tif")
mapview::mapview(sd_maxent003, col.regions = (palSD(10)), at = seq(0, 0.25, 0.04), legend = TRUE)



performance_df003<- model_replicates_003$mod_performance#%>%
  # summarise(mean_threshold.SS= mean(threshold.SS), sd_threshold.SS= sd(threshold.SS), 
  #           mean_threshold.NoO= mean(threshold.NoO), sd_threshold.NoO= sd(threshold.NoO), 
  #           mean_threshold.ESS= mean(threshold.ESS), sd_threshold.ESS= sd(threshold.ESS), 
  #           mean_AUC_presbgd= mean(AUC_presbgd), sd_AUC_presbgd= sd(AUC_presbgd),
  #           mean_AUC_presabs= mean(AUC_presabs), sd_AUC_presabs= sd(AUC_presabs))


variables_importance_table003<- model_replicates_003$variables_importance%>%
  group_by(variable)%>%
  summarize(mean_var_contrib= mean(variable_contrib),
            sd_var_contrib= sd(variable_contrib),
            mean_var_permitImportance= mean(permutation_importance),
            sd_var_permitImportance= sd(permutation_importance))

selected_variable_names004<- variables_importance_table003[which(variables_importance_table003$mean_var_permitImportance>= 11 | variables_importance_table003$mean_var_contrib>= 11),"variable"]
variables_importance_table003[which(variables_importance_table003$mean_var_permitImportance>= 11),"variable"]
selected_variable_names004<- selected_variable_names004$variable
subset004<- selected_variable_names004

subset_ras004<- which(names(stack_vars) %in% subset004)
filtered_vars004 <- raster::subset(stack_vars, subset_ras004)
length(names(filtered_vars004))# 5


train_data_subsetVars004<- train_data[c(1:6,which(colnames(train_data)%in% subset004))]
test_data_subsetVars004<- test_data[c(1:6,which(colnames(test_data) %in% subset004)) ]


model_replicates_004<- model_replicates_Varsfunction_v2(replicates_number= 50, 
                                                        training_data= train_data_subsetVars004, 
                                                        variables= filtered_vars004, 
                                                        n_rdompoints= 10000,
                                                        selected_variables= 5,
                                                        testing_data = test_data_subsetVars004,
                                                        pred= T,
                                                        bgd= "variable")


performance_df004<- model_replicates_004$mod_performance
variables_importance_004<- model_replicates_004$variables_importance
  
variables_importance_table004<- variables_importance_004%>%
  group_by(variable)%>%
  summarize(mean_var_contrib= mean(variable_contrib),
            sd_var_contrib= sd(variable_contrib),
            mean_var_permitImportance= mean(permutation_importance),
            sd_var_permitImportance= sd(permutation_importance))



####============================================================================
### Test for differences among models with different number of variables =======
performance_df_00$Nvariables<- 17#18#20
performance_df_001$Nvariables<- 15#16
performance_df002$Nvariables<- 11
performance_df003$Nvariables<- 9#4
performance_df004$Nvariables<- 5

performance_dfmerged<- rbind(performance_df_00, performance_df_001, performance_df002, performance_df003, performance_df004)

performance_dfmerged$Nvariables<- as.factor(performance_dfmerged$Nvariables)

ajuste <- lm(performance_dfmerged$AUC_presabs~ performance_dfmerged$Nvariables)
summary(ajuste)
anova(ajuste)

a1 <- aov(performance_dfmerged$AUC_presabs~ performance_dfmerged$Nvariables)
posthoc <- TukeyHSD(x=a1, 'performance_dfmerged$Nvariables', conf.level=0.95)
plot(a1)
plot(TukeyHSD(a1, conf.level=.95), las = 2)

cld <- multcompView::multcompLetters4(a1, posthoc)
print(cld)

Tk <- performance_dfmerged%>% group_by(Nvariables) %>%
  summarise(mean_AUC_presbgd= mean(AUC_presbgd), #sd_AUC_presbgd= sd(AUC_presbgd),
            mean_AUC_presabs= mean(AUC_presabs), #sd_AUC_presabs= sd(AUC_presabs)),
            quant = quantile(AUC_presabs, probs = 0.75)) %>%
  arrange(desc(mean_AUC_presabs))

# cld <- as.data.frame.list(cld$`performance_dfmerged$Nvariables`)
Tk$cld <- cld$`performance_dfmerged$Nvariables`$Letters
Tk<- as.data.frame(Tk)
str(Tk)

modelAUC_plot0<- performance_dfmerged%>%
  ggplot(aes(x= as.factor(Nvariables), y=AUC_presabs))+
  geom_boxplot()+# geom_violin()+
  theme_bw()+
  ylim(c(0.5, 0.85))+
  labs(x= "Number of Variables Included", y= "AUC (presence/absence)")+
  geom_text(data = Tk, aes(x = Nvariables, y = quant, label = cld), size = 3, vjust=-1, hjust =-1)


ajuste <- lm(performance_dfmerged$AUC_presbgd~ performance_dfmerged$Nvariables)
summary(ajuste)
anova(ajuste)

a1 <- aov(performance_dfmerged$AUC_presbgd~ performance_dfmerged$Nvariables)
posthoc <- TukeyHSD(x=a1, 'performance_dfmerged$Nvariables', conf.level=0.95)
plot(a1)
plot(TukeyHSD(a1, conf.level=.95), las = 2)

# anova(performance_dfmerged$AUC_presbgd, performance_dfmerged$Nvariables)
cld <- multcompView::multcompLetters4(a1, posthoc)
print(cld)

Tk2 <- performance_dfmerged%>% group_by(Nvariables) %>%
  summarise(mean_AUC_presbgd= mean(AUC_presbgd),
            quant = quantile(AUC_presbgd, probs = 0.75)) %>%
  arrange(desc(mean_AUC_presbgd))

cld2 <- as.data.frame.list(cld$`performance_dfmerged$Nvariables`)
Tk2$cld <- cld2$Letters
Tk2<- as.data.frame(Tk2)
str(Tk2)
# Tk2<- 

modelAUC_plot<- performance_dfmerged%>%
  ggplot(aes(x= as.factor(Nvariables), y=AUC_presbgd))+
  geom_boxplot()+# geom_violin()+
  theme_bw()+
  ylim(c(0.5, 0.85))+
  labs(x= "Number of Variables Included", y= "AUC (presences/background)")+
  geom_text(data = Tk2, aes(x = Nvariables, y = quant, label = cld), size = 3, vjust=-1, hjust =-1)


cowplot::plot_grid(modelAUC_plot0, modelAUC_plot)
plotspath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs"
# ggsave(paste(plotspath,"modelsAUC_comparison_boxplots_March2025.png", sep="/"), units="cm", width=17, height= 9, dpi=300)


# save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/50repMaxent_17_15_11_5variables.RData")
load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/50repMaxent_17_15_11_5variables.RData")


variables_importance_003<- model_replicates_003$variables_importance
unique(variables_importance_004$variable)
unique(variables_importance_003$variable)
unique(variables_importance_002$variable)
unique(variables_importance_00$variable)
unique(variables_importance001$variable)
# 
# 
a003<- variables_importance_003%>%
  ggplot(aes(y= variable, x= variable_contrib))+
  geom_boxplot(outlier.shape = NA, color="blue")+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  # geom_bar( alpha= 0.5, color="blue")+
  labs(y="Environmental Variable", x= "Importance (%)")+
  theme(legend.position = "NULL")+theme_bw()
  # geom_vline(xintercept=8, linetype="dashed",
  #            color = "red", size=1)+ theme_bw()
# 
# b003<- variables_importance_003%>%
#   ggplot(aes(y= variable, x= permutation_importance))+
#   # geom_boxplot(outlier.shape = NA, color="blue")+
#   # geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
#   geom_boxplot(aes(y= variable, x= permutation_importance), color="blue",outlier.shape = NA, )+
#   geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
#   labs(y="Environmental Variable", x= "Permutation Importance")+
#   theme(legend.position = "NULL")+theme_bw()
#   # geom_vline(xintercept=8, linetype="dashed",
#   #            color = "red", size=1)+ theme_bw()
# 
# cowplot::plot_grid(a, b, a002,b002, a003, b003, nrow = 3, labels = c("A)", "","B)", "", "C)"))


path_model_res<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024"
setwd(paste(path_model_res))
cowplot::plot_grid(a, b, a002,b002, a003, b003, nrow = 3, labels = c("A)", "","B)", "", "C)"))
# ggsave("SDMs_13Variables_Importance_boxplots.png", units="cm", width=20, height= 14, dpi=300)











library(raster)
library(rasterVis)
library(classInt)
library(RColorBrewer)

nColor <- 50
break1 <- classIntervals(mean_maxent2[!is.na(mean_maxent2)],
                         n = nColor, style = "quantile")

raster_mean_plot <- levelplot(mean_maxent2, 
                              col.regions = colorRampPalette(brewer.pal(9, 'RdYlGn')), 
                              at = break1$brks, margin = FALSE)
raster_mean_plot 


break_equal <- classIntervals(mean_maxent2[!is.na(mean_maxent2)],
                              n = nColor, style = "equal")

break2 <- classIntervals(sd_maxent2[!is.na(sd_maxent2)],
                         n = nColor, style = "quantile")

raster_sd_plot<- levelplot(sd_maxent2, 
                           col.regions = colorRampPalette(brewer.pal(9, 'RdYlGn')), 
                           at = break2$brks, margin = FALSE)

break_range <- classIntervals(range_preds[!is.na(range_preds)],
                              n = nColor, style = "quantile")
raster_range_plot<- levelplot(range_preds, 
                              col.regions = colorRampPalette(brewer.pal(9, 'RdYlGn')), 
                              at = break_range$brks, margin = FALSE)

library(gridExtra)
grid.arrange(raster_mean_plot, raster_sd_plot, raster_range_plot, ncol=2)




