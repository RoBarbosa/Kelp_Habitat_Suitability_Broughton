###==================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### 4.3- Model accuracy evaluation                              ###############
### Author: Romina Barbosa                                      ################
### Date: 25-July-2024                                          ################
### Last edition: 19-March-2024
###==================================================================
# Load packages
library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(flexsdm)

# SDM_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs"
SDM_path<- "/Volumes/Romina_BATI/BATI/SDMs"

path_model_res<- paste(SDM_path, "outputs_SDMs", sep="/")
path_clusters<- paste(SDM_path, "Clustering/Last version", sep="/")
mypath<- paste(SDM_path, "csvs_to_SDMs", sep="/")
val_path<- paste(SDM_path, "SDMs_validation/March_2025", sep="/")
functions_path<- "C:/Users/romi_/OneDrive/Documents/GitHub/SDMs_Broughton/FINAL_scripts" 
  
setwd((path_model_res))
dir()
pred_testdata<- read.csv(paste(val_path, "pred_testdata_30percent.csv", sep="/"))

pred_testdata_sf <- pred_testdata %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(pred_testdata_sf, cex=2,  zcol="pr_ab", col.regions= c("red", "blue"))# a visual check
mapview::mapview(pred_testdata_sf, cex=2,  zcol="Ensemble_bynary")# a visual check

## Calculate the true positives and negatives
pred_testdata<- na.exclude(pred_testdata)



###=========================================================================================####
### Load the function to calculate the confusion matrix from testing data and model threshold ====
source(paste(functions_path, "4.3.eval_matrix_function.R", sep="/"))



###==========================================================================###
### Calculate confusion matrix for each model and dataset ======================
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

pred_test_sat<- read.csv(paste(val_path, "pred_testdata_satellite.csv", sep="/"))
colnames(pred_test_sat)
# pred_test_sat<- pred_test_sat[,-8]# exclude column "limit_cell"

pred_test_sat<- sdm_extract( data= pred_test_sat,
                             x = "lon",
                             y = "lat",
                             env_layer = stack_vars[[1]],
                             variables = NULL,
                             filter_na = FALSE)

## Summary of kelp records from the independent dataset -- TABLE 1 ======
pred_test_sat%>%
  group_by(Cluster, pr_ab)%>%
  summarize(n= length(pr_ab))

#     Cluster pr_ab  n
# 1       1     0    28
# 2       2     0   222
# 3       2     1   114
# 4       3     0   111
# 5       3     1    47
# 6       4     0   510
# 7       4     1   211
# 8       5     0   459
# 9       5     1    54
# 10      6     0   135
# 11      6     1    45

# pr_ab     n
# 1     0  1472   # N of absences from the independent testing dataset
# 2     1   475   # N of presences from the independent testing dataset


# pred_testdata<- read.csv(paste(path_model_res, "predict_test30_data.csv", sep="/"))


# Model NoO_Threshold
# 1   Maxent         0.033
# 2      RAF         0.445
# 3 Ensemble         0.246
models_NoO_thr<- data.frame("Model"= c("Maxent", "RAF", "Ensemble"), "NoO_Threshold"= c(0.033, 0.445, 0.246))#0.447


# We correct potential disagreement based on model prediction and substrate type 
# i.e., if the SDM prediction of presence was true (TP) but the substrate was sandy (binary=0), then it was a false negative 
model= "Ensemble"
testdata= pred_testdata
threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"]


## ENSEMBLE MODEL ##
matrix_ensemble_Model<- eval_matrix_function(evaluation= "model", bathy= F, output_entire_table = T,
                                             model= "Ensemble", testdata= pred_testdata, 
                                             threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])

matrix_ensemble_Model_sat<- eval_matrix_function(evaluation= "model", bathy= F, output_entire_table = T,
                                                 model= "Ensemble", testdata= pred_test_sat, 
                                             threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])

# After considering available substrate
matrix_ensemble_ModelSubs<- eval_matrix_function(evaluation= "model_substrate", bathy= F, output_entire_table = T,
                                                 model= "Ensemble", testdata= pred_testdata, 
                                             threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])

matrix_ensemble_ModelSubs_sat<- eval_matrix_function(evaluation= "model_substrate", bathy= F, output_entire_table = T,
                                                     model= "Ensemble", testdata= pred_test_sat, 
                                                 threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])

# # Filtering data considering bathymetric limit (15 m)
matrix_ensemble_ModelSubsbathy<- eval_matrix_function(evaluation= "model_substrate", bathy= T, bathy_limit= 15, output_entire_table = T,
                                                      model= "Ensemble", testdata= pred_testdata,
                                                 threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])

matrix_ensemble_ModelSubsbathy_sat<- eval_matrix_function(evaluation= "model_substrate", bathy= T, bathy_limit= 15, output_entire_table = T,
                                                          model= "Ensemble", testdata= pred_test_sat,
                                                     threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])

# After considering bathymetric limit on the raster (15 m)
matrix_ensemble_ModelSubsbathy2<- eval_matrix_function2(evaluation= "model_substrate_bathy", output_entire_table = T,
                                                      model= "Ensemble", testdata= pred_testdata, 
                                                      threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])

matrix_ensemble_ModelSubsbathy_sat2<- eval_matrix_function2(evaluation= "model_substrate_bathy", output_entire_table = T,
                                                          model= "Ensemble", testdata= pred_test_sat, 
                                                          threshold= models_NoO_thr[which(models_NoO_thr$Model=="Ensemble"), "NoO_Threshold"])


matrix_ensemble_Model[[1]]$data<- "30_percent"
matrix_ensemble_Model_sat[[1]]$data<- "ind_satellite"
matrix_ensemble_ModelSubs[[1]]$data<- "30_percent"
matrix_ensemble_ModelSubs_sat[[1]]$data<- "ind_satellite"
matrix_ensemble_ModelSubsbathy[[1]]$data<- "30_percent"
matrix_ensemble_ModelSubsbathy_sat[[1]]$data<- "ind_satellite"
matrix_ensemble_ModelSubsbathy2[[1]]$data<- "30_percent"
matrix_ensemble_ModelSubsbathy_sat2[[1]]$data<- "ind_satellite"


accuracy_ensemble_Model<- rbind(matrix_ensemble_Model[[1]], matrix_ensemble_Model_sat[[1]], 
                                matrix_ensemble_ModelSubs[[1]], matrix_ensemble_ModelSubs_sat[[1]],
                                matrix_ensemble_ModelSubsbathy[[1]], matrix_ensemble_ModelSubsbathy_sat[[1]],
                                matrix_ensemble_ModelSubsbathy2[[1]], matrix_ensemble_ModelSubsbathy_sat2[[1]])

# accuracy_ensemble_Model[which(accuracy_ensemble_Model$evaluation == "XX"),"evaluation"]<- "FN"

str(accuracy_ensemble_Model)
unique(accuracy_ensemble_Model$data)
accuracy_ensemble_Model$assessment<- as.factor(accuracy_ensemble_Model$assessment)
unique(accuracy_ensemble_Model$assessment)

accuracy_ensemble_Model%>%
  ggplot(aes(x= assessment, y= percentage, fill= as.factor(evaluation)))+ 
  geom_bar(stat="identity", width = 0.5)+ facet_wrap(~data) + theme_bw()
    


# # Evaluate the pred after masking by substrate
# # Add a column to include the FP,TP, FN and FP 
# pred_testdata$accuracy_matrix<- NA
# pred_testdata[which(pred_testdata$pr_ab==1 & pred_testdata$Ensemble < models_NoO_thr[which(models_NoO_thr$Model=="ensemble"),"NoO_Threshold"]),"accuracy_matrix"]<- "FN"
# pred_testdata[which(pred_testdata$pr_ab==0 & pred_testdata$Ensemble >= models_NoO_thr[which(models_NoO_thr$Model=="ensemble"),"NoO_Threshold"]),"accuracy_matrix"]<- "FP"
# pred_testdata[which(pred_testdata$pr_ab==1 & pred_testdata$Ensemble >= models_NoO_thr[which(models_NoO_thr$Model=="ensemble"),"NoO_Threshold"]),"accuracy_matrix"]<- "TP"
# pred_testdata[which(pred_testdata$pr_ab==0 & pred_testdata$Ensemble < models_NoO_thr[which(models_NoO_thr$Model=="ensemble"),"NoO_Threshold"]),"accuracy_matrix"]<- "TN"



## MAXENT MODEL ##
matrix_maxent_Model<- eval_matrix_function(evaluation= "model",  bathy= F, output_entire_table = T,
                                           model= "maxent_mean", testdata= pred_testdata,
                                           threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

matrix_maxent_Model_sat<- eval_matrix_function(evaluation= "model",  bathy= F,
                                               model= "maxent_mean", testdata= pred_test_sat, output_entire_table = T,
                                           threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

# After considering available substrate
matrix_maxent_ModelSubs<- eval_matrix_function(evaluation= "model_substrate", bathy= F, output_entire_table = T,
                                               model= "maxent_mean", testdata= pred_testdata,
                                           threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

matrix_maxent_ModelSubs_sat<- eval_matrix_function(evaluation= "model_substrate", bathy= F,  output_entire_table = T,
                                                   model= "maxent_mean", testdata= pred_test_sat,
                                           threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

# After considering bathymetry and substrate
matrix_maxent_ModelSubs_bathy<- eval_matrix_function(evaluation= "model_substrate",  bathy= T, bathy_limit= 15, output_entire_table = T,
                                               model= "maxent_mean", testdata= pred_testdata,
                                               threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

matrix_maxent_ModelSubs_sat_bathy<- eval_matrix_function(evaluation= "model_substrate",  bathy= T, bathy_limit= 15, output_entire_table = T,
                                                   model= "maxent_mean", testdata= pred_test_sat,
                                                   threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

# After considering bathymetric limit on the raster (15 m)
matrix_maxent_ModelSubs_bathy2<- eval_matrix_function2(evaluation= "model_substrate_bathy", output_entire_table = T,
                                                        model= "maxent_mean", testdata= pred_testdata, 
                                                        threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

matrix_maxent_ModelSubs_sat_bathy2<- eval_matrix_function2(evaluation= "model_substrate_bathy", output_entire_table = T,
                                                            model= "maxent_mean", testdata= pred_test_sat, 
                                                            threshold= models_NoO_thr[which(models_NoO_thr$Model=="Maxent"), "NoO_Threshold"])

matrix_maxent_Model[[1]]$data<- "30_percent"
matrix_maxent_Model_sat[[1]]$data<- "ind_satellite"
matrix_maxent_ModelSubs[[1]]$data<- "30_percent"
matrix_maxent_ModelSubs_sat[[1]]$data<- "ind_satellite"
matrix_maxent_ModelSubs_bathy[[1]]$data<- "30_percent"
matrix_maxent_ModelSubs_sat_bathy[[1]]$data<- "ind_satellite"
matrix_maxent_ModelSubs_bathy2[[1]]$data<- "30_percent"
matrix_maxent_ModelSubs_sat_bathy2[[1]]$data<- "ind_satellite"

accuracy_maxent_Model<- rbind(matrix_maxent_Model[[1]], matrix_maxent_Model_sat[[1]], 
                              matrix_maxent_ModelSubs[[1]], matrix_maxent_ModelSubs_sat[[1]],
                              matrix_maxent_ModelSubs_bathy[[1]], matrix_maxent_ModelSubs_sat_bathy[[1]],
                              matrix_maxent_ModelSubs_bathy2[[1]], matrix_maxent_ModelSubs_sat_bathy2[[1]])

accuracy_maxent_Model%>%
  ggplot(aes(x= assessment, y= percentage, fill= as.factor(evaluation)))+ geom_bar(stat="identity")+ facet_wrap(~data) + theme_bw()




## RAF MODEL ##
matrix_raf_Model<- eval_matrix_function(evaluation= "model", bathy= F,
                                        model= "RAF_mean", testdata= pred_testdata, 
                                        threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

matrix_raf_Model_sat<- eval_matrix_function(evaluation= "model", bathy= F,
                                            model= "RAF_mean", testdata= pred_test_sat, 
                                        threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

# After considering available substrate
matrix_raf_ModelSubs<- eval_matrix_function(evaluation= "model_substrate", bathy= F,
                                            model= "RAF_mean", testdata= pred_testdata, 
                                            threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

matrix_raf_ModelSubs_sat<- eval_matrix_function(evaluation= "model_substrate", bathy= F,
                                                model= "RAF_mean", testdata= pred_test_sat, 
                                            threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

# After considering available substrate and bathy
matrix_raf_ModelSubs_bathy<- eval_matrix_function(evaluation= "model_substrate", bathy= T, bathy_limit= 15,
                                                  model= "RAF_mean", testdata= pred_testdata, 
                                            threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

matrix_raf_ModelSubs_sat_bathy<- eval_matrix_function(evaluation= "model_substrate", bathy= T, bathy_limit= 15,
                                                      model= "RAF_mean", testdata= pred_test_sat, 
                                                threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

# After considering bathymetric limit on the raster (15 m)
matrix_raf_ModelSubsbathy2<- eval_matrix_function2(evaluation= "model_substrate_bathy", output_entire_table = T,
                                                        model= "RAF_mean", testdata= pred_testdata, 
                                                        threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

matrix_raf_ModelSubsbathy_sat2<- eval_matrix_function2(evaluation= "model_substrate_bathy", output_entire_table = T,
                                                            model= "RAF_mean", testdata= pred_test_sat, 
                                                            threshold= models_NoO_thr[which(models_NoO_thr$Model=="RAF"), "NoO_Threshold"])

matrix_raf_Model[[1]]$data<- "30_percent"
matrix_raf_Model_sat[[1]]$data<- "ind_satellite"
matrix_raf_ModelSubs[[1]]$data<- "30_percent"
matrix_raf_ModelSubs_sat[[1]]$data<- "ind_satellite"
matrix_raf_ModelSubs_bathy[[1]]$data<- "30_percent"
matrix_raf_ModelSubs_sat_bathy[[1]]$data<- "ind_satellite"
matrix_raf_ModelSubsbathy2[[1]]$data<- "30_percent"
matrix_raf_ModelSubsbathy_sat2[[1]]$data<- "ind_satellite"

accuracy_raf_Model<- rbind(matrix_raf_Model[[1]], matrix_raf_Model_sat[[1]], 
                           matrix_raf_ModelSubs[[1]], matrix_raf_ModelSubs_sat[[1]],
                           matrix_raf_ModelSubs_bathy[[1]], matrix_raf_ModelSubs_sat_bathy[[1]],
                           matrix_raf_ModelSubsbathy2[[1]], matrix_raf_ModelSubsbathy_sat2[[1]])

accuracy_raf_Model%>%
  ggplot(aes(x= assessment, y= percentage, fill= as.factor(evaluation)))+ geom_bar(stat="identity")+ facet_wrap(~data) + theme_bw()


# ==============================================================================
#  Merge confusion matrix and save it ==========================================
# ==============================================================================
accuracy_3Models<- rbind(accuracy_maxent_Model, accuracy_raf_Model, accuracy_ensemble_Model)

droplevels(accuracy_3Models)%>%
  ggplot(aes(x= assessment, y= percentage, fill= as.factor(evaluation)))+ 
  geom_bar(stat="identity")+ facet_wrap(~Model + data, ncol=2) + theme_bw()

# write.csv(accuracy_3Models, paste(val_path, "accuracy_3Models_excluding_confusingPoints.csv", sep="/"))

# accuracy_3Models<- read.csv(paste(val_path, "accuracy_3Models_excluding_confusingPoints.csv", sep="/"))

# ==============================================================================
#  Calculate Overall Accuracy ===================================================
Overal_accuracy_table<- accuracy_3Models%>%
  filter(evaluation != "FN")%>%
  filter(evaluation != "FP")%>%
  group_by(data, Model, NoO_Threshold, assessment)%>%
  summarize(over_acc= sum(percentage))



# Overal_accuracy_table<- accuracy_3Models%>%
#   filter(evaluation == "TP")%>%
#   # filter(evaluation != "FP")%>%
#   group_by(Model, assessment, data)%>%
#   summarize(TP= sum(percentage))

sensitivity_3modelsa<- accuracy_3Models%>%
  filter(evaluation != "TN")%>%
  filter(evaluation != "FP")%>%
  group_by(data, Model, NoO_Threshold, assessment)%>%
  summarize(TP_plus_FN= sum(percentage))

sensitivity_3models<- accuracy_3Models%>%
  filter(evaluation == "TP")%>%
  group_by(data, Model, NoO_Threshold, assessment)%>%
  summarize(TP= sum(percentage))

sensitivity_3models<- merge(sensitivity_3modelsa, sensitivity_3models, by=c("data", "Model", "NoO_Threshold", "assessment"))
sensitivity_3models$sensitivity<- round(sensitivity_3models$TP / sensitivity_3models$TP_plus_FN, 2)

sensitivity_3models %>%
  ggplot(aes(x= assessment, y = sensitivity))+
  geom_bar(stat = "identity")+
  facet_wrap(~Model + data, ncol=2)+ theme_bw()

specificity_3modelsa<- accuracy_3Models%>%
  filter(evaluation != "TP")%>%
  filter(evaluation != "FN")%>%
  group_by(data, Model, NoO_Threshold, assessment)%>%
  summarize(TN_plus_FP= sum(percentage))

specificity_3models<- accuracy_3Models%>%
  filter(evaluation == "TN")%>%
  group_by(data, Model, NoO_Threshold, assessment)%>%
  summarize(TN= sum(percentage))

specificity_3models<- merge(specificity_3modelsa, specificity_3models, by=c("data", "Model", "NoO_Threshold", "assessment"))
specificity_3models$specificity<- round(specificity_3models$TN / specificity_3models$TN_plus_FP, 2)

specificity_3models %>%
  ggplot(aes(x= assessment, y = specificity))+
  geom_bar(stat = "identity")+
  facet_wrap(~Model + data, ncol=2)+ theme_bw()

Overal_accuracy_table<- as.data.frame(Overal_accuracy_table)
TABLE_evaluation3models<- merge(Overal_accuracy_table, sensitivity_3models, 
                                by=c("data", "Model", "NoO_Threshold", "assessment"), all=T)

TABLE_evaluation3models<- merge(TABLE_evaluation3models, specificity_3models, 
                                by=c("data", "Model", "NoO_Threshold", "assessment"), all=T)

colnames(TABLE_evaluation3models)
write.csv(TABLE_evaluation3models, paste(val_path, "accuracy_table_including_confusingPoints.csv", sep="/"))

TABLE_evaluation3models<- TABLE_evaluation3models[,-c(6,7,9,10)]
str(TABLE_evaluation3models)

# Reshaping the data
TABLE_4 <- TABLE_evaluation3models %>%
  filter(assessment!= "model_substrate" & assessment!="model_substrate_bathy")%>%
  tidyr::pivot_longer(cols = c(over_acc, sensitivity, specificity),
               names_to = "Metric",
               values_to = "Value") %>%
  tidyr::pivot_wider(names_from = assessment, values_from = Value)


# write.csv(TABLE_4, paste(val_path, "accuracy_table_4.csv", sep="/"))


### Add column with accuracy of the original model =============================
# accuracy_raf_Model<- rbind(matrix_raf_Model[[2]]$, matrix_raf_Model_sat[[2]],
#                            matrix_raf_ModelSubs[[1]], matrix_raf_ModelSubs_sat[[1]],
#                            matrix_raf_ModelSubs_bathy[[1]], matrix_raf_ModelSubs_sat_bathy[[1]],
#                            matrix_raf_ModelSubsbathy2[[1]], matrix_raf_ModelSubsbathy_sat2[[1]])


pred_eval_data_sat<- cbind(matrix_ensemble_Model_sat[[2]],
      matrix_ensemble_ModelSubs_sat[[2]]$accuracy_ensemble_subs,
       # matrix_ensemble_ModelSubsbathy_sat[[2]]$accuracy_ensemble_subs,
      matrix_ensemble_ModelSubsbathy_sat2[[2]]$accuracy_ensemble_subs_bathy)

colnames(pred_eval_data_sat)[20]<- "accuracy_ens_maskSubs"
colnames(pred_eval_data_sat)[21]<- "accuracy_ens_maskSubsBathy"

# write.csv(pred_eval_data_sat, paste(val_path,"pred_eval_data_sat_excluding_confusingPoints.csv", sep="/"))


# #  Create table to summarize the results per cluster --> this was not presented in the PAPER
# table_per_cluster<- data.frame(matrix(data= NA, ncol=6, nrow= 5))
# colnames(table_per_cluster)<- c("Cluster", "n_total", "n_TP", "n_TN", "n_FP", "n_FN")
# table_per_cluster$Cluster<- seq(2,6,1)
# 
# i=2
# for (i in 2:6) {
#   data_cluster<- pred_eval_data_sat%>%filter(Cluster== i)
#   table_per_cluster[i-1,"n_total"]<- length(data_cluster$pr_ab)
#   table_per_cluster[i-1,"n_TP"]<- length(data_cluster[which(data_cluster$accuracy_ens_maskSubsBathy == "TP"),1])
#   table_per_cluster[i-1,"n_TN"]<- length(data_cluster[which(data_cluster$accuracy_ens_maskSubsBathy == "TN"),1])
#   table_per_cluster[i-1,"n_FP"]<- length(data_cluster[which(data_cluster$accuracy_ens_maskSubsBathy == "FP"),1])
#   table_per_cluster[i-1,"n_FN"]<- length(data_cluster[which(data_cluster$accuracy_ens_maskSubsBathy == "FN"),1])
#   
# }
# 
# table_per_cluster$prop_TP<- round(table_per_cluster$n_TP/table_per_cluster$n_total, 2)
# table_per_cluster$prop_TN<- round(table_per_cluster$n_TN/table_per_cluster$n_total, 2)
# table_per_cluster$prop_FP<- round(table_per_cluster$n_FP/table_per_cluster$n_total, 2)
# table_per_cluster$prop_FN<- round(table_per_cluster$n_FN/table_per_cluster$n_total, 2)
# 
# t(table_per_cluster[,c(1,7:10)])
# library(data.table)
# table_per_clusterlong <- melt(setDT(table_per_cluster[,c(1,7:10)]), id.vars = c("Cluster"), variable.name = "validation")
# 
# 
# ggplot(table_per_clusterlong, aes(x= Cluster, y=value, fill=validation))+
#   geom_bar(stat = "identity")+ labs(y="Proportion of records")+
#   theme_bw()
# 
# # write.csv(table_per_clusterlong, paste(val_path,"table_per_clusterlong_validation.csv", sep="/"))
# 
# 
# 
# 
# pred_eval_data_30perc<- cbind(matrix_ensemble_Model[[2]],
#                            matrix_ensemble_ModelSubs[[2]]$accuracy_ensemble_subs,
#                            # matrix_ensemble_ModelSubsbathy[[2]],
#                            matrix_ensemble_ModelSubsbathy2[[2]]$accuracy_ensemble_subs_bathy)
# # write.csv(pred_eval_data_30perc, paste(val_path,"pred_eval_data_30perc.csv", sep="/"))
# 
# 
# 
# 
# model= "Ensemble"

