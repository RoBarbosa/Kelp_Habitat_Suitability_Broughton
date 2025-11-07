###==================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### 3- Plots of Variables selection from MAxent importance      ################
### Author: Romina Barbosa                                      ################
### Date: 04-Jun-2024                                           ################
### Code edited from: https://sjevelazco.github.io/flexsdm/articles/v01_pre_modeling.html#data-species-occurrence-and-background-data ####
### Last edition: 12-Mar-2025
###==================================================================

# Load packages
library(ggplot2)
library(dplyr)
library(formattable)
library(data.table)



# Load data 
load("/Volumes/Romina_BATI/BATI/SDMs/outputs_SDMs/50repMaxent_17_15_11_5variables.RData")
# load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/50repMaxent_17_15_11_5variables.RData")


variables_importance_00<- mod_maxent00$variables_importance
variables_importance_002<- model_replicates_002$variables_importance
variables_importance_003<- model_replicates_003$variables_importance
variables_importance_004<- model_replicates_004$variables_importance
variables_importance_005<- model_replicates_005$variables_importance

variables_importance_00$variable<- as.factor(variables_importance_00$variable)
levels(variables_importance_00$variable)<- c("eastherness", "winte_BSpd_min" , "winter_BT_ave", "winter_SSpd_min", "summer_BS_ave", 
                                              "summer_BSpd_max", "summer_BT_max", "summer_BT_min",   "summer_SS_ave",  
                                              "summer_SSpd_ave", "summer_SSpd_min", "summer_ST_ave", "northness", 
                                              "slope", "tidal_cur", "TPI", "wind_speed")

variables_importance_002$variable<- as.factor(variables_importance_002$variable)
levels(variables_importance_002$variable)<- c("eastherness", "winter_BT_ave", "winter_SSpd_min", "summer_BS_ave", 
                                              "summer_BSpd_max", "summer_BT_max", "summer_BT_min",   "summer_SS_ave",  
                                              "summer_SSpd_ave", "summer_SSpd_min", "summer_ST_ave", "northness", 
                                              "slope", "tidal_cur", "TPI")

variables_importance_003$variable<- as.factor(variables_importance_003$variable)
levels(variables_importance_003$variable)<- c("eastherness", "winter_BT_ave", "winter_SSpd_min", "summer_BS_ave", 
                                              "summer_BSpd_max", "summer_BT_min",  
                                              "summer_SSpd_min", "summer_ST_ave", 
                                              "slope", "tidal_cur", "TPI")

variables_importance_004$variable<- as.factor(variables_importance_004$variable)
levels(variables_importance_004$variable)<- c("eastherness", "winter_BT_ave", "winter_SSpd_min", "summer_BS_ave", 
                                              "summer_BSpd_max", "summer_BT_min",  
                                              "summer_SSpd_min", "summer_ST_ave", 
                                               "tidal_cur")

variables_importance_005$variable<- as.factor(variables_importance_005$variable)
levels(variables_importance_005$variable)<- c("summer_BS_ave", 
                                              "summer_BT_min",  
                                              "summer_SSpd_min", "summer_ST_ave", 
                                              "tidal_cur")


# Plots to evaluate variables importance - variables selection
a_00<- variables_importance_00%>%
  mutate(variable = fct_reorder(variable, variable_contrib , .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= variable_contrib))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,40))+
  # geom_boxplot(aes(y= variable, x= permutation_importance, outlier.shape = NULL, color="green"))+
  labs(y="Variable", x="Variable Contribution (%)", color= "Replicate")+
  theme_bw()+ theme(legend.position = "NULL")+
  geom_vline(xintercept=2, linetype="dashed",
             color = "red", size=1)+ theme_bw()

b_00<- variables_importance_00%>%
  mutate(variable = fct_reorder(variable, variable_contrib , .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= permutation_importance))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,40))+
  # geom_boxplot(aes(y= variable, x= permutation_importance, outlier.shape = NULL, color="green"))+
  labs(y="Variable", x="Permutation Importance (%)", color= "Replicate")+
  theme_bw()+ theme(legend.position = "NULL")+
  geom_vline(xintercept=2, linetype="dashed",
             color = "red", size=1)+ theme_bw()



# variables_importance_table00<- variables_importance_00%>%
#   group_by(variable)%>%
#   summarize(mean_var_contrib= round(mean(variable_contrib),2),
#             sd_var_contrib= round(sd(variable_contrib),2),
#             mean_var_permitImportance= round(mean(permutation_importance),2),
#             sd_var_permitImportance= round(sd(permutation_importance),2))
# 
# 
# formattable(variables_importance_table00, align =c("l","c","c","c","c"),
#             list(`variable` = formatter("span", style = ~ style(color = "grey", font.weight = "bold"))
#                  # `mean_var_contrib`= color_tile(customGreen, customGreen0)
#             ))


# write.csv(variables_importance_table00, paste(plotspath, "variables_selection/variables_importance_table00.csv", sep="/"))


##==============================================================================

# variables_importance_table002<- variables_importance_002%>%
#   group_by(variable)%>%
#   summarize(mean_var_contrib= mean(variable_contrib),
#             sd_var_contrib= sd(variable_contrib),
#             mean_var_permitImportance= mean(permutation_importance),
#             sd_var_permitImportance= sd(permutation_importance))
# 
# colnames(variables_importance_table002)

a002<- variables_importance_002%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= variable_contrib))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  # geom_bar( alpha= 0.5, color="blue")+
  labs(y="Environmental Variable", x= "Importance (%)")+
  theme(legend.position = "NULL")+theme_bw()+
  geom_vline(xintercept=5, linetype="dashed",
             color = "red", size=1)+ theme_bw()

b002<- variables_importance_002%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= permutation_importance))+
  # geom_boxplot(outlier.shape = NA, color="blue")+
  # geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  geom_boxplot(aes(y= variable, x= permutation_importance),outlier.shape = NA, )+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  labs(y="Environmental Variable", x= "Permutation Importance")+
  theme(legend.position = "NULL")+theme_bw()+
  geom_vline(xintercept=5, linetype="dashed",
             color = "red", size=1)+ theme_bw()



###=============================================================================
### MAXENT ; filtering of variables (>=8% importance for MAXENT) ###############
###=============================================================================

# variables_importance_table003<- variables_importance_003%>%
#   group_by(variable)%>%
#   summarize(mean_var_contrib= mean(variable_contrib),
#             sd_var_contrib= sd(variable_contrib),
#             mean_var_permitImportance= mean(permutation_importance),
#             sd_var_permitImportance= sd(permutation_importance))

a003<- variables_importance_003%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= variable_contrib))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  # geom_bar( alpha= 0.5, color="blue")+
  labs(y="Environmental Variable", x= "Importance (%)")+
  theme(legend.position = "NULL")+theme_bw()+
  geom_vline(xintercept=7, linetype="dashed",
         color = "red", size=1)+ theme_bw()

b003<- variables_importance_003%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= permutation_importance))+
  # geom_boxplot(outlier.shape = NA, color="blue")+
  # geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  geom_boxplot(aes(y= variable, x= permutation_importance),outlier.shape = NA, )+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  labs(y="Environmental Variable", x= "Permutation Importance")+
  theme(legend.position = "NULL")+theme_bw()+
  geom_vline(xintercept=7, linetype="dashed",
  color = "red", size=1)+ theme_bw()


a004<- variables_importance_004%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= variable_contrib))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  # geom_bar( alpha= 0.5, color="blue")+
  labs(y="Environmental Variable", x= "Importance (%)")+
  theme(legend.position = "NULL")+theme_bw()+
  geom_vline(xintercept=11, linetype="dashed",
             color = "red", size=1)+ theme_bw()

b004<- variables_importance_004%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= permutation_importance))+
  # geom_boxplot(outlier.shape = NA, color="blue")+
  # geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  geom_boxplot(aes(y= variable, x= permutation_importance),outlier.shape = NA, )+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  labs(y="Environmental Variable", x= "Permutation Importance")+
  theme(legend.position = "NULL")+theme_bw()+
  geom_vline(xintercept=11, linetype="dashed",
  color = "red", size=1)+ theme_bw()

a005<- variables_importance_005%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= variable_contrib))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  # geom_bar( alpha= 0.5, color="blue")+
  labs(y="Environmental Variable", x= "Importance (%)")+
  theme(legend.position = "NULL")+theme_bw()

b005<- variables_importance_005%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= permutation_importance))+
  # geom_boxplot(outlier.shape = NA, color="blue")+
  # geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  geom_boxplot(aes(y= variable, x= permutation_importance),outlier.shape = NA, )+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "red", size = 1, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  labs(y="Environmental Variable", x= "Permutation Importance")+
  theme(legend.position = "NULL")+theme_bw()
# geom_vline(xintercept=11, linetype="dashed",

path_model_res<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs"
setwd(paste(path_model_res))

cowplot::plot_grid(a_00,b_00, a002,b002, nrow = 2, labels = c("A)", "", "B)", ""))
# ggsave(paste(path_model_res, "SDMs_Figure_S7.png", sep="/"), units="cm", width=16, height= 17, dpi=300)

cowplot::plot_grid(a003, b003, a004, b004, nrow = 2, labels = c("A)", "", "B)", ""))
# ggsave(paste(path_model_res, "SDMs_Figure_S8.png", sep="/"), units="cm", width=18, height= 14, dpi=300)




### PLot FIgure for main manuscript ============================================
variables_importance_003<- model_replicates_003$variables_importance
variables_importance_003$variable<- as.factor(variables_importance_003$variable)
levels(variables_importance_003$variable)<- c("Eastherness", 
                                              "Winter Bottom Average Temperature (°C)",
                                              "winter Surface Minimum Speed (m.s-1)",
                                              "Summer Bottom Average Salinity (psu)", 
                                              "Summer Bottom Maximum Speed (m.s-1)",
                                              "Summer Bottom Minimum Temperature (°C)",  
                                              "Summer Surface Minimum Speed (m.s-1)", 
                                              "Summer Surface Average Temperature (°C)", 
                                              "Slope", "Tidal Current (m.s-1)",
                                              "Topographic Position Index, TPI")


variables_importance_003%>%
  mutate(variable = fct_reorder(variable, permutation_importance, .fun='mean')) %>%
  ggplot( aes(y=reorder(variable, permutation_importance), x=permutation_importance)) + 
  # ggplot(aes(y= variable, x= permutation_importance))+
  # geom_boxplot(outlier.shape = NA, color="blue")+
  # geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  geom_boxplot(aes(y= variable, x= permutation_importance),outlier.shape = NA, )+
  geom_jitter(width = 0.15, size=0.3, alpha=0.5)+
  stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape= 18, na.rm = TRUE)+  # Mean points
  xlim(c(0,55))+
  labs(y="", x= "Permutation Importance (%)")+
  theme(legend.position = "NULL")+theme_bw()

# ggsave(paste(path_model_res, "Figure_5.png", sep="/"), units="cm", width=16, height= 10, dpi=300)


variables_importance_table003<- variables_importance_003%>%
  group_by(variable)%>%
  summarize(mean_var_contrib= mean(variable_contrib),
            sd_var_contrib= sd(variable_contrib),
            mean_var_permitImportance= mean(permutation_importance),
            sd_var_permitImportance= sd(permutation_importance))

# write.csv(variables_importance_003, "Table_variables_importance_Figure_5.csv")
