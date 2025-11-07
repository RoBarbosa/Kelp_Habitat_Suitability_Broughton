###==================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### Barplots of suitable/unsuitable prop area per cluster (Figure 4) ###########
### Create Table 3 of mean and SD of each variable in suitable areas (EnsModel2) ###########
### Author: Romina Barbosa                                      ################
### Date: 08-Oct-2024                                          ################
### Last edition: 20-March-2025
### Threshold for ensemble binary map = 0.246
###==================================================================
# Load packages
library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
# library(flexsdm)

path_model_res<- "/Volumes/Romina_BATI/BATI/SDMs/outputs_SDMs"
path_clusters<- "/Volumes/Romina_BATI/BATI/SDMs/Clustering/Last version"
# val_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/SDMs_validation/Sept2024"

#  Load rasters of models results and clusters
clusters<- raster(paste(path_clusters, "6CLUSTERs_6Variables_07022024_3005XY_FINAL_alignedSDMs.tif", sep="/"))
ensemble_m<- raster(paste(path_model_res, "ensembleRAFMaxent_11Vars.tif", sep="/"))
# suitable_area1<- ensemble_m
# suitable_area1[suitable_area1 >= 0.246]<- 1
# suitable_area1[suitable_area1 < 0.246]<- 0
# the previous is the same than suitable_area1

# Load binary map of suitability
suitable_area1<- raster(paste(path_model_res, "ENSEMBLE_model_binary_NoOmision.tif", sep="/"))
mapview::mapview(suitable_area1, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check

suitable_area2<- raster(paste(path_model_res, "ENSEMBLE_masked_bathySubstrat_NoOmision.tif", sep="/"))


# crop to match extents and stack layers
clusters<- crop(clusters, suitable_area2)
ensemble_m<- crop(ensemble_m, suitable_area2)
suitable_area1<- crop(suitable_area1, suitable_area2)
suitable_area2<- crop(suitable_area2, suitable_area2)

stack_raster<- stack(clusters, ensemble_m, suitable_area1, suitable_area2)

data_stack_df<- as.data.frame(stack_raster, xy=T)
colnames(data_stack_df)[3:6]<-c("cluster", "Ensemble_model", "Ens_model_1_binary", "Ens_model_2_binary")


# Summarize the surface of suitable area per cluster
TABLE_3<- data_stack_df%>%
  filter(!is.na(Ensemble_model))%>%
  filter(Ens_model_1_binary == 1)%>% # select only suitable areas
  group_by(cluster, Ens_model_1_binary)%>%
  summarize(n_cluster= length(cluster), area_suitable_km2= n_cluster* 0.020 * 0.020)

#  REsults March 2024 # checked Oct 2025 --> TABLE 3
#    cluster Ens_model_1_binary n_cluster area_suitable_km2
# 1       1                  1      1403             0.561
# 2       2                  1    122182            48.9  
# 3       3                  1    101109            40.4  
# 4       4                  1    320732           128.   
# 5       5                  1    133228            53.3  
# 6       6                  1     73306            29.3  
# 7      NA                  1    109320            43.7 

cluster_area<- data_stack_df%>%
  filter(!is.na(cluster))%>%
  filter(!is.na(Ens_model_1_binary))%>%
  select(cluster)%>%
  group_by(cluster)%>%
  mutate(cluster_area= length(cluster)* 0.020 * 0.020)

cluster_area<- cluster_area[-which(duplicated(cluster_area)),]

data_stack_df<- data_stack_df %>%
  filter(!is.na(cluster))


suitable_area1<- data_stack_df %>%
  filter(!is.na(cluster))%>%
  filter(!is.na(Ens_model_1_binary))%>%
  select(cluster, Ens_model_1_binary)%>%
  group_by(cluster, Ens_model_1_binary)%>%
  summarize(suitable_area= length(Ens_model_1_binary)* 0.20 * 0.20) 

# suitable_area<- suitable_area[-which(duplicated(suitable_area)),]

suitable_area1<- merge(suitable_area1, cluster_area, by = "cluster")
suitable_area1$prop_suitable<- suitable_area1$suitable_area / suitable_area1$cluster_area

suitable_area1 %>%
  ggplot(aes(x=as.factor(cluster), y=prop_suitable, fill=as.factor(Ens_model_1_binary))) +
  geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  scale_fill_manual(values=c("blue","green")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Type of Variable")

plotspath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/Reports/Paper1_SDMs/Figures"
# ggsave(paste(plotspath,"Figure_4_suitable_EnsModel1.pdf", sep="/"),
#            height = 8, width = 12, units="cm", dpi=300)



# Summarize the surface of Actual suitable area per cluster
data_stack_df%>%
  filter(!is.na(Ens_model_2_binary))%>%
  filter(Ens_model_2_binary == 1)%>%
  group_by(cluster, Ens_model_2_binary)%>%
  summarize(n_cluster= length(cluster), area_suitable_km2= n_cluster* 0.020 * 0.020)

# cluster Ens_model_2_binary n_cluster area_suitable_km2
# <dbl>              <dbl>     <int>             <dbl>
#   1       1                  1       321             0.128
# 2       2                  1     48861            19.5  
# 3       3                  1     28625            11.5  
# 4       4                  1     97056            38.8  
# 5       5                  1     51742            20.7  
# 6       6                  1     24445             9.78 


suitable_area<- data_stack_df %>%
  filter(!is.na(cluster))%>%
  filter(!is.na(Ens_model_2_binary))%>%
  select(cluster, Ens_model_2_binary, Ensemble_model)%>%
  group_by(cluster, Ens_model_2_binary)%>%
  summarize(suitable_area= length(Ens_model_2_binary)* 0.20 * 0.20,
            mean_suitability= mean(Ensemble_model)) 

# suitable_area<- suitable_area[-which(duplicated(suitable_area)),]

suitable_area<- merge(suitable_area, cluster_area, by = "cluster")
suitable_area$prop_suitable<- suitable_area$suitable_area / suitable_area$cluster_area

suitable_area %>%
  ggplot(aes(x=as.factor(cluster), y=prop_suitable, fill=as.factor(Ens_model_2_binary))) +
  geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  scale_fill_manual(values=c("blue","green")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Type of Variable")

suitable_area %>%
  ggplot(aes(x=as.factor(cluster), y=mean_suitability, fill=as.factor(cluster))) +
  geom_bar(stat="identity", width = 0.6)+
  # geom_violin()+
  theme_bw()+
  # scale_fill_manual(values=c("blue","green")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Habitati Suitability", x="Environmental Cluster", fill= "Type of Variable")



# summary per suitable and unsuitable category ================================================
cluster_cells<-  data_stack_df%>%
  filter(!is.na(Ens_model_2_binary))%>%
  group_by(cluster)%>%
  summarize(n_cluster= length(cluster))

summary_cl<- data_stack_df%>%
  filter(!is.na(Ens_model_2_binary))%>%
  group_by(cluster, Ens_model_2_binary)%>%
  summarize(cells_binary= length(cluster), mean_suitability= mean(Ensemble_model))

summary_cl<- merge(summary_cl, cluster_cells, by="cluster")
summary_cl$prop_binary<- round(summary_cl$cells_binary / summary_cl$n_cluster, 2)

summary_cl%>%group_by(cluster)%>%summarize(total= sum(prop_binary)) # check that the sum is equal to 1 (100%)


### Create BoxPLOT of suitability per cluster ==========================
data_stack_df%>%
  filter(!is.na(Ens_model_2_binary))%>%
  group_by(cluster)%>%
   ggplot(aes(x=as.factor(cluster), y=Ensemble_model)) +
  geom_boxplot(outlier.size=0.6)+
  ylim(c(0,1))+
  theme_bw()+
  # scale_fill_manual(values=c("#e41a1c","slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Habitat Suitability", x="Environmental Cluster", fill= "Cluster")+
  theme(legend.position = "null")

# ggsave(paste(path_model_res, "Suitability_x_cluster_boxplot_onlyBinary1.pdf", sep="/"), width = 7, height = 6, dpi = 300, units="cm")

data_stack_df %>%
  filter(!is.na(cluster))%>%
  filter(!is.na(Ens_model_2_binary)) %>%
  ggplot(aes(x=as.factor(cluster), y=Ensemble_model, fill=as.factor(cluster))) +
  geom_boxplot()+
  # geom_violin()+
  theme_bw()+
  # scale_fill_manual(values=c("blue","green")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Habitati Suitability", x="Environmental Cluster", fill= "Type of Variable")


### Create BarPLOT of proportion of suitable area per cluster ==========================

summary_cl$Ens_model_2<- as.factor(summary_cl$Ens_model_2_binary)
levels(summary_cl$Ens_model_2_binary)<- c("Unsuitable", "Suitable")
summary_cl%>%#filter(Cluster!="1")%>%
  ggplot(aes(x=as.factor(cluster), y=prop_binary, fill=as.factor(Ens_model_2_binary))) +
  geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Habitat")


# ggsave(paste(path_model_res, "Binary_model_barplot.pdf", sep="/"), width = 9, height = 6, dpi = 300, units="cm")


### Export tables of summaries =================================================
table_suitab_cluster<- data_stack_df%>%
  filter(!is.na(Ens_model_2_binary))%>%
  filter(Ens_model_2_binary==1)%>%
  group_by(cluster)%>%
  summarize(mean_suitab= mean(Ensemble_model), sd_suitab= sd(Ensemble_model))


table_suitab_cluster<-merge(table_suitab_cluster,summary_cl, by="cluster")
# write.csv(table_suitab_cluster, paste(path_model_res,"table_suitab_cluster.csv", sep="/"))



####### TABLE 3 - Summary of conditions in suitable area =======================
### Assess summary of environemntal conditions in suitable areas per cluster ====
#Load table with environemtnal conditions (variables as columns)
SDM_path<- "/Volumes/Romina_BATI/BATI/SDMs" 
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
filtered_vars001<- stack(filtered_vars001)

filtered_vars001<- crop(filtered_vars001, ensemble_binary)

## Stack variables with model results and cluster raster
filtered_vars001<- crop(filtered_vars001, stack_raster)

stack_raster.a<- stack(stack_raster, filtered_vars001)
# rm(filtered_vars001)

model_variables_cl_stack<- as.data.frame(stack_raster.a, xy=T)
colnames(model_variables_cl_stack)[3:6]<-c("Cluster", "Ensemble_continue", "Ens_model_1_binary", "Ens_model_2_binary")
# model_variables_cl_stack<- model_variables_cl_stack%>%
#   filter(!is.na(Ens_model_2_binary))

      
       


# summary per cluster  ================================================
# Summarizing the mean, max, and min for all columns except the factor column
colnames(model_variables_cl_stack)
cluster_summary <-  model_variables_cl_stack%>%
  filter(!is.na(Ens_model_2_binary))%>%
  group_by(Cluster, Ens_model_2_binary)%>%
  summarise(
    across(
      .cols = where(is.numeric), #starts_with("TPI"),  # Apply to all columns starting with "Value"
      .fns = list(
        SD = ~sd(.),
        Mean = ~mean(.),
        Max = ~max(.),
        Min = ~min(.)
      ),
      .names = "{.col}_{.fn}"  # This will create column names like Value1_Mean, Value1_Max, etc.
    )
  )



cluster_summary_mean_SD <-  model_variables_cl_stack%>%
  filter(!is.na(Ens_model_2_binary))%>%
  filter(Ens_model_2_binary == 1)%>%
  group_by(Cluster, Ens_model_2_binary)%>%
  summarise(
    across(
      .cols = where(is.numeric), #starts_with("TPI"),  # Apply to all columns starting with "Value"
        .fns = list(
          Mean_SD = ~paste0(round(mean(.), 4), " ± ", round(sd(.), 4), ";")
        ),
      .names = "{.col}_Mean_SD"  # This will create column names like Value1_Mean, Value1_Max, etc.
    )
  )

head(cluster_summary_mean_SD)
cluster_summary_mean_SD<- cluster_summary_mean_SD[,-c(2:4,6)]
cluster_summary_mean_SD$Cluster<- as.factor(cluster_summary_mean_SD$Cluster)

df_wide <- cluster_summary_mean_SD %>%
  tidyr::pivot_wider(names_from = Cluster, values_from = Cluster)

df_wide$julBSpd_max_Mean_SD<- as.numeric(as.character(df_wide$julBSpd_max_Mean_SD))

# SAVE TABLE 3 =================================================================
setwd(SDM_path)
# write.csv(cluster_summary_mean_SD, "Table_3_variables_summary_AHSD_Cluster_mean_SD.csv")









#### VIOLIN PLOTS PER VARIABLE =================================================
model_variables_cl_stack%>%
  dplyr::select(!Ensemble_continue)%>%
  dplyr::select(!Ens_model_1_binary)


  
colnames(model_variables_cl_stack)
slope<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability_Ens2 =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y=slope, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Slope", x="Environmental Cluster", fill= "Binary_suitability")

v2<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability_Ens2 =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y=TPI, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="TPI", x="Environmental Cluster", fill= "Binary_suitability_Ens2")

v3<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= eastherness, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Eastherness", x="Environmental Cluster", fill= "Ens_model_2")

v4<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= janBT_ave, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Winter bottom temp (ave.)", x="Environmental Cluster", fill= "Binary_suitability")

v5<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= julBS_ave, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Summer bottom salintity (ave.)", x="Environmental Cluster", fill= "Binary_suitability")


v6<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= julBT_ave, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Summer bottom temp (ave.)", x="Environmental Cluster", fill= "Binary_suitability")


v7<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= julBT_min, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Summer bottom temp (min.)", x="Environmental Cluster", fill= "Binary_suitability")


v8<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= julST_ave, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Summer surface temp (ave.)", x="Environmental Cluster", fill= "Binary_suitability")


v9<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= julBSpd_max, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Summer bottom speed (max.)", x="Environmental Cluster", fill= "Binary_suitability")


v10<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= julSSpd_min, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Summer surface speed (min.)", x="Environmental Cluster", fill= "Binary_suitability")

v11<-  model_variables_cl_stack %>% 
  # filter(Binary_suitability =="1")%>%
  ggplot(aes(x=as.factor(Cluster), y= tidal_cur, fill=as.factor(Ens_model_2_binary))) +
  # geom_bar(stat="identity", width = 0.6)+
  geom_violin()+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Tidal current", x="Environmental Cluster", fill= "Binary_suitability")



cowplot::plot_grid(slope, v2, v3, v4, v5, v6, v7, v8, v9,  v10, v11, ncol=2, align = "v")
# ggsave(paste(path_model_res, "Binary_model_violinplot_March2025.pdf", sep="/"), width = 17, height = 38, dpi = 300, units="cm")
