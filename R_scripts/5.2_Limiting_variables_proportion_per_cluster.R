###=============================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### 5.1- PLOT Maxent Limiting variables results  (Figure 6 boxplot) ############
### Author: Romina Barbosa                                      ################
### Date: 30-Sept-2024                                          ################
### Last edition: 20-March-2025
###=============================================================================
library(raster)
library(rmaxent)
library(dplyr)
library(ggplot2)
library(terra)
library(sf)

# mypath<- "E:/Kelp_postdoc/SDMs/csvs_to_SDMs"
# layers_path<- "E:/Kelp_postdoc/SDMs/layers"
# plotspath<- "E:/Kelp_postdoc/SDMs/Plots"
SDM_path<- "/Volumes/Romina_BATI/BATI/SDMs"

# layers3005_path<- "E:/Kelp_postdoc/SDMs/new_layers_3005"
maxent_outputs_path<- paste(SDM_path, "outputs_SDMs/March2024", sep="/")
path_clusters<- "/Volumes/Romina_BATI/BATI/SDMs/Clustering/Last version"
path_limitVars<- "/Volumes/Romina_BATI/BATI/SDMs/outputs_SDMs/Limiting_variables_"
path_model_res<- "/Volumes/Romina_BATI/BATI/SDMs/outputs_SDMs"
path_model_res0<- "/Volumes/Romina_BATI/BATI/SDMs/outputs_SDMs"

# ==============================================================================
setwd(path_limitVars)
dir()

lims_mode<- raster("Limiting_variables_11_maxentModal.tif")
# stack_lims<- stackOpen("E:/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/Limiting_variables11_stackFINAL.stk")
load("Limiting_variables11_FINAL.RData")


palSD = rgb.palette <- colorRampPalette(c("lightblue", "gold2", "grey", "green", "red", "blue"),
                                        space = "rgb")
mapview::mapview(lims$Limiting_variables11_maxentModal, col.regions = rev(palSD(50)), at = seq(0, 11, 1), legend = TRUE)
mapview::mapview(lims_mode, col.regions = rev(palSD(50)), at = seq(0, 11, 1), legend = TRUE)


### Evaluate limiting variables after masking by substrate =====================
# load("D:/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/Limiting_variables11_FINAL.RData")
lims$limiVars_maxent_1@data@attributes
lims$limiVars_maxent_20@data@attributes
# ID   predictor
# 1   1 eastherness
# 2   2   janBT_ave
# 3   3   julBS_ave
# 4   4 julBSpd_max
# 5   5   julBT_ave
# 6   6   julBT_min
# 7   7 julSSpd_min
# 8   8   julST_ave
# 9   9       slope
# 10 10   tidal_cur
# 11 11         TPI

lims_mode$Class

lims_mode$Limiting_variables11_maxentModal@data@attributes<- lims$limiVars_maxent_1@data@attributes[[1]]
plot(lims_mode)


###  Load the substrate layer to mask the suitable area =================================
subst_raster<- raster(paste(path_model_res0, "substrate_raster.tif", sep="/"))

no_suitable_substrate<- subst_raster # value 3 = sandy areas
no_suitable_substrate[no_suitable_substrate == 3]<- 12 # 12 will be number of the variable "substrate"
no_suitable_substrate[no_suitable_substrate != 12]<- 0 # to exclude all hard (suitable) suitable areas
mapview::mapview(no_suitable_substrate, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check


### The Limiting conditions were evaluated only in suitable areas ==================
# Load binary map of suitability
suitable_area1<- raster(paste(path_model_res, "ENSEMBLE_model_binary_NoOmision.tif", sep="/"))
mapview::mapview(suitable_area1, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check

suitable_area2<- raster(paste(path_model_res, "ENSEMBLE_masked_bathySubstrat_NoOmision.tif", sep="/"))
mapview::mapview(suitable_area2, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check


nosuitable_area_M1<- suitable_area1
nosuitable_area_M1[nosuitable_area_M1 == 0]<- 2
nosuitable_area_M1[nosuitable_area_M1 == 1]<- 3
nosuitable_area_M1[nosuitable_area_M1 == 2]<- 1
nosuitable_area_M1[nosuitable_area_M1 == 3]<- 0


# Give value 1 to no suitable (where limiting factors are evaluated) areas and 0 to suitable areas
nosuitable_area_M2<- suitable_area2
nosuitable_area_M2[nosuitable_area_M2 == 0]<- 2
nosuitable_area_M2[nosuitable_area_M2 == 1]<- 3
nosuitable_area_M2[nosuitable_area_M2 == 2]<- 1
nosuitable_area_M2[nosuitable_area_M2 == 3]<- 0

# Check maps, they should show opposite pattern (inverted values)
mapview::mapview(nosuitable_area_M2, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check
mapview::mapview(suitable_area2, cex=2, zcol="BType1", col.regions= c("blue","red"))# a visual check
  
# Identify area no suitable due to substrate or bathymetry as the difference among no suitable 1 and 2
nosuitbale_area_substrateORdepth<- nosuitable_area_M2 - nosuitable_area_M1
plot(nosuitbale_area_substrateORdepth)
plot(nosuitable_area_M2)
plot(nosuitable_area_M1)

# nosuitable_area_M2<- crop(nosuitable_area_M2, nosuitable_area_M1)
nosuitable_area_M1<- crop(nosuitable_area_M1, nosuitable_area_M2)


###=========================================================================================
# I also check the limiting variables when including the substrate
# suitable_area1<- ensemble_m
# suitable_area1[suitable_area1 >= 0.26]<- 1
# suitable_area1[suitable_area1 < 0.26]<- 0
# 
# plot(suitable_area1)
# mapview::mapview(suitable_area1, cex=2)# a visual check
# nosuitable_area2 <- suitable_area1 - suitable_area2

#### Mask model by bathymetry =================================================
# All no suitable areas deeper than 15 m are limited by depth
# 95% of training data are at bathymetry above 15m
stack_vars<-  terra::rast(paste(SDM_path, "variables_selection/stack_29FilteredVars_stratification_FINAL2.tif", sep="/"))
bathy<- raster(stack_vars[[1]])
bathy_15<- bathy
bathy_15[bathy_15<= 15]<- 0  # convert values to "0" for areas shallower than 15 m
bathy_15[bathy_15> 15]<- 1   # convert values to "1" for areas deeper than 15 m


# limiting_variables_map_f<- function(nosuitable_area= nosuitable_area_M2, 
#                                     no_suitable_substrate= no_suitable_substrate,
#                                     bathy_15= bathy_15){

nosuitable_area= nosuitable_area_M2

  # No suitable area limited by substrate 
  # Multiply the suitable area by the suitable substrate area
  # Areas with value 12 will represent areas limited by substrate 
  # (e.g., 1 (no-suitable area) * 12 (no suitable substrate) = 12 (areas limited by substrate))
  Nosuitable_area_subst<- nosuitbale_area_substrateORdepth * no_suitable_substrate 
  Nosuitable_area_subst[Nosuitable_area_subst == 12]<- 1
  plot(Nosuitable_area_subst)
  
  # No suitable area limited by depth 
  bathy_15<- crop(bathy_15, nosuitable_area)
  Nosuitable_area_bathy<- nosuitbale_area_substrateORdepth * bathy_15 
  plot(Nosuitable_area_bathy)
  
  # Nosuitable_area_bathy[Nosuitable_area_bathy == 1]<- 3
  Nosuitable_area_bathy<- Nosuitable_area_bathy - Nosuitable_area_subst  # exclude area limited by substrate from areas limited by depth
  Nosuitable_area_bathy[Nosuitable_area_bathy == -1]<- 0
  plot(Nosuitable_area_bathy)
  plot(Nosuitable_area_subst)
  
  # Determine areas where other than substrate and depth variables were limiting
  # nosuitable_area2<- crop(nosuitable_area2, lims_mode)
  nosuitable_area_M1<- crop(nosuitable_area_M1, lims_mode)
  lims_vars_M1 <- lims_mode * nosuitable_area_M1
  plot(lims_vars_M1)
  
  # Rename values to 12 again to identify the areas limited by substrate 
  Nosuitable_area_subst[Nosuitable_area_subst == 1]<- 12
  Nosuitable_area_bathy[Nosuitable_area_bathy == 1]<- 13
  
  # Finally merge areas limited by all the env. variables including substarte and depth
  lims_vars_M2<- lims_vars_M1 + Nosuitable_area_bathy + Nosuitable_area_subst
  plot(lims_vars_M2)


# Save rasters of limiting variables for Ens-model 1 and Ens-model 2 ===========
mapview::mapview(lims_vars_M2, cex=2)# a visual check
# writeRaster(lims_vars_M2, paste(path_limitVars,"Limiting_variables_EnsModel2_NoSuitableAreas.tif", sep="/"))
  

lims_mode<- crop(lims_mode, suitable_area2)
plot(lims_vars_M2)
plot(nosuitable_area_M1)
plot(lims_vars_M1)

# writeRaster(lims_vars_M1, paste(path_limitVars,"Limiting_variables_EnsModel1_NoSuitableAreas.tif", sep="/"))




### Reclassify in categories of variables and save raster ======================
#   levels = c(21, 22, 23, 24, 25, 26),
#   labels = c("Topographic", "Temperature","Salinity", "Water dynamics", "Substrate type", "Depth"))

limit_categories_M2<- lims_vars_M2

limit_categories_M2[limit_categories_M2 == 1]<- 21#"eastherness"
limit_categories_M2[limit_categories_M2 == 2]<- 22#"janBT_ave"
limit_categories_M2[limit_categories_M2 == 3]<- 23#"janSSpd_min"
limit_categories_M2[limit_categories_M2 == 4]<- 24#"julBS_ave"
limit_categories_M2[limit_categories_M2 == 5]<- 23#"julBSpd_max"
limit_categories_M2[limit_categories_M2 == 6]<- 22#"julBT_min"
limit_categories_M2[limit_categories_M2 == 7]<- 23#"julSSpd_min"
limit_categories_M2[limit_categories_M2 == 8]<- 22#"julST_ave"
limit_categories_M2[limit_categories_M2 == 9]<- 21#"slope"
limit_categories_M2[limit_categories_M2 == 10]<- 23#"tidal_cur"
limit_categories_M2[limit_categories_M2 == 11]<- 21#"TPI"
limit_categories_M2[limit_categories_M2 == 12]<- 25#"Substrate"
limit_categories_M2[limit_categories_M2 == 13]<- 26#"Depth"

lims_categories_M1<- lims_vars_M1

lims_categories_M1[lims_categories_M1 == 1]<- 21#"eastherness"
lims_categories_M1[lims_categories_M1 == 2]<- 22#"janBT_ave"
lims_categories_M1[lims_categories_M1 == 3]<- 23#"janSSpd_min"
lims_categories_M1[lims_categories_M1 == 4]<- 24#"julBS_ave"
lims_categories_M1[lims_categories_M1 == 5]<- 23#"julBSpd_max"
lims_categories_M1[lims_categories_M1 == 6]<- 22#"julBT_min"
lims_categories_M1[lims_categories_M1 == 7]<- 23#"julSSpd_min"
lims_categories_M1[lims_categories_M1 == 8]<- 22#"julST_ave"
lims_categories_M1[lims_categories_M1 == 9]<- 21#"slope"
lims_categories_M1[lims_categories_M1 == 10]<- 23#"tidal_cur"
lims_categories_M1[lims_categories_M1 == 11]<- 21#"TPI"

### Save limiting variables by category - Raster ================================
mapview::mapview(limit_categories_M2, 
        col.regions = c("grey","lightgreen", "yellow", "orange", "red", "darkred", "blue"),
        legend = TRUE,
        main = "Reclassified Raster with Text Categories")

# writeRaster(limit_categories_M2, paste(path_limitVars,"lims_categories_EnsModel2.tif", sep="/"),overwrite=TRUE)
# writeRaster(lims_categories_M1, paste(path_limitVars,"lims_categories_EnsModel1.tif", sep="/"), overwrite=TRUE)



lims_categories_M2<- raster(paste(path_limitVars,"lims_categories_EnsModel2.tif", sep="/"))
lims_categories_M1<- raster(paste(path_limitVars,"lims_categories_EnsModel1.tif", sep="/"))
plot(lims_categories_M1)
plot(lims_categories_M2)
mapview::mapview(lims_categories_M2, 
                 col.regions = c("grey","lightgreen", "yellow", "orange", "red", "darkred", "blue"),
                 legend = TRUE,
                 main = "Reclassified Raster with Text Categories")


# c("Topographic", "Temperature", "Salinity", "Water dynamics", "Substrate type", "Depth")
# "1" = "Topographic", "2" = "Temperature",
#                "3" = "Water dynamics", "4" = "Salinity", 
#                "5" = "Temperature", "6" = "Temperature",
#                "7" = "Water dynamics", "8" = "Temperature",
#                "9" = "Topographic", "10" = "Water dynamics",
#                "11" = "Topographic", "12" = "Substrate type",
#                "13" = "Depth"



### Load clusters and variables and Bathymetry ==============================================
clusters<- raster(paste(path_clusters, "6CLUSTERs_6Variables_07022024_3005XY_FINAL_alignedSDMs.tif", sep="/"))


#### Stack variables limiting mode and clusters to get summary per cluster =====
# lims_mode<- crop(lims_mode, limit_vars)
clusters<- crop(clusters, lims_vars_M1)
suitable_area1<- crop(suitable_area1, lims_vars_M1)
suitable_area2<- crop(suitable_area2, lims_vars_M1)
lims_categories_r<- crop(lims_categories_r, lims_vars_M1)
lims_categories_M1<- crop(lims_categories_M1, lims_vars_M1)


data_stack<- stack(clusters, lims_vars_M1, lims_vars_M2, lims_categories_M1, limit_categories_M2, suitable_area1, suitable_area2)
names(data_stack)<- c("cluster", "limit_Variables_EnsM1","limit_Variables_EnsM2", "limit_categories_M1", "limit_categories_M2", "Binary_Ens_model_1", "Binary_Ens_model_2")

# Convert raster stack to dataframe to create the barplots
data_stack_df<- as.data.frame(data_stack, xy=T)
data_stack_df<- data_stack_df%>%
  filter(!is.na(cluster))


# Add names of variables (replace numbers per names)
variables_names<- c(
   "eastherness", # 1  
   "janBT_ave",
   "julBS_ave",
   "julBSpd_max",
   "julBT_ave",
   "julBT_min",
   "julSSpd_min",
   "julST_ave",
   "slope",
   "tidal_cur",
   "TPI",
   "substrate",
   "depth")

### Summary per variable =======================================================
total_cells_area_PHSD <- na.exclude(data_stack_df)%>%
  filter(limit_Variables_EnsM2 != "0")%>%
  filter(Binary_Ens_model_1 == "0")
total_cells_area_PHSD<- nrow(total_cells_area_PHSD) * 0.02 * 0.02

# Proportion of PSHD
summary_table_variables_PSHD <- na.exclude(data_stack_df) %>%
  filter(limit_Variables_EnsM1 != "0") %>%
  group_by(limit_Variables_EnsM1) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_cells_area_PHSD *100)

summary_table_variables_PSHD$limit_var_name<- as.factor(summary_table_variables_PSHD$limit_Variables_EnsM1)
levels(summary_table_variables_PSHD$limit_var_name)<- c(variables_names[1:11])

sum(summary_table_variables_PSHD$proportion_total)
print(summary_table_variables_PSHD)

# Proportion of ASHD
total_cells_area_ASHD <- na.exclude(data_stack_df)%>%
  filter(limit_Variables_EnsM2 != "0")%>%
  filter(Binary_Ens_model_2 == "0")
total_cells_area_ASHD<- nrow(total_cells_area_ASHD) * 0.02 * 0.02
  
summary_table_variables_ASHD <- na.exclude(data_stack_df) %>%
  filter(limit_Variables_EnsM2 != "0") %>%
  group_by(limit_Variables_EnsM2) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_cells_area_ASHD *100)

summary_table_variables_ASHD$limit_var_name<- as.factor(summary_table_variables_ASHD$limit_Variables_EnsM2)
levels(summary_table_variables_ASHD$limit_var_name)<- c(variables_names)

sum(summary_table_variables_ASHD$proportion_total)
print(summary_table_variables_ASHD)

# Save table of summary or area limited by each variable (without differentiating per cluster)
colnames(summary_table_variables_PSHD)[2]<- "proportion_of_unsuitablePHSD"
# write.csv(summary_table_variables_PSHD, "summary_limiting_variables_PSHD.csv")

colnames(summary_table_variables_ASHD)[2]<- "proportion_of_unsuitableAHSD"
# write.csv(summary_table_variables_ASHD, "summary_limiting_variables_ASHD.csv")



### Proportion of the PHSD reduced by substrate and depth ======================
total_suitable_area_PHSD <- na.exclude(data_stack_df)%>%
  filter(Binary_Ens_model_1 == "1")
total_suitable_area_PHSD<- nrow(total_suitable_area_PHSD) * 0.02 * 0.02

na.exclude(data_stack_df) %>%
  group_by(limit_Variables_EnsM2) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_suitable_area_PHSD *100)


### Proportion of the PSHD covered by the ASHD =================================
data_stack_df<- na.exclude(data_stack_df)
total_cells_area <- na.exclude(data_stack_df)
total_cells_area<- nrow(total_cells_area) * 0.02 * 0.02

total_area_PSHD <- na.exclude(data_stack_df)%>%
  filter(Binary_Ens_model_1 == "1")
total_area_PSHD<- nrow(total_area_PSHD) * 0.02 * 0.02

total_area_ASHD <- na.exclude(data_stack_df)%>%
  filter(Binary_Ens_model_2 == "1")
total_area_ASHD<- nrow(total_area_ASHD) * 0.02 * 0.02

total_area_ASHD/total_area_PSHD

data_stack_df%>%
  group_by(Binary_Ens_model_1) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_cells_area *100)
#    Binary_Ens_model_1 area_km2 proportion_total
# 1                  0     76.2             20.2
# 2                  1    300.              79.8

# per cluster
data_stack_df%>%
  group_by(Binary_Ens_model_1, cluster) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_cells_area *100)%>%
  filter(Binary_Ens_model_1== 1)
# Binary_Ens_model_1 cluster area_km2 proportion_total
# 1                  1       1    0.558            0.148
# 2                  1       2   48.8             13.0  
# 3                  1       3   40.4             10.7  
# 4                  1       4  128.              34.0  
# 5                  1       5   53.0             14.1  
# 6                  1       6   29.3              7.78 


# ASHD
data_stack_df%>%
  group_by(Binary_Ens_model_2) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_cells_area *100)
#   Binary_Ens_model_2 area_km2 proportion_total
# 1                  0     276.             73.3
# 2                  1     100.             26.7

# suitable areas
data_stack_df%>%
  group_by(Binary_Ens_model_2, cluster) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_cells_area *100)%>%
  filter(Binary_Ens_model_2== 1)


na.exclude(data_stack_df) %>%
  filter(limit_Variables_EnsM2 != "0") %>%
  group_by(limit_Variables_EnsM2) %>%
  summarise(area_km2 = n()* 0.02 * 0.02, .groups = "drop") %>%
  mutate(proportion_total = area_km2 / total_cells_area *100)



### Proportion of exposed and fjord area that was suitable =====================
area_PSHD_clusters<- na.exclude(data_stack_df) %>%
  group_by(cluster)%>%
  mutate(cluster_area= length(cluster)* 0.02 * 0.02)%>%
  group_by(cluster, Binary_Ens_model_1) %>%
  mutate(area_km2 = n()* 0.02 * 0.02, proportion_cluster= area_km2/cluster_area, .groups = "drop") %>%
  summarise(area_km2= area_km2, proportion_total = area_km2 / total_cells_area *100, proportion_cluster= unique(proportion_cluster))

area_PSHD_clusters<- area_PSHD_clusters[-which(duplicated(area_PSHD_clusters)),]
print(area_PSHD_clusters)

total_PSHD_area<- area_PSHD_clusters%>%
  filter(Binary_Ens_model_1 == 1)%>%mutate(area_total= sum(area_km2))
sum(total_PSHD_area$area_total) # 300.3528 km2

area_ASHD_clusters<- na.exclude(data_stack_df) %>%
  # filter(limit_Variables_EnsM2 != "0") %>%
  # filter(Binary_Ens_model_2 == "1")%>%
  group_by(cluster)%>%
  mutate(cluster_area= length(cluster)* 0.02 * 0.02)%>%
  group_by(cluster, Binary_Ens_model_2) %>%
  mutate(area_km2 = n()* 0.02 * 0.02, proportion_cluster= area_km2/cluster_area, .groups = "drop") %>%
  summarise(area_km2= area_km2, proportion_total = area_km2 / total_cells_area *100, proportion_cluster= unique(proportion_cluster))

area_ASHD_clusters<- area_ASHD_clusters[-which(duplicated(area_ASHD_clusters)),]
sum(area_ASHD_clusters$area_km2) - area_ASHD_clusters[which(area_ASHD_clusters$cluster==5), "area_km2"]- area_ASHD_clusters[which(area_ASHD_clusters$cluster==1), "area_km2"]
print(area_ASHD_clusters)

### Summary per category =======================================================
data_stack_df$limit_VariName_EnsM1<- as.factor((data_stack_df$limit_Variables_EnsM1))
levels(data_stack_df$limit_VariName_EnsM1)<- c("0", variables_names)#lims$limiVars_maxent_1@data@attributes[[1]]$predictor

data_stack_df$limit_VariName_EnsM2<- as.factor((data_stack_df$limit_Variables_EnsM2))
levels(data_stack_df$limit_VariName_EnsM2)<- c("0", variables_names)# c("0",lims$limiVars_maxent_1@data@attributes[[1]]$predictor, "Sandy substrate", "Depth")


# Exclude areas not limited by any variable == suitable areas
data_stack_df<- data_stack_df%>% filter(limit_Variables_EnsM2 !="0")



# Group variables per category and name the categories in the dataframe
data_stack_df$limit_CatName_EnsM1<- as.factor(data_stack_df$limit_categories_M1)
levels(data_stack_df$limit_CatName_EnsM1)<- c("Suitable", "Topographic", "Temperature", "Water dynamics", "Salinity")


data_stack_df$limit_CatName_EnsM2<- as.factor(data_stack_df$limit_categories_M2)
levels(data_stack_df$limit_CatName_EnsM2)<- c("Topographic", "Temperature", "Water dynamics", "Salinity", "Substrate type", "Depth")


# write.csv(data_stack_df, paste(path_limitVars, "limit_variables_cluster_EnsModel1_EnsModel2_df.csv", sep="/"))
