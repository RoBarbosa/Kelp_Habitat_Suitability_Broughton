###==================================================================
### SDMs  relating C:N and NUTRIENTS with Habitat suitability   ################
###                                                             ################
### Barplots and boxplots per cluster                           ################
### Author: Romina Barbosa                                      ################
### Date: 08-Oct-2024                                           ################
### Last edition: 08-Oct-2024
###==================================================================
# Load packages
library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(flexsdm)

path_model_res<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024"
path_clusters<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Clustering/Last version"
# val_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/SDMs_validation/Sept2024"

#  Load rasters of models results and clusters
clusters<- raster(paste(path_clusters, "6CLUSTERs_6Variables_07022024_3005XY_FINAL_alignedSDMs.tif", sep="/"))
ensemble_m<- raster(paste(path_model_res, "ensembleRAFMaxent_11Vars.tif", sep="/"))
ensemble_binary<- raster(paste(path_model_res, "Ensemble_bynary_maskSubBathy.tif", sep="/"))

# crop to match extents and stack layers
clusters<- crop(clusters, ensemble_binary)
ensemble_m<- crop(ensemble_m, ensemble_binary)
stack_raster<- stack(clusters, ensemble_m, ensemble_binary)
# model_cl_stack_df<- as.data.frame(stack_raster, xy=T)
# colnames(model_cl_stack_df)[3:5]<-c("Cluster", "Ensemble_model", "Binary_suitability")


#  Load nutrients data
nutrients_Aug2023 <- read.csv("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/Fieldwork_2023/Nutrients/nutrients_scfs_bati_sites_Aug2023_epsg3005.csv")
unique(nutrients_Aug2023$site)
length(unique(nutrients_Aug2023$site))# 52


# Extract model results at locations of nutrient samples
# crs(stack_raster)<- "+init=epsg:3005"

nutrients_Aug2023<- sdm_extract(data= nutrients_Aug2023, 
                            x = "lon3005",
                            y = "lat3005",
                            env_layer = rast(stack_raster),
                            variables = NULL,
                            filter_na = FALSE)

colnames(nutrients_Aug2023)[22:24]<- c("Cluster", "Ensemble_model11", "Binary_suitability")

#  Plot nutrients in map
NO3_data_sf <- (nutrients_Aug2023) %>%
  st_as_sf(coords = c("lon3005", "lat3005"), crs = "EPSG:3005")

mapview::mapview(NO3_data_sf, zcol= "Ensemble_model11",cex=6)#  col.regions= c("red", "blue", "grey", "purple")
mapview::mapview(NO3_data_sf, zcol= "Binary_suitability",cex=6)
mapview::mapview(NO3_data_sf, zcol= "Cluster",cex=6)


#  Summary of nutrient per cluster (mean and SD)
summary_cl<- nutrients_Aug2023%>%
  group_by(Cluster)%>%
  summarize(mean_Ensemble= mean(Ensemble_model11), sd_Ensemble= sd(Ensemble_model11), 
            mean_NO3NO2= mean(NO3.NO2, na.rm=T), sd_NO3NO2= sd(NO3.NO2, na.rm=T), 
            mean_SiO2= mean(SiO2, na.rm=T), sd_SiO2= sd(SiO2, na.rm=T), 
            mean_PO4= mean(PO4, na.rm=T), sd_PO4= sd(PO4, na.rm=T))



NO3_p<- summary_cl%>%
  group_by(Cluster)%>%
  # summarize(mean_ensemble= mean(Ensemble_model), sd_ensemble=sd(Ensemble_model), )
  ggplot(aes(x=Cluster, y=mean_NO3NO2, fill=as.factor(Cluster))) +
  # geom_point(size= 5)+
  geom_bar(stat="identity", width = 0.6)+
  coord_flip()+
  theme_bw()+
  geom_errorbar(aes(ymin=mean_NO3NO2-sd_NO3NO2, ymax=mean_NO3NO2+sd_NO3NO2), width=.1,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Nitrate+nitrite (µM)", x="Environmental Cluster", fill= "Cluster")+
  theme(legend.position = "null")

PO_p<- summary_cl%>%
  group_by(Cluster)%>%
  # summarize(mean_ensemble= mean(Ensemble_model), sd_ensemble=sd(Ensemble_model), )
  ggplot(aes(x=Cluster, y=mean_PO4, fill=as.factor(Cluster))) +
  # geom_point(size= 5)+
  coord_flip()+
  geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  geom_errorbar(aes(ymin=mean_PO4-sd_PO4, ymax=mean_PO4+sd_PO4), width=.1,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Phosphate (µM)", x="Environmental Cluster", fill= "Cluster")+
  theme(legend.position = "null")


SiO2_p<- summary_cl%>%
  group_by(Cluster)%>%
  ggplot(aes(x=Cluster, y=mean_SiO2, fill=as.factor(Cluster))) +
  # geom_point(size= 5)+
  geom_bar(stat="identity", width = 0.6)+
  coord_flip()+
  theme_bw()+
  geom_errorbar(aes(ymin=mean_SiO2-sd_SiO2, ymax=mean_SiO2+sd_SiO2), width=.1,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Silicate (µM)", x="Environmental Cluster", fill= "Cluster")+
  theme(legend.position = "null")

suit_nit<- summary_cl%>%
  group_by(Cluster)%>%
  ggplot(aes(x=mean_NO3NO2, y=mean_Ensemble, color=as.factor(Cluster))) +
  geom_point(size= 3)+
  # geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  geom_errorbar(aes(xmin=mean_NO3NO2-sd_NO3NO2, xmax=mean_NO3NO2+sd_NO3NO2), width=.01,
  position=position_dodge(.9)) +
  scale_color_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Habitat suitability", x="Nitrate+nitrite (µM)", fill= "Cluster")+
  theme(legend.position = "null")

suit_PO<- summary_cl%>%
  group_by(Cluster)%>%
  ggplot(aes(x=mean_PO4, y=mean_Ensemble, color=as.factor(Cluster))) +
  geom_point(size= 3)+
  theme_bw()+
  geom_errorbar(aes(xmin=mean_PO4-sd_PO4, xmax=mean_PO4+sd_PO4), width=.01,
  position=position_dodge(.9)) +
  scale_color_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Habitat suitability", x="Phosphate (µM)", fill= "Cluster")+
  theme(legend.position = "null")

suit_Sil<- summary_cl%>%
  group_by(Cluster)%>%
  ggplot(aes(x=mean_SiO2, y=mean_Ensemble, color=as.factor(Cluster))) +
  geom_point(size= 3)+
  theme_bw()+
  geom_errorbar(aes(xmin=mean_SiO2-sd_SiO2, xmax=mean_SiO2+sd_SiO2), width=.01,
                position=position_dodge(.9)) +
  scale_color_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Habitat suitability", x="Silicate (µM)", fill= "Cluster")+
  theme(legend.position = "null")

cowplot::plot_grid(NO3_p, PO_p, SiO2_p, 
                   suit_nit, suit_PO, suit_Sil, nrow = 2)
# ggsave(paste(path_model_res, "Suitability_x_cluster_boxplot.pdf", sep="/"), width = 7, height = 6, dpi = 300, units="cm")


#  Compare the proportion of suitable area and the average nutrient content ====
model_cl_stack_df<- as.data.frame(stack_raster, xy=T)
colnames(model_cl_stack_df)[3:5]<-c("Cluster", "Ensemble_model", "Binary_suitability")

# summary per variable category ================================================
cluster_cells<-  model_cl_stack_df%>%
  filter(!is.na(Binary_suitability))%>%
  group_by(Cluster)%>%
  summarize(n_cluster= length(Cluster))


summary_cl_binary<- model_cl_stack_df%>%
  filter(!is.na(Binary_suitability))%>%
  group_by(Cluster, Binary_suitability)%>%
  summarize(cells_binary= length(Cluster))

summary_cl_binary<- merge(summary_cl_binary, cluster_cells, by="Cluster")
summary_cl_binary$prop_binary<- round(summary_cl_binary$cells_binary / summary_cl_binary$n_cluster, 2)

summary_cl_binary%>%group_by(Cluster)%>%summarize(total= sum(prop_binary)) # check that the sum is equal to 1 (100%)


summary_cl_binary$Binary_suitability<- as.factor(summary_cl_binary$Binary_suitability)
levels(summary_cl_binary$Binary_suitability)<- c("Unsuitable", "Suitable")

summary_cl_binary%>%#filter(Cluster!="1")%>%
  ggplot(aes(x=as.factor(Cluster), y=prop_binary, fill=as.factor(Binary_suitability))) +
  geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  scale_fill_manual(values=c("blue3",  "green3")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Suitability")


#  Add columns of proportion of area suitable to the table
summary_cl<- merge(summary_cl, summary_cl_binary[which(summary_cl_binary$Binary_suitability == "Suitable"),], by=c("Cluster"))
colnames(summary_cl)[13]<- "prop_suitable_area"

suitBi_nit<-
  summary_cl%>%
  group_by(Cluster)%>%
  ggplot(aes(x=mean_NO3NO2, y=prop_suitable_area, color=as.factor(Cluster))) +
  geom_point(size= 3)+
  # geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  geom_errorbar(aes(xmin=mean_NO3NO2-sd_NO3NO2, xmax=mean_NO3NO2+sd_NO3NO2), width=.01,
                position=position_dodge(.9)) +
  scale_color_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of suitable area", x="Nitrate+nitrite (µM)", fill= "Cluster")+
  theme(legend.position = "null")

suitBi_PO<- summary_cl%>%
  group_by(Cluster)%>%
  ggplot(aes(x=mean_PO4, y=prop_suitable_area, color=as.factor(Cluster))) +
  geom_point(size= 3)+
  theme_bw()+
  geom_errorbar(aes(xmin=mean_PO4-sd_PO4, xmax=mean_PO4+sd_PO4), width=.01,
                position=position_dodge(.9)) +
  scale_color_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of suitable area", x="Phosphate (µM)", fill= "Cluster")+
  theme(legend.position = "null")

suitBi_Sil<- summary_cl%>%
  group_by(Cluster)%>%
  ggplot(aes(x=mean_SiO2, y=prop_suitable_area, color=as.factor(Cluster))) +
  geom_point(size= 3)+
  theme_bw()+
  geom_errorbar(aes(xmin=mean_SiO2-sd_SiO2, xmax=mean_SiO2+sd_SiO2), width=.01,
                position=position_dodge(.9)) +
  scale_color_manual(values=c("slateblue1","seagreen2","turquoise2", "goldenrod2","magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of suitable area", x="Silicate (µM)", fill= "Cluster")+
  theme(legend.position = "null")


cowplot::plot_grid(NO3_p, PO_p, SiO2_p, 
                   suit_nit, suit_PO, suit_Sil,
                   suitBi_nit, suitBi_PO, suitBi_Sil, nrow = 3)

path_plots<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Plots"
# ggsave(paste(path_plots, "Nutrients_suitability_cluster.pdf", sep="/"), width = 17, height = 17, dpi = 300, units="cm")
# ggsave(paste(path_plots, "Nutrients_suitability_cluster.png", sep="/"), width = 17, height = 17, dpi = 300, units="cm")



