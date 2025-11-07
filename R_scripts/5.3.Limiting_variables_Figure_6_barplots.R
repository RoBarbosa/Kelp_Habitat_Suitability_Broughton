

path_limitVars<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Limiting_variables_"

# data_stack_df<- read.csv(paste(path_limitVars, "limit_variables_cluster_EnsModel1_EnsModel2_df.csv", sep="/"))
# data_stack_df<- data_stack_df[,-1]
# head(data_stack_df)

# NOTE: when reading the table I don;t know why the order of colors in the plot change. 
# I used the original table before saving it as csv, so if you use the csv file you should correct the 
# order of colors in the barplot


# Calculate the proportion of the total no suitbale area limited by substrate ad depth: =====

suitable_area_M1<- data_stack_df%>%
  filter(Binary_Ens_model_1 == 1)%>%
  summarize(n_cells_suitable= length(Binary_Ens_model_2), area_suitable= n_cells_suitable * 0.020 * 0.020)
  
#  Proportion of suitable area (Ens model 1) limited by substrate and depth 
depth_substrate_lim_area<- data_stack_df%>%
  filter(Binary_Ens_model_1 == 1)%>%
  group_by(limit_VariName_EnsM2)%>%
  summarize(n_cells_variable= length(cluster), area_variable_km2= n_cells_variable * 0.02 * 0.02)

# Proportion of suitbale area limited by depth:
depth_substrate_lim_area[which(depth_substrate_lim_area$limit_VariName_EnsM2 == "depth"), "area_variable_km2"]/
  suitable_area_M1$area_suitable
# 0.5175259
# 0.3444962 # Nov 2025

depth_substrate_lim_area[which(depth_substrate_lim_area$limit_VariName_EnsM2 == "substrate"), "area_variable_km2"]/
  suitable_area_M1$area_suitable
# 0.4824741
# 0.3211636 # Nov 2025

prop_limited_subs_depth_df<- data_stack_df%>%
  mutate(n_cells= length(cluster))%>%
  group_by(limit_CatName_EnsM2, limit_VariName_EnsM2)%>%
  summarize(prop_variable= length(cluster)/n_cells,
            area_cluster= length(cluster)* 0.020 * 0.020)

prop_limited_subs_depth_df<- prop_limited_subs_depth_df[-which(duplicated(prop_limited_subs_depth_df)),]
# write.csv(prop_limited_subs_depth_df, "prop_limited_subs_depth_entireNoSuitableArea_df.csv")


# Proportion of total no suitable area (ENs-model2) limited by Depth
prop_limited_subs_depth_df[which(prop_limited_subs_depth_df$limit_VariName_EnsM2 == "Ddepth"), "prop_variable"]
# 1         0.375 

# Proportion of total no suitable area (ENs-model2) limited by hard Substrate availability 
prop_limited_subs_depth_df[which(prop_limited_subs_depth_df$limit_VariName_EnsM2 == "Sandy substrate"), "prop_variable"]
# 1         0.349

# Sum of both limited by depth and substrate
prop_limited_subs_depth_df[which(prop_limited_subs_depth_df$limit_VariName_EnsM2 == "depth"), "prop_variable"] +
prop_limited_subs_depth_df[which(prop_limited_subs_depth_df$limit_VariName_EnsM2 == "substrate"), "prop_variable"]
# prop_variable
# 1     0.7240888


### Plots of limiting variables including the substrate type ===================
#  =============================================================================
colnames(data_stack_df)
# "x"                     "y"                     "cluster"               "limit_Variables_EnsM1" "limit_Variables_EnsM2"
# [6] "limit_categories_M1"   "limit_categories_M2"   "Binary_Ens_model_1"    "Binary_Ens_model_2"    "limit_VariName_EnsM1" 
# [11] "limit_VariName_EnsM2"  "limit_CatName_EnsM1"   "limit_CatName_EnsM2"



# Calculate the total area per cluster and the proportion of area limited by each variable ====
summary_lim_cl<- data_stack_df%>%
  # filter(!is.na(limit_Variables))%>%
  group_by(cluster)%>%
  mutate(n_cluster= length(cluster))%>%
  group_by(cluster, limit_CatName_EnsM2, limit_VariName_EnsM2)%>%
  summarize(prop_variable= length(cluster)/n_cluster,
            area_cluster= length(cluster)* 0.020 * 0.020)

summary_lim_cl<- summary_lim_cl[-which(duplicated(summary_lim_cl)),]
summary_lim_cl%>%group_by(cluster)%>%summarize(total= sum(prop_variable)) # check that the sum is equal to 1 (100%)


# summary per variable category ================================================
data_stack_df$cluster<- as.factor(data_stack_df$cluster)

# Calculate the total area per cluster and the proportion of area limited by each variable type ====
summary_limC_cl<- data_stack_df%>%
  # filter(!is.na(limit_VariName_EnsM2))%>%
  group_by(cluster)%>%
  mutate(n_cluster= length(cluster))%>%
  group_by(cluster, limit_CatName_EnsM2)%>%
  summarize(prop_variable= length(cluster)/n_cluster,
            area_cluster= n_cluster* 0.020 * 0.020)

summary_limC_cl<- summary_limC_cl[-which(duplicated(summary_limC_cl)),]
summary_limC_cl%>% group_by(cluster)%>%summarize(total= sum(prop_variable)) # check that the sum is equal to 1 (100%)


### Create BoxPLOT of limiting variables per cluster ==========================

a<- summary_limC_cl%>%#filter(cluster!="1")%>%
  ggplot(aes(x=as.factor(cluster), y=prop_variable, fill=(limit_CatName_EnsM2))) +
  geom_bar(stat="identity", width = 0.6)+
  # geom_point(aes(x=cluster, y= area_cluster* 9.833847e-05), color="white", size=2)+
  theme_bw()+
  scale_fill_manual(values=c("mediumslateblue", "goldenrod3", "seagreen2", "darkturquoise",  "brown4", "darkblue")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Type of Variable")


b<- summary_lim_cl%>%
  ggplot(aes(x=as.factor(cluster), y=prop_variable, fill=as.factor(limit_VariName_EnsM2))) +
  geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  # scale_fill_manual(values=c("mediumslateblue", "goldenrod2", "seagreen2", "darkturquoise",  "brown2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Type of Variable")



cowplot::plot_grid(a, b, nrow=2, align = "v")

plotspath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/Reports/Paper1_SDMs/Figures"
# ggsave(paste(plotspath,"Figure_6_EnsModel2_March2025.pdf", sep="/"),
#        height = 15, width = 12, units="cm", dpi=300)





###=============================================================================
### Plots of limiting variables EXCLUDING the substrate type ===================
#  =============================================================================
# Calculate the total area per cluster and the proportion of area limited by each variable ====
summary_lim_cl<- data_stack_df%>%
  filter(limit_VariName_EnsM1 != "Suitable")%>%
  group_by(cluster)%>%
  mutate(n_cluster= length(cluster), area_cluster= length(cluster)* 0.020 * 0.020)%>%
  group_by(cluster, limit_VariName_EnsM1)%>%
  summarize(prop_variable= length(cluster)/n_cluster)

summary_lim_cl<- data_stack_df%>%
  filter(limit_VariName_EnsM1 != "Suitable")%>%
  group_by(cluster)%>%
  mutate(n_cluster= length(cluster), area_cluster= length(cluster)* 0.020 * 0.020)%>%
  group_by(cluster, limit_VariName_EnsM1)%>%
  summarize(prop_variable= length(cluster)/n_cluster)


summary_lim_cl<- summary_lim_cl[-which(duplicated(summary_lim_cl)),]
summary_lim_cl%>%group_by(cluster)%>%summarize(total= sum(prop_variable)) # check that the sum is equal to 1 (100%)

summary_lim_cl<- summary_lim_cl%>%
  filter(limit_VariName_EnsM1 != 0)

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

summary_lim_cl$variable_name<- as.factor(summary_lim_cl$limit_VariName_EnsM1)
levels(summary_lim_cl$variable_name)<- variables_names
print(summary_lim_cl)
# write.csv(summary_lim_cl, "table_for_barplot_Figure6_all_variables.csv")


# summary per variable category ================================================
# Calculate the total area per cluster and the proportion of area limited by each variable type ====
summary_limC_cl<- data_stack_df%>%
  # filter(limit_VariName_EnsM1 != "Suitable")%>%
  group_by(cluster)%>%
  mutate(n_cluster= length(cluster))%>%
  group_by(cluster, limit_CatName_EnsM1)%>%
  summarize(prop_variable= length(cluster)/n_cluster,
            area_cluster= n_cluster* 0.020 * 0.020)

summary_limC_cl<- summary_limC_cl[-which(duplicated(summary_limC_cl)),]
summary_limC_cl%>%group_by(cluster)%>%summarize(total= sum(prop_variable)) # check that the sum is equal to 1 (100%)

print(summary_lim_cl)
# write.csv(summary_lim_cl, "table_for_barplot_Figure6_categories.csv")

### Create BoxPLOT of limiting variables per cluster ==========================

c<- summary_limC_cl%>%#filter(cluster!="1")%>%
  ggplot(aes(x=as.factor(cluster), y=prop_variable, fill=as.factor(limit_CatName_EnsM1))) +
  geom_bar(stat="identity", width = 0.6)+
  # geom_point(aes(x=cluster, y= area_cluster* 9.833847e-05), color="white", size=2)+
  theme_bw()+
  scale_fill_manual(values=c("red", "mediumslateblue", "goldenrod3", "seagreen2", "darkturquoise",  "brown4", "darkblue")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Type of Variable")


d<- summary_lim_cl%>%
  ggplot(aes(x=as.factor(cluster), y=prop_variable, fill=as.factor(limit_VariName_EnsM1))) +
  geom_bar(stat="identity", width = 0.6)+
  theme_bw()+
  # scale_fill_manual(values=c("mediumslateblue", "goldenrod2", "seagreen2", "darkturquoise",  "brown2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  labs(y="Proportion of area", x="Environmental Cluster", fill= "Type of Variable")



cowplot::plot_grid(c, d, nrow=2, align = "v")

# plotspath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/Reports/Paper1_SDMs/Figures"
# ggsave(paste(plotspath,"Figure_6_EnsModel1_March2025.pdf", sep="/"),
       # height = 15, width = 12, units="cm", dpi=300)


cowplot::plot_grid(a, b, c, d, nrow=4, align = "v")
# ggsave(paste(plotspath,"Figure_6_EnsModel1_March2025.pdf", sep="/"),
#        height = 15, width = 12, units="cm", dpi=300)




# #### Variables Responses =======================================================
# subset001<- c(   "TPI",
#                  "eastherness",
#                  "janBT_ave", #
#                  "julBS_ave", #
#                  "julBSpd_max",
#                  "julBT_ave",
#                  "julBT_min", #
#                  "julSSpd_min", #
#                  "julST_ave", #
#                  "slope", #
#                  "tidal_cur") 
# 
# par(mfrow=c(4,4))
# for (a in 1:11){
r <- dismo::response(model_replicates_003$models[[1]])#, var=subset001[a], title= subset001[a]
# }
# 
# dismo::response(model_replicates_001_v2$models[[30]])
# 
# # recreate the plot:
# plot(r, type="l", ylim=c(0,1), col=2)
