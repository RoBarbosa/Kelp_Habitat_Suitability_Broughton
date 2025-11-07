###==================================================================
###     Species Distribution models    SDMs          ################
###                                                  ################
### 1- Layers Selection                              ################
### Author: Romina Barbosa                           ################
### Last update Date: 10-March-2025                    ################
###==================================================================

library(raster)
library(rasterVis)
library(dplyr)
library(ggplot2)
library(cowplot)
library(terra)
library(flexsdm)
library(corrplot)

# plotspath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Plots"
plotspath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/Reports/Paper1_SDMs/Figures"
SDM_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs"
vars_selection_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/variables_selection"

###======================================================###
#                                                          #
#           Load the stack of all rasters                  #
###======================================================###
# load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/layers/processed_layers/SDM_stacked_57layers_3005.RData")
# stack_variables<- a
# names(stack_variables)[2]<- "bathymetry"

stack_variables<- rast("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/variables_selection/60layersStack_FINAL.tif")

crs(stack_variables)<- "EPSG:3005"
unique(names(stack_variables))
names(stack_variables)# 60 variables

###======================================================###
#                                                          #
#     Exclude  variables                                   #
###======================================================###
### We exclude some variables based on the correlation and vif =================
## Exclude nutrient layer due to their low resolution and poor representation ==
## Exclude DIN and DIF also due to their low resolution and poor representation =
subset<- names(stack_variables)[-c(1:4,38:53)]
which(names(stack_variables) %in% subset)
subset_ras<- which(names(stack_variables) %in% subset)
names(stack_variables)


filtered_vars <- raster::subset(stack_variables, subset_ras)#grep('_GAM', names(my_stack), value = T)
length(names(filtered_vars))# 40

subset %in% names(filtered_vars)

### Save filtered variables
# save.image(paste(SDM_path,"variables_selection/filtered_40variables_FINAL.RData", sep="/"))


###======================================================###
#                                                          #
#           Check colinearity of variables and plot        #
###======================================================###
library(corrplot)
setwd(plotspath)

## Perform pearson collinearity control
# df_variables<- as.data.frame(stack_variables, na.rm=T) # this takes about 15 min
df_variables<- df_variables[,which(colnames(df_variables)%in% subset)]
colnames(df_variables)
df_variables<- df_variables[,-22] # excude raster julBT_ave due to errors in the layer
var<- cor(df_variables)

cor_variables<- as.matrix(round(var, 2))

## text labels rotated 45 degrees and  wider color legend with numbers right aligned
# png(paste(plotspath,"Correlation_AllVariables.png", sep="/"),
# height = 20, width = 20, units="cm", res=300)
corrplot::corrplot(cor_variables, type = 'lower', tl.col = 'black',#order = 'hclust',
                   cl.ratio = 0.15, tl.srt = 45, col = COL2('RdYlBu', 40), cl.cex = 0.8,  tl.cex = 0.6) #COL2('PuOr', 30)
# dev.off()


###======================================================###
#                                                          #
#           Plot of dissmilarity tree                      #
###======================================================###
cor_variables<- as.matrix(round(var, 3))
dissimilarity = 1 - cor_variables
distance = as.dist(dissimilarity) 

hClust<- hclust(distance)
clust <- rect.hclust(hClust, h=0.7, border=0)

# Save plot -
png(paste(vars_selection_path,"Correlation_AllVariables_Tree1.png", sep="/"),
height = 15, width = 29, units="cm", res=300)

pdf(paste(vars_selection_path,"Correlation_40_Variables_Tree1.pdf", sep="/"),
    height = 3.5, width = 9)
par(mar=c(2,4,3,1))
plot(hclust(distance), main="Clustering of variables based on 0.3 dissimilarity (0.7 correlation)", xlab="", ylab="Dissimilarity (1 - Correlation)")
# rect.hclust(hClust, h=0.3, which=which(NumMemb>=1))
abline(h = 0.3, col = 'red', lty = 3, lwd= 3)
dev.off()



###======================================================###
#                                                          #
#           Select non correlated variables                #
###======================================================###
names(filtered_vars)
# [1] "eastherness" "janBS_ave"   "janBS_max"   "janBS_min"   "janBSpd_ave" "janBSpd_max" "janBSpd_min"
# [8] "janBT_ave"   "janBT_max"   "janBT_min"   "janSSpd_ave" "janSSpd_max" "janSSpd_min" "janST_ave"  
# [15] "janST_max"   "julBS_ave"   "julBS_max"   "julBS_min"   "julBSpd_ave" "julBSpd_max" "julBSpd_min"
# [22] "julBT_ave"   "julBT_max"   "julBT_min"   "julSS_ave"   "julSS_max"   "julSS_min"   "julSSpd_ave"
# [29] "julSSpd_max" "julSSpd_min" "julST_ave"   "julST_max"   "julST_min"   "northness"   "roughness"  
# [36] "slope"       "tidal_cur"   "TPI"         "TRI"         "wind"    

subset2<- c("julST_ave", "julST_max",      #1
           "julBT_min",                    #2 # replaced min
           "julBS_ave",                    #3
           # "julBT_ave",                    #4
           "janBT_ave",                    #6
           "eastherness",                  #7
           "TPI",                          #8
           "slope",                        #9
           "northness",                    #10
           "julSS_ave",                    #11
           "janBS_ave",                    #12
           "tidal_cur",                    #13
           "janBSpd_ave",                  #14
           "julSSpd_ave", "julSSpd_max",   #15
           "julBT_max",                    #16
           "janST_max",                    #22 # was not included in the analysis (by  mistake)
           "wind",                         #17
           "janBS_max",                    #18
           "julBSpd_max",                  #19
           "julSSpd_min",                  #20
           "janBSpd_min",                  #21
           "janSSpd_min")                  #22

length(subset2)# 24

which(names(filtered_vars) %in% subset2)
subset_ras2<- which(names(filtered_vars) %in% subset2)
names(filtered_vars)


filtered_vars2 <- raster::subset(filtered_vars, subset_ras2)#grep('_GAM', names(my_stack), value = T)
length(names(filtered_vars2))# 24

subset2 %in% names(filtered_vars)

plot(filtered_vars2$julBT_max)
plot(filtered_vars2$julBT_min)
plot(filtered_vars$julBT_ave)
plot(filtered_vars2$julBT_max - filtered_vars2$julBT_min)


###======================================================###
#                                                          #
#           Check colinearity of selected variables        #
###======================================================###
var2 <- correct_colinvar(env_layer = filtered_vars2, method = c("pearson", th = "0.75"))
cor_variables2<- as.matrix(round(var2$cor_table, 2))

setwd(vars_selection_path)
# png("Correlation_23variables_nonCorrelated.png",height = 20, width = 20, res=300)
# pdf("Correlation_23variables.pdf",height = 7, width = 7)

corrplot::corrplot(cor_variables2, type = 'lower', tl.col = 'black',#order = 'hclust',
                   cl.ratio = 0.15, tl.srt = 45, col = COL2('RdYlBu', 40), cl.cex = 0.8,  tl.cex = 0.6)
# dev.off()


cor_variables2<- as.matrix(round(var2$cor_table, 3))
dissimilarity = 1 - cor_variables2
distance = as.dist(dissimilarity) 
# distance = as.dist(cor_variables) 

hClust<- hclust(distance)
clust <- rect.hclust(hClust, h=0.7, border=0)

### Save plot -==========================
# png(paste(plotspath,"variables_selection/Correlation_37Variables_Tree.png", sep="/"),
# height = 15, width = 29, units="cm", res=300)

# pdf(paste(vars_selection_path,"Correlation_23Variables_Tree2_vFINAL.pdf", sep="/"),
#     height = 4, width = 9)

par(mar=c(2,4,3,1))
plot(hclust(distance), main="Clustering of variables based on 0.3 dissimilarity (0.7 correlation)", xlab="", ylab="Dissimilarity (1 - Correlation)")
# rect.hclust(hClust, h=0.3)
abline(h = 0.3, col = 'red', lty = 3, lwd= 3)
# dev.off()


### We exclude some variables based on the correlation =================
names(filtered_vars2)
subset3<- c(  "julST_ave", #"julST_max",      #1 
              "julBT_max",                    #2
              "julBT_min",                    #3
              "julBS_ave",                    #4
              # "julBT_ave",                    #5
              "eastherness",                  #6
              "TPI",                          #7
              "slope",                        #8
              "northness",                    #9
              "julSS_ave",                    #10  
              "tidal_cur",                    #11   
              "julBSpd_max",                  #12
              "julSSpd_ave",                  #13
              "janBT_ave",                    #14           
              "wind",                         #15
              # "janBS_max",                  #16 --> not included
              # "julBSpd_min",                #17 --> not included
              "julSSpd_min",                  #18
              "janBSpd_min",                  #19
              "janSSpd_min")                  #20

length(subset3)# 17 variables 

subset_ras3<- which(names(filtered_vars2) %in% subset3)

filtered_vars3 <- raster::subset(filtered_vars2, subset_ras3)
length(names(filtered_vars3))

var3 <- correct_colinvar(env_layer = filtered_vars3, method = c("pearson", th = "0.7"))
cor_variables3<- as.matrix(round(var3$cor_table, 2))


png(paste(vars_selection_path, "Correlation_17FINALVariables.png", sep="/"),
height = 14, width = 14, units="cm", res=300)

corrplot::corrplot(cor_variables3, type = 'lower', tl.col = 'black',#order = 'hclust',
                   cl.ratio = 0.15, tl.srt = 45, col = COL2('RdYlBu', 40), cl.cex = 0.8,  tl.cex = 0.6)
dev.off()

# write.csv(cor_variables3, paste(vars_selection_path, "correlation_17variables.csv", sep="/"))
          
cor_variables3<- as.matrix(round(var3$cor_table, 3))
dissimilarity3 = 1 - cor_variables3
distance3 = as.dist(dissimilarity3) 

hClust3<- hclust(distance3)
clust <- rect.hclust(hClust3, h=0.7, border=0)

par(mar=c(2,4,3,1))
plot(hClust3, main="Clustering of variables based on 0.3 dissimilarity (0.7 correlation)", xlab="", ylab="Dissimilarity (1 - Correlation)")
# rect.hclust(hClust, h=0.3)
abline(h = 0.3, col = 'red', lty = 3, lwd= 3)


# save.image("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/variables_selection/variables_selection_correlation.RData")




















###=============================================================================
### Save filtered variables=====================================================

crs(filtered_vars2)  <- "epsg:3005"
names((filtered_vars2))
terra::writeRaster(filtered_vars2, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/variables_selection/stack_25FilteredVars_FINAL.tif")



SDM_path<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs"
setwd(paste(SDM_path,"variables_selection", sep="/"))
dir()

crs(filtered_vars2)<- "epsg:3005"
  
names(filtered_vars2)
for (i in 1:length(names(filtered_vars2))) {
  x <- rast(filtered_vars2[[i]])
  name<- names(filtered_vars2)[[i]]
  terra::writeRaster(x, paste(name, "tif", sep="."))
}


### Scatter plot relationship among variables ==================
# library("PerformanceAnalytics")
# sample_vars_env<- sample_n(as.data.frame(filtered_vars2, na.rm= T), 10000)#, replace=F
# chart.Correlation(sample_vars_env[,1:10], histogram=TRUE, pch=19)
# chart.Correlation(sample_vars_env[,10:30], histogram=TRUE, pch=19)
# # chart.Correlation(sample_vars_env[,1:10], histogram=TRUE, pch=19)
# # chart.Correlation(sample_vars_env[,1:10], histogram=TRUE, pch=19)
# # it take to long to run!!


# ggsave(paste(plotspath,"pars_29Vars.png", sep="/"),
#     height = 30, width = 30, units="cm", dpi=300)
terra::pairs(filtered_vars2)
# dev.off()

###=============================================================================
### Check VIF for the subset variables =========================================
vif_filteredVars<- car::vif(as.data.frame(filtered_vars2))
ggplot(vif_filteredVars, aes(x= Variables, y= VIF))+
  geom_point()+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# ggsave(paste(plotspath,"VIF_FilteresVars2.png", sep="/"),
#     height = 9, width = 15, units="cm", dpi=300)

vif_table_selected<- vif_table%>%
  filter(VIF<=5)
vif_filteredVars<- vif(filtered_vars)

vif_table$selected<- NA
vif_table[which(vif_table$VIF>= 3), "selected"]<- "No"
vif_table[which(vif_table$VIF< 3), "selected"]<- "Yes"

# write.csv(vif_table, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/variables_selection/vif_table_filteredVars.csv")




