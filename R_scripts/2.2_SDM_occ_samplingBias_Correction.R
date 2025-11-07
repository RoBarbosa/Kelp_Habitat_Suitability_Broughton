###==================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### 2- Presence, speudo-absence and background data Preparation ################
### Author: Romina Barbosa                                      ################
### Date: 19-Jan-2024                                           ################
### Code edited from: https://sjevelazco.github.io/flexsdm/articles/v01_pre_modeling.html#data-species-occurrence-and-background-data ####
### Last edition: 18 Sep 2024
###==================================================================
library(raster)
library(dplyr)
library(terra)
library(sf)
library(flexsdm)
library(ggplot2)

SDM_path<- "/Volumes/Romina_BATI/BATI/SDMs"
layers_path<- paste(SDM_path, "layers", sep="/")

plotspath<- paste(SDM_path, "Plots", sep="/")
mypath<- paste(SDM_path, "csvs_to_SDMs", sep="/")


## Load bathymetry raster for plots
dem <- raster(paste(layers_path,"bathy_&_derived/bathy_masked_epsg4269.tif", sep="/"))
demp<- projectRaster(dem, crs= ("+proj=longlat +datum=WGS84")) #NAD83
land<- demp
land[land <= 0] <- NA
# land[land > 0] <- NA
land[land > 0.001] <- 1
plot(land)

plot(demp)
crs(demp)

# ###======================================================###
# #                                                          #
# #      Filter occurrence points                            #
# ###======================================================###
# ## Load dataset of presence/absence from focal sites ==========================
# # table edited first in SDM_layers_prop and added attributes in QGis
# occ_focal<-  read.csv(paste(mypath,"focal_sites_bati_coord3005_covars.csv", sep="/"))
# 
# occ_focal<- occ_focal%>%
#   filter(check_assessment == "ok")
# 
# occ_focal<- occ_focal%>%
#   dplyr::select(x, y, occ)
# 
# occ_focal$lat<- paste(substr(occ_focal$lat,1.2), substr(occ_focal$lat,3.9))
# 
# par(mfrow = c(2, 2))
# cuts=seq(0, 70, 10) #set breaks
# pal <- colorRampPalette(c("lightblue","darkblue"))
# 
# 
# plot(land, main = "Kelp absence data (focal sites)", col = "grey", legend = FALSE)
# plot(dem, main = "",  breaks=cuts, add=T, col = pal(7), legend=T, zlimits=c(0,70))
# points(occ_focal[which(occ_focal$occ== "0"), 1:2], pch = 19, cex = 0.75, col="red")
# points(occ_focal[which(occ_focal$occ== "0"), 1:2], pch = 21, cex = 0.75, col="black")
# 
# #legend
# # plot(dem, legend.only=TRUE, breaks=cuts, add=T, col = pal(7), limits=c(0,70),
# #      legend.args = list(text='Depth [m]'), 
# #      legend.width=1, legend.shrink=0.75)
#      # smallplot=c(0.7,0.90, 0.05,0.2))#; par(mar = par("mar"))
# 
# plot(land, main = "Kelp absence data (focal sites)", col = "grey",legend = FALSE)
# plot(dem, main = "",  breaks=cuts, col = pal(7), add=T, legend = FALSE)
# points(occ_focal[which(occ_focal$occ== "1"), 1:2], pch = 19, cex = 0.75, col="green")
# points(occ_focal[which(occ_focal$occ== "1"), 1:2], pch = 21, cex = 0.75, col="black")
# 
# 
# ## Load dataset of presence/absence from shoreline surveys =====================
# mypath<- "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs"
# shoreline_data<- read.csv(paste(mypath, "presence_abs_with_attributeVars.csv", sep="/"))
# 
# # Select only dataset from 2023
# levels(as.factor(shoreline_data$kelp_dynam))
# colnames(shoreline_data)
# colnames(shoreline_data)[which(colnames(shoreline_data) == "kelp_2023")]<- "occ"
# 
# # replace value 2 by 1 as occurrence
# shoreline_data[shoreline_data$occ == "2","occ"]<- "1"
# 
# levels(as.factor(shoreline_data$occ))
# shoreline_data_pres<- shoreline_data%>%
#   dplyr::filter(occ== "1")%>%
#   dplyr::filter(greater_th == "1")
# 
# shoreline_data_abs<- shoreline_data%>% dplyr::filter(occ== "0")
# 
# 
# shoreline_data<- rbind(shoreline_data_pres, shoreline_data_abs)%>%
#   dplyr::select(x_1, y, occ)
# 
# colnames(occ_focal)<- colnames(shoreline_data)
# 
# shoreline_data$origin<- "shoreline_surveys"
# occ_focal$origin<- "focal_sites"
# 
# # Plots
# # png(paste(plotspath,"presAbs_shoreline_all.png", sep="/"),
# #     height = 14, width = 31, units="cm", res=300)
# 
# par(mfrow = c(1, 2))
# plot(land, main = "Kelp absence data (shoreline surveys)", col = "grey", legend = FALSE)
# plot(dem, main = "",  breaks=cuts, add=T, col = pal(7), legend= F, zlimits=c(0,70))
# points(shoreline_data[which(shoreline_data$occ== "0"), 1:2], pch = 19, cex = 0.5, col="red")
# # points(shoreline_data[which(shoreline_data$occ== "0"), 1:2], pch = 21, cex = 0.75, col="black")
# 
# plot(land, main = "Kelp absence data (shoreline surveys)", col = "grey",legend = FALSE)
# plot(dem, main = "",  breaks=cuts, col = pal(7), add=T, legend = FALSE)
# points(shoreline_data[which(shoreline_data$occ== "1"), 1:2], pch = 19, cex = 0.5, col="green")
# # points(shoreline_data[which(shoreline_data$occ== "1"), 1:2], pch = 21, cex = 0.75, col="black")
# 
# # dev.off()
# 
# 
# 
# 
# 
# occ_data<- rbind(shoreline_data, occ_focal)
# colnames(occ_data)[3]<- "occ"
# colnames(occ_data)[1:2]<- c("x", "y")
# occ_data<- occ_data[-which.max(occ_data$y),]
# occ_data<- occ_data[-which.max(occ_data$y),]
# 
# # write.csv(occ_data, paste(mypath,"occ_focal_shoreline_sites_coord3005.csv", sep="/"))
# # write.csv(occ_data, paste(mypath,"occ_focal_shoreline_sites_greather10perc_coord3005.csv", sep="/"))





# occ_data<- read.csv( paste(mypath,"occ_focal_shoreline_sites_greather10perc_coord3005.csv", sep="/"))

# 
# #########################################################################################
# # Load occurrence data edited to change coordinates to 4269 -same as env layers- 
# occ_data<- read.csv(paste(mypath,"occ_focal_shoreline_sites_projected.csv", sep="/"))
# occ_data<- occ_data[,c(6,7,4,5, 2,3)]
# colnames(occ_data)[1:2]<- c("x", "y")
# occ_data<- occ_data[,1:4]
# # occ_data<- SpatialPoints(as.matrix(occ_data[1:2]))
# # crs(occ_data)<- crs(demp)
# 
# Tribune_occ <- read.csv("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs/Tribune_SalmonSurveys_occ_handEditCoordineatesBathy.csv")
# Tribune_occ<- Tribune_occ[, c(5,4,3)]
# colnames(Tribune_occ)<- c("x", "y", "occ")
# Tribune_occ$origin<- "salmon_surveys2023"
# head(Tribune_occ)
# head(occ_data)
# 
# # occ_data2<- rbind(occ_data, Tribune_occ)
# 
# 
# clusters <- raster::raster(paste(SDM_path,"Clustering/Last version/6clusters.tif", sep="/"))
# plot(clusters)
# plot(demp)
# clusters<- rast(clusters)
# dem<- rast(dem)
# 
# occ_data_cl<- sdm_extract( data= occ_data2, 
#                 x = "x",
#                 y = "y",
#                 env_layer = clusters,
#                 variables = NULL,
#                 filter_na = FALSE)
# 
# occ_data_cldepth<- sdm_extract( data= occ_data_cl, 
#                            x = "x",
#                            y = "y",
#                            env_layer = dem,
#                            variables = NULL,
#                            filter_na = FALSE)
# 
# occ_data2<- occ_data_cldepth
# par(mfrow=c(2,1))
# plot(clusters)
# plot(clusters, col="grey")
# points(occ_data2[1:2], col= occ_data2$cluster6, add=T, cex=0.5)
# 
# occ_data<- occ_data2
# # write.csv(occ_data, paste(mypath,"occ_data_focal_salmon_shoreline.csv", sep="/"))



# occ_data <- read.csv("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs/Edit_occs/join_near_occs_focal_salmon_shoreline2.csv")
# summary(occ_data[which(occ_data$NEAR_Y==00),"cluster6"])
# summary(occ_data)



# occ_data<- read.csv("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs/occs_env/corrected_occs_30Vars.csv")
# colnames(occ_data)


###==============================================================================
## Load occurrence data and exclude absences from shoreline surveys =============
###==============================================================================
occ_data_sdm<- read.csv(paste(SDM_path, "csvs_to_SDMs/occs_env/occs_env_34Vars_3005.csv", sep="/"))
occ_data_sdm<- occ_data_sdm[,-c(1:3, 7:16, 19)]
colnames(occ_data_sdm)[4:5]<- c("x", "y")
summary(occ_data_sdm)
head(occ_data_sdm)
occ_data_sdm<- occ_data_sdm[c(1:6,34)]
summary(occ_data_sdm)



### Load env layers =============================================================
# rastlist<- list_files_with_exts(paste(layers_path, "aligned_rasters", sep="/"), "tif")
# allrasters <- lapply(rastlist, raster)
# a <- stack(allrasters)
# names(a)

# stack_vars<- rast(paste(SDM_path, "variables_selection/stack_28FilteredVars_FINAL.tif", sep="/"))
stack_vars<- rast("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/variables_selection/60layersStack_FINAL.tif")
unique(names(stack_vars))
names(stack_vars)[2]<- "bathymetry"


occ_data_vars<- sdm_extract(
  data = occ_data_sdm[,c(1:7)],
  x = "x",
  y = "y",
  env_layer = stack_vars, # Raster with environmental variables
  variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
  filter_na = F  #NA's   :661, but they should be 315 based on the corrected bathymetry points 
)

summary(occ_data_vars)
length(occ_data_vars$occ)# [1] 5207 # 275
occ_data_vars<- as.data.frame(occ_data_vars)

plot(occ_data_vars$bathymetry_mask3005b, occ_data_vars$bathymetry)

ggplot(occ_data_vars, aes(x= as.factor(occ), y= bathymetry, color= origin))+
  geom_violin()+
  geom_boxplot()+ theme_bw()

occ_data_vars<- occ_data_vars[-7]
occ_data_vars<- occ_data_vars[c(1,4:5,7:length(colnames(occ_data_vars)), 2)]
colnames(WVocc_data_vars)[1]<- "occ"
colnames(occ_data_vars)


####========================================================================####
### WorldView Satellite data ===================================================
WV_data<- read.csv(paste(SDM_path, "csvs_to_SDMs/occ_without_shorelineSurveys/worldview_presence_absence_all_FINAL.csv", 
                         sep="/"), header=FALSE)
colnames(WV_data)<- WV_data[1,]
WV_data<- WV_data[-1,]
WV_data<- WV_data[1:5]
colnames(WV_data)[4:5]<- c("x", "y")
str(WV_data)
WV_data$x<- as.numeric(WV_data$x)
WV_data$y<- as.numeric(WV_data$y)



### Visualize Worldview records and extract variables values ===================
# WV_data_sf <- na.exclude(WV_data[,1:8]) %>%
#   st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4226")
WV_data_sf <- (WV_data) %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:3005")
mapview::mapview(WV_data_sf, cex=2, zcol= "pr_ab", col.regions= c("red", "blue", "grey", "purple"))# a visual check


WVocc_data_vars<- sdm_extract(
  data = WV_data,
  x = "x",
  y = "y",
  env_layer = stack_vars, # Raster with environmental variables
  variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
  filter_na = F  #NA's   :661, but they should be 315 based on the corrected bathymetry points 
)

WVocc_data_vars<- as.data.frame(WVocc_data_vars)
head(WVocc_data_vars)
WVocc_data_vars$origin<- "Worldview_satellite"
WVocc_data_vars<- WVocc_data_vars[3:length(colnames(WVocc_data_vars))]
colnames(WVocc_data_vars)[1]<- "occ"
# WVocc_data_vars<- WVocc_data_vars[,c(1, 61,2:3,5:60)]


# Merge worldview and focal sites/salmon surveys ================
occ_data_vars_merged<- rbind(occ_data_vars, WVocc_data_vars)
occ_data_vars_merged<- occ_data_vars_merged %>%
  filter(occ == "0" | occ == "1")

levels(as.factor(occ_data_vars_merged$occ))

# Extracted raster value in QGis ===
clusters<- rast("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Clustering/Last version/6CLUSTERs_6Variables_07022024_3005XY_FINAL.tif")
plot(clusters)
# join_occs_WV_Vars_3005_cluster

occ_data_vars_merged<- sdm_extract(
  data = occ_data_vars_merged,
  x = "x",
  y = "y",
  env_layer = clusters, # Raster with environmental variables
  variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
  filter_na = F  #NA's   :661, but they should be 315 based on the corrected bathymetry points 
)

colnames(occ_data_vars_merged)[65]<- "cluster6"

occ_data_vars_merged<- as.data.frame(occ_data_vars_merged)

# Explore where are the NAs 
merged_data_sf <- occ_data_vars_merged[which(is.na(occ_data_vars_merged$bathymetry)),] %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:3005")
mapview::mapview(merged_data_sf, cex=2, zcol= "occ", col.regions= c("red", "blue", "grey", "purple"))# a visual check

# For some reason there were points outside the study area (an artefact from QGis...)
# We exclude these NAs
occ_data_vars_merged<- occ_data_vars_merged[-which(is.na(occ_data_vars_merged$bathymetry)),]


# write.csv(occ_data_vars_merged, paste(SDM_path, "csvs_to_SDMs/occs_ALLmerged_60Vars_3005_cluster.csv", sep="/")) #29-04-2024



## %######################################################%##
#                                                          #
####          Filtering occurrences                     ####
#                                                          #
## %######################################################%##
options(digits=13)
occ_data <- read.csv(paste(SDM_path, "csvs_to_SDMs/occ_without_shorelineSurveys/occs_ALLmerged_60Vars_3005_cluster.csv", sep="/")) # 23-07-2024
occ_data<- occ_data%>% filter(origin!= "shoreline_surveys")

### Number of presences and absences from worldview image and focal sites ======
unique(WV_data$pr_ab)
length(WV_data[which(WV_data$pr_ab==1),"pr_ab"])#728
length(WV_data[which(WV_data$pr_ab==0),"pr_ab"])#2651

occ_data<- occ_data%>% filter(origin!= "shoreline_surveys")

length(occ_data[which(occ_data$occ==1 & occ_data$origin == "focal_sites"),"occ"])#100
length(occ_data[which(occ_data$occ==0 & occ_data$origin == "focal_sites"),"occ"])#127



### Visualize records in map and add cluster information =======================
locations_sf <- occ_data %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(locations_sf, cex=2, zcol="occ", col.regions= c("red", "blue"))# a visual check


layers3005_path<- paste(SDM_path,"new_layers_3005", sep="/")
dem<- raster(paste(layers3005_path,"bathymetry.tif", sep="/"))
crs(dem)<- "EPSG:3005"
plot(dem)

df_clusters<- read.csv(paste(SDM_path,"Clustering/Last version/6CLUSTERs_6Variables_07022024_3005XY_FINAL.csv", sep="/"))
head(df_clusters)

cls <- raster(nrow=4349, ncol=6069, xmn=913912.8, xmx= 1035293, ymn= 598508.7, ymx= 685488.7)
cells <- cellFromXY(cls, df_clusters[,11:12])
cls[cells] <- df_clusters[,9]
plot(cls)
names(cls)<- "cluster6"

# writeRaster(cls, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Clustering/Last version/6CLUSTERs_6Variables_07022024_3005XY_FINAL.tif")


# This clusters layer has low resolution (not 20 x 20 m)
# clusters <- terra::rast(paste(SDM_path,"Clustering/Last version/6clusters.tif", sep="/"))
# crs(clusters)<- "epsg:4269"
# clusters<- terra::project(clusters, "epsg:3005")
# plot(clusters)


cl_area<- as.data.frame(df_clusters) %>%
  group_by(cluster6) %>% 
  summarise(pixels = length(cluster6)) %>% # same than using n()
  mutate(`area_m2` = pixels * 20 * 20, #multiply by pixel area
         `area_km2` = round(`area_m2`/1e6,2)) 

# # A tibble: 6 × 4
# cluster6 pixels   area_m2 area_km2
# <int>  <int>     <dbl>    <dbl>
# 1        1  39044  15617600     15.6
# 2        2 140447  56178800     56.2
# 3        3 145533  58213200     58.2
# 4        4 351425 140570000    141. 
# 5        5 228297  91318800     91.3
# 6        6  78932  31572800     31.6

as.data.frame(cls) %>%
  group_by(cluster6) %>% 
  summarise(pixels = length(cluster6)) %>% # same than using n()
  mutate(`area_m2` = pixels * 20 * 20, #multiply by pixel area
         `area_km2` = round(`area_m2`/1e6,2)) 

# # A tibble: 7 × 4
# cluster6   pixels     area_m2 area_km2
# <int>    <int>       <dbl>    <dbl>
# 1        1    39044    15617600     15.6
# 2        2   140447    56178800     56.2
# 3        3   145533    58213200     58.2
# 4        4   351425   140570000    141. 
# 5        5   228297    91318800     91.3
# 6        6    78932    31572800     31.6
# 7       NA 25410403 10164161200  10164. 


cl_area<- na.exclude(cl_area)
sum(as.numeric(cl_area$pixels))

total_area<- sum(cl_area$area_km2)
cl_area$prop_area<- cl_area$area_km2 / total_area *100

occ_data_cl<- merge(occ_data, cl_area[,c(1,5)], by="cluster6") 
summary(occ_data_cl)
# occ_data_cl<- na.exclude(occ_data_cl)

summary(occ_data_cl$bathy3005b)

occ_data_cl %>%
  group_by(cluster6) %>% 
  summarise(n_occ = n())
#   cluster6 n_occ  --> 22-04-2024
# 1        2   773
# 2        3   380
# 3        4  1693
# 4        5  1205
# 5        6   491

# New values with WV presences and absences 
#    cluster6 n_occ
# <int> <int>
# 1        2   886
# 2        3   529
# 3        4  2151
# 4        5  1347
# 5        6   690

# Final values without the shoreline surveys
# cluster6 n_occ
# <int> <int>
# 1        2   509
# 2        3   588
# 3        4  1741
# 4        5   214
# 5        6   521

## Upload data raster for Filtering with distance buffer (0.1 km) ====================================
dem <- raster(paste(layers_path,"bathy_&_derived/bathy_masked_epsg4269.tif", sep="/"))
demp<- projectRaster(dem, crs= ("+proj=longlat +datum=WGS84")) #NAD83
plot(dem)
plot(demp)

demp<- rast(demp) # must be in WGS84
dem<- rast(dem)

locations_sf <- occ_data_cl %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(locations_sf, cex=2, zcol="occ", col.regions= c("red", "blue"))# a visual check

# locations_projected <- locations_sf %>%
#   st_transform(crs = "EPSG:4326") #4269
# mapview::mapview(locations_projected, cex=2,  zcol="occ", col.regions= c("red", "blue"))# a visual check

library(magrittr) #for the pipe
# df <- locations_projected %>%
#   dplyr::mutate(lon = sf::st_coordinates(.)[,1],
#                 lat = sf::st_coordinates(.)[,2])
# 
# # # save.image(paste(SDM_path,"sdm_buildingOccs_vars.RData", sep="/"))  #22-04-2024
# 
# occ_data_cl<- as.data.frame(df)


###=========================================================================####
## Spliting PRESENCES and ABSENCES and filtering with distance buffer (0.1 km) ====================================
pres<- occ_data_cl[which(occ_data_cl$occ==1),]
abs<- occ_data_cl[which(occ_data_cl$occ==0),]


filt_0.1km <- occfilt_geo(
  data = pres,
  x = "x",
  y = "y",
  env_layer = dem,
  method = c("defined", d = "0.1"),
  prj = crs(dem)
)
# Extracting values from raster ... 
# Number of unfiltered records: 2691
# Distance threshold(km): 0.04
# Number of filtered records: 2105

# when filtering to 0.1 km
# Extracting values from raster ... 
# Number of unfiltered records: 2691
# Distance threshold(km): 0.1
# Number of filtered records: 1075

# Extracting values from raster ... PLUS WORLVIEW image data
# Number of unfiltered records: 3501
# Distance threshold(km): 0.1
# Number of filtered records: 1528

# Extracting values from raster ... 
# Number of unfiltered records: 3593
# Distance threshold(km): 0.1
# Number of filtered records: 1793

# Extracting values from raster ... without shoreline data 
# Number of unfiltered records: 847
# Distance threshold(km): 0.1
# Number of filtered records: 772

filt_0.1km %>%
  group_by(cluster6, prop_area) %>% 
  summarise(n_occ = n())
# cluster6 prop_area n_occ
# 1        2     14.3    310
# 2        3     14.8    210
# 3        4     35.7    696
# 4        5     23.2    315
# 5        6      8.02   262

# cluster6 prop_area n_occ
# 1        2     14.3    115
# 2        3     14.8    117
# 3        4     35.7    350
# 4        5     23.2     63
# 5        6      8.02   127

all_pres_plot<- filt_0.1km%>%
  group_by(cluster6, prop_area) %>% 
  summarise(n_occ = n())%>%
  ggplot(aes(x= prop_area, y= n_occ, color=as.factor(cluster6)))+
  geom_point(size=3)+
  ylim(c(40, 400))+
  # geom_abline(intercept = 0, slope = 1, size = 0.5, color="grey") +
  labs(x="Proportion of area (%)", y="Number of presence records", color="Cluster")+
  theme_bw()#+ theme(legend.position="NULL")


### Same for absences ====
filtabs_0.1km <- occfilt_geo(
  data = abs,
  x = "x",
  y = "y",
  env_layer = dem,
  method = c("defined", d = "0.1"),
  prj = crs(dem)
)

# Extracting values from raster ... 
# Number of unfiltered records: 4611
# Distance threshold(km): 0.1
# Number of filtered records: 3393

#Extracting values from raster ... without shoreline surveys
# Number of unfiltered records: 2726
# Distance threshold(km): 0.1
# Number of filtered records: 2638


filtabs_0.1km %>%
  group_by(cluster6, prop_area) %>% 
  summarise(n_occ = n())

# cluster6 prop_area n_occ
# 1        2     14.3    439
# 2        3     14.8    542
# 3        4     35.7   1641
# 4        5     23.2    352
# 5        6      8.02   419

# cluster6 prop_area n_occ
# 1        2     14.3    383
# 2        3     14.8    462
# 3        4     35.7   1329
# 4        5     23.2    100
# 5        6      8.02   364


plot_abs_all<- filtabs_0.1km%>%
  group_by(cluster6, prop_area) %>% 
  summarise(n_occ = n())%>%
  mutate(index= n_occ/prop_area)%>%
  ggplot(aes(x= prop_area, y= n_occ, color=as.factor(cluster6)))+#, size=index
  geom_point(size=3)+
  # geom_abline(intercept = 0, slope = 1, size = 0.5, color="grey") +
  # ylim(c(0, 45))+xlim(c(0, 45))+
  labs(x="Proportion of area (%)", y="Number of absence records", color="Cluster")+
  theme_bw()#+ theme(legend.position="NULL")



###=============================================================================
### Determine the number of occurrences from each cluster ======================
filt_0.1km%>%
  group_by(cluster6, prop_area) %>% 
  summarise(n_occ = n())%>%
  group_by(cluster6)%>%
  mutate(index= n_occ/prop_area)

# New dataset with WorldView data
# cluster6 prop_area n_occ index  # excluding focal sites' records
# cluster6 prop_area n_occ index
# 1        2     14.3    310  21.7
# 2        3     14.8    210  14.2
# 3        4     35.7    696  19.5
# 4        5     23.2    315  13.6 # lowest value
# 5        6      8.02   262  32.7

# cluster6 prop_area n_occ index
# 1        2     14.3    115  8.05
# 2        3     14.8    117  7.91
# 3        4     35.7    350  9.80
# 4        5     23.2     63  2.71 # lowest value
# 5        6      8.02   127 15.8


# I select the cluster with the lowest index (Cluster 4;
# with lowest number of occurrences in relation to its surface area)
# then I divide the presences of cluster 4 into 2 and calculate N for each cluster

selected_index<- round(((63 /2/ 23.2)), 2) # 1.36 index, number of presences per relative area

filt_0.1km<- (as.data.frame(filt_0.1km))


n_occs_perCluster<- filt_0.1km%>%
  group_by(cluster6, prop_area) %>% 
  summarise(n_occ = n())%>%
  group_by(cluster6)%>%
  mutate(index= n_occ/prop_area, ideal_n= round((selected_index*prop_area),0), n_train= round(ideal_n*0.7, 0), n_test= ideal_n- n_train)

# cluster6 prop_area n_occ index ideal_n n_train n_test --> the calculation of selected index was wrong!!! 
# 1        2     14.3    310  21.7     171     120     51
# 2        3     14.8    210  14.2     178     125     53
# 3        4     35.7    696  19.5     429     300    129
# 4        5     23.2    315  13.6     279     195     84
# 5        6      8.02   262  32.7      96      67     29

# cluster6 prop_area n_occ index ideal_n n_train n_test
# 1        2     14.3    115  8.05      19      13      6
# 2        3     14.8    117  7.91      20      14      6
# 3        4     35.7    350  9.80      49      34     15
# 4        5     23.2     63  2.71      32      22     10
# 5        6      8.02   127 15.8       11       8      3


filtered_pres_plot<- filt_0.1km%>%
  group_by(cluster6, prop_area) %>% 
  summarise(n_occ = n())%>%
  group_by(cluster6)%>%
  mutate(index= n_occ/prop_area, ideal_n=  round((selected_index*prop_area),0))%>%
  # ggplot(aes(x= prop_area, y= n_occ, color=as.factor(cluster6)))+
  ggplot(aes(x= prop_area, y= ideal_n, color=as.factor(cluster6)))+
  geom_point(size=3)+
  # ylim(c(40, 750))+
  # geom_abline(intercept = 0, slope = 45, size = 0.5, color="grey") +
  labs(x="Proportion of area (%)", y="Number of presence records", color="Cluster")+
  theme_bw()#+ theme(legend.position="NULL")

cowplot::plot_grid(plot_abs_all,
                   all_pres_plot, filtered_pres_plot, ncol=3)

### PLot filtered occurrences (filtered by 0.04km and bathy<=40 m)
plot(cls) #clusters)
points(filt_0.1km[,c("x", "y")], col= filt_0.1km$cluster6, cex=0.75, pch=16)


###=============================================================================
### Create dataset to run models ===============================================
###=============================================================================
### Separate focal sites records to include them always on the model training ==
occ_focal_sites<- filt_0.1km%>% filter(origin=="focal_sites")

n_focal_cluster<- occ_focal_sites%>%
  group_by(cluster6, occ)%>%
  dplyr::summarize(n= length(cluster6))

locations_focalsites <- occ_focal_sites %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(locations_focalsites, cex=2.5,  zcol="occ", col.regions= c("red", "blue"))# a visual check
  
  
  
filt_0.1km<- filt_0.1km%>% filter(origin!="focal_sites")


###=============================================================================
### Create dataset of model validation or test =================================
# I selected a 30% of the ideal mumber of records of presence for cluster 

rdom_test_df<- data.frame(matrix(ncol=5, nrow = 0))
colnames(rdom_test_df)<- c("cluster6","occ", "x","y", "rdomreplicate")
  

# Add column of dataset and randomly assign records for testing (n ideal per cluster)
filt_0.1km$X<-seq_len(nrow(filt_0.1km))
filt_0.1km$dataset<- NA

for (i in 2:6) {
    cluster_df<- filt_0.1km[which(filt_0.1km$cluster6 == i),]
    n<- c(n_occs_perCluster[which(n_occs_perCluster$cluster6 == i), "n_test"])
    set.seed(i)
    rdom_test_cl<- sample_n(cluster_df, size= as.numeric(n), replace=F)
    filt_0.1km[rdom_test_cl$X, "dataset"]<- "test"
    filt_0.1km[is.na(filt_0.1km$dataset), "dataset"]<- "train"
}
  
presences_test<- filt_0.1km%>%
  filter(dataset=="test")

summary(as.factor(filt_0.1km$dataset))

rdom_test_df_sf <- presences_test %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(rdom_test_df_sf, cex=2,  zcol="occ", col.regions= c("red", "blue"))# a visual check
mapview::mapview(rdom_test_df_sf, cex=2,  zcol="dataset", col.regions= c("orange"))# a visual check

### Save testData2023_X_Cluster ================================================
# write.csv(filt_0.1km, paste(mypath, "test_train_Data2023_X_ClusterFINAL.csv", sep="/"))


###=============================================================================
### Create dataset of model training   ==========================================
# I selected a 30% of the ideal mumber of records of presence for cluster 

# Subset the records for training the model (excluding the testing 30%)
filt_0.1km_train<- filt_0.1km%>%
  filter(dataset=="train")


# Filter one presence per pixel 
# create raster with pixel ID
df_raster<- as.data.frame(dem, xy = TRUE)
df_raster$pixel_ID<- seq(1, length(df_raster[,1]), 1)

dem_a<- rasterFromXYZ(df_raster) #[,c(1:2,4)]
crs(dem_a)<- crs(dem)
dem_a<- rast(dem_a)

colnames(filt_0.1km)[6]<- "bathy_1"


# Add pixel ID based on raster (DEM cells)
press2<- sdm_extract(
  data = filt_0.1km,
  x = "x",
  y = "y",
  env_layer = dem_a, # Raster with environmental variables
  variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
  filter_na = F
)

# See duplicated records per pixel ID
which(duplicated(press2$pixel_ID))# there are no duplicates!


### Create REPLICATE of randomly sampled presences =============================
rdom_occs_df<- data.frame(matrix(ncol=6, nrow = 0))
colnames(rdom_occs_df)<- c("cluster6","occ", "origin","x","y", "rdomreplicate")

for (a in 1:50) {
  for (i in 2:6) {

  cluster_df<- filt_0.1km_train[which(filt_0.1km_train$cluster6 == i),]
  # n = ideal n - records from focal sites
  #--> focal sites have priority and are always included in the dataset
  n<- c(n_occs_perCluster[which(n_occs_perCluster$cluster6 == i), "n_train"]) - n_focal_cluster[which(n_focal_cluster$cluster6==i), "n"]
  set.seed(a)
  rdom_occs_cl<- sample_n( cluster_df, size= as.numeric(n), replace=F)
  
  # merge with all Focal site records from the same cluster
  colnames(rdom_occs_cl)[6]<- "aspect"
  rdom_occs_cl<- rbind(rdom_occs_cl[,1:66], occ_focal_sites[which(occ_focal_sites$cluster6==i),1:66])
  rdom_occs_cl$rdomreplicate<- a # add number of replicate
  rdom_occs_cl<- rdom_occs_cl[,c("cluster6", "origin", "occ", "x","y", "rdomreplicate")]
  rdom_occs_df<- rbind(rdom_occs_df, rdom_occs_cl)
  }
  
}

n_occs_perCluster$n_train == c(summary(as.factor(rdom_occs_df$cluster6))/50)
# 2    3    4    5    6 
# TRUE TRUE TRUE TRUE TRUE  ## train data has the N ideal (standardized by area)
head(rdom_occs_df)
unique(rdom_occs_df$rdomreplicate)



###=============================================================================
### SAME PROCESSING FOR ABSENCE RECORDS
###=============================================================================

### Separate focal sites records to include them always on the model training ===
abs_focal_sites<- filtabs_0.1km%>% filter(origin=="focal_sites")
n_abs_focal_cluster<- abs_focal_sites%>%
  group_by(cluster6, occ)%>%
  dplyr::summarize(n= length(cluster6))

absences_focalsites <- abs_focal_sites %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(absences_focalsites, cex=2.5,  zcol="occ", col.regions= c( "blue"))# a visual check


filtabs_0.1km<- filtabs_0.1km%>% filter(origin!="focal_sites")


###=============================================================================
### Determine the number of absences from each cluster ======================
#Number of absences per Cluster was determined to match the number of absences per cluster

###=============================================================================
### First separate a dataset to test the models ================================
# I selected a 30% of the ideal mumber of records of presence for cluster (same as for training)
rdomAbs_test_df<- data.frame(matrix(ncol=5, nrow = 0))
colnames(rdomAbs_test_df)<- c("cluster6","occ", "x","y", "rdomreplicate")

filtabs_0.1km$X<-seq_len(nrow(filtabs_0.1km))
filtabs_0.1km$dataset<- NA

for (i in 2:6) {
  cluster_df<- filtabs_0.1km[which(filtabs_0.1km$cluster6 == i),]
  n<- c(n_occs_perCluster[which(n_occs_perCluster$cluster6 == i), "n_test"])
  set.seed(i)
  rdom_test_cl<- sample_n( cluster_df, size= as.numeric(n), replace=F)
  filtabs_0.1km[rdom_test_cl$X, "dataset"]<- "test"
  filtabs_0.1km[is.na(filtabs_0.1km$dataset), "dataset"]<- "train"
}


summary(as.factor(filtabs_0.1km$dataset))

rdomAbs_test_df_sf <- filtabs_0.1km %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(rdomAbs_test_df_sf, cex=2,  zcol="cluster6")
mapview::mapview(rdomAbs_test_df_sf, cex=2,  zcol="dataset", col.regions= c("red", "blue"))# a visual check


absences_test<- filtabs_0.1km%>%
  filter(dataset=="test")

Abs_test_df_sf <- absences_test %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(Abs_test_df_sf, cex=2,  zcol="cluster6")
mapview::mapview(Abs_test_df_sf, cex=2,  zcol="occ", col.regions= c("purple"))# a visual check



testing_dataset<- rbind(presences_test, absences_test)
testing_dataset_sf <- testing_dataset %>%
  st_as_sf(coords = c("x", "y"), crs = 3005)
mapview::mapview(testing_dataset_sf, cex=2,  zcol="occ", col.regions= c("blue","GREEN"))# a visual check
mapview::mapview(testing_dataset_sf, cex=2,  zcol="cluster6")# a visual check


testing_dataset%>%
  group_by(cluster6, occ)%>%
  summarize(n_records= length(occ))%>%
  ggplot(aes(x= cluster6, y= n_records, color=occ))+
  geom_point()+ theme_bw()


### Save testData2023_X_Cluster (18-09-2024) ================================================
# mypath= "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/csvs_to_SDMs/training_occ"
# write.csv(testing_dataset, paste(mypath, "testData_30percent2023_X_Cluster.csv", sep="/"))



###=============================================================================
### Check duplicates;  Filter one presence per pixel ===========================
df_raster<- as.data.frame(demp, xy = TRUE)
df_raster$pixel_ID<- seq(1, length(df_raster[,1]), 1)
head(df_raster)
demp_a<- rasterFromXYZ(df_raster) #[,c(1:2,4)]
crs(demp_a)<- crs(demp)
demp_a<- rast(demp_a)


abs2<- sdm_extract(
  data = filtabs_0.1km,
  x = "x",
  y = "y",
  env_layer = dem_a, # Raster with environmental variables
  variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
  filter_na = TRUE
)

which(duplicated(abs2$pixel_ID))# there are no duplicates!


# Exclude duplicated records in same cell by selecting presence over absence
abs2$occ<- 0
colnames(press2)[6]<- "aspect"
pres_abs<- rbind(press2, abs2)
pres_abs<- as.data.frame(pres_abs)
duplicates_exclude<- which(duplicated(pres_abs$pixel_ID) & pres_abs$occ== 0)
duplicates_exclude # There is no duplicated records!
pres_abs[which(duplicated(pres_abs$pixel_ID)),]

length(pres_abs[duplicates_exclude,"pixel_ID"] ) # 0 absences duplicated  

# pres_abs<- pres_abs[-duplicates_exclude,] 


# Separate absences again
filtabs_0.1km<- pres_abs[which(pres_abs$occ==0),]


### Create REPLICATE of randomly sampled absences =============================
rdom_abs_df<- data.frame(matrix(ncol=6, nrow = 0))
colnames(rdom_abs_df)<- c("cluster6","occ", "origin", "x","y", "rdomreplicate")

filtabs_0.1km_train<- filtabs_0.1km%>%
  filter(dataset=="train")


for (a in 1:50) {
  for (i in 2:6) {
    
    cluster_df<- filtabs_0.1km_train[which(filtabs_0.1km_train$cluster6 == i),]
    # n = ideal n - records from focal sites
    #--> focal sites have priority and are always included in the dataset
    n<- c(n_occs_perCluster[which(n_occs_perCluster$cluster6 == i), "n_train"])- n_abs_focal_cluster[which(n_abs_focal_cluster$cluster6==i), "n"]
    set.seed(a)
    rdom_abs_cl<- sample_n( cluster_df, size= as.numeric(n), replace=F)
    
    # merge with all Focal site records from the same cluster
    rdom_abs_cl<- rbind(rdom_abs_cl[,1:66], abs_focal_sites[which(abs_focal_sites$cluster6==i),1:66])
    rdom_abs_cl$rdomreplicate<- a
    rdom_abs_cl<- rdom_abs_cl[,c("cluster6", "origin", "occ", "x","y", "rdomreplicate")]
    rdom_abs_df<- rbind(rdom_abs_df, rdom_abs_cl)
    }
}

summary(as.factor(rdom_abs_df$cluster6))
summary(as.factor(rdom_abs_df$cluster6))/50
# n_abs_perCluster
n_occs_perCluster
colnames(rdom_abs_df)
colnames(rdom_occs_df)
unique(rdom_abs_df$rdomreplicate)



### MERGE PRESeNces AND ABSENCES datasets ========================================
rdom_pres_abs_df<- rbind(rdom_occs_df, rdom_abs_df)
colnames(rdom_pres_abs_df)
head(rdom_pres_abs_df)


# write.csv(rdom_pres_abs_df, paste(SDM_path,
#                                   "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_train_withoutShorelineRecords.csv",
#                                   sep="/"), row.names = F)


occ_data_Filtered_replicas<- read.csv( paste(SDM_path, "csvs_to_SDMs/datset_occ_cl_replicates/50_rdom_occs_abs_train_withoutShorelineRecords.csv",
                                        sep="/"))

length(occ_data_Filtered_replicas$x)
length(unique(occ_data_Filtered_replicas$x))


x<- occ_data_Filtered_replicas %>% group_by(occ, cluster6, rdomreplicate)%>% summarize(n= length(occ))
x%>% group_by(occ, cluster6)%>% dplyr::summarize(n= mean(n))
  # occ cluster6     n
  # 1     0        2    13
  # 2     0        3    14
  # 3     0        4    34
  # 4     0        5    22
  # 5     0        6     8
  # 6     1        2    13
  # 7     1        3    14
  # 8     1        4    34
  # 9     1        5    22
  # 10    1        6     8




training_pres_abs_unique<- occ_data_Filtered_replicas[-which(duplicated(occ_data_Filtered_replicas[,1:5])),]
length(training_pres_abs_unique$x)# 2160

training_pres_abs_unique%>% group_by(occ, cluster6)%>% summarize(n= length(occ))
is.na(training_pres_abs_unique)


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

occ_data_Filtered_replicas<- sdm_extract(data= occ_data_Filtered_replicas, 
             x = "x",
             y = "y",
             env_layer = stack_vars[[1]],
             variables = NULL,
             filter_na = FALSE)

summary(occ_data_Filtered_replicas$bathymetry)
boxplot(occ_data_Filtered_replicas$bathymetry)
ggplot(occ_data_Filtered_replicas, aes(x=as.factor(occ), y=bathymetry))+
  geom_violin()+ theme_bw()




####============================================================================
#  Load complete dataset, no filtered 
setwd(paste(SDM_path, "csvs_to_SDMs", sep="/"))
dir()
setwd(paste(SDM_path, "csvs_to_SDMs/occ_without_shorelineSurveys", sep="/"))
occ_data_noFiltered <- read.csv(paste(SDM_path, "csvs_to_SDMs/occ_without_shorelineSurveys/occs_ALLmerged_60Vars_3005_cluster.csv", sep="/")) # 23-07-2024

occ_data_noFiltered<- occ_data_noFiltered%>% filter(origin!="shoreline_surveys")
unique(occ_data_noFiltered$origin)

occ_data_noFiltered%>% group_by(occ, cluster6)%>% summarize(n= length(occ))

#### Bathymetric threshold corresponding to the 95% of data or quantile 0.95 === 
occ_data_Filtered_replicas%>%filter(occ==1)%>%summarize(quant9= quantile(bathymetry, 0.95))    # 15 m
occ_data_noFiltered%>%filter(occ==1)%>%summarize(quant9= quantile(bathymetry, 0.95))           # 13.92 m



testing_dataset<- read.csv(paste(mypath, "occ_without_shorelineSurveys/testData_30percent2023_X_Cluster.csv", sep="/"))

testing_dataset %>% group_by(occ, cluster6)%>% summarize(n= length(occ))
