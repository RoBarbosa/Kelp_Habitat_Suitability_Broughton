###==================================================================
### Correlate the simulated temperature with the Ladsat Temp    #####
### by Bianucci                                                 #####
###                                                             #####
### Author: Romina Barbosa                                      #####
### Date: 16-Nov-2023                                ################
###==================================================================
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(readxl)
library(sf)
library(tidyverse)
library(raster)
library(terra)
library(mapview)

### Load Landsat everage temperature during 2023 and extract values at mooring sites ============
# Landsat images downloaded with EPSG:4269
coordinates<- read.csv("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/Mooring_sites/Mooring_general_info_list.csv")
colnames(coordinates)[1]<- "site"
coordinates<- coordinates[, c(1:3,6,7)]
head(coordinates)

path_landsat_r<-("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/layers/LST_masked")
setwd(path_landsat_r)
dir()

LST_list<- list.files(path_landsat_r, pattern = ".tif")
# LST_layers<- stack(LST_list[c(1,4)])
# 
# coordinates(coordinates)= ~ lon + lat
# 
# # Extract raster value by points
# lstValue= raster::extract(LST_layers, coordinates)
# 
# # Combine raster values with point and save as a CSV file.
# combinePointlstValue= cbind(coordinates, lstValue)
# 
# # write.csv(combinePointlstValue, "combinePoint_LSTValue_atmoorings.csv")
# 
# as.data.frame(combinePointlstValue)


# Create random points and extract LST values ==================================
LSTp<- raster(LST_list[1])
# LSTp<- projectRaster(LSTp, crs= ("+proj=longlat +datum=NAD83"))

plot(LSTp)


# library(terra)
# LST<- rast(LST_list[1])
# rdn_pts<- terra::spatSample(LST, 10000, "random", cells=TRUE, xy=TRUE, values=FALSE)


# which cells are not NA? These ones:
notna <- which(!is.na(values(LSTp)))

# grab 20 cell index numbers at random
samp <- sample(notna, 1000, replace = FALSE)

# and their values
sampdata <- LSTp[samp]

# and their location coordinates
samplocs <- xyFromCell(LSTp, samp)

# convert to a data frame
samp <- as.data.frame(cbind(samplocs, samp, sampdata))
names(samp) <- c('x', 'y', 'index', 'LST_July_ave2023')

# and optionally, a spatial object, in sf or sp
library(sp)
samp[samp$x <= -126.629771 & samp$y >= 50.986862, "LST_July_ave2023"]<- NA
samp[which(samp$LST_July_ave2023 == 0),"LST_July_ave2023"]<- NA
samp<- samp[!is.na(samp$LST_July_ave2023),]

samp_sp <- samp
coordinates(samp_sp) <- ~x + y
crs(samp_sp) <- CRS('+init=epsg:4269')


library(sf)
samp_sf <- st_as_sf(as.data.frame(samp), coords = c('x', 'y'), crs = 4269)


# plot(samp_sf["value"], axes = TRUE, key.pos = 4, key.width = lcm(1.5))

library(mapview)
mapviewOptions(fgb = FALSE) # needed when creating web pages
mapview(samp_sf["LST_July_ave2023"], col.regions = sf.colors(10), fgb = FALSE, cex= 2)



# Extract values from simulated metrics ========================================
path_rasters<- ("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/Environmental_cond/model_Laura_output_layers/create_raster_LauraOutputs")
setwd(paste(path_rasters))
dir()

raster_list<- list.files(path_rasters, pattern = ".tif")

r1<- raster(paste(path_rasters, "jul19_TSurface_ave.tif", sep="/"))

# Extract raster value by points
simValue= raster::extract(r1, samp_sp)

# add raster 2 max july 2023
r2<- raster(paste(path_rasters, "jul19_TSurface_max.tif", sep="/"))
simValuemax<- raster::extract(r2, samp_sp)

# Combine raster values with point and save as a CSV file.
combinePointValue= cbind(samp_sp, simValue, simValuemax)
as.data.frame(combinePointValue)

names(combinePointValue)[3]<- "sim_July_ave_2019"
names(combinePointValue)[4]<- "sim_July_max_2019"


# Add polygon of study area and mask points to restrict the area
study_area_poly<- st_read("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/layers/study_area_broughton.shp")

combinePointValue<- crop(combinePointValue, study_area_poly)

# standardize values to compare spatial patterns
# combinePointValue$scaled_LST_ave2022<- scale(combinePointValue$LST_July_ave2022)
# combinePointValue$scaled_sim_ave2019<- scale(combinePointValue$sim_July_ave_2019)

# combinePointValue$diff_sim_obs<- combinePointValue$scaled_LST_ave2022 - combinePointValue$scaled_sim_ave2019

# summary(combinePointValue$diff_sim_obs)

# samp_sf2 <- st_as_sf(as.data.frame(combinePointValue), coords = c('x', 'y'), crs = 4269)
# names(samp_sf2)
# 
# samp_sf2$diff_sim_obs<- as.numeric(samp_sf2$diff_sim_obs)
# 
# mapviewOptions(fgb = FALSE) # needed when creating web pages
# mapview(samp_sf2["diff_sim_obs"], col.regions = sf.colors(10), fgb = FALSE, cex= 3, layer.name = "diff_sim_obs")+
# mapview(study_area_poly, col.regions= "lightgrey", alpha.regions = 0.8, legend= F)


# Another efficient way of Normalizing values is through the Min-Max Scaling method.
# With Min-Max Scaling, we scale the data values between a range of 0 to 1 only. 
# Due to this, the effect of outliers on the data values suppresses to a certain extent. 
# Moreover, it helps us have a smaller value of the standard deviation of the data scale.
# https://www.digitalocean.com/community/tutorials/normalize-data-in-r


# In Standard scaling, also known as Standardization of values,
# we scale the data values such that the overall statistical summary of every 
# variable has a mean value of zero and an unit variance value.

combinePointValue$normal_LST_ave2023<- (combinePointValue$LST_July_ave2023 - min(combinePointValue$LST_July_ave2023, na.rm=TRUE))/ (max(combinePointValue$LST_July_ave2023, na.rm=TRUE)- min(combinePointValue$LST_July_ave2023, na.rm=TRUE))
combinePointValue$normal_sim_ave2019<-  (combinePointValue$sim_July_ave_2019 - min(combinePointValue$sim_July_ave_2019, na.rm=TRUE))/ (max(combinePointValue$sim_July_ave_2019, na.rm=TRUE)- min(combinePointValue$sim_July_ave_2019, na.rm=TRUE))
combinePointValue$normal_sim_max2019<-  (combinePointValue$sim_July_max_2019 - min(combinePointValue$sim_July_max_2019, na.rm=TRUE))/ (max(combinePointValue$sim_July_max_2019, na.rm=TRUE)- min(combinePointValue$sim_July_max_2019, na.rm=TRUE))


combinePointValue$diff_norm_julAve<- combinePointValue$normal_sim_ave2019 - combinePointValue$normal_LST_ave2023
combinePointValue$diff_norm_julMax<- combinePointValue$normal_sim_max2019 - combinePointValue$normal_LST_ave2023


samp_sf2 <- st_as_sf(as.data.frame(combinePointValue), coords = c('x', 'y'), crs = 4269)
names(samp_sf2)

samp_sf2$diff_norm_julAve<- as.numeric(abs(samp_sf2$diff_norm_julAve))
samp_sf2$diff_norm_julMax<- as.numeric(abs(samp_sf2$diff_norm_julMax))


setwd("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Plots")
library(utils)
mapviewOptions(fgb = FALSE) # needed when creating web pages
m<- mapview(samp_sf2["diff_norm_julAve"], col.regions = sf.colors(10), fgb = FALSE, cex= 3, layer.name = "diff_ave sim_ave obs")+
mapview(study_area_poly, col.regions= "lightgrey", alpha.regions = 0.8, legend= F)

library(webshot)
# webshot::install_phantomjs()

png_fl = tempfile(fileext = "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/correlation_LST_simJuly.png")

## create standalone .png; temporary .html is removed automatically unless
## 'remove_url = FALSE' is specified
mapshot(m, file = png_fl)
browseURL(png_fl)
mapshot(m, file = png_fl,
        remove_controls = c("homeButton", "layersControl"))
browseURL(png_fl)


m2<- mapview(samp_sf2["diff_norm_julMax"], col.regions = sf.colors(10), fgb = FALSE, cex= 3, layer.name = "diff_max sim_ ave obs")+
  mapview(study_area_poly, col.regions= "lightgrey", alpha.regions = 0.8, legend= F)

png_fl2<- tempfile(fileext = "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/correlation_LST_maxsimJuly.png")

mapshot(m2, file = png_fl2,
        remove_controls = c("homeButton", "layersControl"))
browseURL(png_fl2)

## create standalone .png; temporary .html is removed automatically unless
## 'remove_url = FALSE' is specified
mapshot(m, file = png_fl)

mapshot(m, file = png_fl,
        remove_controls = c("homeButton", "layersControl"))

library(cowplot) # it allows you to save figures in .png file
library(smplot2)
library(ggpmisc)
cor(combinePointValue$normal_sim_ave2019, combinePointValue$normal_LST_ave2023)

cor(combinePointValue$normal_sim_max2019, combinePointValue$normal_LST_ave2023)
cor(combinePointValue$normal_sim_max2019, combinePointValue$normal_sim_ave2019)# average and maximum SST spatial patterns are highly correlated

plot(combinePointValue$normal_sim_ave2019, combinePointValue$normal_LST_ave2023)


as.data.frame(combinePointValue)%>%
  summarize(length(normal_sim_ave2019))

combinePointValuedf<- as.data.frame(combinePointValue)
length(combinePointValuedf[!is.na(combinePointValuedf$normal_sim_ave2019), "normal_sim_ave2019"])# 380 random points 
# write.csv(combinePointValuedf, "combinePointValuedf_correlationsPlot.csv")  



combinePointValuedf<- read.csv("combinePointValuedf_correlationsPlot.csv")  
# LST_July_ave2023 sim_July_ave_2019 sim_July_max_2019 normal_LST_ave2023 normal_sim_ave2019 normal_sim_max2019 diff_norm_julAve diff_norm_julMax

p1<- as.data.frame(combinePointValue)%>%
  ggplot(aes(x= normal_sim_ave2019, y= normal_LST_ave2023, color= x))+
           geom_point()+
  geom_smooth(method="lm")+
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size=1)+
  stat_poly_line(color= "blue") +
  annotate("text", x=0.25, y=0.95, label= paste("Pearson Correlation:", round(cor(combinePointValue$normal_sim_ave2019, combinePointValue$normal_LST_ave2023), 2)))+
  # stat_poly_eq(use_label(c("R2"))) +
  # sm_statCorr(color = '#0f993d', corr_method = 'pearson')+
  labs(x= "Average SST (°C); simulations, July 2019", y= "Average LST (°C); observations, July 2023", color= "Longitud (°)")+
  theme_bw()+ theme(legend.position= "none")
  # sm_corr_theme()
  
p2<- as.data.frame(combinePointValue)%>%
  ggplot(aes(x= normal_sim_max2019, y= normal_LST_ave2023, color= x))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size=1)+
  stat_poly_line(color= "blue") +
  annotate("text", x=0.25, y=0.95, label= paste("Pearson Correlation:", round(cor(combinePointValue$normal_sim_max2019, combinePointValue$normal_LST_ave2023), 2)))+
  labs(x= "Maximum SST (°C); simulations, July 2019", y= "Average LST (°C); observations, July 2023", color= "Longitud (°)")+
  theme_bw()+ theme(legend.position= "none")
     
p3<- as.data.frame(combinePointValue)%>%
  ggplot(aes(y= normal_sim_max2019, x= normal_sim_ave2019, color= x))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size=1)+
  stat_poly_line(color= "blue") +
  annotate("text", x=0.25, y=0.95, label= paste("Pearson Correlation:", round(cor(combinePointValue$normal_sim_max2019, combinePointValue$normal_sim_ave2019), 2)))+
  labs(y= "Maximum SST (°C); simulations, July 2019", x= "Average SST (°C); simulations, July 2019", color= "Longitud (°)")+
  theme_bw()+ theme(legend.position= c(0.8, 0.3))

cowplot::plot_grid(p1, p2, p3, ncol=3)
# ggsave("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/Plots/correlations_LST_SSTsimulated_july_FINAL.png",
#        height = 11, width = 31, units="cm", dpi= 300)
