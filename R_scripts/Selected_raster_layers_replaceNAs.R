

# Load terra
library(terra)
library(raster)

# Set your input folder
input_dir <- "/Volumes/Romina_BATI/BATI/SDMs/layers/aligned_rasters"
output_dir <- "/Volumes/Romina_BATI/BATI/SDMs/layers/FINAL_selected_Variables"

# List all tif files
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
tif_files

# View the names
basename(tif_files)
selected_variables<- c("eastherness.tif",
                       "janBT_ave.tif",
                       "julBS_ave.tif",
                       "julBSpd_max.tif",
                       "julBT_ave.tif",
                       "julBT_min.tif",
                       "julSSpd_min.tif",
                       "julST_ave.tif",
                       "slope.tif",
                       "tidal_cur.tif",
                       "TPI.tif")

# Selected variables only
tif_files<- tif_files[basename(tif_files) %in% selected_variables]

# upload bathymetry to mask layers
stack_vars<-  terra::rast(paste("/Volumes/Romina_BATI/BATI/SDMs", "variables_selection/stack_29FilteredVars_stratification_FINAL2.tif", sep="/"))
names(stack_vars)
bathy<- raster(stack_vars[[1]])
bathy_15<- bathy
bathy_15[bathy_15<= 15]<- 0  # convert values to "0" for areas shallower than 15 m
bathy_15[bathy_15> 15]<- 1   # convert values to "1" for areas deeper than 15 m


# Loop through and fix each file (replace NAs values by NAs, and save with final names in Paper)
for (f in tif_files) {
  # Read raster
  r <- raster(f)
  
  # Replace invalid values (-3.40282347e+38) with NA
  r[r == -3.40282347e+38] <- NA
  
  # Apply bathymetry mask
  # Keep only cells where bathy_15 is not NA
  r<- crop(r, bathy_15)
  r_masked <- mask(r, bathy_15)
  
  # Create output filename
  out_file <- file.path(output_dir, basename(f))
  
  # Write new raster
  writeRaster(r_masked, out_file, overwrite = TRUE)
}


tif_files <- list.files(output_dir, pattern = "\\.tif$", full.names = TRUE)
file<- stack(tif_files)
plot(file)
summary(file)

summary(bathy)
