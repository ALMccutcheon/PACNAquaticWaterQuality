# Title: Processing WQ Images from Fieldmaps Geodatabase
#
# Author: Amanda McCutcheon (amanda_l_mccutcheon@nps.gov)
# Date: May 23, 2024
#
# 1) Navigate to https://nps.maps.arcgis.com/home/item.html?id=20b20a767ffb4d608f292f5ba4619208. You may need to log in to nps.maps.arcgis.com.
# 2) Click "Export Data" in the menu on the right, and Select "Export to FGDB".
# 3) Give the file an appropriate title and export to a folder in your content. Click "Export". This may take several minutes.
# 4) Once exported, click "Download" and download the file, saving it in the "geodatabase" folder within this project.
# 5) It will download as a zip file. Extract the file by right clicking and selecting "Extract All". Save the extracted file to the "geodatabase" folder.
# 6) Rename the geodatabase to something like "YYYYMMDD_WQ_PACN_Field_Images.gdb"
# 7) Update the USER INPUT section in this script. 
# 8) Use the Source button above or highlight the whole script and press ctrl-Enter.
# 9) Contact Amanda if you get an error.
#
# NOTE: The first time you run this you will need to install the three packages listed.
# Removed the # from the next line to install the required packages.
# install.packages("here","dplyr","PACNAquaticWaterQuality")

#install.packages('C:/Users/sbierker/Aquatic_Photo_R_Files/PACNAquaticWaterQuality_1.1.0.tar 2.gz', lib='C:/Users/sbierker/AppData/Local/R/win-library/4.4', repos = NULL, type = 'source')

library(PACNAquaticWaterQuality)
library(here)
library(dplyr)

#setwd('C:/Users/sbierker/Aquatic_Photo_R_Files/PACN-Aquatic-PhotoProcessing/PACN-Aquatic-PhotoProcessing')

#### USER INPUT ####
location <- here("geodatabase") # Enter the folder name where the gdb is stored.
#'here' only works if the R file is opened through opening the R project
name <- "20250915_2024_PACN_WQ_Field_Images.gdb" # Enter the name of the gdb file.
layer <- "PACN_2024_Water_Quality_Points_Photos__v7_"
#layer <- "PACN_2025_Water_Quality_Points_Photos__v03_"# Enter the layer name - this should stay the same unless Mark updates the collection layer title. Use sf::st_layers(here(location, name)) to find layer name.

sf::st_layers(here(location, name))

# Enter Sampling Event Info
unit_code <- c("KALA")
loc_type <- "FW"
select_months <- as.vector(c((8)))
folder <- "watermarked/2024_KALA_test"


#------------------------------------------------------------------------------

#### RUN WATERMARK FUNCTION ####
# This should work as is. No need to update.

options(timeout = 1200)

process_watermark_wq(gdb_name=name,
                     gdb_location=location,
                     gdb_layer=layer,
                     park = unit_code, 
                     loctype = loc_type,
                     select_months = select_months,
                     output_folder = folder)

?process_watermark_wq

