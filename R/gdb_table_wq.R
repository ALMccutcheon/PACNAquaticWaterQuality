#' Make a table from Geodatabase for photo processing
#'
#' Makes a table from the specified geodatabase in the format that is required by the watermark_wq function
#'
#' @param gdb_name the name of the geodatabase
#' @param gdb_location the folder the geodatabase is store in. Typically "geodatabase".
#' @param gdb_layer the name of the layer in the geodatabase. This appears to be the file description used for download.
#' @return returns a table with all the observations in the geodatabase and 30 columns
#' @export

gdb_table_wq <-
function(gdb_name, gdb_location, gdb_layer){

  # Create path for gdb from location and name
  gdb_path <- paste0(gdb_location, "/", gdb_name)

  # Layer File with attributes
  attributes <- sf::read_sf(gdb_path, gdb_layer)

  # Attachments Table containing photos
  layer_attach <- paste0(gdb_layer,"__ATTACH")
  attachments <- sf::read_sf(gdb_path, layer_attach)


# Join the layer data with the _attach table
joined_table <- attachments %>%
  dplyr::left_join(attributes, by = c("REL_GLOBALID" = "GlobalID"))

# Apply function to get coordinates for the joined_table
df <- joined_table$SHAPE
coords<- sf::st_coordinates(df)

# make the time zone for created_date HST
lubridate::tz(joined_table$created_date)<-"Pacific/Honolulu"

# Update created_date to correct time zone
date_table <- joined_table %>%
  dplyr::mutate(Time_Zone = case_when(unit_code%in%c("AMME","WAPA") ~ "Pacific/Guam",
                                      unit_code%in%c("KAHO","ALKA","PUHO","PUHE","HALE","KALA","HAVO") ~ "Pacific/Honolulu",
                                      unit_code%in%c("NPSA") ~ "Pacific/Samoa",
                                      TRUE ~ NA))%>%
  dplyr::rowwise()%>%
  dplyr::do(created_date = lubridate::force_tz(lubridate::with_tz(.$created_date,tzone=.$Time_Zone)),tzone="UTC")%>%
  dplyr::ungroup()

joined_table$created_date <-as.POSIXct(unlist(date_table$created_date))

# Make a date_time column appropriate for file names
joined_table <- joined_table %>%
  dplyr::mutate(date_time_photo = as.character(created_date)) %>%
  dplyr::mutate(date_time_file = lubridate::date(created_date))%>%
  dplyr::mutate(date_time_file = stringr::str_replace_all(date_time_file,"-",""),
                transect = ifelse(is.na(transect),"WQ",transect))%>% #replace NA in Location_Name with a space
  cbind(coords)%>% #add the x,y,z coordinates
  dplyr::mutate(hash = stringr::str_c(date_time_file,station_id,photo_subject,transect)) %>% #creates a field called hash which has the fields that will be in filename
  dplyr::group_by(hash) %>%
  dplyr::mutate(duplication_id = seq(n())-1) %>% #checks for duplication of the filename hash field and add a sequence number for duplicates
  dplyr::ungroup ()%>%
  dplyr::mutate(tag = ifelse(duplication_id==0,"",paste0("_",duplication_id)), #replaces duplication id of zero with nothing
         Location_Name = ifelse(is.na(Location_Name)," ",Location_Name),
         Location_Type = ifelse(is.na(Location_Type)," ",Location_Type))

return(joined_table)
}
