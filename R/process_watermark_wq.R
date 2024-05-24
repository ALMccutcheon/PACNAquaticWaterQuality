#' Process and Watermark WQ photos
#'
#' Combines the gdb_table_wq and watermark_wq functions. Allowing for the user to select the park, location type, and month of sampling
#'
#' @param gdb_name the name of the geodatabase
#' @param gdb_location the folder the geodatabase is store in. Typically "geodatabase".
#' @param gdb_layer the name of the layer in the geodatabase. This appears to be the file description used for download.
#' @param park the unit code desired
#' @param select_months a list of months you want processed
#' @param output_folder where you want the photos to go. Default is "watermarked". A new folder will be created if it does not exist.
#' @param loctype location type for photos to be processed. AP, MR, BB, FW.
#' @return returns a table with all the observations in the geodatabase and 30 columns
#' @export

process_watermark_wq <-
function(gdb_name, gdb_location, gdb_layer,park,loctype,
         select_months=c(1,2,3,4,5,6,7,8,9,10,11,12),output_folder="watermarked"){

  t <-gdb_table_wq(gdb_name, gdb_location, gdb_layer)

  t_select <- t%>%
    dplyr::filter(unit_code==park,Location_Type==loctype)%>%
    dplyr::mutate(file_month = lubridate::month(CreationDate))%>% #create a month field to select on later
    dplyr::filter(file_month%in%select_months)

  apply(X = t_select, MARGIN = 1, FUN = watermark_wq, new_folder = output_folder)

}
