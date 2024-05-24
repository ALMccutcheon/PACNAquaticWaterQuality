#' Watermark water quality photos
#'
#' Takes a table including photos in "DATA" and processes the photos.
#' It adds watermarks to the photos in the format required for PACN water quality.
#'
#' @param x the table created from the geodatabase. Typically the output of gdb_table_wq().
#' @param new_folder the folder name you'd like the watermarked photos to go
#' @return photos are processed and put in the new_folder, nothing is returned
#' @export

watermark_wq <-
function(x, new_folder) {
  # Get watermarking info from the table (x)
  p.dt_photo <- x["date_time_photo"]
  p.title<-paste(x["unit_code"],"Water Quality Monitoring",sep=" ")
  p.direction<- x["photo_subject"]
  p.locname<-x["Location_Name"]
  p.type<-x["Location_Type"]
  p.site <- x["station_id"]
  p.user <- x["Editor"]
  p.tag <- x["tag"]
  p.filename<-paste(x["date_time_file"],p.site,"WQ",p.type,p.direction,sep="_")
  p.filename.tag<-paste0(p.filename,p.tag) #added tag separately because it already includes the "_"
  p.lat <- x$Y
  p.lat <- round(p.lat,6)
  p.long <- x$X
  p.long <- round(p.long,6)

  # Create paths and folders to save each photo
  dir.create(here::here(new_folder), recursive = TRUE, showWarnings = FALSE )
  out.path <- here::here(new_folder)
  out.name <- file.path(out.path, paste0(p.filename.tag,".jpg"))
  print(out.name)

  # Load photo
  image.x <- x["DATA"] %>%
    purrr::pluck(1)


  img.x <- magick::image_read(image.x)

  # Apply auto-orientation "image_orient()" which tries to infer the correct orientation
  # from the Exif data.
  img.x2 <- magick::image_orient(img.x)


  # ---- Watermark photo -----

  # northwest corner
  nw <- dplyr::case_when(p.type=="FW" ~ paste(p.title,p.locname,sep="\n"),
                  p.type=="MR" ~ paste(p.title,p.locname,sep="\n"),
                  p.type=="AP" ~ paste(p.title),
                  p.type=="BB" ~ paste(p.title,p.locname,sep="\n"),
                  TRUE ~ NA)
  img.x2 <- magick::image_annotate(img.x2, nw,
                          size = 25,
                          gravity = "northwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # top center
  n <- paste(p.direction)
  img.x2 <- magick::image_annotate(img.x2, n,
                           size = 25,
                           gravity = "north",
                           font = "Helvetica",
                           color = "white",
                           strokecolor = "black",
                           weight = 900)

  # northeast corner
  ne <- dplyr::case_when(p.type=="FW" ~ paste(p.site),
                  p.type=="MR" ~ paste(p.site),
                  p.type=="AP" ~ paste(p.site,p.locname,sep="\n"),
                  p.type=="BB" ~ paste(p.site),
                  TRUE ~ NA)
  img.x2 <- magick::image_annotate(img.x2, ne,
                          size = 25,
                          gravity = "northeast",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # southwest corner
  sw <- paste(p.dt_photo)
  img.x2 <- magick::image_annotate(img.x2, sw,
                          size = 25,
                          gravity = "southwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)

  # southeast corner
  se <- paste(p.lat,p.long,sep="\n")
  img.x2 <- magick::image_annotate(img.x2, se,
                           size = 25,
                           gravity = "southeast",
                           font = "Helvetica",
                           color = "white",
                           strokecolor = "black",
                           weight = 900)

  # Save photo
  magick::image_write(img.x2, path = out.name, format = "jpg")

}
