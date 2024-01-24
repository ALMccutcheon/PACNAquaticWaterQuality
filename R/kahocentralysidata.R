#' KAHO Anchialine Pools Central Group YSI Data
#'
#' This is the certified data set for KAHO central pool group for 2008-2017. This is an example of the correct format for ysi data.
#' @format There are 19 variables but not all are necessary to run the graphing functions.
#' \describe{
#' \item{unit_code}{character code for park name}
#' \item{start_date}{date of sampling event MM-DD-YYYY format}
#' \item{year_quarter}{numeric value for year and quarter}
#' \item{year}{numeric year}
#' \item{quarter}{quarter value}
#' \item{station_id}{site name}
#' \item{station_type}{fixed or temporary}
#' \item{loc_type}{Resource category}
#' \item{pool_group}{Anchialine pool group if applicable}
#' \item{loc_name}{Location name}
#' \item{parameter}{short parameter name}
#' \item{units}{units for parameter values}
#' \item{depth}{relative depth of sampling}
#' \item{colocation}{colocated protocol name if applicable}
#' \item{qa_flag}{0 or 1 for flag applied}
#' \item{plume}{harbor plume or no}
#' \item{n}{number of values that were averaged}
#' \item{corrected_mean}{mean value for parameter}
#' \item{sd}{standard deviation of the n samples around mean_value}
#' }
#' @examples
#' kahocentralysidata
"kahocentralysidata"
