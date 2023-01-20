#' @export meters_to_decdeg
#'
#' @title Convert from meters to degrees correcting for global position
#'
#' @description
#' \code{meters_to_decdeg} converts from meters to degrees at a specified
#' position on the globe. The use case this function was developed for was to
#' calculate occurence point uncertainty values, which are usually reported in
#' meters, as degrees.
#'
#' The formula for converting from meters to decimal degrees is in part
#' based on information from the ESRI ArcUser magazine at this site
#' \url{https://www.esri.com/news/arcuser/0400/wdside.html}
#'
#' @param occs_df A \code{data.frame} of occurrence locations that incudes
#'   \emph{at least these three columns} - latitude, longitude, and a distance
#'   in meters to be converted to decimal degrees.
#' @param lat_col Name of column of latitude values. Caps sensitive.
#' @param lon_col Name of column of longitude values. Caps sensitive.
#' @param distance Name of column of distance values, in meters. Caps sensitive.
#' @param na_action Enact distance options for NA values. Caps sensitive
#' @return dist_dd A \code{data.frame} of latitude and longitude distances in
#'   units of degree decimal.
#'
meters_to_decdeg <- function(occs_df, lat_col = "latitude",
                             lon_col = "longitude", distance, na_action = "NA as 0") {
  lat <- occs_df[[lat_col]]
  lon <- occs_df[[lon_col]]
  dist <- occs_df[[distance]]
  #enact distance options for NA
  # treat NA as 0
  # treat NA as mean dist
  # treat NA as NA
  if (na_action == "NA as 0") {
    dist <- ifelse (is.na(dist), yes = 0, no = dist )
  } else if (na_action == "NA as mean") {
    dist <- ifelse (is.na(dist), yes = mean(dist, na.rm = TRUE), no = dist )
  } else if (na_action == "NA as NA" ) {
    dist = dist
  } else {
    warning("Incorrect na_action chosen")
    return(0)
  }

  #each degree the radius line of the Earth corresponds to 111139 meters.
  lat_uncertainty <- dist/111325
  #at the equator, longitude approx equals latitude
  #decrease in a trigonometric cosine-based fashion as one moves toward the earth's poles
  lon_uncertainty <- dist / (111325  * cos(lat * (pi / 180)))
  dist_dd <- data.frame(lon_uncertainty = lon_uncertainty,
                        lat_uncertainty = lat_uncertainty)
  return(dist_dd)
}

#' @examples
#' #Create sample data frame for 4 occurrences and their uncertinaty in meters for L. Wiedii
#' df <- data.frame (latitude = as.numeric(c("-12.660784","-30.397000","-30.225833","-28.938616")), 
#'                  longitude = as.numeric(c("-68.75897","-51.45458","-54.75972","-53.35768")), 
#'                  coordinateUncertaintyInMeters = as.numeric(c("31069", NA, NA,"29558")), 
#'                  species =("Leopardus wiedii"))
#' 
#' # Usage of meters_to_decdeg with NA as Zero
#'L_wiedii_uncertainty_naZero <-meters_to_decdeg(occs_df = df, 
#'                                               lat_col = "latitude", 
#'                                               lon_col = "longitude", 
#'                                               distance = "coordinateUncertaintyInMeters", 
#'                                               na_action = "NA as 0")
#'                                               
#' # Usage of meters_to_decdeg with NA as NA
#'L_wiedii_uncertainty_naNA <-meters_to_decdeg(occs_df = df, 
#'                                               lat_col = "latitude", 
#'                                               lon_col = "longitude", 
#'                                               distance = "coordinateUncertaintyInMeters", 
#'                                               na_action = "NA as NA")
#' 
#' # Usage of meters_to_decdeg with NA as Mean
#'L_wiedii_uncertainty_naMean <-meters_to_decdeg(occs_df = df, 
#'                                               lat_col = "latitude", 
#'                                               lon_col = "longitude", 
#'                                               distance = "coordinateUncertaintyInMeters", 
#'                                               na_action = "NA as mean")                                                                                            
