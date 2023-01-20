#' @export generate_occ_uncertain
#'
#' @title Random latitude and longitude coordinates accounting uncertainty values
#'
#' @description
#' \code{generate_occ_uncertain} Given a data frame of georeferenced occurrences this function generates a new set of coordinates with added uncertainty. 
#' @details
#' \strong{Input} as a \code{dataframe} should have the following structure:
#' \tabular{ccccc}{ 
#' [,1] \tab ddlat \tab numeric, latitude (in decimal degrees)\cr 
#' [,2] \tab ddlon \tab numeric, longitude (in decimal degrees)\cr
#' [,3] \tab ddlat unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,4] \tab ddlon unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,5] \tab tax \tab character or factor, taxa names\cr}
#' \strong{It is mandatory to respect field positions, but field names do not
#' matter}
#' @param occs_df A \code{data.frame} of occurrence locations that includes
#'   \emph{at least these four columns} - latitude, longitude, latitude uncertainty and longitude uncertainty in degrees.
#' @param lat_col Name of column of latitude dbl values. Caps sensitive.
#' @param lon_col Name of column of longitude dbl values. Caps sensitive.
#' @param lat_uncertainty Name of column of latitude uncertainty in degree values. Caps sensitive.
#' @param lon_uncertainty Name of column of longitude uncertainty in degree values. Caps sensitive.
#' @param taxa_col Name of column of taxa (species) values. Caps sensitive.
#' @return random_dd A \code{data.frame} of a random latitude, random longitude and taxa name for each occurence record.

generate_occ_uncertain <-
  function(occs_df,
           lat_col = "latitude",
           lon_col = "longitude",
           lat_uncertainty = "latitude_uncertainty",
           lon_uncertainty = "longitude_uncertainty",
           taxa_col = "species"
  ){
  lat <- occs_df[[lat_col]]
  lon <- occs_df[[lon_col]]
  lat_uncertainty <- occs_df[[lat_uncertainty]]
  lon_uncertainty <- occs_df[[lon_uncertainty]]

  lat_random <- stats::runif(n = length(lat),
                      min = lat - lat_uncertainty, max = lat +
                        lat_uncertainty)
  lon_random <- stats::runif(n = length(lon),
                      min = lon - lon_uncertainty, max = lon +
                        lon_uncertainty)
  random_dd <- data.frame(lat_random = lat_random, lon_random = lon_random, tax = occs_df[[taxa_col]] )
  return(random_dd)
}

#' @examples 
#' #Usage of generate_ooc_uncertain for the differnt options for NA values to generate new data frames with uncertinaty
#' L_wiedii_generate_Mean <-generate_occ_uncertain(
#'  L_wiedii_uncertainty_naMean,
#'  lat_col = "latitude",
#'  lon_col = "longitude",
#'  lat_uncertainty = "lat_uncertainty",
#'  lon_uncertainty = "lon_uncertainty",
#'  taxa_col = "species")
#' 
#' L_wiedii_generate_NA <- generate_occ_uncertain(
#'  L_wiedii_uncertainty_naNA,
#'  lat_col = "latitude",
#'  lon_col = "longitude",
#'  lat_uncertainty = "lat_uncertainty",
#'  lon_uncertainty = "lon_uncertainty",
#'  taxa_col = "species")
#'  
#' L_wiedii_generate_Zero <- generate_occ_uncertain(
#'  L_wiedii_uncertainty_naZero,
#'  lat_col = "latitude",
#'  lon_col = "longitude",
#'  lat_uncertainty = "lat_uncertainty",
#'  lon_uncertainty = "lon_uncertainty",
#'  taxa_col = "species")
