## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F, include = FALSE-------------------------------------------
library(occUncertain)
data(Leopardus_wiedii_gbif)
# load("../data/Leopardus_wiedii_gbif.rda")
Leopardus_wiedii_gbif <- Leopardus_wiedii_gbif

## ----TableData,include=TRUE, echo=FALSE---------------------------------------
knitr::kable(head(Leopardus_wiedii_gbif))

## ----uncertainty from meter to radian degrees NA as 0, echo=FALSE-------------
L_wiedii_uncertainty_naZero <-
  meters_to_decdeg(occs_df = Leopardus_wiedii_gbif,
    lat_col = "latitude",
    lon_col = "longitude",
    distance = "coordinateUncertaintyInMeters",
    na_action = "NA as 0")

knitr::kable(head(L_wiedii_uncertainty_naZero))

## ----uncertainty from meter to radian degrees NA as NA, echo=FALSE------------
L_wiedii_uncertainty_naNA <-
  meters_to_decdeg(occs_df = Leopardus_wiedii_gbif,
    distance = "coordinateUncertaintyInMeters",
    na_action = "NA as NA")

knitr::kable(head(L_wiedii_uncertainty_naNA))

## ----uncertainty from meter to radian degrees NA as mean, echo=FALSE----------
L_wiedii_uncertainty_naMean <-
  meters_to_decdeg(occs_df = Leopardus_wiedii_gbif,
    distance = "coordinateUncertaintyInMeters",
    na_action = "NA as mean")

knitr::kable(head(L_wiedii_uncertainty_naMean))

## ---- echo=FALSE--------------------------------------------------------------

L_wiedii_uncertainty_naZero <-
  cbind(Leopardus_wiedii_gbif, L_wiedii_uncertainty_naZero)

L_wiedii_generate_Zero <-
  generate_occ_uncertain(
    L_wiedii_uncertainty_naZero,
    lat_col = "latitude",
    lon_col = "longitude",
    lat_uncertainty = "lat_uncertainty",
    lon_uncertainty = "lon_uncertainty",
    taxa_col = "species"
  )

knitr::kable(head(L_wiedii_generate_Zero))

## ---- echo=FALSE--------------------------------------------------------------
L_wiedii_uncertainty_naNA <-
  cbind(Leopardus_wiedii_gbif, L_wiedii_uncertainty_naNA)

L_wiedii_uncertainty_naNA <-
  dplyr::select(
    L_wiedii_uncertainty_naNA,
    latitude,
    longitude,
    lon_uncertainty,
    lat_uncertainty,
    species
  )
#filter !is.NA lat and lon uncertinaty 
L_wiedii_uncertainty_naNA <- dplyr::filter(L_wiedii_uncertainty_naNA, !is.na(lon_uncertainty))

L_wiedii_generate_NA <-
  generate_occ_uncertain(
    L_wiedii_uncertainty_naNA,
    lat_col = "latitude",
    lon_col = "longitude",
    lat_uncertainty = "lat_uncertainty",
    lon_uncertainty = "lon_uncertainty",
    taxa_col = "species"
  )

knitr::kable(head(L_wiedii_generate_NA))

## ----NA as mean, include=FALSE------------------------------------------------
#combine observed and prodataset with radian 
L_wiedii_uncertainty_naMean <-
  cbind(Leopardus_wiedii_gbif, L_wiedii_uncertainty_naMean)

L_wiedii_uncertainty_naMean <-
  dplyr::select(
    L_wiedii_uncertainty_naMean,
    latitude,
    longitude,
    lon_uncertainty,
    lat_uncertainty,
    species
  )

knitr::kable(head(L_wiedii_uncertainty_naMean))

## ----generate random occ, echo=FALSE------------------------------------------
L_wiedii_generate_Mean <-
  generate_occ_uncertain(
    L_wiedii_uncertainty_naMean,
    lat_col = "latitude",
    lon_col = "longitude",
    lat_uncertainty = "lat_uncertainty",
    lon_uncertainty = "lon_uncertainty",
    taxa_col = "species"
  )

knitr::kable(head(L_wiedii_generate_Mean))

## ----eval=FALSE, dev.args=list(pointsize=2), include=FALSE, list(pointsize=2)----
#  #You can add easily get a map of countries using the package [rnaturalearth](https://CRAN.R-project.org/package=rnaturalearth):
#  land <-
#    rnaturalearth::ne_countries(scale = 50, returnclass = "sp")

## ----echo=FALSE, fig.width = 10, fig.height= 10-------------------------------
library(ggplot2)
data(land)
map <- ggplot2::map_data("world")

ggplot2::theme_set(theme_bw())

#observed data
ggplot2::ggplot(
  data = Leopardus_wiedii_gbif,
  aes(x = longitude, y = latitude,
  colour = "Observed occ"),
  size = 0.1,
  fill = NA,
  shape = 1,
  alpha = 0.2
) +
  
  
  geom_polygon(
    data = map,
    aes(x = long, y = lat, group = group),
    fill = NA,
    colour = "grey"
  ) +
  coord_fixed(xlim = c(-85,-70), ylim = c(-5, 10)) +
  
  #different NA_option results
  geom_point() +
   geom_point(
    data = L_wiedii_generate_Mean,
    aes(x = lon_random, y = lat_random,
    colour = "NA as Mean"),
    #size = 0.01,
    fill = NA,
    shape = 1,
    alpha = 0.5
  ) +
  geom_point(
    data = L_wiedii_generate_Zero,
    aes(x = lon_random, y = lat_random,
    colour = "NA as Zero"),
    shape = 1,
        #size = 1,
    fill = NA,
    alpha = 0.5
  ) +
  geom_point(
    data = L_wiedii_generate_NA,
    aes(x = lon_random, y = lat_random,
    colour = "NA as NA"),
    #size = 0.01,
    fill = NA,
    shape = 1,
    alpha = 0.5
  ) +
  scale_colour_manual(
    "",
    values = c(
      "Observed occ" = "yellow",
      "NA as Mean" = "green",
      "NA as Zero" = "blue",
      "NA as NA" = "red"
    )
  ) +
  
  labs(title = "L. wiedii observed and generated occurrences", subtitle = "Generated occurrences with meters to decimal degree uncertainty NA_action parameters "
    )



## ----file, eval=FALSE, include=FALSE------------------------------------------
#  L_weidii_random_geo_range <- random_geo_range(
#    n_length = 10,
#    occs_df = L_wiedii_uncertainty_naMean,
#    lat_col = "latitude",
#    lon_col = "longitude",
#    lon_uncertainty = "lon_uncertainty",
#    lat_uncertainty = "lat_uncertainty",
#    taxa_col = "species"
#  )

## ----eval=FALSE, include=FALSE------------------------------------------------
#  knitr::kable(head(L_weidii_random_geo_range))

## ----EOO, eval=FALSE, include=FALSE-------------------------------------------
#  ## 5 Computing from observed dataset with ConR
#  library(ConR)
#  data(land)
#  L_weidii_EOO <-
#    EOO.computing(
#      Leopardus_wiedii_random,
#      exclude.area = T,
#      country_map = land,
#      write_results = F
#    )
#  
#  L_weidii_AOO <- AOO.computing(Leopardus_wiedii_random)

## ----IUCN, eval=FALSE, include=FALSE------------------------------------------
#  ## 6.4 Runing IUCN.eval function## 6.4 Runing IUCN.eval function
#  L_weidii_IUCN.eval <- IUCN.eval(Leopardus_wiedii_random,
#    country_map = land,
#    write_results = F)

