library(tidyverse)
library(sf)
library(lwgeom)
library(mapview)

event <- read.csv('dwc/event.csv')

tmp <- event |>
  dplyr::filter(is.na(decimalLongitude)) |>
  mutate(
    transect = gsub('^.*event\\:(.*)\\:\\d.*$', '\\1', eventID),
    transect = gsub('\\:.*$', '', transect)) |>
  select(transect) |>
  summarise(cnt = n(), .by = transect)

locs_raw <- st_read('T:/05_GIS/SEAGRASS_TRANSECTS/transect_routes.shp') |>
  filter(Site %in% tmp$transect)

# bearing from projected coords, start lat/lon from EPSG 4326
start_proj  <- st_coordinates(st_startpoint(locs_raw))
end_proj    <- st_coordinates(st_endpoint(locs_raw))
start_4326  <- st_startpoint(locs_raw) |> st_transform(4326) |> st_coordinates()

locs <- locs_raw |>
  mutate(
    bearing   = atan2(end_proj[, "X"] - start_proj[, "X"],
                      end_proj[, "Y"] - start_proj[, "Y"]) * 180 / pi,
    longitude = start_4326[, "X"],
    latitude  = start_4326[, "Y"]
  )

strlocs <- locs |> 
  select(Site, longitude, latitude, bearing) |>
  st_drop_geometry() |>
  rename(transect = Site) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# mapview(locs) + mapview(strlocs)

save(strlocs, file = 'data/strlocs.rda', compress = 'xz')
