source("obis_api.R")

# -- Summary stats for a species
obis_statistics(scientificname = "Caretta caretta")
# $records: 168590  $species: 1  $datasets: 433  $yearrange: 1758 2026

# -- Occurrence records (single page)
occ <- obis_occurrences(
  scientificname = "Tursiops truncatus",
  size           = 100,
  fields         = c("scientificName", "decimalLatitude", "decimalLongitude", "date_year")
)
cat("Total available:", attr(occ, "total"), "\n")
head(occ)

# -- Spatial filter: Gulf of Mexico bounding box (WKT polygon, lon lat order)
gulf_wkt <- "POLYGON((-97 18, -80 18, -80 31, -97 31, -97 18))"
gulf_occ <- obis_occurrences(geometry = gulf_wkt, size = 200)
cat("Records in Gulf of Mexico:", attr(gulf_occ, "total"), "\n")

# -- Species checklist for an area
gulf_list <- obis_checklist(geometry = gulf_wkt, size = 50)
head(gulf_list[, c("scientificName", "records", "family")])

# -- What species are in a named IHO area? First get the area ID:
areas <- obis_areas(type = "iho")
gulf_id <- areas$id[areas$name == "Gulf of Mexico"]
gulf_species <- obis_checklist(areaid = gulf_id, size = 100)
nrow(gulf_species)

# -- Annual record trend (useful for data quality assessment)
trend <- obis_stats_years(scientificname = "Caretta caretta")
plot(
  trend$year, trend$records,
  type = "l", xlab = "Year", ylab = "Records",
  main = "Loggerhead sea turtle OBIS records per year"
)

# -- Paginate through all records (use filters — unfiltered = 200M+ rows)
all_occ <- obis_occurrences_all(
  scientificname = "Caretta caretta",
  geometry       = gulf_wkt,
  startdate      = "2000-01-01",
  max_records    = 5000,
  page_size      = 1000
)
nrow(all_occ)

# -- Datasets containing a species
ds <- obis_datasets(scientificname = "Caretta caretta", size = 10)
print(ds[, c("title")])
