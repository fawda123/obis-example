# OBIS API client — retrieval only
# Requires: httr2, jsonlite
# Install: install.packages(c("httr2", "jsonlite"))
#
# All endpoints target https://api.obis.org/v3
# Documentation: https://api.obis.org/

library(httr2)
library(jsonlite)

OBIS_BASE <- "https://api.obis.org/v3"

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.obis_get <- function(endpoint, params = list()) {
  params <- Filter(Negate(is.null), params)

  resp <- request(paste0(OBIS_BASE, endpoint)) |>
    req_url_query(!!!params) |>
    req_error(body = function(r) resp_body_string(r)) |>
    req_perform()

  resp_body_json(resp, simplifyVector = TRUE)
}

.check_date <- function(x, name) {
  if (!is.null(x) && !grepl("^\\d{4}-\\d{2}-\\d{2}$", x))
    stop(name, " must be in YYYY-MM-DD format", call. = FALSE)
}

# ---------------------------------------------------------------------------
# Occurrences
# ---------------------------------------------------------------------------

#' Retrieve occurrence records from OBIS
#'
#' @param scientificname  Species name (e.g. "Tursiops truncatus")
#' @param taxonid         WoRMS AphiaID integer
#' @param geometry        WKT polygon for spatial filter, e.g.
#'                        "POLYGON((-90 25, -80 25, -80 30, -90 30, -90 25))"
#' @param areaid          OBIS area ID (see obis_areas())
#' @param datasetid       Dataset UUID
#' @param startdate       Earliest date to include "YYYY-MM-DD"
#' @param enddate         Latest date to include "YYYY-MM-DD"
#' @param startdepth      Minimum depth in metres
#' @param enddepth        Maximum depth in metres
#' @param absence         Include absence records (default FALSE)
#' @param size            Records per page; max ~10 000 (default 500)
#' @param offset          Pagination offset (default 0)
#' @param fields          Character vector of field names to return; NULL = all
#'
#' @return data.frame of occurrence records. The attribute "total" gives the
#'   full count of matching records across all pages.
obis_occurrences <- function(
    scientificname = NULL,
    taxonid        = NULL,
    geometry       = NULL,
    areaid         = NULL,
    datasetid      = NULL,
    startdate      = NULL,
    enddate        = NULL,
    startdepth     = NULL,
    enddepth       = NULL,
    absence        = FALSE,
    size           = 500,
    offset         = 0,
    fields         = NULL
) {
  .check_date(startdate, "startdate")
  .check_date(enddate,   "enddate")

  params <- list(
    scientificname = scientificname,
    taxonid        = taxonid,
    geometry       = geometry,
    areaid         = areaid,
    datasetid      = datasetid,
    startdate      = startdate,
    enddate        = enddate,
    startdepth     = startdepth,
    enddepth       = enddepth,
    absence        = if (isTRUE(absence)) "true" else NULL,
    size           = size,
    offset         = offset,
    fields         = if (!is.null(fields)) paste(fields, collapse = ",") else NULL
  )

  body <- .obis_get("/occurrence", params)
  out  <- body$results
  attr(out, "total") <- body$total
  out
}

#' Paginate through all occurrence records matching a query
#'
#' Calls obis_occurrences() repeatedly until all records are fetched.
#' Large queries can return millions of rows — use filters to limit scope.
#'
#' @inheritParams obis_occurrences
#' @param max_records Cap on total records fetched; NULL = no limit
#' @param page_size   Records per API request (default 1000)
#'
#' @return data.frame of all matching occurrence records
obis_occurrences_all <- function(
    scientificname = NULL,
    taxonid        = NULL,
    geometry       = NULL,
    areaid         = NULL,
    datasetid      = NULL,
    startdate      = NULL,
    enddate        = NULL,
    startdepth     = NULL,
    enddepth       = NULL,
    absence        = FALSE,
    fields         = NULL,
    max_records    = NULL,
    page_size      = 1000
) {
  pages  <- list()
  offset <- 0

  repeat {
    chunk <- obis_occurrences(
      scientificname = scientificname,
      taxonid        = taxonid,
      geometry       = geometry,
      areaid         = areaid,
      datasetid      = datasetid,
      startdate      = startdate,
      enddate        = enddate,
      startdepth     = startdepth,
      enddepth       = enddepth,
      absence        = absence,
      fields         = fields,
      size           = page_size,
      offset         = offset
    )

    total <- attr(chunk, "total")
    if (nrow(chunk) == 0) break

    pages[[length(pages) + 1]] <- chunk
    offset  <- offset + nrow(chunk)
    fetched <- sum(vapply(pages, nrow, integer(1)))
    message(sprintf("Fetched %d / %d records", fetched, total))

    if (offset >= total) break
    if (!is.null(max_records) && fetched >= max_records) break
  }

  out <- do.call(rbind, pages)
  if (!is.null(max_records)) out <- out[seq_len(min(nrow(out), max_records)), ]
  out
}

# ---------------------------------------------------------------------------
# Checklist
# ---------------------------------------------------------------------------

#' Get a species checklist for a spatial/temporal/taxonomic query
#'
#' Returns unique taxa matching the filters with record counts and full
#' taxonomic hierarchy. Useful for "what species occur in this area?"
#'
#' @inheritParams obis_occurrences
#' @param size   Max taxa to return (default 500)
#' @param offset Pagination offset
#'
#' @return data.frame with columns scientificName, records, taxonID, family,
#'   genus, species, is_marine, and full taxonomic hierarchy IDs
obis_checklist <- function(
    scientificname = NULL,
    taxonid        = NULL,
    geometry       = NULL,
    areaid         = NULL,
    datasetid      = NULL,
    startdate      = NULL,
    enddate        = NULL,
    startdepth     = NULL,
    enddepth       = NULL,
    size           = 500,
    offset         = 0
) {
  .check_date(startdate, "startdate")
  .check_date(enddate,   "enddate")

  params <- list(
    scientificname = scientificname,
    taxonid        = taxonid,
    geometry       = geometry,
    areaid         = areaid,
    datasetid      = datasetid,
    startdate      = startdate,
    enddate        = enddate,
    startdepth     = startdepth,
    enddepth       = enddepth,
    size           = size,
    offset         = offset
  )

  body <- .obis_get("/checklist", params)
  out  <- body$results
  attr(out, "total") <- body$total
  out
}

# ---------------------------------------------------------------------------
# Datasets
# ---------------------------------------------------------------------------

#' Search OBIS datasets
#'
#' @param scientificname Filter datasets that contain records for this taxon
#' @param areaid         Filter datasets intersecting this area (see obis_areas())
#' @param size           Max results (default 20)
#' @param offset         Pagination offset
#'
#' @return data.frame with columns id, title, url, statistics, extent, citation, etc.
obis_datasets <- function(
    scientificname = NULL,
    areaid         = NULL,
    size           = 20,
    offset         = 0
) {
  params <- list(
    scientificname = scientificname,
    areaid         = areaid,
    size           = size,
    offset         = offset
  )
  body <- .obis_get("/dataset", params)
  out  <- body$results
  attr(out, "total") <- body$total
  out
}

# ---------------------------------------------------------------------------
# Areas
# ---------------------------------------------------------------------------

#' List OBIS geographic areas
#'
#' Area types:
#'   "iho"  - International Hydrographic Organization seas/oceans
#'   "lme"  - Large Marine Ecosystems
#'   "ebsa" - Ecologically or Biologically Significant Areas
#'   "obis" - OBIS country/territory areas
#'   "abnj" - Areas Beyond National Jurisdiction
#'   "mwhs" - Marine World Heritage Sites
#'
#' @param type   Filter by one of the area types above (NULL = all)
#' @param size   Max results (default 100)
#' @param offset Pagination offset
#'
#' @return data.frame with columns id, name, type
obis_areas <- function(type = NULL, size = 100, offset = 0) {
  body <- .obis_get("/area", list(type = type, size = size, offset = offset))
  out  <- body$results
  attr(out, "total") <- body$total
  out
}

# ---------------------------------------------------------------------------
# Statistics
# ---------------------------------------------------------------------------

#' Get summary statistics for a query
#'
#' Returns aggregate counts: total records, unique species, taxa, datasets,
#' and year range covered.
#'
#' @inheritParams obis_checklist
#' @return Named list with elements records, species, taxa, datasets,
#'   specieslevel, yearrange
obis_statistics <- function(
    scientificname = NULL,
    taxonid        = NULL,
    geometry       = NULL,
    areaid         = NULL,
    datasetid      = NULL,
    startdate      = NULL,
    enddate        = NULL
) {
  .check_date(startdate, "startdate")
  .check_date(enddate,   "enddate")

  params <- list(
    scientificname = scientificname,
    taxonid        = taxonid,
    geometry       = geometry,
    areaid         = areaid,
    datasetid      = datasetid,
    startdate      = startdate,
    enddate        = enddate
  )
  .obis_get("/statistics", params)
}

#' Get record counts by year
#'
#' Useful for plotting temporal trends in data availability for a taxon or area.
#'
#' @inheritParams obis_statistics
#' @return data.frame with columns year (integer) and records (integer)
obis_stats_years <- function(
    scientificname = NULL,
    taxonid        = NULL,
    geometry       = NULL,
    areaid         = NULL,
    datasetid      = NULL
) {
  params <- list(
    scientificname = scientificname,
    taxonid        = taxonid,
    geometry       = geometry,
    areaid         = areaid,
    datasetid      = datasetid
  )
  as.data.frame(.obis_get("/statistics/years", params))
}

# ===========================================================================
# Example usage (run interactively — wrapped in if(FALSE) to prevent
# automatic execution when the file is sourced)
# ===========================================================================

if (FALSE) {

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

}
