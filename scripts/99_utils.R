library(nhdR)
# library(wikilake)
library(WikipediR)
library(rvest)
library(stringr)
library(sf)
library(mapview)

get_content <- function(page_name) {
  res <- WikipediR::page_content("en", "wikipedia", page_name = page_name,
    as_wikitext = FALSE)
  res <- res$parse$text[[1]]
  res <- xml2::read_html(res, encoding = "UTF-8")
  res
}

get_coords <- function(page_name) {
  # page_name <- "James River (Missouri)"
  page_metadata <- page_info("en", "wikipedia", page = page_name)$query$pages

  page_link <- page_metadata[[1]][["fullurl"]]
  message(paste0("Retrieving data from: ", page_link))
  res <- get_content(page_name)

  res        <- rvest::html_nodes(res, "table")
  meta_index <- grep("infobox vcard", rvest::html_attr(res, "class"))

  if (length(meta_index) == 0) meta_index <- 1
  res <- rvest::html_table(res[max(meta_index)])[[1]]
  res <- res[grep("coordinates", res[, 1]), ]

  lat <- as.numeric(stringr::str_extract(res[2, 2], "\\d{2}\\..{1,5}(?=°N)"))
  lon <- as.numeric(stringr::str_extract(res[2, 2], "\\d{2}\\..{1,5}(?=°W)")) * -1

  data.frame(lon = lon, lat = lat, stringsAsFactors = FALSE)
}