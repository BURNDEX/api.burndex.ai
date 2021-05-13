model <- readRDS("kknn_model.rds")
fire_path <- "app/us_fire_perim.gdb"

#* @param pt: [object]
#* @post /aoi
function(pt) {
  pt <- pt %>%
    jsonlite::fromJSON() %>%
    sf::st_as_sf(coords = c("lng", "lat"),
                 crs = 4326)
  aoi <- pt %>%
    sf::st_transform(5070) %>%
    sf::st_buffer(10000) %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_transform(4326) %>%
    sf::st_as_sf() %>%
    geojsonsf::sf_geojson()
  aoi
}

#* @param pt: [object]
#* @param start: [chr]
#* @param end: [chr]
#* @post /predict
function(pt, start, end) {
  agg <- aggregate_maca(grab_aoi(pt),
                        start_date = as.character(start),
                        end_date = as.character(end))
  cbi <- augment(model, agg) %>%
    jsonlite::toJSON()
  cbi
}

#* @post /fires
function(pt) {
  perimeters <- get_fires(grab_aoi(pt), fire_path) %>% geojsonsf::sf_geojson()
  perimeters
}








