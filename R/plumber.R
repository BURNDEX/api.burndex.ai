source("R/utils.R", local = TRUE)
source("R/globals.R", local = TRUE)

#* Predict burning index from parameters for a point
#* @param lat:[string] Latitude of point
#* @param lon:[string] Longitude of point
#* @param date:[string] Date (YYYY-MM-DD)
#* @param prcp:[number] Precipitation Accumulation
#* @param rhmax:[number] Maximum Relative Humidity
#* @param rhmin:[number] Minimum Relative Humidity
#* @param shum:[number] Specific Humidity
#* @param srad:[number] Downward Surface Shortwave Radiation
#* @param tmax:[number] Maximum Air Temperature
#* @param tmin:[number] Minimum Air Temperature
#* @post /predict-point
function(lat, lon, date, prcp, rhmax, rhmin, shum, srad, tmin, tmax) {
    given_data <- tibble::tibble(
        lat   = !!as.double(lat),
        lon   = !!as.double(lon),
        date  = !!as.Date(date),
        prcp  = !!as.double(prcp),
        rhmax = !!as.double(rhmax),
        rhmin = !!as.double(rhmin),
        shum  = !!as.double(shum),
        srad  = !!as.double(srad),
        tmin  = !!as.double(tmin),
        tmax  = !!as.double(tmax)
    )

    bagged_mars$predict(new_data = given_data)[[1]]
}

#* Predict burning index by a point of interest and date
#* @param lat:[string] Latitude of point
#* @param lon:[string] Longitude of point
#* @param date:[string] Date (YYYY-MM-DD)
#* @post /predict-date
function(lat, lon, date) {
    lat  <- as.double(lat)
    lon  <- as.double(lon)
    date <- as.Date(date)

    assertthat::assert_that(date < "2100-01-01")

    poi <- sf::st_point(x = c(lon, lat), dim = "XY") %>%
           sf::st_set_crs(4326) %>%
           sf::st_as_sf()

    if (date > Sys.Date() - 1) {
        given_data <- aggregate_maca(poi, start_date = date, end_date = date)
    } else {
        given_data <- aggregate_gridmet(poi, start_date = date, end_date = date)
    }

    bagged_mars$predict(new_data = given_data)[[1]]
}

#* Predict burning index by an area of interest and date
#* @param xmin:[string] Minimum Longitude
#* @param xmax:[string] Maximum Longitude
#* @param ymin:[string] Minimum Latitude
#* @param ymax:[string] Maximum Latitude
#* @param date:[string] Date (YYYY-MM-DD)
function(xmin, xmax, ymin, ymax, date) {
    date <- as.Date(date)
    xmin <- as.double(xmin)
    xmax <- as.double(xmax)
    ymin <- as.double(ymin)
    ymax <- as.double(ymax)

    assertthat::assert_that(date < "2100-01-01")

    aoi <- expand.grid(c(xmin, xmax),
                       c(ymin, ymax)) %>%
           sf::st_as_sf(coords = c(1, 2)) %>%
           sf::st_bbox()

    if (date > Sys.Date() - 1) {
        given_data <- aggregate_maca(aoi, start_date = date, end_date = date)
    } else {
        given_data <- aggregate_gridmet(aoi, start_date = date, end_date = date)
    }

    bagged_mars$predict(new_data = given_data)[[1]]
}

#* Predict burning index by county and date
#* @param county:[string] US County Name
#* @param state:[string] US State Name
#* @param date:[string] Date (YYYY-MM-DD)
#* @post /predict-county
function(county, state, date) {
    county <- as.character(county)
    state  <- as.character(state)
    date   <- as.Date(date)

    assertthat::assert_that(date < "2100-01-01")

    aoi <- AOI::aoi_get(county = !!county, state = !!state)

    if (date > Sys.Date() - 1) {
        given_data <- aggregate_maca(aoi, start_date = date, end_date = date)
    } else {
        given_data <- aggregate_gridmet(aoi, start_date = date, end_date = date)
    }

    bagged_mars$predict(new_data = given_data)[[1]]
}

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
  agg <- aggregate_maca(pt,
                        start_date = as.character(start),
                        end_date = as.character(end))
  cbi <- augment(model, agg) %>%
    jsonlite::toJSON()
  cbi
}

#* Get fire perimeters near a POI
#* @param lat:[string] Latitude of point
#* @param lon:[string] Longitude of point
#* @post /fires
function(lat, lon) {
    sf::st_point(x = c(lon, lat)) %>%
        sf::st_set_crs(4326) %>%
        AOI::aoi_buffer(10, km = TRUE) %>%
        get_fires(fire_path) %>%
        geojsonsf::sf_geojson()
}