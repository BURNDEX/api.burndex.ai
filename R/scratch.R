library(jsonlite)
lng = -120.5
lat = 39.58
pt <- data.frame(lat, lng)
  # jsonlite::toJSON()
start_date = "2020-09-06"
end_date = "2020-09-06"
model <- readRDS("app/kknn_model.rds")
p <- get_prediction(aoi, start_date, end_date, model)
plot(p)

pp <- rasterToPoints(p)

api_aoi <- function(pt) {
  pt <- jsonlite::toJSON(pt)
  post <- httr::content(httr::POST(
  url = "http://localhost:8000/aoi",
  query = list(pt = pt)
))
  geojsonsf::geojson_sf(post[[1]])
}
aoi <- api_aoi(pt)

predictRaster <- function(pt_rast) {
  # get empty raster, set values to 0
  empty_raster <- raster::projectExtent(object = clim$tmax[[1]],
                                        crs = clim$tmax@crs) %>%
    raster::setValues(empty_raster, value = 0)
  ext <- extent(min(cbi$lon), max(cbi$lon), min(cbi$lat), max(cbi$lat))
  r <- raster(ext, ncol=nrow(cbi), nrow=nrow(cbi))
  r2 <- rasterize(select(cbi, lon, lat), r, field = cbi$.pred)

  raster::crs(r2) <- sf::st_crs(4326)$proj4string

  plot(r2)
  extent()
  # Convert points to sp
  agg <- as(pt_rast, "Spatial")

  # rastorize prediction points into empty raster grid --- currently rasterizes "last" layer in stack
  r <- rasterize(agg, empty_raster, field = "tmax")
}
plot(x = cbi$lon, y= cbi$lat)
# api_raster <- function(aoi, start_date, end_date) {
  pt <- jsonlite::toJSON(pt)
  post <- httr::content(httr::POST(
      url = "http://localhost:8000/predict",
    query = list(pt = pt, start = start_date, end = end_date)))
   httr::stop_for_status(post)

   cbi <- post %>%
      httr::content()
   cbi <- jsonlite::fromJSON(post[[1]]) %>%

    tidy_to_raster(cbi, x = lon, y = lat, z = .pred)
#
# }
r2 <- raster(ncols = nrow(cbi), nrows = nrow(cbi))

r <- api_raster(aoi, start_date, end_date)


httr::content(httr::POST(
  url = "https://localhost:8000/aoi",
  query = list(pt = pt)
))
