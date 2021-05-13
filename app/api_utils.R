api_aoi <- function(pt) {
  pt <- jsonlite::toJSON(pt)
  post <- httr::content(httr::POST(
    url = "http://localhost:8000/aoi",
    query = list(pt = pt)
  ))
  geojsonsf::geojson_sf(post[[1]])
}
