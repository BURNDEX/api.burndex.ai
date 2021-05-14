bagged_mars <- get_mars_model()

fire_path <- sf::read_sf(paste0("https://opendata.arcgis.com/datasets/",
                                "f72ebe741e3b4f0db376b4e765728339_0.geojson"))