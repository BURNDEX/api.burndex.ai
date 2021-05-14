#' @title Download MARS Model from Google Cloud Storage
#' @description
#' Locates the model .qs file and downloads if it doesn't exist,
#' then returns the R6 model after it has been loaded into memory.
#' @return An `R6` object containing the model ensemble internally.
get_mars_model <- function() {
    object_url <- paste0(
        "https://storage.googleapis.com/burndex-models/",
        "mars_ensemble.qs"
    )

    file_path <- file.path("/app/burndex_api/data", "mars_ensemble.qs")

    if (!dir.exists(dirname(file_path))) {
        dir.create(
            dirname(file_path),
            recursive = TRUE
        )
    }

    if (!file.exists(file_path)) {
        download.file(
            url      = object_url,
            destfile = file_path,
            mode     = "w"
        )
    }

    qs::qread(file_path, nthreads = as.integer(parallel::detectCores()[1] - 1))
}

make_prediction <- function(r6_model, new_data) {
    r6_model$predict(new_data = new_data)[[1]]
}

get_fire_data <- function(type = c("perim", "ts")) {
    type <- match.arg(type)

    switch(
        type,
        "perim" = "/app/burndex_api/data/fire_perim.gpkg",
        "ts"    = "/app/burndex_api/data/fire_timeseries.rds"
    )
}

#' @title Convert latlon coordinates to an `sf` object
#' @param lat Latitude
#' @param lon Longitude
#' @return An `sf` object
make_point <- function(lat, lon) {
    sf::st_as_sf(
        tibble::tibble(lat = lat, lon = lon),
        coords = c("lon", "lat"),
        crs = sf::st_crs(4326)
    )
}

#' @title Calculate Chandler Burning Index
#' @param rh Relative Humidity
#' @param t Air Temperature
#' @return Chandler Burning Index
chandler_bi <- function(rh, t) {
    rh_eq  <- (110 - 1.373 * rh)
    t_eq   <- (10.20 - t)
    rh_exp <- 10 ^ (-0.0142 * rh)
    main   <- ((rh_eq - 0.54 * t_eq) * (124 * rh_exp))

    main / 60
}

#' @title Convert Kelvin to Fahrenheit
#' @param t Temperature
#' @return Temperature in Fahrenheit
kelvin_to_fahrenheit <- function(t) {
    (t - 273.15) * (9 / 5) + 32
}

#' @title Tidy a `RasterLayer` into a `tibble`
#' @param raster `RasterLayer` object
#' @return A `tibble`
tidy_raster <- function(raster) {
    rtable <- raster %>%
              raster::rasterToPoints() %>%
              tibble::as_tibble() %>%
              dplyr::relocate(x, y) %>%
              setNames(
                  .,
                  c("lon",
                    "lat",
                    stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
              ) %>%
              tidyr::pivot_longer(
                  cols = c(tidyselect::everything(), -(1:2)),
                  names_to = "date"
              ) %>%
              dplyr::mutate(date = lubridate::ymd(date)) %>%
              dplyr::relocate(lon, lat, value)

    rtable
}

#' @title Tidy a `RasterStack` into a `tibble`
#' @param raster_list `RasterStack` object
#' @param as_sf If TRUE, a `sf` object is returned.
#'              Otherwise, a regular `tibble` is returned.
#' @return A `tibble` or `sf` object.
tidy_stack <- function(raster_list, as_sf = FALSE) {
    param_names <- names(raster_list)
    tidy_stacks <- lapply(X = raster_list, FUN = tidy_raster)

    p <- progressr::progressor(along = param_names)
    tidy_data <-
        lapply(X = param_names,
           FUN = function(rname) {
               p(paste0("Transforming ", rname, "..."))
               setNames(
                   tidy_stacks[[rname]],
                   c("lon", "lat", rname, "date")
               )
            }
        ) %>%
        purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
        dplyr::relocate(lon, lat, date)

    if (as_sf) {
        tidy_data <-
            tidy_data %>%
            sf::st_as_sf(coords = c("lon", "lat")) %>%
            sf::st_set_crs(4326)
    }

    tidy_data
}

#' @title Get common parameters between GridMET and MACA datasets.
#' @return Common parameter names
common_params <- function() {
    grid   <- climateR::param_meta$gridmet$common.name
    maca   <- climateR::param_meta$maca$common.name
    common <- which(grid %in% maca)

    grid[common]
}

#' @title Aggregate GridMET data by AOI and dates
#' @param aoi Area of Interest
#' @param start_date Starting date to index
#' @param end_date Ending date to index
#' @param as_sf If TRUE, a `sf` object is returned.
#'              Otherwise, a regular `tibble` is returned
#' @return A `tibble` or `sf` object
aggregate_gridmet <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
    p <- progressr::progressor(steps = 3L)

    p("Getting GridMET data...")

    climate_data <- climateR::getGridMET(
        AOI       = sf::st_transform(aoi, 4326),
        param     = common_params(),
        startDate = start_date,
        endDate   = end_date
    )

    p("Tidying GridMET data...")

    tidy_clim <-
        tidy_stack(
            c(climate_data),
            as_sf = as_sf
        ) %>%
        dplyr::rename(
            prcp       = tidyselect::contains("prcp"),
            rhmax      = tidyselect::contains("rhmax"),
            rhmin      = tidyselect::contains("rhmin"),
            shum       = tidyselect::contains("shum"),
            srad       = tidyselect::contains("srad"),
            tmin       = tidyselect::contains("tmin"),
            tmax       = tidyselect::contains("tmax"),
        ) %>%
        dplyr::mutate(
            rhavg = (rhmax + rhmin) / 2,
            tavg  = (tmax + tmin) / 2,
            cbi_rhmax_tmax = chandler_bi(rhmax, tmax),
            cbi_rhmin_tmax = chandler_bi(rhmin, tmax),
            cbi_rhavg_tmax = chandler_bi(rhavg, tmax),
            cbi_rhmax_tmin = chandler_bi(rhmax, tmin),
            cbi_rhmin_tmin = chandler_bi(rhmin, tmin),
            cbi_rhavg_tmin = chandler_bi(rhavg, tmin),
            cbi_rhmax_tavg = chandler_bi(rhmax, tavg),
            cbi_rhmin_tavg = chandler_bi(rhmin, tavg),
            cbi_rhavg_tavg = chandler_bi(rhavg, tavg),
            burn_index = (
                cbi_rhmax_tmax + cbi_rhmin_tmax + cbi_rhavg_tmax +
                cbi_rhmax_tmin + cbi_rhmin_tmin + cbi_rhavg_tmin +
                cbi_rhmax_tavg + cbi_rhmin_tavg + cbi_rhavg_tavg
            ) / 9
        ) %>%
        dplyr::select(lat, lon, date, prcp, rhmax, rhmin, shum,
                      srad, tmin, tmax, burn_index)

    p("Tidied!")

    tidy_clim
}

#' @title Aggregate MACA data by AOI and dates
#' @param aoi Area of Interest
#' @param start_date Starting date to index
#' @param end_date Ending date to index
#' @param as_sf If TRUE, a `sf` object is returned.
#'              Otherwise, a regular `tibble` is returned
#' @return A `tibble` or `sf` object
aggregate_maca <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
    p <- progressr::progressor(steps = 3L)

    p("Getting MACA data...")

    relative_humidity <- climateR::getMACA(
        AOI       = aoi,
        param     = c("rhmax", "rhmin"),
        startDate = start_date,
        endDate   = end_date,
        model     = "BNU-ESM"
    )

    other_climate <- climateR::getMACA(
        AOI       = aoi,
        param     = common_params()[common_params() %in% c("rhmax", "rhmin")],
        startDate = start_date,
        endDate   = end_date
    )

    climate_data <- c(other_climate, relative_humidity)

    p("Tidying MACA data...")

    tidy_clim <-
        tidy_stack(
            c(climate_data),
            as_sf = as_sf
        ) %>%
        dplyr::rename(
            prcp  = tidyselect::contains("prcp"),
            rhmax = tidyselect::contains("rhmax"),
            rhmin = tidyselect::contains("rhmin"),
            shum  = tidyselect::contains("shum"),
            srad  = tidyselect::contains("srad"),
            tmin  = tidyselect::contains("tmin"),
            tmax  = tidyselect::contains("tmax")
        ) %>%
        dplyr::mutate(
            rhavg = (rhmax + rhmin) / 2,
            tavg  = (tmax + tmin) / 2,
            cbi_rhmax_tmax = chandler_bi(rhmax, tmax),
            cbi_rhmin_tmax = chandler_bi(rhmin, tmax),
            cbi_rhavg_tmax = chandler_bi(rhavg, tmax),
            cbi_rhmax_tmin = chandler_bi(rhmax, tmin),
            cbi_rhmin_tmin = chandler_bi(rhmin, tmin),
            cbi_rhavg_tmin = chandler_bi(rhavg, tmin),
            cbi_rhmax_tavg = chandler_bi(rhmax, tavg),
            cbi_rhmin_tavg = chandler_bi(rhmin, tavg),
            cbi_rhavg_tavg = chandler_bi(rhavg, tavg),
            burn_index = (
                cbi_rhmax_tmax + cbi_rhmin_tmax + cbi_rhavg_tmax +
                cbi_rhmax_tmin + cbi_rhmin_tmin + cbi_rhavg_tmin +
                cbi_rhmax_tavg + cbi_rhmin_tavg + cbi_rhavg_tavg
            ) / 9
        ) %>%
        dplyr::select(lat, lon, date, prcp, rhmax, rhmin, shum,
                      srad, tmin, tmax, burn_index)

    p("Tidied!")

    tidy_clim
}

#' @title Convert a tidy `tibble` to a `RasterLayer`
#' @param data `tibble`
#' @param x Column in `data` representing the X dimension
#' @param y Column in `data` representing the Y dimension
#' @param z Column in `data` representing the Z dimension
#' @param ... Unused
#' @param res Resolution of `RasterLayer`.
#'            If not used, infers resolution based on coordinates.
#' @return A `RasterLayer` object
tidy_to_raster <- function(data, x, y, z, ..., res = c(NA, NA)) {
    xyz <- data %>%
           dplyr::select({{ x }}, {{ y }}, {{ z }}) %>%
           dplyr::rename(
               x = {{ x }},
               y = {{ y }},
               z = {{ z }}
           )

    raster::rasterFromXYZ(
        xyz = xyz,
        res = res,
        crs = sf::st_crs(4326)$proj4string
    )
}

get_fires <- function(aoi, path) {
    sf::st_read(path) %>%
        sf::st_transform(5070) %>%
        sf::st_filter(sf::st_transform(aoi, 5070)) %>%
        sf::st_transform(4326)
}