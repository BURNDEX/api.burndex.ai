FROM rocker/r-ver:4.0.4

# Install Tidyverse
RUN /rocker_scripts/install_tidyverse.sh

# Install Additional Dependencies
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev

# Install Geospatial Dependencies
COPY scripts/install_geospatial_modified.sh /rocker_scripts
RUN chmod +x /rocker_scripts/install_geospatial_modified.sh
RUN /rocker_scripts/install_geospatial_modified.sh

# Install additional R packages
RUN install2.r -e -s \
    remotes \
    tidymodels \
    timetk \
    earth \
    R6 \
    qs \
    jsonlite \
    geojsonsf

RUN installGithub.r \
    ropensci/USAboundaries \
    ropensci/USAboundariesData \
    mikejohnson51/AOI \
    mikejohnson51/climateR \
    rstudio/plumber

COPY . /app/burndex_api
CMD ["/app/burndex_api/R/api.R"]