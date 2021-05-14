FROM rocker/r-ver:4.0.4
LABEL maintainer="justin@justinsingh.me"

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
    jsonlite

RUN installGithub.r \
    ropensci/USAboundaries \
    ropensci/USAboundariesData \
    mikejohnson51/AOI \
    mikejohnson51/climateR

# Install plumber and setup image to use
RUN Rscript -e "remotes::install_github('rstudio/plumber@master')"
EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(rev(commandArgs())[1]); args <- list(host = '0.0.0.0', port = 8000); if (packageVersion('plumber') >= '1.0.0') { pr$setDocs(TRUE) } else { args$swagger <- TRUE }; do.call(pr$run, args)"]

COPY . /app/burndex_api
CMD ["/app/burndex_api/R/plumber.R"]