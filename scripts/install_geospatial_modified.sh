#!/bin/bash
set -e

# always set this for scripts but don't declare as ENV..
export DEBIAN_FRONTEND=noninteractive

apt-get update -qq \
  && apt-get install -y --no-install-recommends \
    gdal-bin \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    lsb-release \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev

# lwgeom 0.2-2 and 0.2-3 have a regression which prevents install on ubuntu:bionic
## permissionless PAT for builds
UBUNTU_VERSION=${UBUNTU_VERSION:-`lsb_release -sc`}
if [ ${UBUNTU_VERSION} == "bionic" ]; then
  R -e "remotes::install_version('lwgeom', '0.2-4')"
fi

install2.r --error --skipinstalled \
    RColorBrewer \
    RNetCDF \
    proj4 \
    raster \
    rgdal \
    rgeos \
    sf \
    sp \
    stars \
    geosphere \
    geojsonsf

# Clean up
rm -rf /var/lib/apt/lists/*
rm -r /tmp/downloaded_packages