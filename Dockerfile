FROM gcr.io/burndex/bx-base:latest

RUN installGithub.r \
    rstudio/plumber

COPY . /app/burndex_api

RUN mkdir -p /app/burndex_api/data/
RUN apt-get update -qq && \
    apt-get install -y wget

# Get Machine Learning Model
RUN wget -O /app/burndex_api/data/mars_ensemble.qs https://storage.googleapis.com/burndex-models/mars_ensemble.qs

# Get Fire Perimeter Data and clean
RUN wget -O /app/burndex_api/data/fire_perim.gpkg https://storage.googleapis.com/burndex-models/fire_perim.gpkg

CMD ["Rscript", "/app/burndex_api/R/api.R"]