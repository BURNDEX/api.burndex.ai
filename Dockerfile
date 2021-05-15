FROM public.ecr.aws/b2r5r7r2/bx-base:latest

RUN installGithub.r \
    rstudio/plumber

COPY . /app/burndex_api

RUN mkdir -p /app/burndex_api/data/
RUN apt-get update -qq && \
    apt-get install -y wget

# Get Machine Learning Model
RUN wget -O /app/burndex_api/data/mars_ensemble.qs https://burndex-data.s3-us-west-1.amazonaws.com/mars_ensemble.qs

# Get Fire Perimeter Data
RUN wget -O /app/burndex_api/data/fire_perim.gpkg https://burndex-data.s3-us-west-1.amazonaws.com/fire_perim.gpkg

# Get Fire Timeseries Data
RUN wget -O /app/burndex_api/data/fire_timeseries.rds https://burndex-data.s3-us-west-1.amazonaws.com/fire_timeseries_data.rds

CMD ["Rscript", "/app/burndex_api/R/api.R"]