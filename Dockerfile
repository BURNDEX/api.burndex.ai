FROM gcr.io/burndex/bx-container:latest

RUN installGithub.r \
    rstudio/plumber

RUN mkdir -p /app/burndex_api/data/
RUN apt-get update -qq && \
    apt-get install -y wget && \
    wget -O /app/burndex_api/data/mars_ensemble.qs https://storage.googleapis.com/burndex-models/mars_ensemble.qs

COPY . /app/burndex_api
CMD ["Rscript", "/app/burndex_api/R/api.R"]