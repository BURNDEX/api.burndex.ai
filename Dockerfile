FROM gcr.io/burndex/bx-base:latest

RUN installGithub.r \
    rstudio/plumber

COPY . /app/burndex_api

RUN mkdir -p /app/burndex_api/data/
RUN apt-get update -qq && \
    apt-get install -y wget && \
    wget -O /app/burndex_api/data/mars_ensemble.qs https://storage.googleapis.com/burndex-models/mars_ensemble.qs

CMD ["Rscript", "/app/burndex_api/R/api.R"]