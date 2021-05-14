library(plumber)
library(tidymodels)
library(timetk)
library(earth)
library(R6)
library(AOI)
library(climateR)


pr("/app/burndex_api/plumber.R") %>%
  pr_set_api_spec(function(spec) {
    spec$info <- list(
      title = "Burndex API",
      description = paste(
        "Generate burning index predictions",
        "based on climate variables"
      ),
      contact = list(
        name = "BURNDEX",
        email = "support@burndex.ai"
      ),
      license = list(
        name = "MIT",
        url = "https://opensource.org/licenses/MIT"
      ),
      version = "0.0.1"
    )

    spec
  }) %>%
  pr_run(host = "0.0.0.0", port = 8000)