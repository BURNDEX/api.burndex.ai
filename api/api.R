library(plumber)
library(parsnip)


pr("app/plumber.R") %>%
  pr_set_api_spec(function(spec) {
    spec$info <- list(
      title = "Burndex API",
      description = paste(
        "Generate predictions based on",
        "climate variables"
      ),
      contact = list(
        name = "Justin Singh-Mohudpur",
        email = "justinsingh-mohudpur@ucsb.edu"
      ),
      license = list(
        name = "MIT",
        url = "https://opensource.org/licenses/MIT"
      ),
      version = "0.0.2"
    )

    spec
  }) %>%
  pr_run(host = "0.0.0.0", port = 8000)




