library(dplyr)
library(here)

load(here('data/examples/rider_records_giro2020.rda'))
load(here('data/examples/rider_profiles_giro2020.rda'))

# Prepare the data if not loaded
if (! (exists("rider_records_giro2020") && exists("rider_profiles_giro2020")))
{
  source("R/data-raw/functions.R")
  # Get rider IDs from Giro 2020 startlist
  rider_ids <-
    get_rider_urls_sl('https://www.procyclingstats.com/race/giro-d-italia/2020/gc/startlist')

  # Get profiles and results for each starting rider
  pcs_data <- get_pcs_data(rider_ids, seasons =  c(2019, 2020))

  rider_profiles_giro2020 <- pcs_data$profiles
  rider_records_giro2020 <- pcs_data$results

  usethis::use_data(rider_profiles_giro2020,
                    rider_records_giro2020,
                    overwrite = TRUE)
}
