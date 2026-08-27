#Fall 2026

require(f1dataR)
require(tidyverse)

#retrieving the final driver's championship points for each season 2016-2025
champ_data = bind_rows(lapply(2016:2025, function(x) {
    load_standings( season = x, round = "last", type = "driver") |>
      mutate(season = x)
  }))

#retrieving driver information, age, nationality, 
#as well as ID and season to join to champ_data
driver_info = bind_rows(lapply(2016:2025, function(x) {
  load_drivers(season = x) |>
    mutate(season = x) |>
    select(driver_id, nationality, date_of_birth, season)
}))

#joining the champ_data and driver_info data together
champ_data_16_25 = left_join(champ_data, driver_info, by = c("driver_id", "season")) |>
  mutate(driver_age = (season - as.numeric(format(as.Date(date_of_birth), "%Y"))))

str(champ_data_16_25)

#potentially join to champ_data_16_25 to be able to rank teams as top, middle, bottom
constructor_data = bind_rows(lapply(2016:2025, function(x){
  load_standings(season = x, round = "last", type = "constructor") |>
    mutate(season = x)
  }))
