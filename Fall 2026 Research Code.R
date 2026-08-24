#Fall 2026

require(f1dataR)
require(tidyverse)

champ_data = bind_rows(lapply(2016:2025, function(x) {
    load_standings( season = x, round = "last", type = "driver") |>
      mutate(season = x)
  }))

driver_info = bind_rows(lapply(2016:2025, function(x) {
  load_drivers(season = x) |>
    mutate(season = x) |>
    select(driver_id, nationality, date_of_birth, season)
}))

champ_data_16_25 = left_join(champ_data, driver_info, by = c("driver_id", "season"))