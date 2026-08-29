#Fall 2026

require(f1dataR)
require(tidyverse)
require(reticulate)

theme_set(theme_bw())

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
  mutate(driver_age = (season - as.numeric(format(as.Date(date_of_birth), "%Y")))) |>
  mutate(Season=season-2015) |>
  mutate(points = as.numeric(points))

str(champ_data_16_25)

#potentially join to champ_data_16_25 to be able to rank teams as top, middle, bottom
constructor_data = bind_rows(lapply(2016:2025, function(x){
  load_standings(season = x, round = "last", type = "constructor") |>
    mutate(season = x)
  }))

champ_data_16_25 = left_join(champ_data_16_25, constructor_data, by = c("constructor_id", "season")) |>
  rename(driver_points = points.x, driver_position = position.x,
         driver_wins = wins.x, constructor_pos = position.y,
         constructor_points = points.y, constructor_wins = wins.y)

#Original Scatter plot from spring but nicer 
champ_data_16_25 |>
  ggplot(aes(driver_age, driver_points)) + geom_point(col = "green2", size = 2.5) + geom_jitter(alpha = 0.5, size = 3)
#the scatter plot shows a potential bimodal trend that could be just one peak 
#however we do have a pretty clean cut-off at about age 37 where the driver 
#does not get above 300 points

#another version of the scatter plot from above
champ_data_16_25 |>
  ggplot(aes(driver_age, driver_points)) + geom_point(position = position_jitter(), alpha=0.5, size = 3)


champ_data_16_25 |>
  ggplot(aes(driver_age, driver_points, colour = constructor_id)) + 
  geom_point(position = position_jitter(), alpha = 0.5, size = 3) 

py_require("indycarpy")
