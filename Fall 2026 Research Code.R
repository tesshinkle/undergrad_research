#Fall 2026

require(f1dataR)
require(tidyverse)
require(mgcv)
require(rsample)
#require(reticulate)

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

summary(champ_data_16_25)

champ_data_16_25 = left_join(champ_data_16_25, constructor_data, by = c("constructor_id", "season")) |>
  rename(driver_points = points.x, driver_position = position.x,
         driver_wins = wins.x, constructor_pos = position.y,
         constructor_points = points.y, constructor_wins = wins.y)

#Original Scatter plot from spring but nicer 
champ_data_16_25 |>
  ggplot(aes(driver_age, driver_points)) + geom_point(col = "green2", size = 2.5) + geom_jitter(alpha = 0.5, size = 3)
#the scatter plot shows a potential bi-modal trend that could be just one peak 
#however we do have a pretty clean cut-off at about age 37 where the driver 
#does not get above 300 points

#another version of the scatter plot from above
champ_data_16_25 |>
  ggplot(aes(driver_age, driver_points)) + geom_point(position = position_jitter(), alpha=0.5, size = 3)


champ_data_16_25 |>
  ggplot(aes(driver_age, driver_points, colour = constructor_id)) + 
  geom_point(position = position_jitter(), alpha = 0.5, size = 3) 

champ_data_16_25 = champ_data_16_25 |>
  mutate(constructor_group = case_when(constructor_pos == "1" ~ "top_three",
                                       constructor_pos == "2" ~ "top_three",
                                       constructor_pos == "3" ~ "top_three",
                                       constructor_pos == "8" ~ "bottom_field",
                                       constructor_pos == "9" ~ "bottom_field",
                                       constructor_pos == "10" ~ "bottom_field",
                                       constructor_pos == "11" ~ "bottom_field",
                                       constructor_pos >= "4" | constructor_pos <= "7" ~ "mid_field")) |>
  mutate(constructor_group = as.factor(constructor_group)) |>
  mutate(driver_id = as.factor(driver_id)) |>
  mutate(constructor_id = as.factor(constructor_id)) |>
  mutate(season = as.factor(season))

champ_data_16_25 = champ_data_16_25 |>
  group_by(driver_id) |>
  mutate(n_teams = n_distinct(constructor_id)) |>
  ungroup()

longevity_data = champ_data_16_25 |>
  group_by(driver_id, constructor_id) |>
  summarize(years_at_team = n() , .groups = "drop")|>
  ungroup()

champ_data_16_25 = left_join(champ_data_16_25, longevity_data, 
                             by = c("driver_id", "constructor_id"), 
                             relationship = "many-to-many")

champ_data_16_25 |>
  ggplot(aes(driver_age, driver_points, colour = constructor_group)) +
  geom_point(position = position_jitter(), alpha = 0.5, size = 3)
#We can see an overlapping in the mid-field teams and top three teams. 
#The drivers in the mid-field that are matching several top team drivers 
#I would estimate as having the potential to move to a top three team or that 
#team was competing with the top three teams as a fourth team (currently happening)
#The top three team drivers that are on the lower end of the points, I would 
#estimate to be dropped by the team 

champ_data_16_25 |>
  ggplot(aes(years_at_team, driver_points)) + 
  geom_point(position = position_jitter(), alpha = 0.5, size = 3)

#Practice models
control.mod = gam(driver_points~s(driver_age)+s(driver_id,bs="re")+s(Season,bs="re")+s(driver_age,driver_id,bs="re")+s(driver_id,Season,bs="re"),
                  data = champ_data_16_25,method="REML")
summary(control.mod)
summary(control.mod)$s.table

f1.mod1 = gam(driver_points ~ s(driver_age) + s(driver_id, bs = "re") + s(constructor_group, bs= "re"),
              data = champ_data_16_25, method = "REML")
summary(f1.mod1) #already has a smaller REML compared to control model

f1.mod2 = gam(driver_points ~ s(driver_age) + s(driver_id, bs = "re") + s(constructor_id, bs= "re"),
              data = champ_data_16_25, method = "REML")
summary(f1.mod2) #separating by teams themselves does not explain the deviance better

f1.mod3 = gam(driver_points ~ s(driver_age) + s(driver_id, bs = "re") + 
                s(constructor_group, bs= "re") + years_at_team,
              data = champ_data_16_25, method = "REML")
summary(f1.mod3) 
#years_at_team is a significant predictor however it lowers 
#deviance explained but by only 0.2%


#training set/ cross-validation since the data set is small
set.seed(090126) #from the date

cv_splits = vfold_cv(champ_data_16_25, v = 5, repeats = 2, strata = season)
print(cv_splits)

first_split = cv_splits$splits[[1]]

train_data = analysis(first_split)

train_data2 = training(first_split)

#py_require("indycarpy") #package doesn't work
