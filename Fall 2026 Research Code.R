#Fall 2026

require(f1dataR)
require(nascaR.data)
require(tidyverse)
require(mgcv)
require(caret)
require(lme4)
#require(reticulate)

theme_set(theme_bw())

##Formula One Data----
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

##We can see an overlapping in the mid-field teams and top three teams. 
##The drivers in the mid-field that are matching several top team drivers 
##I would estimate as having the potential to move to a top three team or that 
##team was competing with the top three teams as a fourth team (currently happening)
##The top three team drivers that are on the lower end of the points, I would 
##estimate to be dropped by the team 


champ_data_16_25 |>
  ggplot(aes(years_at_team, driver_points)) + 
  geom_point(position = position_jitter(), alpha = 0.5, size = 3)

#Practice models
#model from spring, baseline model
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

f1lmermod = lmer(driver_points ~ driver_age + (1|driver_id) + 
                   (1|constructor_group) + years_at_team,
                 data = champ_data_16_25)
summary(f1lmermod)

f1lmmod = lm(driver_points ~ driver_age + driver_id + 
               constructor_group + years_at_team,
             data = champ_data_16_25)
summary(f1lmmod)

#training set/ K- Fold cross-validation since the data set is small
set.seed(090126) #from the date

require(caret)

require(cv)

model.f1 = f1.mod3
summary(model.f1$gam)

model.f1_cv.results = cv::cv(model.f1, criterion = rmse, k = 10, 
                             clusterVariables = "driver_id", 
                             predict.clusters.args = list(allow.new.levels = TRUE),
                             seed = 92526)

summary(model.f1_cv.results)

f1cv.ratio = (model.f1_cv.results[["CV crit"]])/(model.f1_cv.results[["full crit"]])
f1cv.ratio #ration slightly larger than 1

lmemod.f1 = f1lmermod
lmef1.model_cv.results = cv::cv(lmemod.f1, criterion = rmse, k = 10,
                                clusterVariables = "driver_id",
                                seed = 10426)
summary(lmef1.model_cv.results)

f1lmecv.ratio = (lmef1.model_cv.results[["CV crit"]])/(lmef1.model_cv.results[["full crit"]])
f1lmecv.ratio

#error in trying to get cv for linear model
lm.f1 = f1lmmod
lmf1.model_cv.results = cv::cv(lm.f1, criterion = rmse, k = 52,
                                seed = 10426)
summary(lmf1.model_cv.results)

f1lmcv.ratio = (lmf1.model_cv.results[["CV crit"]])/(lmf1.model_cv.results[["full crit"]])
f1lmcv.ratio

#yprob <- predict(diabetes.model,newdata=Pima.te,type="response")
#yhat <- factor(ifelse(yprob>0.5,"Yes","No"))
#y <- Pima.te$type
#caret::confusionMatrix(yhat,y)


##NASCAR data----

series_data = load_series("cup") |>
  filter(Season >= 2016 & Season <= 2025) |>
  select(-c("S1", "S2", "S3", "Track", "Name")) |>
  group_by(Season) |>
  filter(Race == max(Race)) |>#gets the data for just the last race of the season
  ungroup()

final_series_data = load_series("cup") |>
  filter(Season >= 2016 & Season <= 2025) |>
  group_by(Driver, Season) |>
  summarise(points = sum(Pts),
            avg_finish = mean(Finish, na.rm = TRUE),
            .groups = "drop")

nascar_data = left_join(series_data,final_series_data, by = c("Driver", "Season"))

nascar_longevity = series_data |>
  group_by(Driver, Team) |>
  summarize(years_at_team = n() , .groups = "drop")|>
  ungroup()
summary(nascar_longevity)

nascar_data = left_join(nascar_data, nascar_longevity, by = c("Driver", "Team"))

summary(nascar_data)


team_points_16_25 = load_series("cup") |>
  filter(Season >= 2016 & Season <= 2025) |>
  group_by(Season, Team) |>
  summarize(
    total_races   = n(),
    total_wins    = sum(Finish == 1, na.rm = TRUE),
    laps_led      = sum(Led, na.rm = TRUE),
    avg_finish    = mean(Finish, na.rm = TRUE),
    total_points  = sum(Pts, sa.rm = TRUE),
    .groups = "drop"
  )

str(team_points_16_25)
summary(team_points_16_25$total_points)

team_points_16_25 |> 
  ggplot(aes(total_points)) + geom_boxplot()
#may do a cluster analysis to define groups for team fields

#K-means clusters decided because I want 3 clusters
require(classInt)
set.seed(91)
pointBreaks = team_points_16_25 |>
  distinct(row_number(), .keep_all = TRUE) |>
  pull(total_points) |>
  classIntervals(n = 3, style = "jenks")

team_points_16_25 = team_points_16_25 |>
  mutate(bins = cut(total_points,
               breaks = pointBreaks$brks,
               include.lowest = TRUE,
               labels = c("bottom_teams", "mid_field", "top_teams"))) |>
  rename(team_group = bins) |>
  select(-c(total_races, total_wins, laps_led, avg_finish))

nascar_data = left_join(nascar_data,team_points_16_25, by = c("Team", "Season")) 

nascar_data = nascar_data |>
  rename(total_team_points = total_points)

require(gt)
team_points_16_25 |>
  group_by(team_group) |>
  summarise(
    Team_Count = n(),
    Min_Points = min(total_points, na.rm = TRUE),
    Max_Points = max(total_points, na.rm = TRUE),
    Avg_Points = mean(total_points, na.rm = TRUE)
  ) |>
  gt()
pointBreaks
# Team_count matches with the output when "pointBreaks" runs 


#loading driver info for 2016-2025
full_series_data = load_series("cup") |>
  filter(Season >= 2016 & Season <= 2025)

driver_list = final_series_data |>
  distinct(Driver) |>
  pull(Driver)
  
nascar_driver_info = map_df(driver_list, function(driver_name){
  get_driver_info(driver = driver_name,
                  series = "cup", 
                  type = "season",
                  interactive = FALSE)
}) |>
  filter(Season >= 2016 & Season <= 2025)

##The driver info function did not include birth dates so they are being manually entered

###----

view(driver_list)
write.csv(driver_list, "nascar_drvier_list.csv", row.names = FALSE)

nascar_driver_dob_list = readxl::read_excel("nascar_driver_dob_list.xlsx")

nascar_data = left_join(nascar_data, nascar_driver_dob_list, by = c("Driver"))

nascar_data = nascar_data |>
  mutate(Age = (Season - as.numeric(format(as.Date(DOB), "%Y")))) |>
  mutate(Driver = as.factor(Driver))

summary(nascar_data$Age)
summary(champ_data_16_25$driver_age)

str(nascar_data)

#Trying to get manufacturer data


###NASCAR modeling----
nascar.mod = gam(points~ s(Age) + s(Driver, bs= "re"), 
                 data = nascar_data, method = "REML")
summary(nascar.mod)

nascar.mod2 = gam(points~ s(Age) + s(Driver, bs= "re") + s(years_at_team), 
                  data = nascar_data, method = "REML")
summary(nascar.mod2)

nascar.mod3 = gam(points~s(Age) + s(Driver, bs = "re") + 
                    s(years_at_team) + s(team_group, bs = "re"),
                  data = nascar_data, method = "REML")
summary(nascar.mod3)

#need to get years at team and number of teams, and driver age

nascar.mod2.cv_results = cv(nascar.mod2, criterion = rmse, k = 10,
                                   clusterVariables = "Driver",
                                   predict.clusters.args = list(allow.new.levels = TRUE), 
                                   seed = 9252026)
summary(nascar.mod2.cv_results)

nascarcv.ratio = nascar.mod2.cv_results[["CV crit"]]/nascar.mod2.cv_results[["full crit"]]
nascarcv.ratio # more overfitting than f1 cv but still need to add variables.


##Third Motorsport----
# Either Indycar or MotoGP data (from an API)
require(httr2)
require(httr)
require(jsonlite)


req = request("https://api.motogp.pulselive.com/motogp/v1")
req

resp = req_perform(req)
resp

httr::GET("https://api.motogp.pulselive.com/motogp/v1")

seasons = httr::GET("https://api.motogp.pulselive.com/motogp/v1/results/seasons")

seasons = content(seasons, as = "text", encoding = "UTF-8") |>
  fromJSON()

motogp_seasons = seasons |>
  filter(year >=2016 & year < 2026) |>
  select(-c(name,current))

categories25 = httr::GET("https://api.motogp.pulselive.com/motogp/v1/results/categories?seasonUuid=ae6c6f0d-c652-44f8-94aa-420fc5b3dab4")

categories25 = content(categories25, as = "text", encoding = "UTF-8") |>
  fromJSON()

##Change the below code.
season_uuid = motogp_seasons |>
  distinct(id) |>
  pull(id)

events = httr::GET("https://api.motogp.pulselive.com/motogp/v1/results/events", 
                   query = list(seasonUuid = season_uuid))
