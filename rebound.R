library(hoopR)
library(dplyr)
library(caret)

pbp <- load_nba_pbp(2014:2024) %>% filter(season != 2020, type_text == "Offensive Rebound" | type_text == "Defensive Rebound", !is.na(athlete_id_1)) %>%
  mutate(type_text = as.factor(type_text), score_diff = ifelse(team_id == home_team_id, home_score - away_score, away_score - home_score), szn_type = as.factor(ifelse(season_type == 2, "REG", "POST")), home_adv = ifelse(team_id == home_team_id, 1, 0), period = as.factor(period_number), posteam = ifelse(team_id == home_team_id, home_team_abbrev, away_team_abbrev)) %>%
  select(season, game_id, player = athlete_id_1, team = posteam, rebound_type = type_text, score_diff, szn_type, home_adv, period, time = start_quarter_seconds_remaining)

schedule <- load_nba_schedule(2014:2024) %>% select(game_id, home_winner)

pbp <- left_join(pbp, schedule, by = "game_id") %>%
  mutate(win = ifelse((home_winner == "FALSE" & home_adv == 0) | (home_winner == "TRUE" & home_adv == 1), 1, 0)) %>%
  select(-home_winner)

dummy <- dummyVars(" ~ .-team", data = pbp)
final_data <- data.frame(predict(dummy, newdata = pbp))

final_data$team <- pbp$team
final_data <- final_data %>% select(season, game_id, player, team, everything())

xgboost_train <- final_data %>%
  filter(season < 2022) 

xgboost_test <- final_data %>%
  filter(season >= 2022) 

labels_train <- as.matrix(xgboost_train[, 20]) 
xgboost_trainfinal <- as.matrix(xgboost_train[, c(5:19)]) 
xgboost_testfinal <- as.matrix(xgboost_test[, c(5:19)])

