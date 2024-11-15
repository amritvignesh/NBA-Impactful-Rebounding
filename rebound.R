library(hoopR)
library(dplyr)
library(caret)
library(tidyverse)
library(xgboost)
library(gt)
library(gtExtras)
library(vip)

pbp <- load_nba_pbp(2014:2024) %>% filter(season != 2020, type_text == "Offensive Rebound" | type_text == "Defensive Rebound", !is.na(athlete_id_1)) %>%
  mutate(type_text = as.factor(type_text), score_diff = ifelse(team_id == home_team_id, home_score - away_score, away_score - home_score), szn_type = as.factor(ifelse(season_type == 2, "REG", "POST")), home_adv = ifelse(team_id == home_team_id, 1, 0), period = as.factor(period_number), posteam = ifelse(team_id == home_team_id, home_team_abbrev, away_team_abbrev)) %>%
  select(id, season, game_id, player = athlete_id_1, team = posteam, type_text, score_diff, szn_type, home_adv, period, time = start_quarter_seconds_remaining)

pbp_bef <- load_nba_pbp(2014:2024) %>% filter(season != 2020, lead(type_text) == "Offensive Rebound" | lead(type_text) == "Defensive Rebound", !is.na(lead(athlete_id_1))) %>%
  mutate(type_text = as.factor(type_text), score_diff = ifelse(team_id == home_team_id, home_score - away_score, away_score - home_score), szn_type = as.factor(ifelse(season_type == 2, "REG", "POST")), home_adv = ifelse(team_id == home_team_id, 1, 0), period = as.factor(period_number), posteam = ifelse(team_id == home_team_id, home_team_abbrev, away_team_abbrev)) %>%
  select(id, season, game_id, player = athlete_id_1, team = posteam, type_text, score_diff, szn_type, home_adv, period, time = start_quarter_seconds_remaining)

pbp <- pbp %>% mutate(row_num = row_number())
pbp_bef <- pbp_bef %>% mutate(row_num = row_number())

pbp <- bind_rows(pbp_bef, pbp) %>% arrange(row_num)

pbp <- pbp %>%
  select(-row_num) %>%
  mutate(row_num = row_number(), possession = ifelse(row_num %% 2 == 1, ifelse(lead(type_text) == "Offensive Rebound", 1, 0), 1))

pbp <- left_join(pbp, schedule, by = "game_id") %>%
  mutate(possession = as.factor(possession), win = ifelse((home_winner == "FALSE" & home_adv == 0) | (home_winner == "TRUE" & home_adv == 1), 1, 0)) %>%
  select(-home_winner, -type_text)

pbp <- pbp %>% mutate(score_diff = ifelse(row_num %% 2 == 1, lead(score_diff), score_diff), home_adv = ifelse(row_num %% 2 == 1, lead(home_adv), home_adv), win = ifelse(row_num %% 2 == 1, lead(win), win))
pbp <- pbp %>% select(-row_num)

dummy <- dummyVars(" ~ .-team", data = pbp)
final_data <- data.frame(predict(dummy, newdata = pbp))

final_data$team <- pbp$team
final_data <- final_data %>% select(id, season, game_id, player, team, everything())

xgboost_train <- final_data %>%
  filter(season < 2022) 

xgboost_test <- final_data %>%
  filter(season >= 2022) 

labels_train <- as.matrix(xgboost_train[, 21]) 
xgboost_trainfinal <- as.matrix(xgboost_train[, c(6:20)]) 
xgboost_testfinal <- as.matrix(xgboost_test[, c(6:20)])

reb_wpa_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

wpa_preds <- as.data.frame(
  matrix(predict(reb_wpa_model, as.matrix(final_data[,c(6:20)])))
)

wpa_preds <- wpa_preds %>% mutate(row_num = row_number(), r_wpa = V1 - lag(V1)) %>% filter(row_num %% 2 == 0) %>% select(r_wpa)

stats_2024 <- final_data %>% mutate(row_num = row_number()) %>% filter(row_num %% 2 == 0) %>% cbind(wpa_preds) %>%
  filter(season == 2024) %>%
  group_by(player) %>%
  summarize(team = first(team), rebounds = n(), r_wpa = mean(r_wpa)) %>%
  arrange(-r_wpa)

stats_2024_filt <- stats_2024 %>% filter(rebounds >= 100, r_wpa > 0, r_wpa < 1)

dist <- fitdistrplus::fitdist(stats_2024_filt$r_wpa, "beta")

dist_alpha <- dist$estimate['shape1']
dist_beta <- dist$estimate['shape2']

stats_2024 <- stats_2024 %>%
  mutate(iri = (r_wpa * rebounds + dist_alpha)/(rebounds + dist_alpha + dist_beta))

players <- load_nba_player_box() %>%
  distinct(player = athlete_id, name = athlete_display_name)

stats_2024 <- stats_2024 %>%
  arrange(-iri) %>%
  mutate(iri = scale(iri)) %>%
  left_join(players, by = "player")

t10 <- stats_2024 %>% head(10) %>% dplyr::select(player, name, team, rebounds, iri) %>%
  rowwise() %>%
  mutate(headshot_url = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/", player, ".png&w=350&h=254"))

teams <- espn_nba_teams() %>% dplyr::select(team = abbreviation, logo)

t10 <- t10 %>% left_join(teams, by = "team") %>% dplyr::select(headshot_url, name, logo, rebounds, iri)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>hoopR</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

table <- t10 %>% mutate(iri = round(iri, 2)) %>% gt() %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_img_rows(columns = headshot_url, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot_url, name, logo, rebounds, iri)
  ) %>%
  gt_hulk_col_numeric(c(rebounds, iri)) %>%
  cols_label(
    headshot_url = md(""),
    name = md("**Player**"),
    logo = md("**Team**"),
    rebounds = md("**REB**"),
    iri = md("**IRI**")
  ) %>%
  tab_header(
    title = "2023/24 NBA Impactful Rebounding Index (IRI) Leaders",
    subtitle = md("*Based On **Win Probability Added** | **IRI** Is A **Scaled** Value*")
  ) %>% 
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(name, rebounds, iri)
    )
  ) 

gtsave(table, "table.png", vwidth = 1500, vheight = 1000, zoom = 1)