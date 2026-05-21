# Install packages
install.packages("nflfastR")
install.packages("broom.mixed")
install.packages("lme4")

# Load packages
library(tidyverse)
library(broom.mixed)
library(lme4)
library(nflfastR)
library(splines)

# Set up global variables
current_year <- 2024
historical_seasons <- 20
all_seasons <- (current_year - historical_seasons + 1):current_year

# Team names conversion
team_conversion <- tibble(
  old_abbreviation = c("STL", "SD", "OAK"),
  new_abbreviation = c("LA", "LAC", "LV"),
  move_season = c(2016, 2017, 2020)
)

convert_team_abbreviation <- function (data, team_conversion, col_name = "team") {
  data <- data %>%
    rename(team_rename = all_of(col_name))
  # Forward conversion: old abbreviation to new abbreviation
  data <- data %>%
    left_join(team_conversion, by = c("team_rename" = "old_abbreviation")) %>%
    mutate(team_rename = ifelse(season >= move_season & !is.na(new_abbreviation), new_abbreviation, team_rename),
           move_season = NULL,
           new_abbreviation = NULL)
  # Reverse conversion: new abbreviation to old abbreviation
  data <- data %>%
    left_join(team_conversion, by = c("team_rename" = "new_abbreviation")) %>%
    mutate(team_rename = ifelse(season < move_season & !is.na(old_abbreviation), old_abbreviation, team_rename),
           move_season = NULL,
           old_abbreviation = NULL)
  # Rename column(s) back
  data <- data %>%
    rename_at(vars(team_rename), ~ str_replace(.x, "team_rename", col_name))
  return(data)
}

# Load data from nflfastR
nfl_schedule <- nflreadr::load_schedules()
nfl_pbp <- load_pbp(all_seasons)

# Explore data
head(nfl_pbp)
glimpse(nfl_pbp)
str(nfl_pbp)

# Data cleaning
# Indicator column for home/away/neutral
nfl_pbp <- nfl_pbp %>%
  mutate(posteam_site_id = case_when(location == "Neutral" ~ 0,
                                     posteam == home_team ~ 1,
                                     posteam == away_team ~ -1,
                                     TRUE ~ NA_real_),
         posteam_site = case_when(location == "Neutral" ~ "neutral",
                                  posteam == home_team ~ "home",
                                  posteam == "away_team" ~ "away",
                                  TRUE ~ NA_character_))
# Convert team abbreviations
nfl_pbp <- convert_team_abbreviation(data = nfl_pbp, team_conversion = team_conversion, col_name = "posteam")
nfl_pbp <- convert_team_abbreviation(data = nfl_pbp, team_conversion = team_conversion, col_name = "defteam")

# Data filtering and calculate EPA per game
nfl_epa_game <- nfl_pbp %>%
  filter(!play_type %in% c("qb_kneel", "qb_spike", "kickoff"),
         between(wp, 0.05, 0.95),
         !is.na(posteam),
         !is.na(epa)) %>%
  group_by(game_id, season, posteam, defteam, posteam_site_id, posteam_site) %>%
  summarize(epa_game = sum(epa),
            epa_play = mean(epa),
            plays = n()) %>%
  group_by(season) %>%
  mutate(epa_play = epa_play - weighted.mean(epa_play, w = plays),
         epa_game = epa_game - weighted.mean(epa_game, w = plays),
         epa_per_70 = epa_play * 70) %>%
  ungroup()
nfl_epa_game

# Add predictors from nfl_schedule
nfl_epa_game <- nfl_epa_game %>%
  left_join(nfl_schedule %>% select(game_id, home_team, away_team, roof, surface, referee, home_rest, away_rest),
            by = ("game_id" = "game_id")) %>%
  mutate(team_rest = ifelse(posteam == home_team, home_rest, away_rest),
         opp_rest = ifelse(posteam == home_team, away_rest, home_rest),
         team_rest_effect = team_rest - opp_rest,
         indoors = ifelse(roof == "dome" | roof == "closed", 1, 0),
         grass = ifelse(str_detect(surface, "grass"), 1, 0))

# Create unique variable for each team/defteam and season
nfl_epa_game <- nfl_epa_game %>%
  mutate(team_season = paste0(posteam, "-", season),
         defteam_season = paste0(defteam, "-", season))
nfl_epa_game

# Mixed effects model
epa_game_model <- lmer(epa_per_70 ~ bs(team_rest_effect, df = 3) +
                         indoors +
                         grass +
                         (1 | team_season) +
                         (1 | defteam_season) +
                         factor(season) +
                         factor(season):posteam_site_id,
                       data = nfl_epa_game)

# Power rating
team_power_rating <- tidy(epa_game_model, effects = 'ran_vals') %>%
    filter(str_detect(group, "team_season|defteam_season")) %>%
    mutate(side = case_when(group == "team_season" ~ "off",
                            group == "defteam_season" ~ "def",
                            TRUE ~ NA_character_)) %>%
    filter(!is.na(side)) %>%
    mutate(team = str_replace(substr(level, start = 1, stop = 3), "-", "") %>% as.character(),
           season = str_extract(level, "[[:digit:]]{4}"),
           estimate = ifelse(side == "def", -estimate, estimate)) %>%
    select(team, season, side, estimate) %>%
    pivot_wider(values_from = "estimate", names_from = "side") %>%
    mutate(tot = off + def)
team_power_rating
team_power_rating %>%
  filter(season == 2024) %>%
  arrange(desc(tot)) %>%
  print(n = 32)

# Filter for Super Bowl teams
team_power_rating %>%
  mutate(across(off:tot, ~round(.x, 2))) %>%
  filter(season == 2024, team %in% c("KC", "PHI"))

# Extracting home effects
home_effects<- tidy(epa_game_model) %>%
  filter(str_detect(term, "posteam_site")) %>%
  mutate(season = str_extract(term, "[[:digit:]]{4}"),
         home_vs_away_full_game = estimate * 4)

home_effects %>%
  select(season, estimate, home_vs_away_full_game) %>%
  mutate(across(c(estimate, home_vs_away_full_game), ~round(.x, 2))) %>%
  rename("EPA vs Neutral 70 Plays" = estimate,
         "Season" = season,
         "EPA vs Road per Game" = home_vs_away_full_game)

avg_hfa <- home_effects %>%
  summarize(avg_hfa = mean(home_vs_away_full_game))
home_effects %>%
  ggplot(aes(x = as.numeric(season), y= home_vs_away_full_game)) +
  geom_line() +
  geom_hline(data = avg_hfa, aes(yintercept = avg_hfa), linetype = 2, col = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  theme_minimal() +
  labs(title = "Home Field Advantage by Year",
       subtitle = "2005-2024",
       x = "EPA/game",
       y = "Season")

# Do penalties explain home field advantage?
nfl_pbp %>%
  filter(!is.na(penalty), penalty == 1) %>%
  select(contains("penalty"))

# Exploratory data analysis for penalties
avg_penalty_by_site <- nfl_pbp %>%
  filter(!is.na(penalty), penalty == 1) %>%
  group_by(season, posteam, posteam_site, game_id) %>%
  summarize(penalties = sum(penalty),
            penalty_yards = sum(penalty_yards)) %>%
  group_by(season, posteam, posteam_site) %>%
  summarize(avg_penalties = mean(penalties),
            avg_penalty_yards = mean(penalty_yards),
            .groups = 'drop') %>%
  pivot_wider(names_from = posteam_site,
              values_from = c(avg_penalties, avg_penalty_yards)) %>%
  mutate(home_vs_away_penalties = avg_penalties_home - avg_penalties_NA,
         home_vs_away_penalty_yards = avg_penalty_yards_home - avg_penalty_yards_NA) %>%
  rename(avg_penalties_away = avg_penalties_NA,
         avg_penalty_yards_away = avg_penalty_yards_NA)

avg_penalty_by_site %>%
  filter(season == 2024) %>%
  arrange(desc(home_vs_away_penalty_yards)) %>%
  mutate(row_num = 1:n()) %>%
  select(-contains("neutral")) %>%
  select(row_num, everything()) %>%
  mutate_at(vars(avg_penalties_home:home_vs_away_penalty_yards), ~ round(.x, digits = 1)) %>%
  rename("Row" = row_num,
         "Avg Penalties (Home)" = avg_penalties_home,
         "Avg Penalties (Away)" = avg_penalties_away,
         "Avg Penalty Yards (Home)" = avg_penalty_yards_home,
         "Avg Penalty Yards (Away)" = avg_penalty_yards_away,
         "Home vs Away Penalties" = home_vs_away_penalties,
         "Home vs Away Penalty Yards" = home_vs_away_penalty_yards) %>%
  print(n = 32)