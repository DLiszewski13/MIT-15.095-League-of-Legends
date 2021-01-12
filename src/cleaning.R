################### ML PROJECT #########################
########### Data Cleaning ##############

# Libraries ---------------------------------------------------------------

##### Loading Libraries
library(tidyverse) 
library(lubridate)
library(purrr)

##### Set working directory
# David's wd
# setwd("/Users/pan/Dropbox (MIT)/ML_league")
setwd("~/Dropbox (MIT)/ML_league")

# Reading / Merging Data --------------------------------------------------


##### Reading in data
lol_2014 <- read_csv("data_source/2014_LoL_esports_match_data_from_OraclesElixir_20201023.csv")
lol_2015 <- read_csv("data_source/2015_LoL_esports_match_data_from_OraclesElixir_20201023.csv")
lol_2016 <- read_csv("data_source/2016_LoL_esports_match_data_from_OraclesElixir_20201023.csv")
lol_2017 <- read_csv("data_source/2017_LoL_esports_match_data_from_OraclesElixir_20201023.csv")
lol_2018 <- read_csv("data_source/2018_LoL_esports_match_data_from_OraclesElixir_20201023.csv")
lol_2019 <- read_csv("data_source/2019_LoL_esports_match_data_from_OraclesElixir_20201023.csv")
lol_2020 <- read_csv("data_source/2020_LoL_esports_match_data_from_OraclesElixir_20201023.csv")

##### Merging datasets
lol_data <- lol_2014 %>%
  merge(lol_2015, all.x = TRUE, all.y = TRUE) %>%
  merge(lol_2016, all.x = TRUE, all.y = TRUE) %>%
  merge(lol_2017, all.x = TRUE, all.y = TRUE) %>%
  merge(lol_2018, all.x = TRUE, all.y = TRUE) %>%
  merge(lol_2019, all.x = TRUE, all.y = TRUE) %>%
  merge(lol_2020, all.x = TRUE, all.y = TRUE)
lol_data <- as_tibble(lol_data)

### Removes original data files for cleanliness
rm(lol_2014,lol_2015,lol_2016,lol_2017,lol_2018,lol_2019,lol_2020)

# Filtering Data ----------------------------------------------------------

# renaming columns
names(lol_data) <- sub(" ", "_", names(lol_data))
# Figuring out which matches do not have full data or no gameid
lol_bad_gameids <- lol_data %>% 
  group_by(gameid) %>% 
  summarise(n=n()) %>% 
  filter(n != 12)
# Analyzing partial data rows
lol_partial_data <- lol_data %>%
  filter(datacompleteness == "partial")
# gameids with partial data
lol_partial_games <- lol_partial_data %>%
  group_by(gameid) %>%
  summarise(n())
# rows with dpm missing
lol_no_dpm <- lol_partial_data %>%
  filter(is.na(dpm)) %>%
  group_by(gameid) %>%
  summarise(n())
# kills outliers
lol_kill_outliers <- lol_data %>%
  filter(kills > 30 | minionkills > 2000) %>%
  group_by(gameid) %>%
  summarise(n())
# Establishing periods for the purpose of aggregation statistics
month_group_1 <- c(1:5)
month_group_2 <- c(6:12)

# Performing filtering
lol_data_new <- lol_data %>%
  # Filtering gameid with players missing, missing gameids
  filter(!is.na(gameid)) %>%
  filter(!(gameid %in% c(lol_bad_gameids$gameid))) %>%
  # Filtering gameids with rows that do not contain dpm information
  filter(!(gameid %in% c(lol_no_dpm$gameid))) %>%
  # Removing gameids that have partial data
  filter(!(gameid %in% c(lol_partial_games$gameid))) %>%
  # Removing gameids with users that have an outlier amount of kills or minion kills
  filter(!(gameid %in% c(lol_kill_outliers$gameid))) %>%
  # Removing draws from dataset
  filter(gameid != "3627-4571") %>%
  filter(gameid != "3627-4572") %>%
  # Removing Leagues that have few / inconsistent games (to avoid NA's in stat calculations)
  filter(!(league %in% c("CIS CL", "IEM", "TCS", "DC", "TRA"))) %>%
  # REmoving unnecesary rows
  select(-c(datacompleteness,url,29:32,39:63,visionscore,vspm)) %>%
  # Creating row to designate if a row contains player data or team data
  mutate(player_row = ifelse(position == "team",0,1)) %>%
  filter(player_row==1) %>%
  mutate(datetime = date) %>%
  mutate(date = as_date(datetime)) %>%
  arrange(datetime) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  # Creating aggregate period (6 months) for summary statistics
  mutate(agg_period = case_when(year==2017 & month %in% month_group_2 ~ '2017.2',
                                year==2018 & month %in% month_group_1 ~ '2018.1',
                                year==2018 & month %in% month_group_2 ~ '2018.2',
                                year==2019 & month %in% month_group_1 ~ '2019.1',
                                year==2019 & month %in% month_group_2 ~ '2019.2',
                                year==2020 & month %in% month_group_1 ~ '2020.1',
                                year==2020 & month %in% month_group_2 ~ '2020.2'))

rm(month_group_1,month_group_2, lol_bad_gameids, lol_kill_outliers, lol_no_dpm, lol_partial_data, lol_partial_games)

# PAST 6 MONTHS STATS ------------------------------------------------

##### CHAMPION STATS
lol_num_matches <- length(unique(lol_data_new$gameid))

champ_data_6mon <- lol_data_new %>%
  group_by(agg_period) %>%
  mutate(period_games = n_distinct(gameid)) %>%
  ungroup() %>%
  group_by(agg_period,champion) %>%
  summarise(cwr_6mon = mean(result),
            cpr_6mon = n() / mean(period_games)) %>%
  filter(!is.na(agg_period)) %>% 
  ungroup()

champ_data_6mon <- lol_data_new %>%
  group_by(agg_period) %>%
  mutate(period_games = n_distinct(gameid)) %>%
  ungroup() %>%
  group_by(agg_period,champion,position) %>%
  summarise(cwr_role_6mon = mean(result),
            cpr_role_6mon = n() / mean(period_games)) %>%
  filter(!is.na(agg_period)) %>%
  ungroup() %>%
  right_join(champ_data_6mon)

# Determing number of periods to filter by
periods <- unique(lol_data_new$agg_period)
periods <- periods[!is.na(periods)]

# Filtering to get unique df for each period
ch_data_list_6mon <- vector("list", length(periods))
names(ch_data_list_6mon) <- periods
for (i in 1:length(periods)){
  ch_data_list_6mon[[i]] <- champ_data_6mon %>% 
    filter(agg_period==periods[i]) %>% 
    select(-agg_period)
}

##### PLAYER STATS

# Assigning 1 to 0's to avoid dividing by 0 in calculations 
lol_data_new$deaths[lol_data_new$deaths == 0] <- 1
lol_data_new$teamkills[lol_data_new$teamkills == 0] <- 1
lol_data_new$teamdeaths[lol_data_new$teamdeaths == 0] <- 1

# Player stats for the player in a certain position
player_stats_6mon <- lol_data_new %>%
  mutate(kda = (kills+assists)/deaths,
         r_kda = (kills+assists)/teamkills,
         r_death = deaths/teamdeaths) %>%
  group_by(player,agg_period,position) %>%
  summarize(avg_kda_6mon = mean(kda,na.rm = TRUE),
            avg_rkda_6mon = mean(r_kda,na.rm = TRUE),
            avg_death_6mon = mean(r_death,na.rm = TRUE),
            avg_dpm_6mon = mean(dpm,na.rm = TRUE),
            avg_wardsplaced_6mon = mean(wardsplaced,na.rm = TRUE),
            avg_wardskilled_6mon = mean(wardskilled,na.rm = TRUE),
            avg_cs_6mon = mean(total_cs,na.rm = TRUE),
            avg_mk_6mon = mean(monsterkills,na.rm = TRUE),
            avg_mk_enemy_6mon = mean(monsterkillsenemyjungle,na.rm = TRUE),
            avg_goldspent_6mon = mean(goldspent,na.rm = TRUE),
            avg_csdiff_10_6mon = mean(csdiffat10,na.rm = TRUE),
            avg_gddiff_10_6mon = mean(golddiffat10,na.rm = TRUE),
            avg_xpdiff_10_6mon = mean(xpdiffat10,na.rm = TRUE),
            avg_csdiff_15_6mon = mean(csdiffat15,na.rm = TRUE),
            avg_gddiff_15_6mon = mean(golddiffat15,na.rm = TRUE),
            avg_xpdiff_15_6mon = mean(xpdiffat15,na.rm = TRUE),
            player_wr_6mon = sum(result)/n(),
            player_fb_6mon = sum(firstblood)/n(),
            player_fbvictim_6mon = sum(firstbloodvictim)/n()) %>% 
  filter(!is.na(agg_period)) %>%
  ungroup() 

# Filtering to get unique df for each period
pl_data_list_6mon <- vector("list", length(periods))
names(pl_data_list_6mon) <- periods
for (i in 1:length(periods)){
  pl_data_list_6mon[[i]] <- player_stats_6mon %>% 
    filter(agg_period==periods[i]) %>% 
    select(-agg_period)
}

# Player stats for an individual champion
player_stats_champ_6mon <- lol_data_new %>%
  group_by(player,agg_period,champion,position) %>%
  summarize(n_games_champions_6mon = n()) %>%
  filter(!is.na(agg_period)) %>%
  ungroup() 

# Filtering to get unique df for each period
pl_data_ch_list_6mon <- vector("list", length(periods))
names(pl_data_ch_list_6mon) <- periods
for (i in 1:length(periods)){
  pl_data_ch_list_6mon[[i]] <- player_stats_champ_6mon %>% 
    filter(agg_period==periods[i]) %>% 
    select(-agg_period)
}


# LAST YEAR STATS --------------------------------------------------

##### CHAMP STATS
champ_data_year <- lol_data_new %>%
  group_by(year) %>%
  mutate(period_games = n_distinct(gameid)) %>%
  ungroup() %>%
  group_by(year,champion) %>%
  summarise(cwr_year = mean(result),
            cpr_year = n() / mean(period_games)) %>%
  filter(!is.na(year)) %>% 
  ungroup()

champ_data_year <- lol_data_new %>%
  group_by(year) %>%
  mutate(period_games = n_distinct(gameid)) %>%
  ungroup() %>%
  group_by(year,champion,position) %>%
  summarise(cwr_role_year = mean(result),
            cpr_role_year = n() / mean(period_games)) %>%
  filter(!is.na(year)) %>%
  ungroup() %>%
  right_join(champ_data_year)

# Determing number of years to filter by
years <- unique(lol_data_new$year)
years <- years[!is.na(years)]

# Filtering to get unique df for each year
ch_data_list_year <- vector("list", length = length(years))
names(ch_data_list_year) <- years
for (i in 1:length(years)){
  ch_data_list_year[[i]] <- champ_data_year %>% 
    filter(year==years[i]) %>% 
    select(-year)
}

##### PLAYER STATS
# Player stats for the player in a certain position
lol_data_new$deaths[lol_data_new$deaths == 0] <- 1
lol_data_new$teamkills[lol_data_new$teamkills == 0] <- 1
lol_data_new$teamdeaths[lol_data_new$teamdeaths == 0] <- 1

player_stats_year <- lol_data_new %>%
  mutate(kda = (kills+assists)/deaths,
         r_kda = (kills+assists)/teamkills,
         r_death = deaths/1,teamdeaths) %>%
  group_by(player,year,position) %>%
  summarize(avg_kda_year = mean(kda,na.rm = TRUE),
            avg_rkda_year = mean(r_kda,na.rm = TRUE),
            avg_death_year = mean(r_death,na.rm = TRUE),
            avg_dpm_year = mean(dpm,na.rm = TRUE),
            avg_wardsplaced_year = mean(wardsplaced,na.rm = TRUE),
            avg_wardskilled_year = mean(wardskilled,na.rm = TRUE),
            avg_cs_year = mean(total_cs,na.rm = TRUE),
            avg_mk_year = mean(monsterkills,na.rm = TRUE),
            avg_mk_enemy_year = mean(monsterkillsenemyjungle,na.rm = TRUE),
            avg_goldspent_year = mean(goldspent,na.rm = TRUE),
            avg_csdiff_10_year = mean(csdiffat10,na.rm = TRUE),
            avg_gddiff_10_year = mean(golddiffat10,na.rm = TRUE),
            avg_xpdiff_10_year = mean(xpdiffat10,na.rm = TRUE),
            avg_csdiff_15_year = mean(csdiffat15,na.rm = TRUE),
            avg_gddiff_15_year = mean(golddiffat15,na.rm = TRUE),
            avg_xpdiff_15_year = mean(xpdiffat15,na.rm = TRUE),
            player_wr_year = sum(result)/n(),
            player_fb_year = sum(firstblood)/n(),
            player_fbvictim_year = sum(firstbloodvictim)/n()
  ) %>% 
  filter(!is.na(year)) %>%
  ungroup() 

# Filtering to get unique df for each year
pl_data_list_year <- vector("list", length(years))
names(pl_data_list_year) <- years
for (i in 1:length(years)){
  pl_data_list_year[[i]] <- player_stats_year %>% 
    filter(year==years[i]) %>% 
    select(-year)
}

player_stats_champ_year <- lol_data_new %>%
  group_by(player,year,champion,position) %>%
  summarize(n_games_champions_year = n()) %>%
  filter(!is.na(year)) %>%
  ungroup() 

# Filtering to get unique df for each year
pl_data_ch_list_year <- vector("list", length(years))
names(pl_data_ch_list_year) <- years
for (i in 1:length(years)){
  pl_data_ch_list_year[[i]] <- player_stats_champ_year %>% 
    filter(year==years[i]) %>% 
    select(-year)
}

# RESTRUCTURING DATASET --------------------------------------------------

# Subsetting the useful variables from full dataset of games
useful_data <- lol_data_new %>%
  select(c(1:21,73:77)) %>%
  # filter(!is.na(agg_period)) 
  filter(year >= 2017)

##### Gathering games that matter for periods were focusing on

period_data_list <- vector("list", length(2:6))
names(period_data_list) <- periods[2:6]
## Go from 2: length-1 (6) because we only want observations (games) from 2018-beginning 2020
for (i in 2:6){
  i_name <- as.character(periods[i])
  i_prev <- as.character(periods[i-1])
  period_data_list[[i_name]] <- useful_data %>%
    filter(agg_period==periods[i]) %>% 
    left_join(pl_data_list_6mon[[i_prev]]) %>%
    left_join(pl_data_ch_list_6mon[[i_prev]]) %>%
    left_join(ch_data_list_6mon[[i_prev]]) %>%
    pivot_wider(id_cols = c(gameid,side,result,league,playoffs,game,gamelength,patch,
                            agg_period,year,month,day,date,datetime),  
                names_from = position,
                values_from = c(player,champion,27:50))
}

year_data_list <- vector("list", 3)
names(year_data_list) <- c("2018","2019","2020")
## Go from 5:7  because we only want data observations (games) from 2018 - 2020
for (i in 2018:2020){
  # i-4 so that index starts at 1
  i_name <- as.character(i)
  i_prev <- as.character(i-1)
  year_data_list[[i_name]] <- useful_data %>%
    filter(year==i) %>%
    left_join(pl_data_list_year[[i_prev]]) %>%
    left_join(pl_data_ch_list_year[[i_prev]]) %>%
    left_join(ch_data_list_year[[i_prev]]) %>%
    pivot_wider(id_cols = c(gameid,side,result,league,playoffs,game,gamelength,patch,
                            agg_period,year,month,day,date,datetime),  
                names_from = position,
                values_from = c(player,champion,27:50))
}

##### Joining previous period summary stats to each game from period of interest
# Ex: year 2018 games have year 2017 summary stats

year_data_full <- year_data_list$'2018' %>%
  full_join(year_data_list$'2019') %>%
  full_join(year_data_list$'2020')

match_data_full <- period_data_list$'2018.1' %>%
  full_join(period_data_list$'2018.2') %>%
  full_join(period_data_list$'2019.1') %>%
  full_join(period_data_list$'2019.2') %>%
  full_join(period_data_list$'2020.1') %>%
  full_join(year_data_full)


rm(player_stats_6mon,player_stats_champ_6mon,player_stats_champ_year,player_stats_year,
   champ_data_6mon,champ_data_year,
   pl_data_ch_list_6mon,pl_data_ch_list_year,pl_data_list_6mon,pl_data_list_year,
   ch_data_list_6mon,ch_data_list_year,
   i,i_name,i_prev,lol_num_matches, periods, years)

# COMBINING WITH ENEMY DATA ----------------------------------------------------

##### Using blue side as the result were trying to predict against the enemies on the red side
# match_data <- match_data_full %>%
#   pivot_wider(id_cols = c(gameid,4:14),
#               names_from = side,
#               values_from = c(15:264)) %>%
#   left_join(match_data_full %>% select(gameid,player_top,result),
#             by = c("gameid", "player_top_Blue" = "player_top"))


##### Including enemy data but keeping two rows
# cols for determing which rows to keep/remove when moving to one row
match_data_full$obs_num <- seq.int(nrow(match_data_full))
match_data_full$unofficial_match_num <- rep(seq.int(nrow(match_data_full) / 2), each = 2)

match_data_full_red <- match_data_full %>% filter(side=="Red")
match_data_full_blue <- match_data_full %>% filter(side=="Blue")
# Adding red sides as enemies to blue rows
match_data_full_blue_2 <- match_data_full_blue %>%
  full_join(match_data_full_red,
            by = c("gameid","league","playoffs","game","gamelength","patch",
                   "agg_period","year","month","day","date","datetime"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))
# Adding blue sides as enemies to red rows
match_data_full_red_2 <- match_data_full_red %>%
  full_join(match_data_full_blue,
            by = c("gameid","league","playoffs","game","gamelength","patch",
                   "agg_period","year","month","day","date","datetime"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))

## FINAL DATASET FOR USE (2 rows)
match_data <- full_join(match_data_full_blue_2,match_data_full_red_2)

##### CODE TO GET ONE ROW INSTEAD OF 2

## FINAL DATASET FOR USE (1 rows)
match_data_single <- match_data %>%
  filter( (obs_num %% 2 == 0 & unofficial_match_num %% 2 == 0) |
            (obs_num %% 2 == 1 & unofficial_match_num %% 2 == 1))

rm(match_data_full_blue,match_data_full_blue_2,match_data_full_red,match_data_full_red_2)

# yay


# EXPORTING DATA ----------------------------------------------------------


############# Export data
match_data$playoffs <- as.factor(match_data$playoffs)
match_data$result <- as.factor(match_data$result)
match_data$patch <- as.factor(match_data$patch)
match_data$year <- as.factor(match_data$year)
match_data$month <- as.factor(match_data$month)
match_data$day <- as.factor(match_data$day)
str(match_data)

match_data_cleaned <- match_data %>% select(-c('game', 'agg_period', 'date', 'datetime','gameid' ))
str(match_data_cleaned[,1:80])

train_2y1y <- match_data_cleaned %>% filter(year != 2020)
test_2y1y <- match_data_cleaned %>% filter(year == 2020)

match_data_cleaned_selectedfeatures <- match_data_cleaned %>% select(-c('result', 'patch', 'month','day','player_top' 
                                                                        ,'player_jng','player_mid','player_bot','player_sup'
                                                                        ,'unofficial_match_num','side_enemy','player_top_enemy'
                                                                        ,'player_jng_enemy','player_mid_enemy','player_bot_enemy'
                                                                        ,'player_sup_enemy','obs_num'))
train_2y1y_champ <- match_data_cleaned_selectedfeatures %>% filter(year != 2020) %>% select(-c('year'))
test_2y1y_champ <- match_data_cleaned_selectedfeatures %>% filter(year == 2020) %>% select(-c('year'))

match_data_cleaned_selectedfeatures_nochampion <- match_data_cleaned %>% select(-c('result', 'patch', 'month','day','player_top' 
                                                                        ,'player_jng','player_mid','player_bot','player_sup'
                                                                        ,'unofficial_match_num','side_enemy','player_top_enemy'
                                                                        ,'player_jng_enemy','player_mid_enemy','player_bot_enemy'
                                                                        ,'player_sup_enemy','obs_num','champion_top_enemy'
                                                                        ,'champion_jng_enemy','champion_mid_enemy'
                                                                        ,'champion_bot_enemy','champion_sup_enemy','champion_top' 
                                                                        ,'champion_jng','champion_mid','champion_bot','champion_sup'))
train_2y1y_nochamp <- match_data_cleaned_selectedfeatures_nochampion %>% filter(year != 2020) %>% select(-c('year'))
train_2y1y_nochamp_2019 <- match_data_cleaned_selectedfeatures_nochampion %>% filter(year == 2019) %>% select(-c('year'))
test_2y1y_nochamp <- match_data_cleaned_selectedfeatures_nochampion %>% filter(year == 2020) %>% select(-c('year'))

# Export data to CSV
write.csv(train_2y1y,"/Users/pan/Dropbox (MIT)/ML_league/data/train_2y1y_2row.csv", row.names = FALSE)
write.csv(test_2y1y,"/Users/pan/Dropbox (MIT)/ML_league/data/test_2y1y_2row.csv", row.names = FALSE)

write.csv(train_2y1y_champ,"/Users/pan/Dropbox (MIT)/ML_league/data/train_2y1y_2row_champ.csv", row.names = FALSE)

write.csv(train_2y1y_nochamp,"/Users/pan/Dropbox (MIT)/ML_league/data/train_2y1y_2row_nochamp.csv", row.names = FALSE)
write.csv(test_2y1y_nochamp,"/Users/pan/Dropbox (MIT)/ML_league/data/test_2y1y_2row_nochamp.csv", row.names = FALSE)

write.csv(train_2y1y_nochamp_2019,"/Users/pan/Dropbox (MIT)/ML_league/data/train_2y1y_2row_nochamp_2019.csv", row.names = FALSE)

train_2y1y_nochamp_2019[,240:251]

head(train_2y1y_nochamp)


# RECOMBINING IMPUTED DATA WITH ENEMY -------------------------------------

train_2y1y_2row_noenemy_nocat_imputed <- read_csv("../data/train_2y1y_2row_nochamp_noenemy_nocat_imputed.csv")
test_2y1y_2row_noenemy_nocat_imputed <- read_csv("../data/test_2y1y_2row_nochamp_noenemy_nocat_imputed.csv")
train_2y1y_2row <- read_csv("../data/train_2y1y_2row.csv")
test_2y1y_2row <- read_csv("../data/test_2y1y_2row.csv")

train_2y1y_2row_removed <- train_2y1y_2row %>%
  select(c('side', 'league', 'result', 'patch','year', 'month','day',
#           'player_top' ,'player_jng','player_mid','player_bot','player_sup',
            'unofficial_match_num','obs_num','champion_top' 
           ,'champion_jng','champion_mid','champion_bot','champion_sup'))

#, 'side_enemy','player_top_enemy','player_jng_enemy','player_mid_enemy','player_bot_enemy','player_sup_enemy','champion_top_enemy','champion_jng_enemy','champion_mid_enemy','champion_bot_enemy','champion_sup_enemy'

test_2y1y_2row_removed <- test_2y1y_2row %>%
  select(c('side', 'league', 'result', 'patch','year', 'month','day',
#           'player_top' ,'player_jng','player_mid','player_bot','player_sup',
           'unofficial_match_num','obs_num','champion_top' 
           ,'champion_jng','champion_mid','champion_bot','champion_sup'))

#,'player_bot_enemy','player_sup_enemy','champion_top_enemy','champion_jng_enemy','champion_mid_enemy','champion_bot_enemy','champion_sup_enemy', 'side_enemy','player_top_enemy','player_jng_enemy','player_mid_enemy'

train_2y1y_2row_merged <- cbind(train_2y1y_2row_removed,train_2y1y_2row_noenemy_nocat_imputed)
test_2y1y_2row_merged <- cbind(test_2y1y_2row_removed,test_2y1y_2row_noenemy_nocat_imputed)

## TRAIN DATA
train_2y1y_2row_merged_red <- train_2y1y_2row_merged %>% filter(side=="Red")
train_2y1y_2row_merged_blue <- train_2y1y_2row_merged %>% filter(side=="Blue")
# Adding red sides as enemies to blue rows
train_2y1y_2row_merged_blue_2 <- train_2y1y_2row_merged_blue %>%
  full_join(train_2y1y_2row_merged_red,
            by = c("league","playoffs","gamelength","patch",
                   "year","month","day"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))

# Adding blue sides as enemies to red rows
train_2y1y_2row_merged_red_2 <- train_2y1y_2row_merged_red %>%
  full_join(train_2y1y_2row_merged_blue,
            by = c("league","playoffs","gamelength","patch",
                   "year","month","day"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))

## FINAL DATASET FOR USE (2 rows)
train_2y1y_2row_merged_enemy <- full_join(train_2y1y_2row_merged_blue_2,train_2y1y_2row_merged_red_2) %>%
                                    select(-c(day, month, obs_num, unofficial_match_num, side_enemy))
train_2y1y_2row_merged_enemy$year = as.integer(train_2y1y_2row_merged_enemy$year)
train_2y1y_2row_merged_enemy$result = as.factor(train_2y1y_2row_merged_enemy$result)
train_2y1y_2row_merged_enemy$playoffs = as.factor(train_2y1y_2row_merged_enemy$playoffs)

rm(train_2y1y_2row_merged_blue,train_2y1y_2row_merged_blue_2,train_2y1y_2row_merged_red,train_2y1y_2row_merged_red_2)

## TEST DATA
test_2y1y_2row_merged_red <- test_2y1y_2row_merged %>% filter(side=="Red")
test_2y1y_2row_merged_blue <- test_2y1y_2row_merged %>% filter(side=="Blue")
# Adding red sides as enemies to blue rows
test_2y1y_2row_merged_blue_2 <- test_2y1y_2row_merged_blue %>%
  full_join(test_2y1y_2row_merged_red,
            by = c("league","playoffs","gamelength","patch",
                   "year","month","day"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))
# Adding blue sides as enemies to red rows
test_2y1y_2row_merged_red_2 <- test_2y1y_2row_merged_red %>%
  full_join(test_2y1y_2row_merged_blue,
            by = c("league","playoffs","gamelength","patch",
                   "year","month","day"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))

## FINAL DATASET FOR USE (2 rows)
test_2y1y_2row_merged_enemy <- full_join(test_2y1y_2row_merged_blue_2,test_2y1y_2row_merged_red_2) %>%
  select(-c(day, month, obs_num, unofficial_match_num, side_enemy))
test_2y1y_2row_merged_enemy$year = as.integer(test_2y1y_2row_merged_enemy$year)
test_2y1y_2row_merged_enemy$result = as.factor(test_2y1y_2row_merged_enemy$result)
test_2y1y_2row_merged_enemy$playoffs = as.factor(test_2y1y_2row_merged_enemy$playoffs)

rm(test_2y1y_2row_merged_blue,test_2y1y_2row_merged_blue_2,test_2y1y_2row_merged_red,test_2y1y_2row_merged_red_2)

test_2y1y_2row_merged_enemy <- test_2y1y_2row_merged_enemy %>%
  filter(!(league %in% c("MSC", "PCS", "Rio")))

### EXPORT DATA
write.csv(train_2y1y_2row_merged_enemy,"../data/train_2y1y_2row_imputed_full.csv", row.names = FALSE)
write.csv(test_2y1y_2row_merged_enemy,"../data/test_2y1y_2row_imputed_full.csv", row.names = FALSE)



# RECOMBING WITH ENEMY DATA (1 YEAR, 1 ROW) ------------------------------

train_2y1y_2row_noenemy_nocat_imputed <- read_csv("../data/train_2y1y_2row_nochamp_noenemy_nocat_imputed.csv")
train_2y1y_2row <- read_csv("../data/train_2y1y_2row.csv")
train_2y1y_2row_removed <- train_2y1y_2row %>%
  select(c('side', 'league', 'result', 'patch','year', 'month','day',
           #           'player_top' ,'player_jng','player_mid','player_bot','player_sup',
           'unofficial_match_num','obs_num','champion_top' 
           ,'champion_jng','champion_mid','champion_bot','champion_sup'))
train_2y1y_2row_merged <- cbind(train_2y1y_2row_removed,train_2y1y_2row_noenemy_nocat_imputed)

train_2row_2019 <- train_2y1y_2row_merged %>%
  filter(year == 2019)

## Combining enemy data
train_2row_2019_red <- train_2row_2019 %>% filter(side=="Red")
train_2row_2019_blue <- train_2row_2019 %>% filter(side=="Blue")
# Adding red sides as enemies to blue rows
train_2row_2019_blue_2 <- train_2row_2019_blue %>%
  full_join(train_2row_2019_red,
            by = c("league","playoffs","gamelength","patch",
                   "year","month","day"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))

# Adding blue sides as enemies to red rows
train_2row_2019_red_2 <- train_2row_2019_red %>%
  full_join(train_2row_2019_blue,
            by = c("league","playoffs","gamelength","patch",
                   "year","month","day"),
            suffix = c("","_enemy")) %>%
  select(-c(result_enemy, obs_num_enemy, unofficial_match_num_enemy))

## FINAL DATASET FOR USE (2 rows)
train_2row_2019_enemy <- full_join(train_2row_2019_blue_2,train_2row_2019_red_2) 
train_2row_2019_enemy$year = as.integer(train_2row_2019_enemy$year)
train_2row_2019_enemy$result = as.factor(train_2row_2019_enemy$result)
train_2row_2019_enemy$playoffs = as.factor(train_2row_2019_enemy$playoffs)

rm(train_2row_2019_blue,train_2row_2019_blue_2,train_2row_2019_red,train_2row_2019_red_2)

## FINAL DATASET FOR USE (1 row)
data_1row_2019 <- train_2row_2019_enemy %>%
  filter( (obs_num %% 2 == 0 & unofficial_match_num %% 2 == 0) |
            (obs_num %% 2 == 1 & unofficial_match_num %% 2 == 1))

train_1row_2019 <- data_1row_2019 %>%
  filter(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 ) %>%
  select(-c(day, month, obs_num, unofficial_match_num, league))

test_1row_2019 <- data_1row_2019 %>%
  filter(month == 6 | month == 7 | month == 8 | month == 9 | month == 10 | month == 11 | month == 12 ) %>%
  select(-c(day, month, obs_num, unofficial_match_num, league))

# Filtering champions that dont appear in the training set
test_tmp <- test_1row_2019 %>% 
  filter(champion_top %in% train_1row_2019$champion_top) %>% 
  filter(champion_jng %in% train_1row_2019$champion_jng) %>% 
  filter(champion_bot %in% train_1row_2019$champion_bot) %>% 
  filter(champion_mid %in% train_1row_2019$champion_mid) %>% 
  filter(champion_sup %in% train_1row_2019$champion_sup) %>% 
  filter(champion_top_enemy %in% train_1row_2019$champion_top_enemy) %>% 
  filter(champion_jng_enemy %in% train_1row_2019$champion_jng_enemy) %>% 
  filter(champion_bot_enemy %in% train_1row_2019$champion_bot_enemy)%>% 
  filter(champion_mid_enemy %in% train_1row_2019$champion_mid_enemy)%>% 
  filter(champion_sup_enemy %in% train_1row_2019$champion_sup_enemy)

write.csv(train_1row_2019,"../data/train_1row_2019_imputed_full.csv", row.names = FALSE)
write.csv(test_1row_2019,"../data/test_1row_2019_imputed_full.csv", row.names = FALSE)


# Export data for 0.5/0.5 Y2019 (1 YEAR, 1 ROW) ------------------------------
match_data_cleaned <- match_data_single %>% select(-c('game', 'agg_period', 'date', 'datetime','gameid' ))

train_0.5_2019 <- match_data_cleaned %>% filter(year == 2019, month <= 5)
test_0.5_2019 <- match_data_cleaned %>% filter(year == 2019, month > 5)

test_0.5_2019 <- test_0.5_2019 %>% 
  filter(champion_top %in% train_0.5_2019$champion_top) %>% 
  filter(champion_jng %in% train_0.5_2019$champion_jng) %>% 
  filter(champion_bot %in% train_0.5_2019$champion_bot) %>% 
  filter(champion_mid %in% train_0.5_2019$champion_mid) %>% 
  filter(champion_sup %in% train_0.5_2019$champion_sup) %>% 
  filter(champion_top_enemy %in% train_0.5_2019$champion_top_enemy) %>% 
  filter(champion_jng_enemy %in% train_0.5_2019$champion_jng_enemy) %>% 
  filter(champion_bot_enemy %in% train_0.5_2019$champion_bot_enemy)%>% 
  filter(champion_mid_enemy %in% train_0.5_2019$champion_mid_enemy)%>% 
  filter(champion_sup_enemy %in% train_0.5_2019$champion_sup_enemy)

train_0.5_2019_selectedfeatures <- train_0.5_2019 %>% select(-c('result', 'patch','year','month','day','player_top' 
                                                                        ,'player_jng','player_mid','player_bot','player_sup'
                                                                        ,'unofficial_match_num','side_enemy','player_top_enemy'
                                                                        ,'player_jng_enemy','player_mid_enemy','player_bot_enemy'
                                                                        ,'player_sup_enemy','obs_num','champion_top_enemy'
                                                                        ,'champion_jng_enemy','champion_mid_enemy'
                                                                        ,'champion_bot_enemy','champion_sup_enemy','champion_top' 
                                                                        ,'champion_jng','champion_mid','champion_bot','champion_sup'))

test_0.5_2019_selectedfeatures <- test_0.5_2019 %>% select(-c('result', 'patch','year','month','day','player_top' 
                                                                ,'player_jng','player_mid','player_bot','player_sup'
                                                                ,'unofficial_match_num','side_enemy','player_top_enemy'
                                                                ,'player_jng_enemy','player_mid_enemy','player_bot_enemy'
                                                                ,'player_sup_enemy','obs_num','champion_top_enemy'
                                                                ,'champion_jng_enemy','champion_mid_enemy'
                                                                ,'champion_bot_enemy','champion_sup_enemy','champion_top' 
                                                                ,'champion_jng','champion_mid','champion_bot','champion_sup'))


# Export data to CSV
write.csv(train_0.5_2019_selectedfeatures,"/Users/pan/Dropbox (MIT)/ML_league/data/train_0.5_2019.csv", row.names = FALSE)
write.csv(test_0.5_2019_selectedfeatures,"/Users/pan/Dropbox (MIT)/ML_league/data/test_0.5_2019.csv", row.names = FALSE)


# APPEND RESULTS/CHAMP COLS 0.5/0.5 2019 (1 YEAR, 1 ROW) --------------------------

### TRAIN  DATA
train_0.5_2019_nochamp_noenemy_nocat_imputed <- read_csv("data/train_0.5_2019_nochamp_noenemy_nocat_imputed.csv")
## train_0.5_2019 comes from above (HAVE TO RUN ALL LINES, INEFFICIENT)
# train_0.5_2019 <- read_csv("../data/train_0.5_2019.csv")
train_0.5_2019_removed <- train_0.5_2019 %>%
  select(c('side', 'league', 'result', 'patch','year', 'month','day',
           #           'player_top' ,'player_jng','player_mid','player_bot','player_sup',
           'unofficial_match_num','obs_num',
           'champion_top_enemy',
           'champion_jng_enemy','champion_mid_enemy'
           ,'champion_bot_enemy','champion_sup_enemy',
           'champion_top' 
           ,'champion_jng','champion_mid','champion_bot','champion_sup'))
train_0.5_2019_merged <- cbind(train_0.5_2019_removed,train_0.5_2019_nochamp_noenemy_nocat_imputed)

train_0.5_2019_merged <- train_0.5_2019_merged %>%
  select(-c(day, month, obs_num, unofficial_match_num, league))
train_0.5_2019_merged$year = as.integer(train_0.5_2019_merged$year)
train_0.5_2019_merged$result = as.factor(train_0.5_2019_merged$result)
train_0.5_2019_merged$playoffs = as.factor(train_0.5_2019_merged$playoffs)

### TEST DATA
test_0.5_2019_nochamp_noenemy_nocat_imputed <- read_csv("data/test_0.5_2019_nochamp_noenemy_nocat_imputed.csv")
## test_0.5_2019 comes from above (HAVE TO RUN ALL LINES, INEFFICIENT)
# test_0.5_2019 <- read_csv("../data/test_0.5_2019.csv")
test_0.5_2019_removed <- test_0.5_2019 %>%
  select(c('side', 'league', 'result', 'patch','year', 'month','day',
           #           'player_top' ,'player_jng','player_mid','player_bot','player_sup',
           'unofficial_match_num','obs_num', 
           'champion_top_enemy'
           ,'champion_jng_enemy','champion_mid_enemy'
           ,'champion_bot_enemy','champion_sup_enemy',
           'champion_top' 
           ,'champion_jng','champion_mid','champion_bot','champion_sup'))
test_0.5_2019_merged <- cbind(test_0.5_2019_removed,test_0.5_2019_nochamp_noenemy_nocat_imputed)

test_0.5_2019_merged <- test_0.5_2019_merged %>%
  select(-c(day, month, obs_num, unofficial_match_num, league))
test_0.5_2019_merged$year = as.integer(test_0.5_2019_merged$year)
test_0.5_2019_merged$result = as.factor(test_0.5_2019_merged$result)
test_0.5_2019_merged$playoffs = as.factor(test_0.5_2019_merged$playoffs)

rm(train_0.5_2019_nochamp_noenemy_nocat_imputed, train_0.5_2019_removed, 
   test_0.5_2019_nochamp_noenemy_nocat_imputed, test_0.5_2019_removed)

## ADDING TEAM WR

tmp <- train_0.5_2019_merged %>%
  mutate(team_wr_6mon = (player_wr_6mon_top + player_wr_6mon_jng + player_wr_6mon_mid + 
           player_wr_6mon_bot + player_wr_6mon_sup) / 5) %>%
  mutate(team_wr_year = (player_wr_year_top + player_wr_year_jng + player_wr_year_mid +
                              player_wr_year_bot + player_wr_year_sup) /5 )

tmp %>%
  select(team_wr_year,team_wr_6mon)
  


### EXPORTING
write.csv(train_0.5_2019_merged,"data/Imputed_Final/train_0.5_2019_imputed_full.csv", row.names = FALSE)
write.csv(test_0.5_2019_merged,"data/Imputed_Final/test_0.5_2019_imputed_full.csv", row.names = FALSE)



# Bar Plots for slides ----------------------------------------------------



tmp <- data.frame("3Y_3Y_Imp" = c(.526,.573),
                  "1Y_3Y_Imp" = c(.547,.553),
                  "1Y_1Y_Imp" = c(.505,.562))

tmp2 <- data.frame(full = c(.518,.519,.536),
                   sparse = c(.568,.537,.536),
                   type= c("3Y_3Y_Imp", "1Y_3Y_Imp", "1Y_1Y_Imp"))

tmp3 <- tmp2 %>%
  pivot_longer(cols = c("full","sparse"), names_to = "Variable") 

tmp3 %>%
  ggplot(aes(x=type, y=value, fill=Variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x="Type of Analysis", y = "AUC") +
  coord_cartesian(ylim = c(0.45, 0.6))
  ylim(0.3,0.6)
  scale_y_continuous()

tmp2 %>%
  ggplot() +
  geom_bar(aes(type,full), position = "dodge", stat = "identity") +
  geom_bar(aes(type,sparse), position = "dodge", stat = "identity")
  
  
  geom_bar(aes(X3Y_3Y_Imp),position = "stack", stat = "bin") +
  geom_bar(aes(X1Y_3Y_Imp)) + 
  geom_bar(aes(X1Y_1Y_Imp))
