################### ML PROJECT #########################
########### Data Cleaning ##############

# Libraries ---------------------------------------------------------------

##### Loading Libraries
library(dplyr)
library(tidyverse) 
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2)

##### Set working directory
setwd("/Users/Pan/Dropbox (MIT)/ML_league")

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

########################### Exploratory Data Analysis ############################################

df <- as.data.frame(match_data)
df
# Explore the league
league_2017 <- lol_data_new %>% filter(year == 2017)
nrow(league_2017)
hist(league_2017$month)

unique(league_2017$league) #LPL, LCK
league_2017_check <- league_2017 %>% group_by(league) %>% summarize(games = n()) %>% as.data.frame()
league_2017_check

league_2018 <- lol_data_new %>% filter(year == 2018)
nrow(league_2018)
hist(league_2018$month)

league_2018_1h <- lol_data_new %>% filter(year == 2018,month<6)
league_2018_2h <- lol_data_new %>% filter(year == 2018,month>=6)
nrow(league_2018_1h)
nrow(league_2018_2h)

unique(league_2018$league) #LPL, LCK

league_2018_check <- league_2018 %>% group_by(league) %>% summarize(games = n()) %>% as.data.frame()
league_2018_check

league_2019 <- lol_data_new %>% filter(year == 2019)
nrow(league_2019)
hist(league_2019$month)

league_2020 <- lol_data_new %>% filter(year == 2020)
nrow(league_2020)
hist(league_2020$month)

unique(league_2020$league) #LPL, LCK

league_2019_check <- league_2019 %>% group_by(league) %>% summarize(games = n()) %>% as.data.frame()
league_2019_check

league_2020_check <- league_2020 %>% group_by(league) %>% summarize(games = n()) %>% as.data.frame()
league_2020_check

a = intersect(unique(league_2018$league),unique(league_2019$league),unique(league_2020$league))
a

league_2017 <- lol_data_new %>% filter(team == "Monolith Gaming")
league_2018 <- lol_data_new %>% filter(league == 'LCS.A', year==2018)

unique(league_2017$league)
unique(league_2018$team)

league_2017$team

league_2017_2 <- lol_data_new %>% filter(agg_period == '2017.2')
nrow(league_2017_2)
unique(league_2017_2$league)

league_2018_1 <- lol_data_new %>% filter(agg_period == '2018.1')
nrow(league_2018_1)
unique(league_2018_1$league)

league_2018_2 <- lol_data_new %>% filter(agg_period == '2018.2')
nrow(league_2018_2)
unique(league_2018_2$league)

league_2019_1 <- lol_data_new %>% filter(agg_period == '2019.1')
nrow(league_2019_1)
unique(league_2019_1$league)

league_2019_2 <- lol_data_new %>% filter(agg_period == '2019.2')
nrow(league_2019_2)
unique(league_2019_2$league)

# number of rows
nrow(df)
# number of unique matches
length(unique(df$gameid))

# check game with draw
check_draw <- df %>%
  group_by(gameid,result) %>%
  summarize(no_game = n()) %>%
  filter(no_game > 1)
check_draw 

# explore basic statistics
df_win = df %>% filter(result==1)
df_lose = df %>% filter(result==0)

# number of unique matches
length(unique(df_win$gameid))
length(unique(df_lose$gameid))

# check overall statistics
summary(df_win[,500:531])

check1 <- lol_data_new %>% filter(year =='2018') %>% select(player)
check2 <- lol_data_new %>% filter(year =='2019') %>% select(player)

nrow(unique(check1))
nrow(unique(check2))
a = intersect(check1,check2)
nrow(a)
989/1685

unique(lol_data_new$league)

# check NA
na_count_win <-sapply(df_win, function(y) sum(length(which(is.na(y))))/length(y)*100)
na_count_win <- data.frame(na_count_win) %>% mutate(result = 1, col_name = colnames(df_win)) %>% rename(na_count = na_count_win)
na_count_win

na_count_lose <-sapply(df_lose, function(y) sum(length(which(is.na(y))))/length(y)*100)
na_count_lose <- data.frame(na_count_lose) %>% mutate(result = 0, col_name = colnames(df_win)) %>% rename(na_count = na_count_lose)
na_count_lose

check_na <- function(start,end){
  na_plot <- as.data.frame(rbind(na_count_win[start:end,],na_count_lose[start:end,]))
  ggplot(data=na_plot, aes(x=col_name,y=na_count, fill=as.factor(result))) + 
    geom_bar(stat="identity",position="dodge2", alpha=0.7) +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("red","darkgreen"))

}
nrow(df_win)
nrow(df_lose)

check_na(1,30)
check_na(31,60)
check_na(61,90)
check_na(115,126)
check_na(145,170)
check_na(171,190)
check_na(191,220)
check_na(221,261)

################## plot data
# function to plot
plot_winlost <- function(start,end,pr){
  df_win_plot <- df_win %>% filter(agg_period==pr)  
  df_win_plot <- df_win_plot[,start:end] %>% select(where(is.numeric))
  
  df_lose_plot <- df_lose %>% filter(agg_period==pr)  
  df_lose_plot <- df_lose_plot[,start:end] %>% select(where(is.numeric))
  
  ggplot() +
    #geom_histogram(data=gather(df_win_plot)
     #            , aes(x=value)
      #           , alpha=0.5, fill ='green') +
    geom_histogram(data=gather(df_lose_plot)
                 , aes(x=value)
                 , alpha=0.5, fill ='red') +
    facet_wrap(~key, scales = 'free_x')
}

######## aggregate period = 2
ap = 2018.1
plot_winlost(1,20,ap) # same! which means general data make sense
plot_winlost(21,29,ap) # winning champions tend to be selected again next season
plot_winlost(30,44,ap) # death / kda / rkda -> makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(60,74,ap) # makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(75,79,ap) # makes sense
plot_winlost(80,94,ap) # makes sense
plot_winlost(95,109,ap) # makes sense
plot_winlost(110,124,ap) # makes sense
plot_winlost(125,144,ap) # makes sense
plot_winlost(145,175,ap) # makes sense
plot_winlost(176,200,ap) # makes sense
plot_winlost(201,250,ap) # makes sense
plot_winlost(251,280,ap) # makes sense

######## aggregate period = 3
ap = 3
plot_winlost(1,20,ap) # same! which means general data make sense
plot_winlost(21,29,ap) # winning champions tend to be selected again next season
plot_winlost(30,44,ap) # death / kda / rkda -> makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(60,74,ap) # makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(75,79,ap) # makes sense
plot_winlost(80,94,ap) # makes sense
plot_winlost(95,109,ap) # makes sense
plot_winlost(110,124,ap) # makes sense
plot_winlost(125,144,ap) # makes sense

######## aggregate period = 4
ap = 4
plot_winlost(1,20,ap) # same! which means general data make sense
plot_winlost(21,29,ap) # winning champions tend to be selected again next season
plot_winlost(30,44,ap) # death / kda / rkda -> makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(60,74,ap) # makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(75,79,ap) # makes sense
plot_winlost(80,94,ap) # makes sense
plot_winlost(95,109,ap) # makes sense
plot_winlost(110,124,ap) # makes sense
plot_winlost(125,144,ap) # makes sense

######## aggregate period = 5
ap = 5
plot_winlost(1,20,ap) # same! which means general data make sense
plot_winlost(21,29,ap) # winning champions tend to be selected again next season
plot_winlost(30,44,ap) # death / kda / rkda -> makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(60,74,ap) # makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(75,79,ap) # makes sense
plot_winlost(80,94,ap) # makes sense
plot_winlost(95,109,ap) # makes sense
plot_winlost(110,124,ap) # makes sense
plot_winlost(125,144,ap) # makes sense

######## aggregate period = 6
ap = 6
plot_winlost(1,20,ap) # same! which means general data make sense
plot_winlost(21,29,ap) # winning champions tend to be selected again next season
plot_winlost(30,44,ap) # death / kda / rkda -> makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(60,74,ap) # makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(75,79,ap) # makes sense
plot_winlost(80,94,ap) # makes sense
plot_winlost(95,109,ap) # makes sense
plot_winlost(110,124,ap) # makes sense
plot_winlost(125,144,ap) # makes sense

colnames(df_win)

df[,21:29]


########################## EDA: Imputed data

train <- read_csv("data/Imputed_Final/train_2y1y_2row_imputed_full.csv")
test <- read_csv("data/Imputed_Final/test_2y1y_2row_imputed_full.csv")

summary(train[,250:350])

combined <- bind_rows(train, test)
df <- as.data.frame(train)

train_notimputed <- read_csv("data/train_2y1y_2row.csv")

sum(is.na(train_notimputed))/(sum(!is.na(train_notimputed))+sum(is.na(train_notimputed)))

  
df_list <- list(train_notimputed, train)
cols <- reduce(df_list, .f = ~ intersect(colnames(.x), colnames(.y)))

train_imputed <- train %>% select(cols) %>% as.data.frame()
train_nonimputed <- train_notimputed  %>% select(cols) %>% as.data.frame()

# number of rows
nrow(df)
# number of unique matches
length(unique(df$gameid))

# check game with draw
check_draw <- df %>%
  group_by(gameid,result) %>%
  summarize(no_game = n()) %>%
  filter(no_game > 1)
check_draw 

# explore basic statistics
df_win = df %>% filter(result==1)
df_lose = df %>% filter(result==0)

df_win_IP = train_imputed %>% filter(result==1)
df_lose_IP = train_imputed %>% filter(result==0)

df_win_noIP = train_nonimputed %>% filter(result==1)
df_lose_noIP = train_nonimputed %>% filter(result==0)

# number of unique matches
length(unique(df_win$gameid))
length(unique(df_lose$gameid))

# check overall statistics
summary(df_win[,500:531])

check1 <- lol_data_new %>% filter(year =='2018') %>% select(player)
check2 <- lol_data_new %>% filter(year =='2019') %>% select(player)

nrow(unique(check1))
nrow(unique(check2))
a = intersect(check1,check2)
nrow(a)
989/1685

unique(lol_data_new$league)

# check NA
na_count_win <-sapply(df_win, function(y) sum(length(which(is.na(y))))/length(y)*100)
na_count_win <- data.frame(na_count_win) %>% mutate(result = 1, col_name = colnames(df_win)) %>% rename(na_count = na_count_win)
na_count_win

na_count_lose <-sapply(df_lose, function(y) sum(length(which(is.na(y))))/length(y)*100)
na_count_lose <- data.frame(na_count_lose) %>% mutate(result = 0, col_name = colnames(df_win)) %>% rename(na_count = na_count_lose)
na_count_lose

check_na <- function(start,end){
  na_plot <- as.data.frame(rbind(na_count_win[start:end,],na_count_lose[start:end,]))
  ggplot(data=na_plot, aes(x=col_name,y=na_count, fill=as.factor(result))) + 
    geom_bar(stat="identity",position="dodge2", alpha=0.7) +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("red","darkgreen"))
}
nrow(df_win)
nrow(df_lose)

check_na(1,30)
check_na(31,60)
check_na(61,90)
check_na(91,144)
check_na(145,170)
check_na(171,190)
check_na(191,220)
check_na(221,261)

################## plot data
# function to plot
plot_winlost <- function(start,end,pr){
  df_win_plot_IP <- df_win_IP %>% filter(year==pr)  
  df_win_plot_IP <- df_win_plot_IP[,start:end] %>% select(where(is.numeric))
  
  df_win_plot_noIP <- df_win_noIP %>% filter(year==pr)  
  df_win_plot_noIP <- df_win_plot_noIP[,start:end] %>% select(where(is.numeric))
  
  df_lose_plot_IP <- df_lose_IP %>% filter(year==pr)  
  df_lose_plot_IP <- df_lose_plot_IP[,start:end] %>% select(where(is.numeric))
  
  df_lose_plot_noIP <- df_lose_noIP %>% filter(year==pr)  
  df_lose_plot_noIP <- df_lose_plot_noIP[,start:end] %>% select(where(is.numeric))
  
  ggplot() +
    geom_histogram(data=gather(df_win_plot_IP)
                   , aes(x=value)
                   , alpha=0.5, fill ='green') +
    #geom_histogram(data=gather(df_lose_plot_IP)
     #              , aes(x=value)
      #             , alpha=0.5, fill ='red') +
    geom_histogram(data=gather(df_win_plot_noIP)
                   , aes(x=value)
                   , alpha=0.5, fill ='blue') +
    #geom_histogram(data=gather(df_lose_plot_noIP)
     #              , aes(x=value)
      #             , alpha=0.5, fill ='grey') +
    facet_wrap(~key, scales = 'free_x')
}

######## year = 2018
ap = 2018
plot_winlost(1,20,ap) # same! which means general data make sense
plot_winlost(21,44,ap) # winning champions tend to be selected again next season
plot_winlost(45,59,ap) # makes sense
plot_winlost(60,74,ap) # makes sense
plot_winlost(45,59,ap) # makes sense
plot_winlost(75,79,ap) # makes sense
plot_winlost(80,94,ap) # makes sense
plot_winlost(95,109,ap) # makes sense
plot_winlost(110,124,ap) # makes sense
plot_winlost(80,83,ap) # makes sense
plot_winlost(145,175,ap) # makes sense
plot_winlost(176,200,ap) # makes sense
plot_winlost(201,250,ap) # makes sense
plot_winlost(251,280,ap) # makes sense
plot_winlost(281,320,ap) # makes sense
plot_winlost(400,450,ap) # makes sense

########################## --------------------------------------- EDA: Imputed data 0.5/2019

train <- read_csv("/Users/pan/Dropbox (MIT)/ML_league/data/Imputed_Final/train_0.5_2019_imputed_full.csv")

df <- as.data.frame(train)

train_notimputed <- read_csv("/Users/pan/Dropbox (MIT)/ML_league/data/train_0.5_2019.csv")
train_notimputed$result = train$result
df_list <- list(train_notimputed, train)
cols <- reduce(df_list, .f = ~ intersect(colnames(.x), colnames(.y)))

train_imputed <- train %>% select(cols) %>% as.data.frame()
train_nonimputed <- train_notimputed  %>% select(cols) %>% as.data.frame()

# explore basic statistics
df_win = df %>% filter(result==1)
df_lose = df %>% filter(result==0)

df_win_IP = train_imputed %>% filter(result==1)
df_lose_IP = train_imputed %>% filter(result==0)

df_win_noIP = train_nonimputed %>% filter(result==1)
df_lose_noIP = train_nonimputed %>% filter(result==0)

# check NA
na_count_win <-sapply(df_win, function(y) sum(length(which(is.na(y))))/length(y)*100)
na_count_win <- data.frame(na_count_win) %>% mutate(result = 1, col_name = colnames(df_win)) %>% rename(na_count = na_count_win)
na_count_win

na_count_lose <-sapply(df_lose, function(y) sum(length(which(is.na(y))))/length(y)*100)
na_count_lose <- data.frame(na_count_lose) %>% mutate(result = 0, col_name = colnames(df_win)) %>% rename(na_count = na_count_lose)
na_count_lose

check_na <- function(start,end){
  na_plot <- as.data.frame(rbind(na_count_win[start:end,],na_count_lose[start:end,]))
  ggplot(data=na_plot, aes(x=col_name,y=na_count, fill=as.factor(result))) + 
    geom_bar(stat="identity",position="dodge2", alpha=0.7) +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("red","darkgreen"))
}
nrow(df_win)
nrow(df_lose)

check_na(1,30)
check_na(31,60)
check_na(61,90)
check_na(91,144)
check_na(145,170)
check_na(171,190)
check_na(191,220)
check_na(221,261)

################## plot data
# function to plot
plot_winlost <- function(start,end){
  df_win_plot_IP <- df_win_IP 
  df_win_plot_IP <- df_win_plot_IP[,start:end] %>% select(where(is.numeric))
  
  df_win_plot_noIP <- df_win_noIP 
  df_win_plot_noIP <- df_win_plot_noIP[,start:end] %>% select(where(is.numeric))
  
  df_lose_plot_IP <- df_lose_IP  
  df_lose_plot_IP <- df_lose_plot_IP[,start:end] %>% select(where(is.numeric))
  
  df_lose_plot_noIP <- df_lose_noIP   
  df_lose_plot_noIP <- df_lose_plot_noIP[,start:end] %>% select(where(is.numeric))
  
  ggplot() +
    geom_histogram(data=gather(df_win_plot_IP)
                   , aes(x=value)
                   , alpha=0.5, fill ='green') +
    #geom_histogram(data=gather(df_lose_plot_IP)
    #              , aes(x=value)
    #             , alpha=0.5, fill ='red') +
    geom_histogram(data=gather(df_win_plot_noIP)
                   , aes(x=value)
                   , alpha=0.5, fill ='grey') +
    #geom_histogram(data=gather(df_lose_plot_noIP)
    #              , aes(x=value)
    #             , alpha=0.5, fill ='grey') +
    facet_wrap(~key, scales = 'free_x')
}

######## year = 2018
plot_winlost(1,20) # same! which means general data make sense
plot_winlost(21,44) # winning champions tend to be selected again next season
plot_winlost(45,59) # makes sense
plot_winlost(60,74) # makes sense
plot_winlost(45,59) # makes sense
plot_winlost(75,79) # makes sense
plot_winlost(80,94) # makes sense
plot_winlost(95,109) # makes sense
plot_winlost(110,124) # makes sense
plot_winlost(125,144) # makes sense
plot_winlost(145,175) # makes sense
plot_winlost(176,200) # makes sense
plot_winlost(201,250) # makes sense
plot_winlost(251,280) # makes sense
plot_winlost(281,320) # makes sense
plot_winlost(400,450) # makes sense

