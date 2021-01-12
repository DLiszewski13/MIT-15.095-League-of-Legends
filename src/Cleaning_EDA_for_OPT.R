# Libraries ---------------------------------------------------------------

##### Loading Libraries
library(tidyverse) 
library(lubridate)
library(purrr)

setwd("~/Dropbox (MIT)/ML_league")

train <- read_csv("data/train_2y1y_2row_nochamp_noenemy_nocat_imputed.csv")
test <- read_csv("data/test_2y1y_2row_nochamp_noenemy_nocat_imputed.csv")

df = as.data.frame(rbind(train,test))

train_2y1y_2row <- read_csv("data/train_2y1y_2row.csv")
test_2y1y_2row <- read_csv("data/test_2y1y_2row.csv")

test_2y1y_2row

train_player <- train_2y1y_2row %>% select(player_mid,champion_mid,result,champion_top_enemy,champion_jng_enemy,champion_mid_enemy,champion_bot_enemy
                                           ,champion_sup_enemy,champion_top,champion_jng,champion_bot,champion_sup)
test_player <- test_2y1y_2row %>% select(player_mid,champion_mid,result,champion_top_enemy,champion_jng_enemy,champion_mid_enemy,champion_bot_enemy
                                         ,champion_sup_enemy,champion_top,champion_jng,champion_bot,champion_sup)


df_player = as.data.frame(rbind(train_player,test_player))

df$player_mid = df_player$player_mid
df$champion_mid = df_player$champion_mid
df$result = df_player$result
df$champion_top_enemy = df_player$champion_top_enemy
df$champion_jng_enemy = df_player$champion_jng_enemy
df$champion_mid_enemy = df_player$champion_mid_enemy
df$champion_bot_enemy = df_player$champion_bot_enemy
df$champion_sup_enemy = df_player$champion_sup_enemy
df$champion_top = df_player$champion_top
df$champion_jng = df_player$champion_jng
df$champion_bot = df_player$champion_bot
df$champion_sup = df_player$champion_sup
df

df_faker = df %>% filter(player_mid == 'Faker')
fav = c('Orianna','Azir','Ryze','LeBlanc')

df_faker <- mutate(df_faker, treatment = ifelse(champion_mid %in% fav, "fav", "not_fav"))
df_faker_final <- df_faker %>% select(-c(champion_mid,player_mid))
df_faker_final

write.csv(df_faker_final,"data/Imputed_Final/2y1y_OPT.csv", row.names = FALSE)

## ----------------------------------------------------------------

df_faker_check = df %>% filter(player_mid == 'Faker') %>% group_by(champion_mid) %>% summarize(n_games = n()) 
df_faker_check

df_sort <- df_faker[order(df_faker$n_games,decreasing=TRUE),]
df_sort

df_faker = df %>% filter(player_mid == 'Faker')
summary(df_faker)

df_win = df_faker %>% filter(result==1)
df_lose = df_faker %>% filter(result==0)

nrow(df_win)
nrow(df_lose)

################## plot data
# function to plot
plot_winlost <- function(start,end){
  df_win_plot <- df_win 
  df_win_plot <- df_win_plot[,start:end] %>% select(where(is.numeric))
  
  df_lose_plot <- df_lose
  df_lose_plot <- df_lose_plot[,start:end] %>% select(where(is.numeric))
  
  ggplot() +
    geom_histogram(data=gather(df_win_plot)
                , aes(x=value)
               , alpha=0.5, fill ='green') +
    geom_histogram(data=gather(df_lose_plot)
                   , aes(x=value)
                   , alpha=0.5, fill ='red') +
    facet_wrap(~key, scales = 'free_x')
}

######## aggregate period = 2
plot_winlost(1,20)
plot_winlost(21,29) # winning champions tend to be selected again next season
plot_winlost(30,44) # death / kda / rkda -> makes sense
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

## ---------------------------------------------------------------- KDA!!!
setwd("~/Dropbox (MIT)/ML_league")

train <- read_csv("data/train_2y1y_2row_nochamp_noenemy_nocat_imputed.csv")
test <- read_csv("data/test_2y1y_2row_nochamp_noenemy_nocat_imputed.csv")

df = as.data.frame(rbind(train,test))

train_2y1y_2row <- read_csv("data/train_2y1y_2row_faker_OPT.csv")
test_2y1y_2row <- read_csv("data/test_2y1y_2row_faker_OPT.csv")


train_player <- train_2y1y_2row %>% select(player_mid,champion_mid,result,champion_top_enemy,champion_jng_enemy,champion_mid_enemy,champion_bot_enemy
                                           ,champion_sup_enemy,champion_top,champion_jng,champion_bot,champion_sup,kills_mid,deaths_mid,assists_mid)
test_player <- test_2y1y_2row %>% select(player_mid,champion_mid,result,champion_top_enemy,champion_jng_enemy,champion_mid_enemy,champion_bot_enemy
                                         ,champion_sup_enemy,champion_top,champion_jng,champion_bot,champion_sup,kills_mid,deaths_mid,assists_mid)

df_player = as.data.frame(rbind(train_player,test_player))
df_player$deaths_mid[df_player$deaths_mid == 0] <- 1

df_player <- df_player %>% mutate(KDA = (kills_mid+assists_mid)/deaths_mid)

df$player_mid = df_player$player_mid
df$champion_mid = df_player$champion_mid
df$result = df_player$result
df$champion_top_enemy = df_player$champion_top_enemy
df$champion_jng_enemy = df_player$champion_jng_enemy
df$champion_mid_enemy = df_player$champion_mid_enemy
df$champion_bot_enemy = df_player$champion_bot_enemy
df$champion_sup_enemy = df_player$champion_sup_enemy
df$champion_top = df_player$champion_top
df$champion_jng = df_player$champion_jng
df$champion_bot = df_player$champion_bot
df$champion_sup = df_player$champion_sup
df$KDA = df_player$KDA
df

df_faker = df %>% filter(player_mid == 'Faker')
fav = c('Orianna','Azir','Ryze','LeBlanc')

df_faker <- mutate(df_faker, treatment = ifelse(champion_mid %in% fav, "main", "not_main"))
df_faker_final <- df_faker %>% select(-c(champion_mid,player_mid))
df_faker_final

write.csv(df_faker_final,"data/Imputed_Final/2y1y_OPT_KDA.csv", row.names = FALSE)
