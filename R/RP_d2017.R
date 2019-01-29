# Get the unique game ids
game_ids <- unique(d2017$GAME_ID)

# Find the starting pithers for both teams in every game
SPs <- d2017%>%
  filter(INN_CT == 1, LEADOFF_FL=='TRUE') %>%
  select(GAME_ID, PIT_ID)

# Get the first game
SP1 <- SPs %>% filter(GAME_ID == game_ids[1])

# Add new column to first game
game <- d2017%>%
  filter(GAME_ID == game_ids[1])%>%
  mutate(RP = case_when(
    PIT_ID %in% SP1$PIT_ID ~ 0 ,
    TRUE ~ 1
  ))

# Add new column to rest of games, rbinding each new game to previous set
for (i in 2:length(game_ids)){
  SP <- SPs %>% filter(GAME_ID == game_ids[i])
  new_game <- d2017%>%
    filter(GAME_ID == game_ids[i]) %>%
    mutate(RP = case_when(
      PIT_ID %in% SP$PIT_ID ~ 0 ,
      TRUE ~ 1
    ))
  
  game <- rbind(game,new_game)
  }

