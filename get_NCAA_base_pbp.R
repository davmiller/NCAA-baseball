#install.packages('tidyverse')
library(tidyverse)
#install.packages('XML')
library(XML)
#install.packages('stringr')
library(stringr)
#install.packages('RCurl')
library(RCurl)

stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

teams_schedule <- read.csv("~/OneDrive - University of Hartford/NCAA Baseball scrapR/NCAA_base_schedule.csv")

# NCAA_pbp_17 <- read.csv("~/OneDrive - University of Hartford/NCAA Baseball scrapR/pbp_2017.csv")

base_url='http://stats.ncaa.org/game/play_by_play/'


score_fill=function(score_in){
  m=length(score_in)
  score_in[1]=0
  for(i in 2:m){
    if (is.na(score_in[i])){
      score_in[i]=score_in[i-1]
    }
  }
  return(score_in)
}

# Get pbp from all games for one team
get_team_pbp=function(team, year, division=1){
  all_team_games=get_team_schedule(team, year, division=1)

  
  games=all_team_games%>%
    distinct(game_id, .keep_all = TRUE)%>%
    mutate(home_team=ifelse(loc%in%c('H', 'N'), team, opp),
           away_team=ifelse(loc=='H', opp, team ))%>%
    select(year, date, game_id, home_team, away_team)
  
  for (k in 1:nrow(games)){
    base_url='http://stats.ncaa.org/game/play_by_play/'
    game_id=games$game_id[k]
    year=games$year[k]
    x= paste(base_url, game_id, sep='')
    y=getURL(x)
    # Play by play is in table form
    y=readHTMLTable(y)
    
    if (length(y) < ncol(y[[1]]) | length(y) == 0) {
      print(paste("Play by Play data not available for game", game_id, sep=' '))
      next
    }
    else{
      j=1
      for (i in 1:length(y)){
        # Disgard NULL tables
        if (is.null(y[[i]])==FALSE){
          # Only take pbp tables (3 cols)
          if (ncol(y[[i]])==3){
            inn=as.data.frame(y[[i]])%>%
              mutate(inning=j,
                     game_id=game_id,
                     year=year)%>%
              select(year,game_id,inning,everything())
            j=j+1
            if (j==2){pbp=inn}
            else{pbp=rbind(pbp, inn)}
          }
        }
      }
    }
    pbp=pbp%>%
      mutate(away_team = colnames(pbp)[4],
             home_team = colnames(pbp)[6],
             away_score = as.integer(gsub('-.*', '', Score)),
             home_score = as.integer(gsub('.*-', '', Score)),
             away_score=score_fill(away_score),
             home_score=score_fill(home_score))%>%
      rename(away_text = 4,
             home_text = 6)%>%
      filter(substr(away_text,1,3)!='R: ')%>%
      select(year, game_id, inning, away_team, home_team, away_score, home_score, away_text, home_text)
    if (k==1){NCAA_pbp=pbp}
    else {NCAA_pbp = rbind(NCAA_pbp, pbp)}
  }
  return(NCAA_pbp)
}

View(get_team_pbp('Hartford',2018))





# Get pbp from all games in one season
get_season_pbp=function(year, division=1){
  all_season_games=get_season_schedule(year_start=year, year_end=year, division=1)
  
  games=all_season_games%>%
    distinct(game_id, .keep_all = TRUE)%>%
    mutate(home_team=ifelse(loc%in%c('H', 'N'), team, opp),
           away_team=ifelse(loc=='H', opp, team ))%>%
    select(year, date, game_id, home_team, away_team)
  
  for (k in 1:nrow(games)){
    base_url='http://stats.ncaa.org/game/play_by_play/'
    game_id=games$game_id[k]
    year=games$year[k]
    x= paste(base_url, game_id, sep='')
    y=getURL(x)
    y=readHTMLTable(y)
    if (length(y) < ncol(y[[1]]) | length(y) == 0) {
      print(paste("Play by Play data not available for game", game_id, sep=' '))
      next
    }
    else{
      j=1
      for (i in 1:length(y)){
        # Disgard NULL tables
        if (is.null(y[[i]])==FALSE){
          # Only take pbp tables (3 cols)
          if (ncol(y[[i]])==3){
            inn=as.data.frame(y[[i]])%>%
              mutate(inning=j,
                     game_id=game_id,
                     year=year)%>%
              select(year,game_id,inning,everything())
            j=j+1
            if (j==2){pbp=inn}
            else{pbp=rbind(pbp, inn)}
          }
        }
      }
    }
    pbp=pbp%>%
      mutate(away_team = colnames(pbp)[4],
             home_team = colnames(pbp)[6],
             away_score = as.integer(gsub('-.*', '', Score)),
             home_score = as.integer(gsub('.*-', '', Score)),
             away_score=score_fill(away_score),
             home_score=score_fill(home_score))%>%
      rename(away_text = 4,
             home_text = 6)%>%
      filter(substr(away_text,1,3)!='R: ')%>%
      select(year, game_id, inning, away_team, home_team, away_score, home_score, away_text, home_text)
    if (k==1){NCAA_pbp=pbp}
    else {NCAA_pbp = rbind(NCAA_pbp, pbp)}
  }
  return(NCAA_pbp)
}





for (k in 1:nrow(games_2014)){
  base_url='http://stats.ncaa.org/game/play_by_play/'
  game_id=games_2014$game_id[k]
  year=games_2014$year[k]
  x= paste(base_url, game_id, sep='')
  y=getURL(x)
  y=readHTMLTable(y)
  if (length(y) < ncol(y[[1]]) | length(y) == 0) {
    print(paste("Play by Play data not available for game", game_id, sep=' '))
    next
  }
  else{
    j=1
    for (i in 1:length(y)){
      # Disgard NULL tables
      if (is.null(y[[i]])==FALSE){
        # Only take pbp tables (3 cols)
        if (ncol(y[[i]])==3){
          inn=as.data.frame(y[[i]])%>%
            mutate(inning=j,
                   game_id=game_id,
                   year=year)%>%
            select(year,game_id,inning,everything())
          j=j+1
          if (j==2){pbp=inn}
          else{pbp=rbind(pbp, inn)}
        }
      }
    }
  }
  pbp=pbp%>%
    mutate(away_team = colnames(pbp)[4],
           home_team = colnames(pbp)[6],
           away_score = as.integer(gsub('-.*', '', Score)),
           home_score = as.integer(gsub('.*-', '', Score)),
           away_score=score_fill(away_score),
           home_score=score_fill(home_score))%>%
    rename(away_text = 4,
           home_text = 6)%>%
    filter(substr(away_text,1,3)!='R: ')%>%
    select(year, game_id, inning, away_team, home_team, away_score, home_score, away_text, home_text)
  if (k==1){NCAA_pbp=pbp}
  else {NCAA_pbp = rbind(NCAA_pbp, pbp)}
}



k

NCAA_pbp_14=NCAA_pbp


write_csv(NCAA_pbp_14, 'pbp_2014.csv')


View(NCAA_pbp_18%>%
       filter(home_team=='Hartford' | away_team=='Hartford')%>%
       distinct(game_id, .keep_all = TRUE))

View(game_ids_2018%>%
       filter(game_id=='4586103'))

View(teams_schedule%>%
       filter(team=='Hartford'))

substr(tmp_text[63],(str_locate(tmp_text[63],'to p for')[2]+1),(str_length(tmp_text[63])-1))

outs_on_play = function(text){
  tp=c('triple play')
  dp=c('double play')
  outs=c('out', 'popped up')
  m=length(text)
  outs_on_play=integer(m)
  for (i in 1:m){
    outs_on_play[i]=case_when(
      str_count(text[i], tp) == 1 ~ 3,
      str_count(text[i], dp) == 1 ~ 2,
      TRUE ~ sum(as.numeric(str_count(text[i], outs)))
    )
  }
  return(outs_on_play)
}




inn_end = function(home,away){
  m=length(home)
  inn_end=integer(m)
  for (i in 1:(m-1)){
    inn_end[i]=ifelse((home[i]=='' & home[i+1]!='') | (away[i]=='' & away[i+1]!=''),1,0)
  }
  inn_end[m]=1
  return(inn_end)
}

outs_before= function(outs_on_play){
  m=length(outs_on_play)
  inn_outs=integer(m)
  for (i in 2:m){
    inn_outs[i]=((inn_outs[i-1]+outs_on_play[i-1]) %% 3)
  }
  return(inn_outs)
}

runs_on_play= function(a_txt, h_txt, a_score,h_score){
  m=length(a_txt)
  runs_on_play=integer(m)
  runs_on_play[1]=a_score[1]
  for (i in 2:m){
    runs_on_play[i]=case_when(
      a_txt[i]=='' ~ as.integer(h_score[i]-h_score[i-1]),
      a_txt[i]!='' ~ as.integer(a_score[i]-a_score[i-1])
    )
  }
  return(runs_on_play)
}

pbp_parse=pbp_parse%>%
  filter(substr(away_text,1,3)!='R: ')%>%
  mutate(bat_name = ifelse(away_text!='', batter_name(away_text), batter_name(home_text)),
         bat_nm=batter_name(tmp_text),
         outs_on_play=outs_on_play(tmp_text),
         outs_before=outs_before(outs_on_play),
         end_half_inn=inn_end(home_text,away_text),
         away_score=score_fill(away_score),
         home_score=score_fill(home_score),
         runs_on_play=runs_on_play(away_text, home_text, away_score, home_score),
         pitch_name=pitch_name(tmp_text)
  )

