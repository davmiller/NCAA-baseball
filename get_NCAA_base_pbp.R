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

