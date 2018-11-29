library(tidyverse)

stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

# base url
base_url <- "http://stats.ncaa.org"

# Functions for scraping #############
get_team_url <- function(school, year, base_url){
  teamid <- team_ids%>%
    filter(team==school)
  teamid <- toString(teamid['id'])
  
  yearid <- year_ids%>%
    filter(year==target_year)
  yearid <- toString(yearid[2])
  
  team_url <- paste(base_url, "/team/index/", yearid, "?org_id=", teamid, sep="")
  return(team_url)
}


get_game_id=function(x){
  # Get results
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x)
  game_results_rows <- game_date_rows+6
  game_id <- as.numeric(gsub("^.*game/index/([0-9]*).*$", "\\1", x[game_results_rows]))
  return(game_id)
}

get_game_results=function(x){
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x)
  game_results_rows <- game_date_rows+6
  game_results <- gsub("<[^<>]*>", "",x[game_results_rows])
  game_results <- stripwhite(game_results)
  return(game_results)
}

get_game_opponent=function(x){
  # get game opponents and location
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x)
  game_opploc_rows <- game_date_rows+3
  game_opploc <- gsub("<[^<>]*>", "", x[game_opploc_rows])
  game_opploc <- stripwhite(game_opploc)
  return(game_opploc)
}

get_game_date=function(x){
  # get game dates
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x)
  game_dates <- stripwhite(gsub("<[^<>]*>", "", x[game_date_rows]))
  return(game_dates)
}
########################################################  

  



# Get one team's schedule in one year
get_team_schedule <- function(team_name, year, division=1){
  
  year_ids = get_year_ids(year, year, division)
  team_ids = get_team_ids(year, year, division)
  
  team <- toString(team_name)
  teamid <- toString(team_ids$id[grep(team_name,team_ids$team)])
  
  year <- toString(year)
  yearid <- toString(year_ids$year_id[grep(year,year_ids$year)])
  
  team_url <- paste(base_url, "/team/index/", yearid, "?org_id=", teamid, sep="")

  x <- try(scan(team_url, what="", sep="\n"))
  if (class(x)=='try-error'){
    print(paste('Cannot connect to server for', team, 'in', year))}
  else{
    if (length(grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x))==0){
      print(paste(team,' did not play any games in ', year, sep=""))}
    else{
      game_results=get_game_results(x)
      game_dates=get_game_date(x)
      game_opploc=get_game_opponent(x)
      game_id=as.numeric(get_game_id(x))
      # Put everything in a data frame
      team_schedule <- data.frame(year=year, date=game_dates, game_id=game_id, team=team, opploc=game_opploc, result=game_results, stringsAsFactors=FALSE)%>%
        mutate(
          # location = home (H) / away (A) / neutral (N)
          loc = case_when(
            grepl("^@", opploc) ~ "A",
            grepl(" @ ", opploc, fixed=TRUE) ~ "N",
            TRUE ~ "H"),
          # opponent
          opp = case_when(
            loc=="A" ~ gsub("@","",opploc),
            loc=="H" ~ opploc,
            loc=="N" ~ substring(opploc, 1, regexpr("@", opploc)-2)))%>%
        # select important all but opploc
        select(year, date, game_id, team, opp, loc, result)
    }
  }
  return(team_schedule)
}


############################################

# Get schedule for every team over years
get_season_schedule = function(year_start, year_end, division=1){
  if(year_start>year_end){
    print('End year must same as, or later than, starting year')
  }
  else{
    year_ids = get_year_ids(year_start, year_end, division=1)
    team_ids = get_team_ids(year_start, year_end, division=1)
    
    for (i in 1:nrow(team_ids)){
      for (j in 1:nrow(year_ids)){
        team <- toString(team_ids[i,'team'])
        teamid <- toString(team_ids[i,'id'])
        
        year <- toString(year_ids[j,'year'])
        yearid <- toString(year_ids[j,'year_id'])
        
        team_url <- paste(base_url, "/team/index/", yearid, "?org_id=", teamid, sep="")
        
        x <- try(scan(team_url, what="", sep="\n"))
        if (class(x)=='try-error'){
          print(paste('Cannot connect to server for', team, 'in', year))}
        else{
          if (length(grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x))==0){
            print(paste(team,' did not play any games in ', year, sep=""))}
          else{
            game_results=get_game_results(x)
            game_dates=get_game_date(x)
            game_opploc=get_game_opponent(x)
            game_id=as.numeric(get_game_id(x))
            # Put everything in a data frame
            team_schedule <- data.frame(year=year, date=game_dates, game_id=game_id, team=team, opploc=game_opploc, result=game_results, stringsAsFactors=FALSE)%>%
              mutate(
                # location = home (H) / away (A) / neutral (N)
                loc = case_when(
                  grepl("^@", opploc) ~ "A",
                  grepl(" @ ", opploc, fixed=TRUE) ~ "N",
                  TRUE ~ "H"),
                # opponent
                opp = case_when(
                  loc=="A" ~ gsub("@","",opploc),
                  loc=="H" ~ opploc,
                  loc=="N" ~ substring(opploc, 1, regexpr("@", opploc)-2)))%>%
              # select important all but opploc
              select(year, date, game_id, team, opp, loc, result)
            if (i==1){
              schedule <- team_schedule} 
            else{
              schedule <- rbind(schedule, team_schedule)}
          }
        }
      }
    }
    return(schedule)
  }
}
