library(tidyverse)

# functions
# Stripwhite function
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

# year / division
division = 1

# base url
base_url = 'http://stats.ncaa.org'


get_year_ids = function(year_start, year_end, division=1){
  if(year_start>year_end){
    print('End year must same as, or later than, starting year')
  }
  else{
  years=toString(year_start)
  for (i in (year_start+1):year_end){
    years=c(years, toString(i))
  }
  
  # Scrape team and year ids within year range
  for (year in years){
    
    # year division url
    year_division_url = paste("http://stats.ncaa.org/team/inst_team_list?sport_code=MBA&academic_year=",year,"&division=",division,"&conf_id=-1&schedule_date=", sep='')
    
    x = scan(year_division_url, what="", sep="\n")
    
    # get lines containing the links 
    team_links = grep("/team/[0-9]*/", x)
    x = x[team_links]
    
    year_id = as.numeric(gsub("^.*team/([0-9]*)/([0-9]*).*$", "\\2", x))[1]
    name = gsub("<[^<>]*>", "", x)
    
   yearid=data.frame(year=year, year_id=year_id,stringsAsFactors=FALSE)
    
    if (year==years[1]){
        year_ids=yearid} 
      else{
       year_ids=rbind(year_ids, yearid)
      }
  }
  
  return(year_ids)
  }
}


get_team_ids = function(year_start, year_end,division=1){
  if(year_start>year_end){
    print('End year must same as, or later than, starting year')
  }
  else{
  years=toString(year_start)
  for (i in (year_start):year_end){
    years=c(years, toString(i))
  }
  
  # Scrape team and year ids within year range
  for (year in years){
    
    # year division url.  Can apply to division 2 or 3 too.
    year_division_url = paste("http://stats.ncaa.org/team/inst_team_list?sport_code=MBA&academic_year=",year,"&division=",division,"&conf_id=-1&schedule_date=", sep='')
    
    x = scan(year_division_url, what="", sep="\n")
    
    # get lines containing the links 
    team_links = grep("/team/[0-9]*/", x)
    x = x[team_links]
    
   # year_id = as.numeric(gsub("^.*team/([0-9]*)/([0-9]*).*$", "\\2", x))[1]
    team_id = as.numeric(gsub("^.*team/([0-9]*)/([0-9]*).*$", "\\1", x))
    name = gsub("<[^<>]*>", "", x)
    name = stripwhite(name)
    
    teamid = data.frame(year=year, team=name, id=team_id, stringsAsFactors=FALSE)%>%
      mutate(team=gsub("&#x27;", "'", team),
             team=gsub("&amp;", "&", team))
    
  #  yearid=data.frame(year=year, year_id=year_id,stringsAsFactors=FALSE)
    
    if (year==years[1]){
      team_ids=teamid}
    else{
      team_ids=rbind(team_ids, teamid)
    }
  }
 # remove repeated team ids and find last year of team existence
  team_ids=team_ids%>%
    group_by(team,id)%>%
    summarize(last_year=max(as.integer(year)))
  return(team_ids)
  }
}
