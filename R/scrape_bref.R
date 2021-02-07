library(rvest)
library(xml2)
library(janitor)
library(dplyr)
library(magrittr)
library(purrr)

#TODO enforce numeric columns (chr defaults)

scrape_single_table <- function(url) {
  url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table() %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    clean_names()
}

standings_expanded <- function(year) {
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("year must be a four digit number")
  }
  url <- paste0(
    "https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F", 
    year,
    "-standings.shtml%3Fsr%26utm_source%3Ddirect%26utm_medium%3DShare%26utm_campaign%3DShareTool&div=div_expanded_standings_overall")
  
  url %>% 
    scrape_single_table() %>% 
    mutate(year = year)
}

standings_expanded_years <- function(years) {
  years %>% 
    map(standings_expanded) %>% 
    bind_rows()
}

team_batting <- function(year) {
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("year must be a four digit number")
  }
  
  url <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
                year,
                ".shtml&div=div_teams_standard_batting")
  
  url %>% 
    scrape_single_table() %>% 
    filter(tm != "Tm") %>% 
    mutate(tm = if_else(tm == "", "LgTot", tm)) %>% 
    mutate(year = year)
}

team_batting_against <- function(year) {
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("year must be a four digit number")
  }
  
  url <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
                year,
              "-batting-pitching.shtml&div=div_teams_batting_pitching")
  
  url %>% 
    scrape_single_table() %>% 
    filter(tm != "Tm") %>% 
    mutate(tm = if_else(tm == "", "LgTot", tm)) %>% 
    mutate(year = year)
}

team_relief_pitching <- function(year) {
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("year must be a four digit number")
  }
  url <- paste0("https://www.baseball-reference.com/leagues/MLB/",
                year,
                "-reliever-pitching.shtml")
  
  url %>% 
    scrape_single_table() %>% 
    filter(tm != "Tm") %>% 
    mutate(tm = if_else(tm == "", "LgTot", tm)) %>% 
    mutate(year = year)
}

team_relief_pitching_years <- function(years) {
  years %>% 
    map(team_relief_pitching) %>% 
    bind_rows()
}
