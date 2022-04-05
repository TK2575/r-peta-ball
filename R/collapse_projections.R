library(tidyverse)
library(here)
library(janitor)

read_with_filename <- function(path) {
  path %>% 
    read_csv(show_col_types = FALSE) %>% 
    janitor::clean_names() %>% 
    mutate(source = (path %>% sub("^.+/","",.) %>% tools::file_path_sans_ext())) %>% 
    select(source, team, name, war) %>% 
    rename(player = name)
}

collapse_projections <- function(path) {
  file_names <- 
    list.files(path = path,
               pattern = "*.csv",
               full.names = TRUE)
  
  file_names %>% 
    map(read_with_filename) %>% 
    bind_rows() %>% 
    group_by(team, player) %>% 
    summarize(war = (mean(war) %>% round(1))) %>% 
    ungroup()
}






