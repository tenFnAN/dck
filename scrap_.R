#load libs
#library(dplyr)  ; library(collapse)   
#library(rvest)   
#library(RSelenium)
library(dplyr)  ; library(collapse)  ; library(data.table); library(purrr) ; library(lubridate)
library(rvest)  ; library(polite)    ; library(stringr)   ; library(readr)
#source('/home/seluser/functions.R')
source('functions.R')
print('inside script')
print(available_memory())
print(R.version)

#print('system htop')
tns_schedule = data.frame()
 for(type_ in c('atp', 'wta')){
  tb_schedule = get_schedule( url = stringr::str_glue('https://live-tennis.eu/en/{type_}-schedule') ) 
  
  tb_schedule_ = tb_schedule %>%
    reshape2::melt('Player_Name_1') %>%
    fmutate( Data = ymd(paste0(substr(variable, 13, 16), substr(variable, 7, 8), substr(variable, 10, 11) )),
             Position     = '',
             Priority     = '1',
             Information = ifelse(grepl('Qual', value), 'QUALIFYING', 'MAIN DRAW'),
             Kraj         = '',
             Nawierzchnia = '' ) %>%
    frename(Player = Player_Name_1, Turniej = value) %>%
    fsubset(Player != 'player name') %>%
    fselect(-variable) %>%
    fmutate(update = as.Date(Sys.time())) %>%
    fsubset(!Turniej %in% c('-', '') & Player != '(available slot)') %>%
    fmutate(Turniej = as.factor(Turniej)) %>%
    roworderv(c('Player', 'Data')) %>%
    fmutate(type = type_) %>%
    select(type, everything())
  
  tns_schedule = rbind(tns_schedule, tb_schedule_)
}

print('end script')
