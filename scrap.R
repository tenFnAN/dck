#load libs
library(dplyr)  ; library(collapse)  ; library(data.table); library(purrr) ; library(lubridate)
library(rvest)  ; library(polite)    ; library(stringr)   ; library(readr)
#  
library(RSelenium)
source('functions.R')
# https://stackoverflow.com/questions/65926899/how-can-i-get-an-r-environment-via-sys-getenv-with-github-actions-using-secret
# https://stackoverflow.com/questions/77936473/how-can-i-access-file-downloaded-using-rselenium-in-github-actions

print('inside script')
tns_schedule = data.frame()

for(type_ in c('atp', 'wta')){
  tb_schedule = get_schedule( url = str_glue('https://live-tennis.eu/en/{type_}-schedule') ) 
  
  tb_schedule_ = tb_schedule %>%
    reshape2::melt('Player_Name_1') %>%
    fmutate( Data = ymd(paste0(substr(variable, 13, 16), substr(variable, 7, 8), substr(variable, 10, 11) )),
             Position     = '',
             Priority     = '1',
             Information = ifelse(str_detect(value, 'Qual'), 'QUALIFYING', 'MAIN DRAW'),
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

   #print('new test')
   #rD <- rsDriver(browser = "firefox", port = 4444L, verbose = FALSE)
   #remote_driver <- rD[["client"]]
   #print('after new test')
  print(.Platform$OS.type) 
  
  scrap_start_session( ) # check = F
  #print('session have started')
  #print(names(remote_driver))
  print('before navigate')
  scrap_navigate('https://www.flashscore.com/tennis/')
  Sys.sleep(3) 
  print(remote_driver$getTitle())
  print(remote_driver$getCurrentUrl())
  
  www_ = remote_driver$getPageSource()[[1]] %>% read_html()
  tb_ = www_ %>% 
    html_nodes('.event__match') %>% 
    polite::html_attrs_dfr() %>%
    fselect(id, .text) %>% 
    funique() %>%
    fmutate(
      id    = substring(id, 5), 
      type  = ifelse(str_count(.text, '[a-z]{3,} [A-Z]{1}\\.') >= 3, 'double', 'single'), 
      .text = gsub('FRO', '', .text) # TWK
    ) 
  
  print(head(tb_))


tns_schedule %>%
  write.csv(paste0('data/schedule_tour_', round(as.numeric(Sys.time())), '.csv')) 


if(F){
  print(.Platform$OS.type) 
  
  # scrap_start_session()
  scrap_start_session2( ) # check = F
  print('session have started')
  print(names(remote_driver))
  
  scrap_navigate('https://www.flashscore.com/tennis/')
  Sys.sleep(3) 
  print(remote_driver$getTitle())
  print(remote_driver$getCurrentUrl())
  
  www_ = remote_driver$getPageSource()[[1]] %>% read_html()
  tb_ = www_ %>% 
    html_nodes('.event__match') %>% 
    polite::html_attrs_dfr() %>%
    fselect(id, .text) %>% 
    funique() %>%
    fmutate(
      id    = substring(id, 5), 
      type  = ifelse(str_count(.text, '[a-z]{3,} [A-Z]{1}\\.') >= 3, 'double', 'single'), 
      .text = gsub('FRO', '', .text) # TWK
    ) 
  
  print(head(tb_))
  remote_driver$close() 
  
  
}
