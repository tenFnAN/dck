
scrap_start_session = function(.browser = 'firefox', .port = 132L, .hide = F, ...){ 
  # .browser ='chrome'
  # driver <- RSelenium::rsDriver(browser=c("chrome"),  chromever="91.0.4472.101",  port=4545L,  verbose=F)
  
  if(.hide){
    driver        <- RSelenium::rsDriver(browser = .browser, port = .port ,
                                         # hide Browser 
                                         extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless')))
                                         # , check = F 
                                         ,...)  
  }else{
    driver        <- RSelenium::rsDriver(browser = .browser, port = .port,... )  
  }
  
  pid           <- driver$server$process$get_pid() # get the PID of the process you launched
  remote_driver <- driver[["client"]] 
  assign("pid",           pid,           envir = .GlobalEnv)
  assign("remote_driver", remote_driver, envir = .GlobalEnv)
  # driver$client
}

scrap_start_session = function(.port = 4444L, ...){ 
  # .browser ='chrome'
  # driver <- RSelenium::rsDriver(browser=c("chrome"),  chromever="91.0.4472.101",  port=4545L,  verbose=F)
  
  fprof <- RSelenium::makeFirefoxProfile(
    list(
      browser.download.dir = "/home/seluser/Downloads"
    )
  )
  driver        <- RSelenium::rsDriver(browser = 'firefox', port = .port, extraCapabilities = fprof, ...)
  pid           <- driver$server$process$get_pid() # get the PID of the process you launched
  remote_driver <- driver[["client"]] 
  assign("pid",           pid,           envir = .GlobalEnv)
  assign("remote_driver", remote_driver, envir = .GlobalEnv) 
  # driver$client
}
scrap_start_session = function(.port = 4444L){ 
  # .browser ='chrome'
  # driver <- RSelenium::rsDriver(browser=c("chrome"),  chromever="91.0.4472.101",  port=4545L,  verbose=F)
  
  fprof <- RSelenium::makeFirefoxProfile(
    list(
      browser.download.dir = "/home/seluser/Downloads"
    )
  )
  
  remote_driver <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = .port,
    browserName = "firefox",
    extraCapabilities = fprof
  )
  
  remote_driver$open(silent = TRUE) 
  
  assign("remote_driver", remote_driver, envir = .GlobalEnv)
  # driver$client
}
scrap_navigate = function(.link, .sleep = 1, .driver = remote_driver){
  .driver$navigate(.link)  
  Sys.sleep( .sleep )
}

scrap_kill_session = function(.pid = pid, .driver = remote_driver){
  tryCatch(.driver$close(), error = function(e) {  })
  if(exists('remote_driver', envir = .GlobalEnv)){  rm(remote_driver, envir = .GlobalEnv) ; gc() }
  # kill the processes
  if(.Platform$OS.type == 'windows'){
    system(paste0("Taskkill /F /T" ," /PID ", .pid))  
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  } else if(.Platform$OS.type  == 'unix'){
    system(paste0('kill -9 ', .pid))
  } 
  if(exists('pid', envir = .GlobalEnv)){  rm(pid, envir = .GlobalEnv) ; gc() } 
}

tidy_rownumber = function(dat_, by_, col_name = 'id'){
  ds = as.data.table(dat_) 
  str_eval = paste0('ds[,', parse(text=col_name)  , ' := as.integer(row.names(.SD)), by = list(', paste0("get('", by_ ,"')", collapse = ','), ')]')
  ds = eval(parse(text=str_eval))
  
  # ds[, 'id' := row.names(.SD), by = list(P1)] 
  as.data.frame(ds)
}
tidy_slice_rows = function(ds, by_ = c(param_grouping, 'date_'), iloc = 1){
  str_eval = paste0('ds[ds[, .I[1:get("iloc")], by = list(', paste0("get('", by_ ,"')", collapse = ','), ') ]$V1]') 
  ds = data.table::as.data.table(ds)
  ds = eval(parse(text=str_eval))
  # ds = ds[ds[, .I[1], by = list(P1, yr, NAME_T) ]$V1] 
  ds = eval(parse(text=paste0('ds[!is.na(get( by_[1] ) ),]'))) # filtrowanie brakow
  ds = as.data.frame(ds)
  ds
}

change_polish_characters = function(x){
  gsub('ń', 'n', x, perl = T) %>% 
    gsub('ą', 'a', ., perl = T) %>% 
    gsub('ż|ź', 'z', ., perl = T) %>% 
    gsub('ć', 'c', ., perl = T) %>% 
    gsub('ó', 'o', ., perl = T) %>% 
    gsub('ł', 'l', ., perl = T) %>% 
    gsub('ę', 'e', ., perl = T) %>% 
    gsub('ś', 's', ., perl = T)
}

get_schedule = function(url){ 
  tab_ = tryCatch({
    session <- tryCatch(polite::bow(url, user_agent = "Webscrapdding Tutorial"), error = function(e) { 'err'})
    website1 = polite::scrape(session) %>% html_nodes("#u868")
    tab_ = html_table(website1[[1]],fill=TRUE)
    tab_ 
  }, error = function(e) { 
    simple <- read_html( url ) %>% html_nodes("#u868") %>% html_table %>% as.data.frame()  
    
    tab_ = simple   
    
    tab_
  }) 
  
  tbl2     <- tab_ %>% 
    dplyr::filter(X3 != '', !str_detect(X3, 'adsbygoogle|Advertisement')) %>% 
    dplyr::select(-X1, -X2, -X4, -X5) %>% 
    .[2:nrow(.),] %>%  
    setNames( paste0(gsub(' ', '_', .[1,]), '_', 1:length(names(.))) ) 
  
  tbl2 = tbl2 %>%
    dplyr::rename(
      Player_Name_1 = names(.)[1]
    ) %>% 
    dplyr::mutate( 
      Player_Name_1 = case_when(   
        Player_Name_1 ==  'Catherine Bellis'    ~ 'Catherine Cartan Bellis',
        Player_Name_1 ==  'Darya Kasatkina'     ~ 'Daria Kasatkina',
        Player_Name_1 ==  'Irina Bara'          ~ 'Irina Maria Bara',
        Player_Name_1 ==  'Stefanie Vogele'     ~ 'Stefanie Voegele',
        Player_Name_1 ==  'Julia Gorges'        ~ 'Julia Goerges',
        Player_Name_1 ==  'Elena-Gabriela Ruse'  ~ 'Elena Gabriela Ruse',
        Player_Name_1 ==  'Giulia Gatto-Monticone'  ~ 'Giulia Gatto-Monticone',
        Player_Name_1 ==  'Greet Minnen'         ~ 'Greetje Minnen',
        Player_Name_1 ==  'Bianca Andreescu'     ~ 'Bianca Vanessa Andreescu',
        Player_Name_1 ==  'Danielle Collins'     ~ 'Danielle Rose Collins',
        Player_Name_1 ==  'Kristyna Pliđkova'    ~ 'Kristyna Pliskova',
        Player_Name_1 ==  'Su-wei Hsieh'         ~ 'Su-Wei Hsieh',
        Player_Name_1 ==  'Aliona Bolsova Zadoinov'  ~ 'Aliona Bolsova',
        Player_Name_1 ==  'Catherine McNally'    ~ 'Caty McNally',
        Player_Name_1 ==  'Christina McHale'     ~ 'Christina Mchale',
        Player_Name_1 ==  'Leylah Fernandez'     ~ 'Leylah Annie Fernandez',
        Player_Name_1 ==  'Ludmilla Samsonova'   ~ 'Liudmila Samsonova',
        Player_Name_1 ==  'Anna Karolina Schmiedlova'   ~ 'Anna Schmiedlova',
        Player_Name_1 ==  'Georgina Garcia Perez'   ~ 'Georgina Garcia-Perez',
        Player_Name_1 ==  'Indy de Vroome'       ~ 'Indy De Vroome',
        Player_Name_1 ==  'Laura Ioana Paar'     ~ 'Laura-Ioana Paar',
        Player_Name_1 ==  'Lesia Tsurenko'       ~ 'Lesya Tsurenko',
        Player_Name_1 ==  'Xinyu Wang'           ~ 'Xin Yu Wang',
        Player_Name_1 ==  'CoCo Vandeweghe'      ~ 'Coco Vandeweghe',
        Player_Name_1 ==  'En Liang'             ~ 'En Shuo Liang',
        Player_Name_1 ==  'Jiajing Lu'           ~ 'Jia-Jing Lu',
        Player_Name_1 ==  'Jodie Burrage'        ~ 'Jodie Anna Burrage',
        Player_Name_1 ==  'Kathinka von Deichmann' ~ 'Kathinka Von Deichmann',
        Player_Name_1 ==  'Kaylah McPhee'        ~ 'Kaylah Mcphee',
        Player_Name_1 ==  'Na-lae Han'           ~ 'Na-Lae Han',
        Player_Name_1 ==  'Nuria Parrizas Diaz'  ~ 'Nuria Parrizas-Diaz',
        Player_Name_1 ==  'Reka Luca Jani'       ~ 'Reka-Luca Jani',
        Player_Name_1 ==  'Carolina Alves'       ~ 'Carolina Meligeni Alves',
        Player_Name_1 ==  'Da-bin Kim'           ~ 'Dabin Kim',
        Player_Name_1 ==  'Daria Lopatetska'     ~ 'Daria Lopatetskaya',
        Player_Name_1 ==  'Eudice Chong'         ~ 'Eudice Wong Chong',
        Player_Name_1 ==  'Georgia Craciun'      ~ 'Georgia Andreea Craciun',
        Player_Name_1 ==  'Guiomar Maristany Zuleta De Reales'           ~ 'Guiomar Zuleta De Reales',
        Player_Name_1 ==  'Han-na Chang'         ~ 'Hanna Chang',
        Player_Name_1 ==  'Miriam Bulgaru'       ~ 'Miriam Bianca Bulgaru',
        Player_Name_1 ==  'Paige Hourigan'       ~ 'Paige Mary Hourigan',
        Player_Name_1 ==  'Raluca Serban'        ~ 'Raluca Georgiana Serban',
        Player_Name_1 ==  'Su-jeong Jang'        ~ 'Su Jeong Jang',
        Player_Name_1 ==  ''           ~ '',
        
        
        
        
        Player_Name_1 ==  'Dominik Kopfer'  ~ 'Dominik Koepfer',
        Player_Name_1 ==  'Pedro Martinez'  ~ 'Pedro Martinez Portero',
        Player_Name_1 ==  'Pierre Hugues Herbert'  ~ 'Pierre-Hugues Herbert',
        Player_Name_1 ==  'Ricardas Berankis'  ~ 'Richard Berankis',
        Player_Name_1 ==  'Soonwoo Kwon'  ~ 'Soon-Woo Kwon',
        Player_Name_1 ==  'Botic Van de Zandschulp'  ~ 'Botic Van De Zandschulp',
        Player_Name_1 ==  'Carlos Alcaraz'           ~ 'Carlos Alcaraz Garfia',
        Player_Name_1 ==  'Cedrik Marcel Stebe'      ~ 'Cedrik-Marcel Stebe',
        Player_Name_1 ==  'Guillermo Garcia Lopez'   ~ 'Guillermo Garcia-Lopez',
        Player_Name_1 ==  'Juan Martin del Potro'    ~ 'Juan Martin Del Potro',
        Player_Name_1 ==  'Marc-Andrea Husler'       ~ 'Marc-Andrea Huesler',
        Player_Name_1 ==  'Thai Son Kwiatkowski'     ~ 'Thai Kwiatkowski',
        Player_Name_1 ==  'Chun hsin Tseng'          ~ 'Chun Hsin Tseng',
        Player_Name_1 ==  'Duck-hee Lee'             ~ 'Duckhee Lee',
        Player_Name_1 ==  'Jason Kubler'             ~ 'Jason Murray Kubler',
        Player_Name_1 ==  'JC Aragone'               ~ 'Jc Aragone',
        Player_Name_1 ==  'Ji-sung Nam'              ~ 'Ji Sung Nam',
        Player_Name_1 ==  'Mackenzie McDonald'       ~ 'Mackenzie Mcdonald',
        Player_Name_1 ==  'Roberto Ortega Olmedo'    ~ 'Roberto Ortega-Olmedo',
        Player_Name_1 ==  'Sasikumar Mukund'         ~ 'Sasi Kumar Mukund',
        Player_Name_1 ==  'Teymuraz Gabashvili'      ~ 'Teimuraz Gabashvili',
        Player_Name_1 ==  'Tung Lin Wu'              ~ 'Tung-Lin Wu',
        Player_Name_1 ==  'Aleksandr Nedovyesov'     ~ 'Aleksandr Nedovesov',
        Player_Name_1 ==  'Carlos Gomez Herrera'     ~ 'Carlos Gomez-Herrera',
        Player_Name_1 ==  'Johannes Harteis'         ~ 'Johannes Haerteis',
        Player_Name_1 ==  'John Patrick Smith'       ~ 'John-Patrick Smith',
        Player_Name_1 ==  'Jose Hernandez Fernandez' ~ 'Jose Hernandez-Fernandez',
        Player_Name_1 ==  'Karim Mohamed Maamoun'    ~ 'Karim-Mohamed Maamoun',
        Player_Name_1 ==  'Khumoyun Sultanov'        ~ 'Khumoun Sultanov',
        Player_Name_1 ==  'Nikolas Sanchez Izquierdo'  ~ 'Nikolas Sanchez-Izquierdo',
        Player_Name_1 ==  'Tim van Rijthoven'        ~ 'Tim Van Rijthoven',
        Player_Name_1 ==  'Tsung Hua Yang'           ~ 'Tsung-Hua Yang',
        Player_Name_1 ==  'Yun-seong Chung'          ~ 'Yunseong Chung',
        Player_Name_1 ==  'Alex de Minaur'  ~ 'Alex De Minaur',
        Player_Name_1 ==  'Pablo Carreno Busta'  ~ 'Pablo Carreno-Busta',
        Player_Name_1 ==  'Albert Ramos Vinolas'  ~ 'Albert Ramos-Vinolas',
        Player_Name_1 ==  'Cristian Garin'        ~ 'Christian Garin',
        Player_Name_1 ==  'Jan Lennard Struff'        ~ 'Jan-Lennard Struff',
        Player_Name_1 ==  'Diego Schwartzman'     ~ 'Diego Sebastian Schwartzman',
        Player_Name_1 ==  'Felix Auger-Aliassime'     ~ 'Felix Auger Aliassime',
        Player_Name_1 ==  'Thiago Monteiro'   ~ 'Thiago Moura Monteiro',
        Player_Name_1 ==  'Taylor Fritz'     ~ 'Taylor Harry Fritz',
        Player_Name_1 ==  'J.J. Wolf'     ~ 'Jeff Wolf',
        Player_Name_1 ==  'Jaume Munar'   ~ 'Jaume Antoni Munar Clar',
        TRUE                              ~ Player_Name_1   )
      
    ) %>% 
    dplyr::select(-contains('tmp')) %>%
    dplyr::filter(Player_Name_1 != 'Advertisement') %>%
    dplyr::mutate_all(as.factor) %>%
    dplyr::select(matches('Player|\\/'))
  
  return( tbl2 )
}


