## Code snippet: ACS PUMS API
## AH 2025.01.05

## Census PUMS API: https://www.census.gov/data/developers/data-sets/census-microdata-api.html
## User guide: https://www2.census.gov/data/api-documentation/microdata-api-user-guide.pdf

# rm(list=ls()); gc()

set_year <- 2022

## Load libraries #####
library(spatstat)    ## install.packages("spatstat")
library(tidyverse) ## install.packages("tidyverse")
#library(haven)
library(scales)
library(Hmisc)
#library(dplyr)
library(forcats)
library(httr)  ## HUD API
library(jsonlite)
library(openxlsx)
library(RColorBrewer)

# 
## Set directories ####

if (Sys.info()["sysname"] != "Darwin"){
  
  # parent_dir <- "//Chgoldfs/pru/Hill_work/Requests...TBD"
  parent_dir <- "C:/Users/lcatalan/OneDrive - NYC OTI/Documents/dev/migration-flows/"
  wk_dir <- paste0(parent_dir, "")
  api_dir <-"a9eb34d84ff4ca0a2f905d4446b154deb90398c8"
  
  acs_directory <-  "C:/Users/lcatalan/OneDrive - NYC OTI/Documents/dev/migration-flows/data/acs/"
  
  source("C:/Users/lcatalan/OneDrive - NYC OTI/Documents/dev/migration-flows/code/acs_convenience_functions.r")
  
  ## for HUD API
  # proxy_url <- "http://bcpxy.nycnet:8080"
  # Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
  # options(scipen=999)
  
}else{
  
  parent_dir <- "/Users/annehill/Dropbox/NYCO_remote/PRU_2022_misc/PRU_2024.01/thresholds/"
  wk_dir <- paste0(parent_dir, "HOOP/")
  acs_directory <- "/Users/annehill/Dropbox/NYCO_remote/poverty_research/ACS_data/1yr_files/"
  #fiveYr_povFile <- "/Users/annehill/Dropbox/NYCO_remote/PRU_2022_misc/PRU_2023.11.08/disconnected_youth/data/FINAL_POVERTY_2017_2021.rds"
  
  api_dir <- paste0("/Users/annehill/Dropbox/NYCO_remote/PRU_2022_misc/poverty_day/", "APIs/")
  
  source("/Users/annehill/Dropbox/NYCO_remote/PRU_2022_misc/PRU_2023.11.08/R_temp/acs_convenience_functions.r")
  
}

## Create/set output directory
out_dir <- paste0(wk_dir, "outputs/")
if(!dir.exists(out_dir)){dir.create(out_dir)}

## API PUMS data
yr_dir <- paste0(wk_dir, "data/acs/")
if(!dir.exists(yr_dir)){dir.create(yr_dir)}

## plots
plt_dir <- paste0(wk_dir, "outputs/")
if(!dir.exists(plt_dir)){dir.create(plt_dir)}

#
## Set up API key ########

## Read in key
# census_api_toRead <- paste0(api_dir, "AH_census_API_key.txt")
# census_api_toUse <- read.table(census_api_toRead, header = FALSE, sep = "", dec = ".")[[1]]
census_api_toUse <- "a9eb34d84ff4ca0a2f905d4446b154deb90398c8"

## Load API
#census_api_key(census_api_toUse)


#
## Load keys/data #####

st_fips <- read.csv(paste0(parent_dir, "data/state_fips_key.csv"), stringsAsFactors = F) %>%
  filter(ast != 1) %>%
  mutate(fips_code = ifelse(Num_code <10, paste0("0", Num_code), as.character(Num_code))) %>%
  select(Name, alpha_code, fips_code)


#
#################### ~~ BEGIN ~~ ##################
donotrun <- FALSE  ## CHANGE THIS TO F to run 

if(donotrun == FALSE){
  
  ######## Prepare Code and Qulity checks (only run once) ############
  ## Example call ##########
  
  ## First pull NYS, since will be looping through states
  
  test_call_url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums?get=SEX,PWGTP,PUMA,MAR&for=state:*&SCHL=24&key=",
                          census_api_toUse)
  
  raw_acs_api <- httr::GET(test_call_url)
  acs_api_temp_contents <- fromJSON(rawToChar(raw_acs_api$content))
  
  testdf <- acs_api_temp_contents[c(2:nrow(acs_api_temp_contents)),] %>% 
    as.data.frame()
  names(testdf) <- acs_api_temp_contents[1,]
  
  testdf %>% distinct(state) %>% arrange(state) %>% 
    left_join(st_fips, by = c("state" = "fips_code"))
  
  for_state_loop <- testdf %>% distinct(state) %>% arrange(state) %>% pull(state)
  saveRDS(for_state_loop, paste0(parent_dir, "list_state_fips_for_API_loop.rds"))
  
  # ## build API call (ESTIMATE)
  # temp_api_call <- paste0("https://api.census.gov/data/", 
  #                         yr,
  #                         "/",
  #                         table_api_snippet, 
  #                         "get=", 
  #                         "NAME,", 
  #                         vec_vec, #"B19082",   #  "B01001", 
  #                         "&for=", 
  #                         geo_api, ##"us:1", 
  #                         "&key=", 
  #                         census_api_toUse)  #  "YOUR_KEY_GOES_HERE"
  # 
  # raw_acs_api <- httr::GET(temp_api_call)
  # acs_api_temp_contents <- fromJSON(rawToChar(raw_acs_api$content))
  # 
  # ## Fill in DF with estimate (but if not available, flag -99)
  # error_getting_estimate <- FALSE
  # tryCatch(acs_api_temp_contents <- fromJSON(rawToChar(raw_acs_api$content)), error = function(e){error_getting_estimate <<- TRUE})
  # 
  # if(error_getting_estimate == F){ 
  #   
  #   vec_index <- which(acs_api_temp_contents[1,] %in% var_temp)
  #   tempE <- t(acs_api_temp_contents[c(1, 2),vec_index]) %>% 
  #     as.data.frame() 
  #   
  #   
  #   index_est.temp <- est.temp[est.temp$Year == yr & est.temp$variable_name %in% tempE$V1 &
  #                                est.temp$Area_Name == geo_temp, c("variable_name", "Estimate")] 
  #   
  #   est.temp[est.temp$Year == yr & est.temp$variable_name %in% tempE$V1 &
  #              est.temp$Area_Name == geo_temp, c("variable_name", "Estimate")] <- tempE
  # 
  # }
  
  
  ######## Get API data (loop through states) #######
  ## LOOP: try calling all states and looping by year #####
  
  years_to_loop <- c(2016:2019, 2021:2023)
  
  for(y in 1:length(years_to_loop)){  #y=1
    
    temp_year <- years_to_loop[[y]]
    print(paste0("Starting ", temp_year))
    
    temp_typeVar <- ifelse(temp_year >= 2020, "TYPEHUGQ", "TYPE")
    temp_relVar <- ifelse(temp_year >= 2019, "RELSHIPP", "RELP")
    temp_stVar <- ifelse(temp_year >= 2023, "STATE", "ST")
    
    # UPDATE VARIABLES BASED ON CALL
    variables_to_get <- c("SERIALNO", "SPORDER", "PWGTP", "WGTP", 
                          temp_stVar, "PUMA", "MIGPUMA", "MIGSP", # geography
                          "AGEP", "OCCP", "SCHL", "ESR", #dems of interest (age, occuptation, education)
                          "HINCP", "FINCP", "GRPIP","ADJINC", # income
                          "RAC1P", "HISP") # ethnicity
    api_vars <- paste(variables_to_get, collapse = ",")
    
    ## Set up API call
    acs_call_url <- paste0("https://api.census.gov/data/", 
                           temp_year,
                           "/acs/acs1/pums?get=", 
                           api_vars, 
                           "&for=state:*", # DATA FOR ALL STATES
                           #temp_st,
                           "&", 
                           temp_typeVar, 
                           "=1&key=",
                           census_api_toUse)
    
    raw_acs_api <- httr::GET(acs_call_url)
    
    error_getting_data <- FALSE
    tryCatch(acs_api_temp_contents <- fromJSON(rawToChar(raw_acs_api$content)), 
             error = function(e){error_getting_data <<- TRUE})
    
    if(error_getting_data == F){
      
      tempdf <- acs_api_temp_contents[c(2:nrow(acs_api_temp_contents)),] %>% 
        as.data.frame()
      
      names(tempdf) <- acs_api_temp_contents[1,]
      
      tempdf2 <- 
        tempdf #%>% 
        # mutate_at(c(2:20), ~as.numeric(.)) %>% 
        # mutate(YEAR = temp_year)
      
      
      rds_name <- paste0("migration_acs_", "allStates", "_", temp_year, ".rds")
      
      saveRDS(tempdf2, paste0(yr_dir, rds_name))
      
      rm(tempdf, tempdf2);gc()
      
    }else{
      
      stop(paste0("ERROR calling data: ", temp_st_code, " (", temp_st, ") ", temp_year))
    }
    
  }
  
  #
}
#################### ~~ END ~~~~###############################