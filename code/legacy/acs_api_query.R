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
  ## HOOP code sample, ACS 2022 (rewrite and make sure replicates at cell level) ############
  
  ## from: (2022 report dir, COMPONENTS/UNITS/hoop-thing? 2022_Housing_Adjustment_Calc_1-18-2024.R)
  
  AT <- readRDS(paste0(acs_directory, set_year, "/", set_year, "_ACS_NYC_TYPE1.rds"))
  
  acs22 <- readRDS(paste0(acs_directory, set_year, "/", set_year, "_ACS_NYC_TYPE1.rds"))
  
  
  hsg22 <- 
    acs22 %>% 
    ## Housing status
    mutate(NONBLDG = ifelse(BLD %in% c(1, 10), 1, 0), ## NON-BUILDING RESIDENTS
           NOCASHRENT = ifelse(TEN == 4, 1, 0), ## NON-CASH RENTERS
           OTHERHSG = ifelse(NONBLDG == 1 | NOCASHRENT == 1, 1, 0), ## NON-BUILDING OR NON-CASH
           RENTER = ifelse(BLD %in% c(2:9) & TEN == 3, 1, 0),         ## Renters
           OWNER = ifelse(BLD %in% c(2:9) & TEN %in% c(1, 2),1,0),     ## Owners
           HOUSING_STATUS = case_when(RENTER == 1 ~ 1,
                                      OWNER == 1 ~ 2,
                                      NONBLDG == 1 ~ 3,
                                      NOCASHRENT == 1 ~ 4,
                                      TRUE ~ 0),
           ## ISSUE, nonbldg and nocash rent aren't distinct!
           ## Prior code overwrite non-distNONBLDG as NOCASHRENT, so line below does as well
           ## but REALLY need to take to PRU!!!!
           HOUSING_STATUS = ifelse(NOCASHRENT == 1, 4, HOUSING_STATUS)
    ) %>% 
    ## Calculate Utility costs for non-cash renters & non-bldg residents
    mutate(elep_No_cash_rent = ifelse(OTHERHSG == 1 & ELEFP==3, ELEP*(ADJHSG/1000000)*12, 0),
           gasp_No_cash_rent = ifelse(OTHERHSG == 1 & GASFP==4, GASP*(ADJHSG/1000000)*12, 0),
           fulp_No_cash_rent = ifelse(OTHERHSG == 1 & FULFP==3, FULP*(ADJHSG/1000000), 0),
           watp_No_cash_rent = ifelse(OTHERHSG == 1 & WATFP==3, WATP*(ADJHSG/1000000), 0)
    ) %>% 
    ## Calculate HOOP
    mutate(HOOP_NYCGOV = 0,
           HOOP_NYCGOV = case_when(RENTER == 1 ~ GRNTP*12,
                                   OWNER == 1 ~ SMOCP*12,
                                   NOCASHRENT == 1 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   NONBLDG == 1 & TEN %in% c(1,2) ~ SMOCP*12,
                                   NONBLDG == 1 & TEN == 3 ~ GRNTP*12,
                                   NONBLDG == 1 & TEN == 4 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   TRUE ~ 0))
  
  
  ## Compare DFs 
  hsg22_test <- hsg22 %>% 
    select(SERIALNO, SPORDER, HOUSING_STATUS, elep_No_cash_rent:HOOP_NYCGOV) %>% 
    arrange(SERIALNO, SPORDER)
  
  AT_test <- AT %>% 
    select(SERIALNO, SPORDER, HOUSING_STATUS, elep_No_cash_rent:HOOP_NYCGOV) %>% 
    arrange(SERIALNO, SPORDER)
  
  compDF <- hsg22_test != AT_test %>% as.data.frame()
  
  comp_vars <- colSums(compDF, na.rm = T)
  comp_vars[comp_vars != 0]
  
  # options(scipen=999)
  # diff_slr <- dft.og$pov_unit_prob_value - dft.fin$pov_unit_prob_value
  # range(diff_slr)
  
  
  ### To see the 10 instances on non-dostinct nocash rent and nonbld, run hsg22 without 
  ### the seconf HOUSING_STATUS line
  bothDF <- hsg22_test %>% 
    left_join(AT_test, by = c("SERIALNO", "SPORDER")) %>% 
    mutate(diff_hs = ifelse(HOUSING_STATUS.x != HOUSING_STATUS.y, 1, 0)) %>% 
    filter(diff_hs ==1) #%>% distinct(HOUSING_STATUS.x, HOUSING_STATUS.y)
  
  bothDF %>% head()
  
  ## Pull the diff ones
  hsg22 %>% 
    filter(SERIALNO %in% bothDF$SERIALNO) %>% 
    select(SERIALNO, SPORDER, 
           BLD, TEN, NONBLDG:OWNER,
           HOUSING_STATUS, elep_No_cash_rent:HOOP_NYCGOV)
  
  ## ELEP: electricity cost
  ## GASP: gas
  ## FULP: fuel
  ## WATP: water
  
  ######### (1) Determine housing status 
  #NON-BUILDING RESIDENTS
  NONBLDG<-AT$BLD%in%c(1,10)
  #NON-CASH RENTERS
  NOCASHRENT<-AT$TEN==4
  #NON-BUILDING OR NON-CASH
  OTHERHSG<-AT$BLD%in%c(1,10) | AT$TEN==4
  #RENTERS
  RENTER<-AT$BLD%in%c(2:9) & AT$TEN==3 
  #OWNERS
  OWNER<-AT$BLD%in%c(2:9) & AT$TEN%in%c(1,2) 
  
  AT$HOUSING_STATUS<-0
  AT$HOUSING_STATUS[RENTER]<-1
  AT$HOUSING_STATUS[OWNER]<-2
  AT$HOUSING_STATUS[NONBLDG]<-3
  AT$HOUSING_STATUS[NOCASHRENT]<-4
  table(AT$HOUSING_STATUS)
  
  
  ########### (2) Calculate Utility costs for non-cash renters & non-bldg residents
  AT$elep_No_cash_rent<-0
  AT$gasp_No_cash_rent<-0
  AT$fulp_No_cash_rent<-0
  AT$watp_No_cash_rent<-0
  
  
  #if (YEAR>=2018) {
  #  if (YEAR>=2021){FF<-AT$HsgStatus%in%c(6,7)} else {FF<-AT$HsgStatus%in%c(7,10)}
  AT$ELEP[OTHERHSG & is.na(AT$ELEP)]<-0    
  AT$GASP[OTHERHSG & is.na(AT$GASP)]<-0
  AT$FULP[OTHERHSG & is.na(AT$FULP)]<-0
  AT$WATP[OTHERHSG & is.na(AT$WATP)]<-0
  AT$elep_No_cash_rent[OTHERHSG & AT$ELEFP==3]<- (AT$ELEP[OTHERHSG & AT$ELEFP==3]*(AT$ADJHSG[OTHERHSG & AT$ELEFP==3]/1000000))*12
  AT$gasp_No_cash_rent[OTHERHSG & AT$GASFP==4]<- (AT$GASP[OTHERHSG & AT$GASFP==4]*(AT$ADJHSG[OTHERHSG & AT$GASFP==4]/1000000))*12
  AT$fulp_No_cash_rent[OTHERHSG & AT$FULFP==3]<- (AT$FULP[OTHERHSG & AT$FULFP==3]*(AT$ADJHSG[OTHERHSG & AT$FULFP==3]/1000000))
  AT$watp_No_cash_rent[OTHERHSG & AT$WATFP==3]<- (AT$WATP[OTHERHSG & AT$WATFP==3]*(AT$ADJHSG[OTHERHSG & AT$WATFP==3]/1000000))
  
  #for (i in 1:nrow(AT[FF,])) {
  AT[OTHERHSG & AT$ELEFP%in% c(1,2),"elep_No_cash_rent"]<-0
  AT[OTHERHSG & AT$GASFP%in% c(1,2,3),"gasp_No_cash_rent"]<-0 
  AT[OTHERHSG & AT$FULFP%in% c(1,2),"fulp_No_cash_rent"]<-0 
  AT[OTHERHSG & AT$WATFP%in% c(1,2),"watp_No_cash_rent"]<-0
  
  # } else if (YEAR<2018){
  #   FF<-AT$HsgStatus%in%c(7,10)
  #   AT$elep_No_cash_rent[FF]<- AT$ELEP[FF]*(AT$ADJHSG[FF]/1000000)
  #   AT$gasp_No_cash_rent[FF]<- AT$GASP[FF]*(AT$ADJHSG[FF]/1000000)
  #   AT$fulp_No_cash_rent[FF]<- AT$FULP[FF]*(AT$ADJHSG[FF]/1000000)
  #   AT$watp_No_cash_rent[FF]<- AT$WATP[FF]*(AT$ADJHSG[FF]/1000000)
  #   
  #   AT[FF & AT$ELEP%in% c(1,2),"elep_No_cash_rent"]<-0
  #   AT[FF & AT$GASP%in% c(1,2,3),"gasp_No_cash_rent"]<-0
  #   AT[FF & AT$FULP%in%c(1,2),"fulp_No_cash_rent"]<-0
  #   AT[FF & AT$WATP%in% c(1,2),"watp_No_cash_rent"]<-0
  # }
  
  # calculate hoop for all housing types
  AT$HOOP_NYCGOV <- 0
  #RENTERS
  AT$HOOP_NYCGOV[RENTER]<-AT$GRNTP[RENTER] * 12
  #OWNERS
  AT$HOOP_NYCGOV[OWNER]<-AT$SMOCP[OWNER] * 12
  # Non-Cash Renters
  AT$HOOP_NYCGOV[NOCASHRENT]<-((12 * AT$elep_No_cash_rent[NOCASHRENT]) 
                               + (12 * AT$gasp_No_cash_rent[NOCASHRENT]) 
                               + AT$fulp_No_cash_rent[NOCASHRENT] 
                               + AT$watp_No_cash_rent[NOCASHRENT])
  # Non-building residents
  AT$HOOP_NYCGOV[NONBLDG & AT$TEN%in%c(1,2)] <- 
    AT$SMOCP[NONBLDG & AT$TEN%in%c(1,2)] * 12
  AT$HOOP_NYCGOV[NONBLDG & AT$TEN==3] <- 
    AT$GRNTP[NONBLDG & AT$TEN==3] * 12
  AT$HOOP_NYCGOV[NONBLDG & AT$TEN==4] <- 
    (12 * AT$elep_No_cash_rent[NONBLDG & AT$TEN==4]) + 
    (12 * AT$gasp_No_cash_rent[NONBLDG & AT$TEN==4]) + 
    (AT$fulp_No_cash_rent[NONBLDG & AT$TEN==4]) + 
    (AT$watp_No_cash_rent[NONBLDG & AT$TEN==4]) 
  
  
  #
  ##
  ## HOOP code sample, ACS 2018 (rewrite and make sure replicates at cell level) ############
  
  ## Doing a replic of 2018 bc I'm currently gettign $0 hoop for n cash rent, which is sus
  
  ## issue seems to be ADJHSG is diff represented in 2019 & 2019 API data only??????
  ## it's "1" instead of 1000000 ?!?!?!?!
  
  ## from: \\Chgoldfs\PRU\18_2005_2018_Poverty_Report\DATA\1yr\2018_BASE_IV_FINAL_withGap.CSV
  
  AT18 <- read.csv("//Chgoldfs/PRU/18_2005_2018_Poverty_Report/DATA/1yr/2018_BASE_IV_FINAL_withGap.CSV", stringsAsFactors = F)
  
  set_year <- 2018
  acs18 <- readRDS(paste0(acs_directory, set_year, "/", set_year, "_ACS_NYC_TYPE1.rds"))
  
  
  hsg18 <- 
    acs18 %>% 
    ## Housing status
    mutate(NONBLDG = ifelse(BLD %in% c(1, 10), 1, 0), ## NON-BUILDING RESIDENTS
           NOCASHRENT = ifelse(TEN == 4, 1, 0), ## NON-CASH RENTERS
           OTHERHSG = ifelse(NONBLDG == 1 | NOCASHRENT == 1, 1, 0), ## NON-BUILDING OR NON-CASH
           RENTER = ifelse(BLD %in% c(2:9) & TEN == 3, 1, 0),         ## Renters
           OWNER = ifelse(BLD %in% c(2:9) & TEN %in% c(1, 2),1,0),     ## Owners
           HOUSING_STATUS = case_when(RENTER == 1 ~ 1,
                                      OWNER == 1 ~ 2,
                                      NONBLDG == 1 ~ 3,
                                      NOCASHRENT == 1 ~ 4,
                                      TRUE ~ 0),
           ## ISSUE, nonbldg and nocash rent aren't distinct!
           ## Prior code overwrite non-distNONBLDG as NOCASHRENT, so line below does as well
           ## but REALLY need to take to PRU!!!!
           HOUSING_STATUS = ifelse(NOCASHRENT == 1, 4, HOUSING_STATUS)
    ) %>% 
    ## Calculate Utility costs for non-cash renters & non-bldg residents
    mutate(elep_No_cash_rent = ifelse(OTHERHSG == 1 & ELEFP==3, ELEP*(ADJHSG/1000000)*12, 0),
           gasp_No_cash_rent = ifelse(OTHERHSG == 1 & GASFP==4, GASP*(ADJHSG/1000000)*12, 0),
           fulp_No_cash_rent = ifelse(OTHERHSG == 1 & FULFP==3, FULP*(ADJHSG/1000000), 0),
           watp_No_cash_rent = ifelse(OTHERHSG == 1 & WATFP==3, WATP*(ADJHSG/1000000), 0)
    ) %>% 
    ## Calculate HOOP
    mutate(HOOP_NYCGOV = 0,
           HOOP_NYCGOV = case_when(RENTER == 1 ~ GRNTP*12,
                                   OWNER == 1 ~ SMOCP*12,
                                   NOCASHRENT == 1 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   NONBLDG == 1 & TEN %in% c(1,2) ~ SMOCP*12,
                                   NONBLDG == 1 & TEN == 3 ~ GRNTP*12,
                                   NONBLDG == 1 & TEN == 4 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   TRUE ~ 0))
  
  ## For some reason the source code provided has diff names and the statuses aren't in the pov file?!?!?!?!!?
  AThsg <- AT18 %>% 
    ## Housing status
    mutate(NONBLDG = ifelse(BLD %in% c(1, 10), 1, 0), ## NON-BUILDING RESIDENTS
           NOCASHRENT = ifelse(TEN == 4, 1, 0), ## NON-CASH RENTERS
           OTHERHSG = ifelse(NONBLDG == 1 | NOCASHRENT == 1, 1, 0), ## NON-BUILDING OR NON-CASH
           RENTER = ifelse(BLD %in% c(2:9) & TEN == 3, 1, 0),         ## Renters
           OWNER = ifelse(BLD %in% c(2:9) & TEN %in% c(1, 2),1,0),     ## Owners
           HOUSING_STATUS = case_when(RENTER == 1 ~ 1,
                                      OWNER == 1 ~ 2,
                                      NONBLDG == 1 ~ 3,
                                      NOCASHRENT == 1 ~ 4,
                                      TRUE ~ 0),
           ## ISSUE, nonbldg and nocash rent aren't distinct!
           ## Prior code overwrite non-distNONBLDG as NOCASHRENT, so line below does as well
           ## but REALLY need to take to PRU!!!!
           HOUSING_STATUS = ifelse(NOCASHRENT == 1, 4, HOUSING_STATUS)
    ) %>% 
    ## Calculate Utility costs for non-cash renters & non-bldg residents
    mutate(elep_No_cash_rent = ifelse(OTHERHSG == 1 & ELEFP==3, ELEP*(ADJHSG/1000000)*12, 0),
           gasp_No_cash_rent = ifelse(OTHERHSG == 1 & GASFP==4, GASP*(ADJHSG/1000000)*12, 0),
           fulp_No_cash_rent = ifelse(OTHERHSG == 1 & FULFP==3, FULP*(ADJHSG/1000000), 0),
           watp_No_cash_rent = ifelse(OTHERHSG == 1 & WATFP==3, WATP*(ADJHSG/1000000), 0)
    ) %>% 
    ## Calculate HOOP
    mutate(HOOP_NYCGOV = 0,
           HOOP_NYCGOV = case_when(RENTER == 1 ~ GRNTP*12,
                                   OWNER == 1 ~ SMOCP*12,
                                   NOCASHRENT == 1 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   NONBLDG == 1 & TEN %in% c(1,2) ~ SMOCP*12,
                                   NONBLDG == 1 & TEN == 3 ~ GRNTP*12,
                                   NONBLDG == 1 & TEN == 4 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   TRUE ~ 0)) %>% 
    ## diff from native hoop?
    mutate(noNA_hoopceo = ifelse(is.na(HOOP_CEO), 0, HOOP_CEO),
           diff_hoop = HOOP_NYCGOV - noNA_hoopceo,
           hoop_diff = ifelse(abs(diff_hoop) > 0.001, 1, 0))
  
  sum(AThsg$hoop_diff)
  hist(AThsg$HOOP_CEO)
  hist(AThsg$diff_hoop)
  
  ## are hoop just diff for no cash rent?
  AThsg %>% 
    ggplot(aes(x = factor(HOUSING_STATUS), y = diff_hoop, fill = HOUSING_STATUS)) +
    geom_bar(stat = "identity", position = "dodge")
  
  ## Plot 2018 HOOP by HS to compare tomy API values
  hsg18 %>% 
    filter(RELP == 0) %>% 
    group_by(HOUSING_STATUS) %>% 
    summarise(med = weighted.median(HOOP_NYCGOV, WGTP)) %>% 
    arrange(HOUSING_STATUS)
  
  ## Compare DFs 
  hsg18_test <- hsg18 %>% 
    select(SERIALNO, SPORDER, HOUSING_STATUS, elep_No_cash_rent:HOOP_NYCGOV) %>% 
    arrange(SERIALNO, SPORDER)
  
  names_from_hsg18 <- names(hsg18_test)
  
  
  # AT_test_hoop <- AT18 %>% 
  #   mutate(HOOP_NYCGOV = HOOP_CEO) %>% 
  #   select(SERIALNO, SPORDER, HOOP_NYCGOV)
  
  AT18 %>% 
    select(all_of(names_from_hsg18)) %>% 
    arrange(SERIALNO, SPORDER)
  
  compDF <- hsg22_test != AT_test %>% as.data.frame()
  
  comp_vars <- colSums(compDF, na.rm = T)
  comp_vars[comp_vars != 0]
  
  # options(scipen=999)
  # diff_slr <- dft.og$pov_unit_prob_value - dft.fin$pov_unit_prob_value
  # range(diff_slr)
  
  
  ### To see the 10 instances on non-dostinct nocash rent and nonbld, run hsg22 without 
  ### the seconf HOUSING_STATUS line
  bothDF <- hsg22_test %>% 
    left_join(AT_test, by = c("SERIALNO", "SPORDER")) %>% 
    mutate(diff_hs = ifelse(HOUSING_STATUS.x != HOUSING_STATUS.y, 1, 0)) %>% 
    filter(diff_hs ==1) #%>% distinct(HOUSING_STATUS.x, HOUSING_STATUS.y)
  
  bothDF %>% head()
  
  ## Pull the diff ones
  hsg22 %>% 
    filter(SERIALNO %in% bothDF$SERIALNO) %>% 
    select(SERIALNO, SPORDER, 
           BLD, TEN, NONBLDG:OWNER,
           HOUSING_STATUS, elep_No_cash_rent:HOOP_NYCGOV)
  
  ## ELEP: electricity cost
  ## GASP: gas
  ## FULP: fuel
  ## WATP: water
  
  ######### (1) Determine housing status 
  #NON-BUILDING RESIDENTS
  NONBLDG<-AT$BLD%in%c(1,10)
  #NON-CASH RENTERS
  NOCASHRENT<-AT$TEN==4
  #NON-BUILDING OR NON-CASH
  OTHERHSG<-AT$BLD%in%c(1,10) | AT$TEN==4
  #RENTERS
  RENTER<-AT$BLD%in%c(2:9) & AT$TEN==3 
  #OWNERS
  OWNER<-AT$BLD%in%c(2:9) & AT$TEN%in%c(1,2) 
  
  AT$HOUSING_STATUS<-0
  AT$HOUSING_STATUS[RENTER]<-1
  AT$HOUSING_STATUS[OWNER]<-2
  AT$HOUSING_STATUS[NONBLDG]<-3
  AT$HOUSING_STATUS[NOCASHRENT]<-4
  table(AT$HOUSING_STATUS)
  
  
  ########### (2) Calculate Utility costs for non-cash renters & non-bldg residents
  AT$elep_No_cash_rent<-0
  AT$gasp_No_cash_rent<-0
  AT$fulp_No_cash_rent<-0
  AT$watp_No_cash_rent<-0
  
  
  #if (YEAR>=2018) {
  #  if (YEAR>=2021){FF<-AT$HsgStatus%in%c(6,7)} else {FF<-AT$HsgStatus%in%c(7,10)}
  AT$ELEP[OTHERHSG & is.na(AT$ELEP)]<-0    
  AT$GASP[OTHERHSG & is.na(AT$GASP)]<-0
  AT$FULP[OTHERHSG & is.na(AT$FULP)]<-0
  AT$WATP[OTHERHSG & is.na(AT$WATP)]<-0
  AT$elep_No_cash_rent[OTHERHSG & AT$ELEFP==3]<- (AT$ELEP[OTHERHSG & AT$ELEFP==3]*(AT$ADJHSG[OTHERHSG & AT$ELEFP==3]/1000000))*12
  AT$gasp_No_cash_rent[OTHERHSG & AT$GASFP==4]<- (AT$GASP[OTHERHSG & AT$GASFP==4]*(AT$ADJHSG[OTHERHSG & AT$GASFP==4]/1000000))*12
  AT$fulp_No_cash_rent[OTHERHSG & AT$FULFP==3]<- (AT$FULP[OTHERHSG & AT$FULFP==3]*(AT$ADJHSG[OTHERHSG & AT$FULFP==3]/1000000))
  AT$watp_No_cash_rent[OTHERHSG & AT$WATFP==3]<- (AT$WATP[OTHERHSG & AT$WATFP==3]*(AT$ADJHSG[OTHERHSG & AT$WATFP==3]/1000000))
  
  #for (i in 1:nrow(AT[FF,])) {
  AT[OTHERHSG & AT$ELEFP%in% c(1,2),"elep_No_cash_rent"]<-0
  AT[OTHERHSG & AT$GASFP%in% c(1,2,3),"gasp_No_cash_rent"]<-0 
  AT[OTHERHSG & AT$FULFP%in% c(1,2),"fulp_No_cash_rent"]<-0 
  AT[OTHERHSG & AT$WATFP%in% c(1,2),"watp_No_cash_rent"]<-0
  
  # } else if (YEAR<2018){
  #   FF<-AT$HsgStatus%in%c(7,10)
  #   AT$elep_No_cash_rent[FF]<- AT$ELEP[FF]*(AT$ADJHSG[FF]/1000000)
  #   AT$gasp_No_cash_rent[FF]<- AT$GASP[FF]*(AT$ADJHSG[FF]/1000000)
  #   AT$fulp_No_cash_rent[FF]<- AT$FULP[FF]*(AT$ADJHSG[FF]/1000000)
  #   AT$watp_No_cash_rent[FF]<- AT$WATP[FF]*(AT$ADJHSG[FF]/1000000)
  #   
  #   AT[FF & AT$ELEP%in% c(1,2),"elep_No_cash_rent"]<-0
  #   AT[FF & AT$GASP%in% c(1,2,3),"gasp_No_cash_rent"]<-0
  #   AT[FF & AT$FULP%in%c(1,2),"fulp_No_cash_rent"]<-0
  #   AT[FF & AT$WATP%in% c(1,2),"watp_No_cash_rent"]<-0
  # }
  
  # calculate hoop for all housing types
  AT$HOOP_NYCGOV <- 0
  #RENTERS
  AT$HOOP_NYCGOV[RENTER]<-AT$GRNTP[RENTER] * 12
  #OWNERS
  AT$HOOP_NYCGOV[OWNER]<-AT$SMOCP[OWNER] * 12
  # Non-Cash Renters
  AT$HOOP_NYCGOV[NOCASHRENT]<-((12 * AT$elep_No_cash_rent[NOCASHRENT]) 
                               + (12 * AT$gasp_No_cash_rent[NOCASHRENT]) 
                               + AT$fulp_No_cash_rent[NOCASHRENT] 
                               + AT$watp_No_cash_rent[NOCASHRENT])
  # Non-building residents
  AT$HOOP_NYCGOV[NONBLDG & AT$TEN%in%c(1,2)] <- 
    AT$SMOCP[NONBLDG & AT$TEN%in%c(1,2)] * 12
  AT$HOOP_NYCGOV[NONBLDG & AT$TEN==3] <- 
    AT$GRNTP[NONBLDG & AT$TEN==3] * 12
  AT$HOOP_NYCGOV[NONBLDG & AT$TEN==4] <- 
    (12 * AT$elep_No_cash_rent[NONBLDG & AT$TEN==4]) + 
    (12 * AT$gasp_No_cash_rent[NONBLDG & AT$TEN==4]) + 
    (AT$fulp_No_cash_rent[NONBLDG & AT$TEN==4]) + 
    (AT$watp_No_cash_rent[NONBLDG & AT$TEN==4]) 
  
  
  #
  ##
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
  
  
  ## Test: HOOP on NYC subset from API (verify cell-2-cell replication) ######
  
  years_to_loop <- c(2018:2022)
  #states_to_loop <- c()
  variables_to_get <- c("SERIALNO", "SPORDER", "PWGTP", "WGTP", "PUMA",
                        "BLD", "TEN", "ADJHSG", "GRNTP", "SMOCP",
                        "ELEFP", "ELEP", "GASFP", "GASP", "FULFP", "FULP", "WATFP", "WATP")
  
  api_vars <- paste(variables_to_get, collapse = ",")
  
  nys_call_url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums?get=", api_vars, "&for=state:36&TYPEHUGQ=1&key=",
                         census_api_toUse)
  
  raw_acs_api <- httr::GET(nys_call_url)
  acs_api_temp_contents <- fromJSON(rawToChar(raw_acs_api$content))
  
  testdf <- acs_api_temp_contents[c(2:nrow(acs_api_temp_contents)),] %>% 
    as.data.frame()
  names(testdf) <- acs_api_temp_contents[1,]
  
  testdf %>% distinct(state) %>% arrange(state) %>% 
    left_join(st_fips, by = c("state" = "fips_code"))
  
  nysdf <- 
    testdf %>% 
    mutate_at(c(2:19), ~as.numeric(.)) %>% 
    mutate(YEAR = 2022)
  #str()
  
  nychsg <- nysdf %>% 
    filter(PUMA %in% c(4103:4503)) %>% 
    ## Housing status
    mutate(NONBLDG = ifelse(BLD %in% c(1, 10), 1, 0), ## NON-BUILDING RESIDENTS
           NOCASHRENT = ifelse(TEN == 4, 1, 0), ## NON-CASH RENTERS
           OTHERHSG = ifelse(NONBLDG == 1 | NOCASHRENT == 1, 1, 0), ## NON-BUILDING OR NON-CASH
           RENTER = ifelse(BLD %in% c(2:9) & TEN == 3, 1, 0),         ## Renters
           OWNER = ifelse(BLD %in% c(2:9) & TEN %in% c(1, 2),1,0),     ## Owners
           HOUSING_STATUS = case_when(RENTER == 1 ~ 1,
                                      OWNER == 1 ~ 2,
                                      NONBLDG == 1 ~ 3,
                                      NOCASHRENT == 1 ~ 4,
                                      TRUE ~ 0),
           ## ISSUE, nonbldg and nocash rent aren't distinct!
           ## Prior code overwrite non-distNONBLDG as NOCASHRENT, so line below does as well
           ## but REALLY need to take to PRU!!!!
           HOUSING_STATUS = ifelse(NOCASHRENT == 1, 4, HOUSING_STATUS)
    ) %>% 
    ## Calculate Utility costs for non-cash renters & non-bldg residents
    mutate(elep_No_cash_rent = ifelse(OTHERHSG == 1 & ELEFP==3, ELEP*(ADJHSG/1000000)*12, 0),
           gasp_No_cash_rent = ifelse(OTHERHSG == 1 & GASFP==4, GASP*(ADJHSG/1000000)*12, 0),
           fulp_No_cash_rent = ifelse(OTHERHSG == 1 & FULFP==3, FULP*(ADJHSG/1000000), 0),
           watp_No_cash_rent = ifelse(OTHERHSG == 1 & WATFP==3, WATP*(ADJHSG/1000000), 0)
    ) %>% 
    ## Calculate HOOP
    mutate(HOOP_NYCGOV = 0,
           HOOP_NYCGOV = case_when(RENTER == 1 ~ GRNTP*12,
                                   OWNER == 1 ~ SMOCP*12,
                                   NOCASHRENT == 1 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   NONBLDG == 1 & TEN %in% c(1,2) ~ SMOCP*12,
                                   NONBLDG == 1 & TEN == 3 ~ GRNTP*12,
                                   NONBLDG == 1 & TEN == 4 ~ (elep_No_cash_rent*12) + (gasp_No_cash_rent*12) + fulp_No_cash_rent + watp_No_cash_rent,
                                   TRUE ~ 0))
  
  ## Compare DFs 
  hsg22_test <- hsg22 %>% 
    select(SERIALNO, SPORDER, HOUSING_STATUS, elep_No_cash_rent:HOOP_NYCGOV) %>% 
    arrange(SERIALNO, SPORDER)
  
  nychsg_test <- nychsg %>% 
    select(SERIALNO, SPORDER, HOUSING_STATUS, elep_No_cash_rent:HOOP_NYCGOV) %>% 
    arrange(SERIALNO, SPORDER)
  
  compDF <- hsg22_test != nychsg_test %>% as.data.frame()
  
  comp_vars <- colSums(compDF, na.rm = T)
  comp_vars[comp_vars != 0]
  
  ## YAY, complete match! Carry on with loop
  
  
  #
  ######## Get API data (loop through states) #######
  ## LOOP: try calling all states and looping by year #####
  
  years_to_loop <- c(2018, 2019, 2021:2023)
  
  for(y in 1:length(years_to_loop)){  #y=1
    
    temp_year <- years_to_loop[[5]]
    print(paste0("Starting ", temp_year))
    
    temp_typeVar <- ifelse(temp_year >= 2020, "TYPEHUGQ", "TYPE")
    temp_relVar <- ifelse(temp_year >= 2019, "RELSHIPP", "RELP")
    temp_stVar <- ifelse(temp_year >= 2023, "STATE", "ST")
    
    # UPDATE VARIABLES BASED ON CALL
    variables_to_get <- c("SERIALNO", "SPORDER", "PWGTP", "WGTP", "PUMA", "MIGPUMA", temp_relVar,
                          "AGEP", "ADJHSG", temp_stVar, "MIGSP", "OCCP", "SCHL",
                          "RAC1P", "HISP")
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