## Recode, relabel, and breakout convenience function for ACS data
## AH: Last updated 2024.06.10

###### AGGREGATE RECODE FUNCTION ###########

## For dependencies, look in function
## Hard-coded most things...

## NOTE: EdAttain here has 6 levels
fxn_recode <- function(dep1, dep2, var_temp, YEAR) {
  
  # dep1 = first dependency (e.g. AGEP)
  # dep2 = second dependency, if needed (e.g. HISP)
  # var_temp = var_temp (e.g. "age_recode" or var_temp)
  ## Added YEAR to account for geo changes in for ACS 2022+
  
  ## TO CHECK DEPENDENCIES: see comments above variable. Also, "var_temp" must exactly match z 
  
  if(missing(dep1)){ stop("ERROR: Missing dependencies in fxn_recode (dep1)") } 
  if(missing(dep2)){ dep2 <- NA} 
  if(missing(YEAR)){ YEAR <- NA } 
  if(missing(var_temp)){ stop("ERROR: Missing var_temp dependency in fxn_recode (variable name)") } 
  if(is.character(var_temp) == F){ stop("ERROR: Third arugument (variable name) must be character") } 
  
  ## Recode bc I copy/paste a lot of this stuff
  x <- as.numeric(dep1)
  y <- as.numeric(dep2)
  z <- as.character(var_temp)
  YEAR <- as.numeric(YEAR)
  
  
  newVal <- case_when(
    
    ## if NA, then NA  (bc of how missing handled, can't do this for y)
    is.na(x) ~ as.numeric(NA),
    
    ## age_recode (dep1 = AGEP) ##
    z == "age_recode" & x < 5 ~ 1,
    z == "age_recode" & x >= 5  & x < 10 ~ 2,
    z == "age_recode" & x >= 10 & x < 15 ~ 3,
    z == "age_recode" & x >= 15 & x < 20 ~ 4,
    z == "age_recode" & x >= 20 & x < 25 ~ 5,
    z == "age_recode" & x >= 25 & x < 30 ~ 6,
    z == "age_recode" & x >= 30 & x < 35 ~ 7,
    z == "age_recode" & x >= 35 & x < 40 ~ 8,
    z == "age_recode" & x >= 40 & x < 45 ~ 9,
    z == "age_recode" & x >= 45 & x < 50 ~ 10,
    z == "age_recode" & x >= 50 & x < 55 ~ 11,
    z == "age_recode" & x >= 55 & x < 60 ~ 12,
    z == "age_recode" & x >= 60 & x < 65 ~ 13,
    z == "age_recode" & x >= 65 & x < 70 ~ 14,
    z == "age_recode" & x >= 70 & x < 75 ~ 15,
    z == "age_recode" & x >= 75 & x < 80 ~ 16,
    z == "age_recode" & x >= 80 & x < 85 ~ 17,
    z == "age_recode" & x >= 85 & x < 90 ~ 18,
    z == "age_recode" & x >= 90 ~ 19,
    
    ## age_3groups  (dep1 = AGEP) ##
    z %in% c("age_3groups", "AgeForReport") & x < 18 ~ 1,
    z %in% c("age_3groups", "AgeForReport") & x >= 18  & x <= 64 ~ 2,
    z %in% c("age_3groups", "AgeForReport") & x >= 65 ~ 3,
    
    ## age_6groups  (dep1 = AGEP) ##
    z == "age_6groups" & x < 16 ~ 1,
    z == "age_6groups" & x >= 16  & x <= 24 ~ 2,
    z == "age_6groups" & x >= 25  & x <= 40 ~ 3,
    z == "age_6groups" & x >= 41  & x <= 64 ~ 4,
    z == "age_6groups" & x >= 65  & x <= 80 ~ 5,
    z == "age_6groups" & x >= 81 ~ 6,
    
    ## hht_recode  (dep1 = HHT) ##
    z == "hht_recode" & x == 1 ~ 1,
    z == "hht_recode" & x == 2 ~ 2,
    z == "hht_recode" & x == 3 ~ 3,
    z == "hht_recode" & x == 4 ~ 4,
    z == "hht_recode" & x == 6 ~ 4,
    z == "hht_recode" & x == 5 ~ 5,
    z == "hht_recode" & x == 7 ~ 5,
    
    ## esr_recode  (dep1 = ESR) ##
    z %in% c("esr_recode", "esr_recode_GQE") & x == 1   ~ 1,
    z %in% c("esr_recode", "esr_recode_GQE") & x == 2   ~ 1,
    z %in% c("esr_recode", "esr_recode_GQE") & x == 4  ~ 1,
    z %in% c("esr_recode", "esr_recode_GQE") & x == 5  ~ 1,
    z %in% c("esr_recode", "esr_recode_GQE") & x == 3 ~ 2,
    z %in% c("esr_recode", "esr_recode_GQE") & x == 6 ~ 3,
    
    ## Ethnicity  (dep1 = RAC1P, dep2 = HISP)
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & y > 1  ~ 4,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 1 & y == 1 ~ 1,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 2 & y == 1 ~ 2,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 6 & y == 1 ~ 3,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 3 & y == 1 ~ 5,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 4 & y == 1 ~ 5,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 5 & y == 1 ~ 5,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 7 & y == 1 ~ 5,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 8 & y == 1 ~ 5,
    z %in% c("Ethnicity", "Ethnicity_GQE", "ethnicity", "ethnicity_GQE") & x == 9 & y == 1 ~ 5,
    
    ## Ethnicity_recode   (dep1 = RAC1P, dep2 = HISP)
    z %in% c("Ethnicity_recode", "ethnicity_recode") & y > 1  ~ 4,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 1 & y == 1 ~ 1,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 2 & y == 1 ~ 2,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 6 & y == 1 ~ 3,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 3 & y == 1 ~ 6,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 4 & y == 1 ~ 6,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 5 & y == 1 ~ 6,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 7 & y == 1 ~ 5,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 8 & y == 1 ~ 7,
    z %in% c("Ethnicity_recode", "ethnicity_recode") & x == 9 & y == 1 ~ 8,
    
    ## AH NOTE: In CTs, EdAttain has 6 levels. In Report et al, it has 4
    ## As of 2022 CTs, reran so EdAttain is 4 l3v3ls and added EdAttain6
    ## Also corrected 2021 5yr file
    ## Keeping the 6 levels here because this fxn is specifically for CTs
    
    ## EdAttain   (dep1 = SCHL)
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR <= 2007 & x >= 1 & x <= 8 ~ 1,  ## Less than HS
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR <= 2007 & x == 9 ~ 2,  ## HS or equiv
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR <= 2007 & x >= 10 & x <= 12 ~ 3,  ## Some college
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR <= 2007 & x >= 13 ~ 4,  ## BA+
    ## 2008+
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR %in% c(2008:2108, NA) & x >= 1 & x <= 15 ~ 1,  ## Less than HS
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR %in% c(2008:2108, NA) & x %in% c(16, 17) ~ 2,  ## HS or equiv
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR %in% c(2008:2108, NA) & x >= 18 & x <= 20 ~ 3,  ## Some college
    z %in% c("EdAttain", "EdAttain_GQE") & YEAR %in% c(2008:2108, NA) & x >= 21 ~ 4,  ## BA+
    
    ## 2023.12.01
    ## EdAttain6   (dep1 = SCHL)
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR <= 2007 & x == 1 ~ 1,  ## none completed
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR <= 2007 & x > 1 & x <= 8 ~ 2,  ## Less than HS
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR <= 2007 & x == 9 ~ 3,  ## HS or equiv
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR <= 2007 & x %in% c(10, 11) ~ 4,  ## Some collage, no degree
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR <= 2007 & x == 12 ~ 5,  ## Assoc
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR <= 2007 & x >= 13 ~ 6,  ## Bach +
    ## 2008+
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR %in% c(2008:2108, NA) & x == 1 ~ 1,  ## none completed
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR %in% c(2008:2108, NA) & x > 1 & x <= 15 ~ 2,  ## Less than HS
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR %in% c(2008:2108, NA) & x > 15 & x <= 17 ~ 3,  ## HS or equiv
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR %in% c(2008:2108, NA) & x > 17 & x <= 19 ~ 4,  ## Some collage, no degree
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR %in% c(2008:2108, NA) & x == 20 ~ 5,  ## Assoc
    z %in% c("EdAttain6", "EdAttain6_GQE") & YEAR %in% c(2008:2108, NA) & x >= 21 ~ 6,  ## Bach +
    
    ## edu2   (dep1 == SCHL)
    z %in% c("edu2", "Edu2") & YEAR <= 2007 & x <= 12 ~ 1,  ## lt BA
    z %in% c("edu2", "Edu2") & YEAR <= 2007 & x >= 13 ~ 2,  ## BA+
    ## 2008+
    z %in% c("edu2", "Edu2") & YEAR %in% c(2008:2108, NA) & x <= 20 ~ 1,  ## lt BA
    z %in% c("edu2", "Edu2") & YEAR %in% c(2008:2108, NA) & x >= 21 ~ 2,  ## BA+
    
    ## enrolled
    
    ## CitizenStat   (dep1 = CIT)
    z %in% c("CitizenStat", "CitizenStat_GQE") & x == 1 ~ 1,
    z %in% c("CitizenStat", "CitizenStat_GQE") & x == 2 ~ 1,
    z %in% c("CitizenStat", "CitizenStat_GQE") & x == 3 ~ 1,
    z %in% c("CitizenStat", "CitizenStat_GQE") & x == 4 ~ 2,
    z %in% c("CitizenStat", "CitizenStat_GQE") & x == 5 ~ 3,
    
    ## IndShort  (dep1 = INDP)
    z == "IndShort" & x == 9920 ~ 0,   ## Unemployed And Last Worked 5 Years Ago Or Earlier Or Never Worked
    z == "IndShort" & x %in% c(170:490,770) ~ 1,
    z == "IndShort" & x %in% c(1070:3990) ~ 2,
    z == "IndShort" & x %in% c(4070:4590) ~ 3,
    z == "IndShort" & x %in% c(4670:5790) ~ 4,
    z == "IndShort" & x %in% c(570:690,6070:6390) ~ 5,
    z == "IndShort" & x %in% c(6470:6780) ~ 6,
    z == "IndShort" & x %in% c(6870:7190) ~ 7,
    z == "IndShort" & x %in% c(7270:7790) ~ 8,
    z == "IndShort" & x %in% c(7860:8470) ~ 9,
    z == "IndShort" & x %in% c(8560:8690) ~ 10,
    z == "IndShort" & x %in% c(8770:9290) ~ 11,
    z == "IndShort" & x %in% c(9370:9590) ~ 12,
    z == "IndShort" & x %in% c(9670:9870) ~ 13,
    ## NA note: N/A (less than 16 years old/NILF who last worked more than 5 years ago or never worked)
    
    ## occ_recode   (dep1 = OCCP)
    z == "occ_recode" & x >= 10 & x < 500 ~ 1,
    z == "occ_recode" & x >= 500 & x < 3700 ~ 2,
    z == "occ_recode" & x >= 3700 & x < 4700 ~ 3,
    z == "occ_recode" & x >= 4700 & x < 5000 ~ 4,
    z == "occ_recode" & x >= 5000 & x < 6000 ~ 5,
    z == "occ_recode" & x >= 6000 & x <= 7640 ~ 6,
    z == "occ_recode" & x >= 7700 & x <= 9760 ~ 7,
    z == "occ_recode" & x >= 9800 & x < 9920 ~ 8,
    z == "occ_recode" & x == 9920  ~ 9, ## Unemployed And Last Worked 5 Years Ago Or Earlier Or Never Worked
    ## NA note: N/A (less than 16 years old/NILF who last worked more than 5 years ago or never worked)
    
    ## Boro   (dep1 = PUMA)
    z %in% c("Boro", "boro", "Boro_GQE") & x >= 3701 & x <= 3710 & YEAR <= 2021 ~ 1,
    z %in% c("Boro", "boro", "Boro_GQE") & x >= 4001 & x <= 4018 & YEAR <= 2021 ~ 2,
    z %in% c("Boro", "boro", "Boro_GQE") & x >= 3801 & x <= 3811 & YEAR <= 2021 ~ 3,
    z %in% c("Boro", "boro", "Boro_GQE") & x >= 4101 & x <= 4114 & YEAR <= 2021 ~ 4,
    z %in% c("Boro", "boro", "Boro_GQE") & x >= 3901 & x <= 3903 & YEAR <= 2021 ~ 5,
    
    z %in% c("Boro", "boro", "Boro_GQE") & x %in% c(4200:4299) & YEAR >= 2022 ~ 1,
    z %in% c("Boro", "boro", "Boro_GQE") & x %in% c(4300:4399) & YEAR >= 2022 ~ 2,
    z %in% c("Boro", "boro", "Boro_GQE") & x %in% c(4100:4199) & YEAR >= 2022 ~ 3,
    z %in% c("Boro", "boro", "Boro_GQE") & x %in% c(4400:4499) & YEAR >= 2022 ~ 4,
    z %in% c("Boro", "boro", "Boro_GQE") & x %in% c(4500:4599) & YEAR >= 2022 ~ 5,
    
    ## CD   (dep1 = PUMA)
    ## Updated 2023.10.24 to incorp new geographies for ACS years 2022+
    ## See key/cross walk: /Users/annehill/Dropbox/NYCO_remote/PRU_2022_misc/PRU_2023.10.23/2022_scratchpad/geo_resources/NOTES_PUMAS_2010and2020.xlsx
    z == "CD" & x == 3710 & YEAR <= 2021 ~ 1,
    z == "CD" & x == 3705 & YEAR <= 2021 ~ 2,
    z == "CD" & x == 3708 & YEAR <= 2021 ~ 3,
    z == "CD" & x == 3707 & YEAR <= 2021 ~ 4,
    z == "CD" & x == 3706 & YEAR <= 2021 ~ 5,
    z == "CD" & x == 3701 & YEAR <= 2021 ~ 6,
    z == "CD" & x == 3709 & YEAR <= 2021 ~ 7,
    z == "CD" & x == 3703 & YEAR <= 2021 ~ 8,
    z == "CD" & x == 3704 & YEAR <= 2021 ~ 9,
    z == "CD" & x == 3702 & YEAR <= 2021 ~ 10,
    z == "CD" & x == 4001 & YEAR <= 2021 ~ 11,
    z == "CD" & x == 4004 & YEAR <= 2021 ~ 12,
    z == "CD" & x == 4003 & YEAR <= 2021 ~ 13,
    z == "CD" & x == 4002 & YEAR <= 2021 ~ 14,
    z == "CD" & x == 4008 & YEAR <= 2021 ~ 15,
    z == "CD" & x == 4005 & YEAR <= 2021 ~ 16,
    z == "CD" & x == 4012 & YEAR <= 2021 ~ 17,
    z == "CD" & x == 4006 & YEAR <= 2021 ~ 18,
    z == "CD" & x == 4011 & YEAR <= 2021 ~ 19,
    z == "CD" & x == 4013 & YEAR <= 2021 ~ 20,
    z == "CD" & x == 4017 & YEAR <= 2021 ~ 21,
    z == "CD" & x == 4014 & YEAR <= 2021 ~ 22,
    z == "CD" & x == 4018 & YEAR <= 2021 ~ 23,
    z == "CD" & x == 4015 & YEAR <= 2021 ~ 24,
    z == "CD" & x == 4016 & YEAR <= 2021 ~ 25,
    z == "CD" & x == 4007 & YEAR <= 2021 ~ 26,
    z == "CD" & x == 4010 & YEAR <= 2021 ~ 27,
    z == "CD" & x == 4009 & YEAR <= 2021 ~ 28,
    z == "CD" & x == 3810 & YEAR <= 2021 ~ 29,
    z == "CD" & x == 3809 & YEAR <= 2021 ~ 30,
    z == "CD" & x == 3807 & YEAR <= 2021 ~ 31,
    z == "CD" & x == 3808 & YEAR <= 2021 ~ 32,
    z == "CD" & x == 3806 & YEAR <= 2021 ~ 33,
    z == "CD" & x == 3805 & YEAR <= 2021 ~ 34,
    z == "CD" & x == 3802 & YEAR <= 2021 ~ 35,
    z == "CD" & x == 3803 & YEAR <= 2021 ~ 36,
    z == "CD" & x == 3804 & YEAR <= 2021 ~ 37,
    z == "CD" & x == 3801 & YEAR <= 2021 ~ 38,
    z == "CD" & x == 4101 & YEAR <= 2021 ~ 39,
    z == "CD" & x == 4109 & YEAR <= 2021 ~ 40,
    z == "CD" & x == 4102 & YEAR <= 2021 ~ 41,
    z == "CD" & x == 4107 & YEAR <= 2021 ~ 42,
    z == "CD" & x == 4110 & YEAR <= 2021 ~ 43,
    z == "CD" & x == 4108 & YEAR <= 2021 ~ 44,
    z == "CD" & x == 4103 & YEAR <= 2021 ~ 45,
    z == "CD" & x == 4106 & YEAR <= 2021 ~ 46,
    z == "CD" & x == 4111 & YEAR <= 2021 ~ 47,
    z == "CD" & x == 4113 & YEAR <= 2021 ~ 48,
    z == "CD" & x == 4104 & YEAR <= 2021 ~ 49,
    z == "CD" & x == 4112 & YEAR <= 2021 ~ 50,
    z == "CD" & x == 4105 & YEAR <= 2021 ~ 51,
    z == "CD" & x == 4114 & YEAR <= 2021 ~ 52,
    z == "CD" & x == 3903 & YEAR <= 2021 ~ 53,
    z == "CD" & x == 3902 & YEAR <= 2021 ~ 54,
    z == "CD" & x == 3901 & YEAR <= 2021 ~ 55,
    ## 2020 geos needed for ACS years 2022+
    z == "CD" & x == 4221 & YEAR >= 2022 ~ 1,
    z == "CD" & x == 4263 & YEAR >= 2022 ~ 2,
    z == "CD" & x == 4204 & YEAR >= 2022 ~ 3,
    z == "CD" & x == 4205 & YEAR >= 2022 ~ 4,
    z == "CD" & x == 4207 & YEAR >= 2022 ~ 5,
    z == "CD" & x == 4208 & YEAR >= 2022 ~ 6,
    z == "CD" & x == 4209 & YEAR >= 2022 ~ 7,
    z == "CD" & x == 4210 & YEAR >= 2022 ~ 8,
    z == "CD" & x == 4211 & YEAR >= 2022 ~ 9,
    z == "CD" & x == 4212 & YEAR >= 2022 ~ 10,
    z == "CD" & x == 4301 & YEAR >= 2022 ~ 11,
    z == "CD" & x == 4302 & YEAR >= 2022 ~ 12,
    z == "CD" & x == 4303 & YEAR >= 2022 ~ 13,
    z == "CD" & x == 4304 & YEAR >= 2022 ~ 14,
    z == "CD" & x == 4305 & YEAR >= 2022 ~ 15,
    z == "CD" & x == 4306 & YEAR >= 2022 ~ 16,
    z == "CD" & x == 4307 & YEAR >= 2022 ~ 17,
    z == "CD" & x == 4308 & YEAR >= 2022 ~ 18,
    z == "CD" & x == 4309 & YEAR >= 2022 ~ 19,
    z == "CD" & x == 4310 & YEAR >= 2022 ~ 20,
    z == "CD" & x == 4311 & YEAR >= 2022 ~ 21,
    z == "CD" & x == 4312 & YEAR >= 2022 ~ 22,
    z == "CD" & x == 4313 & YEAR >= 2022 ~ 23,
    z == "CD" & x == 4314 & YEAR >= 2022 ~ 24,
    z == "CD" & x == 4315 & YEAR >= 2022 ~ 25,
    z == "CD" & x == 4316 & YEAR >= 2022 ~ 26,
    z == "CD" & x == 4317 & YEAR >= 2022 ~ 27,
    z == "CD" & x == 4318 & YEAR >= 2022 ~ 28,
    z == "CD" & x == 4121 & YEAR >= 2022 ~ 29,
    z == "CD" & x == 4103 & YEAR >= 2022 ~ 30,
    z == "CD" & x == 4104 & YEAR >= 2022 ~ 31,
    z == "CD" & x == 4165 & YEAR >= 2022 ~ 32,
    z == "CD" & x == 4107 & YEAR >= 2022 ~ 33,
    z == "CD" & x == 4108 & YEAR >= 2022 ~ 34,
    z == "CD" & x == 4109 & YEAR >= 2022 ~ 35,
    z == "CD" & x == 4110 & YEAR >= 2022 ~ 36,
    z == "CD" & x == 4111 & YEAR >= 2022 ~ 37,
    z == "CD" & x == 4112 & YEAR >= 2022 ~ 38,
    z == "CD" & x == 4401 & YEAR >= 2022 ~ 39,
    z == "CD" & x == 4402 & YEAR >= 2022 ~ 40,
    z == "CD" & x == 4403 & YEAR >= 2022 ~ 41,
    z == "CD" & x == 4404 & YEAR >= 2022 ~ 42,
    z == "CD" & x == 4405 & YEAR >= 2022 ~ 43,
    z == "CD" & x == 4406 & YEAR >= 2022 ~ 44,
    z == "CD" & x == 4407 & YEAR >= 2022 ~ 45,
    z == "CD" & x == 4408 & YEAR >= 2022 ~ 46,
    z == "CD" & x == 4409 & YEAR >= 2022 ~ 47,
    z == "CD" & x == 4410 & YEAR >= 2022 ~ 48,
    z == "CD" & x == 4411 & YEAR >= 2022 ~ 49,
    z == "CD" & x == 4412 & YEAR >= 2022 ~ 50,
    z == "CD" & x == 4413 & YEAR >= 2022 ~ 51,
    z == "CD" & x == 4414 & YEAR >= 2022 ~ 52,
    z == "CD" & x == 4501 & YEAR >= 2022 ~ 53,
    z == "CD" & x == 4502 & YEAR >= 2022 ~ 54,
    z == "CD" & x == 4503 & YEAR >= 2022 ~ 55,
    
    ## SKIPPING CD2. For code, see: ADD SPSS PATH
    
    ######################.
    ## INCOME VARIABLES   (dep1 = <income variable>, dep2 = ADJINC) ##
    ## PINCP, HINCP, SEMP, OIP, WAGP, PAP, SSIP, SSP, INTP, RETP
    ######################.
    
    z %in% c("IncHH_adj", "Inc_adj", "OI_adj", "Wage_adj", "PA_adj", 
             "Semp_adj", "SSI_adj", "SS_adj", "Int_adj", "Ret_adj") ~ x * (y/1000000),
    
    
    TRUE ~ -9999999) ### if this value ever returned, it's an error
  
  return(newVal)
}


#
###### AGGREGATE LABEL FUNCTION  ###########

## Produces labels for general purposes ()
fxn_label <- function(x, variable_name, YEAR) {
  
  ## x = variable you want labels for
  ## YEAR = ACS year (Needed if labeling geographies or year-dependent values. Defaults to NA)
  ## variable_name: OPTIONAL. Use if name of x is other that usual variable name (i.e. in control total syntax)
  ## note, YEAR argument at end because only needed for CD labels that ALSO require variable_name
  
  ## If x missing, full stop
  if(missing(x)){stop("ERROR: no arguments specified")}
  
  ## Assume name of x is variable_name and YEAR doesn't matter if not specified
  if(missing(YEAR)){ YEAR <- NA }
  if(missing(variable_name)){variable_name <- rlang::as_label(rlang::enquo(x))}
  
  x <- as.numeric(x)
  
  newVal <- case_when(
    
    is.na(x) ~ as.character(NA),
    
    ## type_recode ##
    variable_name == "type_recode" & x == 1 ~ "Housing unit",
    variable_name == "type_recode" & x == 2 ~ "Institutional group quarters",
    variable_name == "type_recode" & x == 3 ~ "Noninstitutional group quarters",
    
    ## age_recode ##
    variable_name == "age_recode" & x == 1 ~ "0-4",
    variable_name == "age_recode" & x == 2 ~ "5-9",
    variable_name == "age_recode" & x == 3 ~ "10-14",
    variable_name == "age_recode" & x == 4 ~ "15-19",
    variable_name == "age_recode" & x == 5 ~ "20-24",
    variable_name == "age_recode" & x == 6 ~ "25-29",
    variable_name == "age_recode" & x == 7 ~ "30-34",
    variable_name == "age_recode" & x == 8 ~ "35-39",
    variable_name == "age_recode" & x == 9 ~ "40-44",
    variable_name == "age_recode" & x == 10 ~ "45-49",
    variable_name == "age_recode" & x == 11 ~ "50-54",
    variable_name == "age_recode" & x == 12 ~ "55-59",
    variable_name == "age_recode" & x == 13 ~ "60-64",
    variable_name == "age_recode" & x == 14 ~ "65-69",
    variable_name == "age_recode" & x == 15 ~ "70-74",
    variable_name == "age_recode" & x == 16 ~ "75-79",
    variable_name == "age_recode" & x == 17 ~ "80-84",
    variable_name == "age_recode" & x == 18 ~ "85-89",
    variable_name == "age_recode" & x == 19 ~ "90+",
    variable_name == "age_recode" & x == 100 ~ "65 and Over",
    
    
    ## age_3groups, AgeForReport ##
    variable_name %in% c("age_3groups", "AgeForReport") & x == 1 ~ "Less than 18",
    variable_name %in% c("age_3groups", "AgeForReport") & x == 2 ~ "18 - 64",
    variable_name %in% c("age_3groups", "AgeForReport") & x == 3 ~ "65 & over",
    
    ## age_6groups ##
    variable_name == "age_6groups" & x == 1 ~ "Less than 16",
    variable_name == "age_6groups" & x == 2 ~ "16 - 24",
    variable_name == "age_6groups" & x == 3 ~ "25 - 40",
    variable_name == "age_6groups" & x == 4 ~ "41 - 64",
    variable_name == "age_6groups" & x == 5 ~ "65 - 80",
    variable_name == "age_6groups" & x == 6 ~ "81 & over",
    
    ## SSMC (2013-2018) ##
    variable_name == "SSMC" & x == 0 ~ "Households without a same-sex married couple",
    variable_name == "SSMC" & x == 1 ~ "Same-sex married-couple household where not all relevant data shown as reported",
    variable_name == "SSMC" & x == 2 ~ "All other same-sex married-couple households",
    
    ## SSMC (2019+) ##
    variable_name == "CPLT" & x == 1 ~ "Opposite-sex husband/wife/spouse household",
    variable_name == "CPLT" & x == 2 ~ "Same-sex husband/wife/spouse household",
    variable_name == "CPLT" & x == 3 ~ "Opposite-sex unmarried partner household",
    variable_name == "CPLT" & x == 4 ~ "Same-sex unmarried partner household",
    
    ## MAR (Marital status) ##
    variable_name == "MAR" & x == 1 ~ "Married",
    variable_name == "MAR" & x == 2 ~ "Widowed",
    variable_name == "MAR" & x == 3 ~ "Divorced",
    variable_name == "MAR" & x == 4 ~ "Separated",
    variable_name == "MAR" & x == 5 ~ "Never married or under 15 years old",
    
    ## Gender/SEX ##
    variable_name %in% c("SEX", "sex", "Gender", "gender") & x == 1 ~ "Male",
    variable_name %in% c("SEX", "sex", "Gender", "gender") & x == 2 ~ "Female",
    
    ## AH: Report used 4 levels, but for a while my code had 6 (in control totals and some things)
    ## Watch out for inconsistencies!
    # ## EdAttain
    # variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 1 ~ "None Completed",  ## none completed
    # variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 2 ~ "Less than High School",  ## Less than HS
    # variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 3 ~ "High School or Equivalent",  ## HS or equiv
    # variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 4 ~ "Some College",  ## Some collage, no degree
    # variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 5 ~ "Associate's Degree",  ## Assoc
    # variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 6 ~ "Bachelors or Higher",  ## Bach +
    
    ## EdAttain
    variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 1 ~ "Less than High School",  
    variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 2 ~ "High School or Equivalent",   
    variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 3 ~ "Some College",   
    variable_name %in% c("EdAttain", "EdAttain_GQE") & x == 4 ~ "Bachelors or Higher", 
    
    ## EdAttain6
    variable_name %in% c("EdAttain6", "EdAttain6_GQE") & x == 1 ~ "None Completed",  ## none completed
    variable_name %in% c("EdAttain6", "EdAttain6_GQE") & x == 2 ~ "Less than High School",  ## Less than HS
    variable_name %in% c("EdAttain6", "EdAttain6_GQE") & x == 3 ~ "High School or Equivalent",  ## HS or equiv
    variable_name %in% c("EdAttain6", "EdAttain6_GQE") & x == 4 ~ "Some College",  ## Some collage, no degree
    variable_name %in% c("EdAttain6", "EdAttain6_GQE") & x == 5 ~ "Associate's Degree",  ## Assoc
    variable_name %in% c("EdAttain6", "EdAttain6_GQE") & x == 6 ~ "Bachelors or Higher",  ## Bach +
    
    ## edu2 
    variable_name %in% c("edu2", "Edu2") & x == 1 ~ "Less than Bachelors",
    variable_name %in% c("edu2", "Edu2") & x == 2 ~ "Bachelors or Higher",
    
    ## SCH (School Enrollment) ##
    variable_name == "SCH" & x == 1 ~ "Not Enrolled",    # No, has not attended in the last 3 months 
    variable_name == "SCH" & x == 2 ~ "Enrolled (Public)", # Yes, public school or public college 
    variable_name == "SCH" & x == 3 ~ "Enrolled (Private)", 
    variable_name == "SCH" & x == 0 ~ "Not Applicable",    ## Census codes as NA but Tax calc sets NA to 0
    
    ## HHT ##
    variable_name == "HHT" & x == 1 ~ "Married couple household",
    variable_name == "HHT" & x == 2 ~ "Other family household: Male householder, no spouse present",
    variable_name == "HHT" & x == 3 ~ "Other family household: Female householder, no spouse present",
    variable_name == "HHT" & x == 4 ~ "Nonfamily household: Male householder: Living alone",
    variable_name == "HHT" & x == 5 ~ "Nonfamily household: Male householder: Not living alone",
    variable_name == "HHT" & x == 6 ~ "Nonfamily household: Female householder: Living alone",
    variable_name == "HHT" & x == 7 ~ "Nonfamily household: Female householder: Not living alone",
    
    ## HHT2 ##
    variable_name == "HHT2" & x == 1 ~ "Married couple household with own children <18",
    variable_name == "HHT2" & x == 2 ~ "Married couple household, NO own children <18",
    variable_name == "HHT2" & x == 3 ~ "Cohabiting couple household with own children <18",
    variable_name == "HHT2" & x == 4 ~ "Cohabiting couple household, NO own children <18",
    variable_name == "HHT2" & x == 5 ~ "Female householder, no spouse/partner present, Living alone",
    variable_name == "HHT2" & x == 6 ~ "Female householder, no spouse/partner present, with own children <18",
    variable_name == "HHT2" & x == 7 ~ "Female householder, no spouse/partner present, with relatives, NO own children <18",
    variable_name == "HHT2" & x == 8 ~ "Female householder, no spouse/partner present, only nonrelatives present",
    variable_name == "HHT2" & x == 9 ~ "Male householder, no spouse/partner present, Living alone",
    variable_name == "HHT2" & x == 10 ~ "Male householder, no spouse/partner present, with own children <18",
    variable_name == "HHT2" & x == 11 ~ "Male householder, no spouse/partner present, with relatives, NO own children <18",
    variable_name == "HHT2" & x == 12 ~ "Male householder, no spouse/partner present, only nonrelatives present",
    
    ## hht_recode ##
    variable_name == "hht_recode" & x == 1 ~ "Married-Couple Family",
    variable_name == "hht_recode" & x == 2 ~ "Male Householder, no spouse",
    variable_name == "hht_recode" & x == 3 ~ "Female Householder, no spouse",
    variable_name == "hht_recode" & x == 4 ~ "Living alone",
    variable_name == "hht_recode" & x == 5 ~ "Unrelated living alone",
    
    
    ## BLD ##
    variable_name == "BLD" & x == 1 ~ "Mobile home or trailer",
    variable_name == "BLD" & x == 2 ~ "One-family house detached",
    variable_name == "BLD" & x == 3 ~ "One-family house attached",
    variable_name == "BLD" & x == 4 ~ "2 Apartments",
    variable_name == "BLD" & x == 5 ~ "3-4 Apartments",
    variable_name == "BLD" & x == 6 ~ "5-9 Apartments",
    variable_name == "BLD" & x == 7 ~ "10-19 Apartments",
    variable_name == "BLD" & x == 8 ~ "20-49 Apartments",
    variable_name == "BLD" & x == 9 ~ "50 or more apartments",
    variable_name == "BLD" & x == 10 ~ "Boat, RV, van, etc.",
    
    
    ## RELSHIPP ##
    variable_name == "RELSHIPP" & x == 20 ~ "Reference person",
    variable_name == "RELSHIPP" & x == 21 ~ "Opposite-sex husband/wife/spouse",
    variable_name == "RELSHIPP" & x == 22 ~ "Opposite-sex unmarried partner",
    variable_name == "RELSHIPP" & x == 23 ~ "Same-sex husband/wife/spouse",
    variable_name == "RELSHIPP" & x == 24 ~ "Same-sex unmarried partner",
    variable_name == "RELSHIPP" & x == 25 ~ "Biological son or daughter",
    variable_name == "RELSHIPP" & x == 26 ~ "Adopted son or daughter",
    variable_name == "RELSHIPP" & x == 27 ~ "Stepson or stepdaughter",
    variable_name == "RELSHIPP" & x == 28 ~ "Brother or sister",
    variable_name == "RELSHIPP" & x == 29 ~ "Father or mother",
    variable_name == "RELSHIPP" & x == 30 ~ "Grandchild",
    variable_name == "RELSHIPP" & x == 31 ~ "Parent-in-law",
    variable_name == "RELSHIPP" & x == 32 ~ "Son-in-law or daughter-in-law",
    variable_name == "RELSHIPP" & x == 33 ~ "Other relative",
    variable_name == "RELSHIPP" & x == 34 ~ "Roommate or housemate",
    variable_name == "RELSHIPP" & x == 35 ~ "Foster child",
    variable_name == "RELSHIPP" & x == 36 ~ "Other nonrelative",
    variable_name == "RELSHIPP" & x == 37 ~ "Institutionalized group quarters population",
    variable_name == "RELSHIPP" & x == 38 ~ "Noninstitutionalized group quarters population",
    
    ## REL or RELP ##
    (variable_name == "REL" | variable_name == "RELP") & x == 0 ~ "Reference person",
    (variable_name == "REL" | variable_name == "RELP") & x == 1 ~ "Husband/wife",
    (variable_name == "REL" | variable_name == "RELP") & x == 2 ~ "Biological son or daughter",
    (variable_name == "REL" | variable_name == "RELP") & x == 3 ~ "Adopted son or daughter",
    (variable_name == "REL" | variable_name == "RELP") & x == 4 ~ "Stepson or stepdaughter",
    (variable_name == "REL" | variable_name == "RELP") & x == 5 ~ "Brother or sister",
    (variable_name == "REL" | variable_name == "RELP") & x == 6 ~ "Father or mother",
    (variable_name == "REL" | variable_name == "RELP") & x == 7 ~ "Grandchild",
    (variable_name == "REL" | variable_name == "RELP") & x == 8 ~ "Parent-in-law",
    (variable_name == "REL" | variable_name == "RELP") & x == 9 ~ "Son-in-law or daughter-in-law",
    (variable_name == "REL" | variable_name == "RELP") & x == 10 ~ "Other relative",
    (variable_name == "REL" | variable_name == "RELP") & x == 11 ~ "Roomer or boarder",
    (variable_name == "REL" | variable_name == "RELP") & x == 12 ~ "Housemate or roommate",
    (variable_name == "REL" | variable_name == "RELP") & x == 13 ~ "Unmarried partner",
    (variable_name == "REL" | variable_name == "RELP") & x == 14 ~ "Foster child",
    (variable_name == "REL" | variable_name == "RELP") & x == 15 ~ "Other nonrelative",
    
    ## CIT ##
    variable_name == "CIT" & x == 1 ~ "Born in the US",
    variable_name == "CIT" & x == 2 ~ "Born in Puerto Rico, Guam, the US Virgin Islands, or the Northern Marianas",
    variable_name == "CIT" & x == 3 ~ "Born abroad of American parent(s)",
    variable_name == "CIT" & x == 4 ~ "Citizen, Naturalized",
    variable_name == "CIT" & x == 5 ~ "Not a Citizen",
    
    ## CitizenStat ##
    variable_name %in% c("CitizenStat", "CitizenStat_GQE") & x == 1 ~ "Citizen by birth",
    variable_name %in% c("CitizenStat", "CitizenStat_GQE") & x == 2 ~ "Citizen, Naturalized",
    variable_name %in% c("CitizenStat", "CitizenStat_GQE") & x == 3 ~ "Not a Citizen",
    
    ## Ethnicity ##
    variable_name %in% c("Ethnicity", "ethnicity", "Ethnicity_GQE") & x == 1 ~ "NH White",
    variable_name %in% c("Ethnicity", "ethnicity", "Ethnicity_GQE") & x == 2 ~ "NH Black",
    variable_name %in% c("Ethnicity", "ethnicity", "Ethnicity_GQE") & x == 3 ~ "NH Asian",
    variable_name %in% c("Ethnicity", "ethnicity", "Ethnicity_GQE") & x == 4 ~ "Hispanic, Any Race",
    variable_name %in% c("Ethnicity", "ethnicity", "Ethnicity_GQE") & x == 5 ~ "Other Race/Ethnic Group",
    
    ## Ethnicity_long ##
    variable_name %in% c("Ethnicity_long", "ethnicity_long") & x == 1 ~ "Non-Hispanic White",
    variable_name %in% c("Ethnicity_long", "ethnicity_long") & x == 2 ~ "Non-Hispanic Black",
    variable_name %in% c("Ethnicity_long", "ethnicity_long") & x == 3 ~ "Non-Hispanic Asian",
    variable_name %in% c("Ethnicity_long", "ethnicity_long") & x == 4 ~ "Hispanic, Any Race",
    variable_name %in% c("Ethnicity_long", "ethnicity_long") & x == 5 ~ "Other Race/Ethnic Group",
    
    ## Ethnicity_recode ##
    variable_name == "Ethnicity_recode" & x == 1 ~ "NH White",
    variable_name == "Ethnicity_recode" & x == 2 ~ "NH Black",
    variable_name == "Ethnicity_recode" & x == 3 ~ "NH Asian",
    variable_name == "Ethnicity_recode" & x == 4 ~ "Hispanic, Any Race",
    variable_name == "Ethnicity_recode" & x == 5 ~ "Native Hawaiian and Other Asian and PI nonhispanic",
    variable_name == "Ethnicity_recode" & x == 6 ~ "American Indian and Alaska Native nonhispanic",
    variable_name == "Ethnicity_recode" & x == 7 ~ "some other race nonhispanic",
    variable_name == "Ethnicity_recode" & x == 8 ~ "two or more races, nonhispanic",
    
    ## ESR ##
    variable_name == "ESR" & x == 1 ~ "Civilian employed, at work",
    variable_name == "ESR" & x == 2 ~ "Civilian employed, with a job but not at work",
    variable_name == "ESR" & x == 3 ~ "Unemployed",
    variable_name == "ESR" & x == 4 ~ "Armed forces, at work",
    variable_name == "ESR" & x == 5 ~ "Armed forces, with a job but not at work",
    variable_name == "ESR" & x == 6 ~ "Not in labor force",
    
    ## esr_recode ##
    variable_name %in% c("esr_recode", "esr_recode_GQE") & x == 1 ~ "Employed",
    variable_name %in% c("esr_recode", "esr_recode_GQE") & x == 2 ~ "Unemployed",
    variable_name %in% c("esr_recode", "esr_recode_GQE") & x == 3 ~ "Not in LF",
    
    ## WorkExp ##
    variable_name == "WorkExp" & x == 1 ~ "Full Time Year Round",
    variable_name == "WorkExp" & x == 2 ~ "Less than Full Time Year Round",
    variable_name == "WorkExp" & x == 3 ~ "No Work",
    
    ## FS ##
    variable_name == "FS" & x == 1 ~ "Received Food Stamps",
    variable_name == "FS" & x == 2 ~ "Did NOT Receive Food Stamps",
    
    ## SFN ##
    variable_name == "SFN" & x == 1 ~ "In subfamily 1",
    variable_name == "SFN" & x == 2 ~ "In subfamily 2",
    variable_name == "SFN" & x == 3 ~ "In subfamily 3",
    variable_name == "SFN" & x == 4 ~ "In subfamily 4",
    variable_name == "SFN" & x == 5 ~ "In subfamily 5",
    variable_name == "SFN" & x == 6 ~ "In subfamily 6",
    variable_name == "SFN" & is.na(x) ~ "Not in subfamily",
    
    ## FILESTAT ##
    variable_name == "FILESTAT" & x == 1 ~ "Married Filing Joint",
    variable_name == "FILESTAT" & x == 2 ~ "Head of Household",
    variable_name == "FILESTAT" & x == 3 ~ "Married Filing Separate",
    variable_name == "FILESTAT" & x == 4 ~ "Single",
    
    ## FILESTAT (abbreviated) ##
    variable_name == "FILESTAT_abbr" & x == 1 ~ "MFJ",
    variable_name == "FILESTAT_abbr" & x == 2 ~ "HOH",
    variable_name == "FILESTAT_abbr" & x == 3 ~ "MFS",
    variable_name == "FILESTAT_abbr" & x == 4 ~ "S",
    
    ## FILETYPE ##
    variable_name == "FILETYPE" & x == 1 ~ "Normal 1040",
    variable_name == "FILETYPE" & x == 2 ~ "Dependent filer",
    variable_name == "FILETYPE" & x == 3 ~ "Filer inc. < thresh.",
    variable_name == "FILETYPE" & x == 4 ~ "Filer neg. income",
    
    ## FILETYPE ##
    variable_name == "FILER" & x == 0 ~ "Non-filer",
    variable_name == "FILER" & x == 1 ~ "Filer",
    
    
    ## IndShort ##
    variable_name == "IndShort" & x == 0 ~ "Unemployed and last worked 5 years ago or earlier or never", 
    variable_name == "IndShort" & x == 1 ~ "Extraction/Construction",
    variable_name == "IndShort" & x == 2 ~ "Manufacturing",
    variable_name == "IndShort" & x == 3 ~ "Wholesalers",
    variable_name == "IndShort" & x == 4 ~ "Retailers",
    variable_name == "IndShort" & x == 5 ~ "Transportation & Utilities",
    variable_name == "IndShort" & x == 6 ~ "Information",
    variable_name == "IndShort" & x == 7 ~ "Financial",
    variable_name == "IndShort" & x == 8 ~ "Professional",
    variable_name == "IndShort" & x == 9 ~ "Education/Health/Human Svcs",
    variable_name == "IndShort" & x == 10 ~ "Entertainment Svcs",
    variable_name == "IndShort" & x == 11 ~ "Other Svcs",
    variable_name == "IndShort" & x == 12 ~ "International Affairs",
    variable_name == "IndShort" & x == 13 ~ "Military",
    ## NA: N/A (less than 16 years old/NILF who last worked more than 5 years ago or never worked)
    
    ## occ_recode ##
    variable_name == "occ_recode" & x == 1 ~ "Managerial",
    variable_name == "occ_recode" & x == 2 ~ "Professional",
    variable_name == "occ_recode" & x == 3 ~ "Service",
    variable_name == "occ_recode" & x == 4 ~ "Sales",
    variable_name == "occ_recode" & x == 5 ~ "Administrative",
    variable_name == "occ_recode" & x == 6 ~ "Construction, Extraction & Maintenance",
    variable_name == "occ_recode" & x == 7 ~ "Production, Transportation, & Material Moving",
    variable_name == "occ_recode" & x == 8 ~ "Military",
    variable_name == "occ_recode" & x == 9 ~ "Unemployed and last worked 5 years ago or earlier or never", 
    ## NA: N/A (less than 16 years old/NILF who last worked more than 5 years ago or never worked)
    
    ## Boro ##
    variable_name %in% c("Boro", "boro", "Boro_GQE", "boro_GQE") & x == 1 ~ "Bronx",
    variable_name %in% c("Boro", "boro", "Boro_GQE", "boro_GQE") & x == 2 ~ "Brooklyn",
    variable_name %in% c("Boro", "boro", "Boro_GQE", "boro_GQE") & x == 3 ~ "Manhattan",
    variable_name %in% c("Boro", "boro", "Boro_GQE", "boro_GQE") & x == 4 ~ "Queens",
    variable_name %in% c("Boro", "boro", "Boro_GQE", "boro_GQE") & x == 5 ~ "Staten Island",
    
    ## CD_code
    ## Updated 2023.10 to incorp new geo for ACS yrs 2022+
    variable_name == "CD_code" & x == 1 & YEAR <= 2021 ~ "BX 1 & 2",
    variable_name == "CD_code" & x == 2 & YEAR <= 2021 ~ "BX 3 & 6",
    variable_name == "CD_code" & x == 3 & YEAR <= 2021 ~ "BX 4",
    variable_name == "CD_code" & x == 4 & YEAR <= 2021 ~ "BX 5",
    variable_name == "CD_code" & x == 5 & YEAR <= 2021 ~ "BX 7",
    variable_name == "CD_code" & x == 6 & YEAR <= 2021 ~ "BX 8",
    variable_name == "CD_code" & x == 7 & YEAR <= 2021 ~ "BX 9",
    variable_name == "CD_code" & x == 8 & YEAR <= 2021 ~ "BX 10",
    variable_name == "CD_code" & x == 9 & YEAR <= 2021 ~ "BX 11",
    variable_name == "CD_code" & x == 10 & YEAR <= 2021 ~ "BX 12",
    variable_name == "CD_code" & x == 11 & YEAR <= 2021 ~ "BK 1",
    variable_name == "CD_code" & x == 12 & YEAR <= 2021 ~ "BK 2",
    variable_name == "CD_code" & x == 13 & YEAR <= 2021 ~ "BK 3",
    variable_name == "CD_code" & x == 14 & YEAR <= 2021 ~ "BK 4",
    variable_name == "CD_code" & x == 15 & YEAR <= 2021 ~ "BK 5",
    variable_name == "CD_code" & x == 16 & YEAR <= 2021 ~ "BK 6",
    variable_name == "CD_code" & x == 17 & YEAR <= 2021 ~ "BK 7",
    variable_name == "CD_code" & x == 18 & YEAR <= 2021 ~ "BK 8",
    variable_name == "CD_code" & x == 19 & YEAR <= 2021 ~ "BK 9",
    variable_name == "CD_code" & x == 20 & YEAR <= 2021 ~ "BK 10",
    variable_name == "CD_code" & x == 21 & YEAR <= 2021 ~ "BK 11",
    variable_name == "CD_code" & x == 22 & YEAR <= 2021 ~ "BK 12",
    variable_name == "CD_code" & x == 23 & YEAR <= 2021 ~ "BK 13",
    variable_name == "CD_code" & x == 24 & YEAR <= 2021 ~ "BK 14",
    variable_name == "CD_code" & x == 25 & YEAR <= 2021 ~ "BK 15",
    variable_name == "CD_code" & x == 26 & YEAR <= 2021 ~ "BK 16",
    variable_name == "CD_code" & x == 27 & YEAR <= 2021 ~ "BK 17",
    variable_name == "CD_code" & x == 28 & YEAR <= 2021 ~ "BK 18",
    variable_name == "CD_code" & x == 29 & YEAR <= 2021 ~ "MN 1 & 2",
    variable_name == "CD_code" & x == 30 & YEAR <= 2021 ~ "MN 3",
    variable_name == "CD_code" & x == 31 & YEAR <= 2021 ~ "MN 4 & 5",
    variable_name == "CD_code" & x == 32 & YEAR <= 2021 ~ "MN 6",
    variable_name == "CD_code" & x == 33 & YEAR <= 2021 ~ "MN 7",
    variable_name == "CD_code" & x == 34 & YEAR <= 2021 ~ "MN 8",
    variable_name == "CD_code" & x == 35 & YEAR <= 2021 ~ "MN 9",
    variable_name == "CD_code" & x == 36 & YEAR <= 2021 ~ "MN 10",
    variable_name == "CD_code" & x == 37 & YEAR <= 2021 ~ "MN 11",
    variable_name == "CD_code" & x == 38 & YEAR <= 2021 ~ "MN 12",
    variable_name == "CD_code" & x == 39 & YEAR <= 2021 ~ "QN 1",
    variable_name == "CD_code" & x == 40 & YEAR <= 2021 ~ "QN 2",
    variable_name == "CD_code" & x == 41 & YEAR <= 2021 ~ "QN 3",
    variable_name == "CD_code" & x == 42 & YEAR <= 2021 ~ "QN 4",
    variable_name == "CD_code" & x == 43 & YEAR <= 2021 ~ "QN 5",
    variable_name == "CD_code" & x == 44 & YEAR <= 2021 ~ "QN 6",
    variable_name == "CD_code" & x == 45 & YEAR <= 2021 ~ "QN 7",
    variable_name == "CD_code" & x == 46 & YEAR <= 2021 ~ "QN 8",
    variable_name == "CD_code" & x == 47 & YEAR <= 2021 ~ "QN 9",
    variable_name == "CD_code" & x == 48 & YEAR <= 2021 ~ "QN 10",
    variable_name == "CD_code" & x == 49 & YEAR <= 2021 ~ "QN 11",
    variable_name == "CD_code" & x == 50 & YEAR <= 2021 ~ "QN 12",
    variable_name == "CD_code" & x == 51 & YEAR <= 2021 ~ "QN 13",
    variable_name == "CD_code" & x == 52 & YEAR <= 2021 ~ "QN 14",
    variable_name == "CD_code" & x == 53 & YEAR <= 2021 ~ "SI 1",
    variable_name == "CD_code" & x == 54 & YEAR <= 2021 ~ "SI 2",
    variable_name == "CD_code" & x == 55 & YEAR <= 2021 ~ "SI 3",
    ## New Geos
    variable_name == "CD_code" & x == 1 & YEAR >= 2022 ~ "BX 1 & 2",
    variable_name == "CD_code" & x == 2 & YEAR >= 2022 ~ "BX 3 & 6",
    variable_name == "CD_code" & x == 3 & YEAR >= 2022 ~ "BX 4",
    variable_name == "CD_code" & x == 4 & YEAR >= 2022 ~ "BX 5",
    variable_name == "CD_code" & x == 5 & YEAR >= 2022 ~ "BX 7",
    variable_name == "CD_code" & x == 6 & YEAR >= 2022 ~ "BX 8",
    variable_name == "CD_code" & x == 7 & YEAR >= 2022 ~ "BX 9",
    variable_name == "CD_code" & x == 8 & YEAR >= 2022 ~ "BX 10",
    variable_name == "CD_code" & x == 9 & YEAR >= 2022 ~ "BX 11",
    variable_name == "CD_code" & x == 10 & YEAR >= 2022 ~ "BX 12",
    variable_name == "CD_code" & x == 11 & YEAR >= 2022 ~ "BK 1",
    variable_name == "CD_code" & x == 12 & YEAR >= 2022 ~ "BK 2",
    variable_name == "CD_code" & x == 13 & YEAR >= 2022 ~ "BK 3",
    variable_name == "CD_code" & x == 14 & YEAR >= 2022 ~ "BK 4",
    variable_name == "CD_code" & x == 15 & YEAR >= 2022 ~ "BK 5",
    variable_name == "CD_code" & x == 16 & YEAR >= 2022 ~ "BK 6",
    variable_name == "CD_code" & x == 17 & YEAR >= 2022 ~ "BK 7",
    variable_name == "CD_code" & x == 18 & YEAR >= 2022 ~ "BK 8",
    variable_name == "CD_code" & x == 19 & YEAR >= 2022 ~ "BK 9",
    variable_name == "CD_code" & x == 20 & YEAR >= 2022 ~ "BK 10",
    variable_name == "CD_code" & x == 21 & YEAR >= 2022 ~ "BK 11",
    variable_name == "CD_code" & x == 22 & YEAR >= 2022 ~ "BK 12",
    variable_name == "CD_code" & x == 23 & YEAR >= 2022 ~ "BK 13",
    variable_name == "CD_code" & x == 24 & YEAR >= 2022 ~ "BK 14",
    variable_name == "CD_code" & x == 25 & YEAR >= 2022 ~ "BK 15",
    variable_name == "CD_code" & x == 26 & YEAR >= 2022 ~ "BK 16",
    variable_name == "CD_code" & x == 27 & YEAR >= 2022 ~ "BK 17",
    variable_name == "CD_code" & x == 28 & YEAR >= 2022 ~ "BK 18",
    variable_name == "CD_code" & x == 29 & YEAR >= 2022 ~ "MN 1 & 2",
    variable_name == "CD_code" & x == 30 & YEAR >= 2022 ~ "MN 3",
    variable_name == "CD_code" & x == 31 & YEAR >= 2022 ~ "MN 4",
    variable_name == "CD_code" & x == 32 & YEAR >= 2022 ~ "MN 5 & 6",
    variable_name == "CD_code" & x == 33 & YEAR >= 2022 ~ "MN 7",
    variable_name == "CD_code" & x == 34 & YEAR >= 2022 ~ "MN 8",
    variable_name == "CD_code" & x == 35 & YEAR >= 2022 ~ "MN 9",
    variable_name == "CD_code" & x == 36 & YEAR >= 2022 ~ "MN 10",
    variable_name == "CD_code" & x == 37 & YEAR >= 2022 ~ "MN 11",
    variable_name == "CD_code" & x == 38 & YEAR >= 2022 ~ "MN 12",
    variable_name == "CD_code" & x == 39 & YEAR >= 2022 ~ "QN 1",
    variable_name == "CD_code" & x == 40 & YEAR >= 2022 ~ "QN 2",
    variable_name == "CD_code" & x == 41 & YEAR >= 2022 ~ "QN 3",
    variable_name == "CD_code" & x == 42 & YEAR >= 2022 ~ "QN 4",
    variable_name == "CD_code" & x == 43 & YEAR >= 2022 ~ "QN 5",
    variable_name == "CD_code" & x == 44 & YEAR >= 2022 ~ "QN 6",
    variable_name == "CD_code" & x == 45 & YEAR >= 2022 ~ "QN 7",
    variable_name == "CD_code" & x == 46 & YEAR >= 2022 ~ "QN 8",
    variable_name == "CD_code" & x == 47 & YEAR >= 2022 ~ "QN 9",
    variable_name == "CD_code" & x == 48 & YEAR >= 2022 ~ "QN 10",
    variable_name == "CD_code" & x == 49 & YEAR >= 2022 ~ "QN 11",
    variable_name == "CD_code" & x == 50 & YEAR >= 2022 ~ "QN 12",
    variable_name == "CD_code" & x == 51 & YEAR >= 2022 ~ "QN 13",
    variable_name == "CD_code" & x == 52 & YEAR >= 2022 ~ "QN 14",
    variable_name == "CD_code" & x == 53 & YEAR >= 2022 ~ "SI 1",
    variable_name == "CD_code" & x == 54 & YEAR >= 2022 ~ "SI 2",
    variable_name == "CD_code" & x == 55 & YEAR >= 2022 ~ "SI 3",
    
    
    ## CD_neighborhoods
    ## https://www1.nyc.gov/assets/planning/download/pdf/data-maps/nyc-population/census2010/puma_cd_map.pdf
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 1 & YEAR <= 2021 ~ "Hunts Point, Longwood & Melrose",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 2 & YEAR <= 2021 ~ "Belmont, Crotona Park East & East Tremont",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 3 & YEAR <= 2021 ~ "Concourse, Highbridge & Mount Eden",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 4 & YEAR <= 2021 ~ "Morris Heights, Fordham South & Mount Hope",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 5 & YEAR <= 2021 ~ "Bedford Park, Fordham North & Norwood",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 6 & YEAR <= 2021 ~ "Riverdale, Fieldston & Kingsbridge",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 7 & YEAR <= 2021 ~ "Castle Hill, Clason Point & Parkchester",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 8 & YEAR <= 2021 ~ "Co-op City, Pelham Bay & Schuylerville",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 9 & YEAR <= 2021 ~ "Pelham Parkway, Morris Park & Laconia",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 10 & YEAR <= 2021 ~ "Wakefield, Willamsbridge & Woodlawn",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 11 & YEAR <= 2021 ~ "Greenpoint & Williamsburg",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 12 & YEAR <= 2021 ~ "Brooklyn Heights & Fort Greene",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 13 & YEAR <= 2021 ~ "Bedford-Stuyvesant",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 14 & YEAR <= 2021 ~ "Bushwick",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 15 & YEAR <= 2021 ~ "East New York & Starrett City",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 16 & YEAR <= 2021 ~ "Park Slope, Carroll Gardens & Red Hook",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 17 & YEAR <= 2021 ~ "Sunset Park & Windsor Terrace",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 18 & YEAR <= 2021 ~ "Crown Heights North & Prospect Heights",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 19 & YEAR <= 2021 ~ "Crown Heights So, Prospect Lefferts & Wingate",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 20 & YEAR <= 2021 ~ "Bay Ridge & Dyker Heights",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 21 & YEAR <= 2021 ~ "Bensonhurst & Bath Beach",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 22 & YEAR <= 2021 ~ "Borough Park, Kensington & Ocean Parkway",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 23 & YEAR <= 2021 ~ "Brighton Beach & Coney Island",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 24 & YEAR <= 2021 ~ "Flatbush & Midwood",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 25 & YEAR <= 2021 ~ "Sheepshead Bay, Gerritsen Beach & Homecrest",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 26 & YEAR <= 2021 ~ "Brownsville & Ocean Hill",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 27 & YEAR <= 2021 ~ "East Flatbush, Farragut & Rugby",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 28 & YEAR <= 2021 ~ "Canarsie & Flatlands",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 29 & YEAR <= 2021 ~ "Battery Park City, Greenwich Village & Soho",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 30 & YEAR <= 2021 ~ "Chinatown & Lower East Side",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 31 & YEAR <= 2021 ~ "Chelsea, Clinton & Midtown Business District",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 32 & YEAR <= 2021 ~ "Murray Hill, Gramercy & Stuyvesant Town",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 33 & YEAR <= 2021 ~ "Upper West Side & West Side",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 34 & YEAR <= 2021 ~ "Upper East Side",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 35 & YEAR <= 2021 ~ "Hamilton Hts, Manhattanville & West Harlem",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 36 & YEAR <= 2021 ~ "Central Harlem",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 37 & YEAR <= 2021 ~ "East Harlem",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 38 & YEAR <= 2021 ~ "Washington Heights, Inwood & Marble Hill",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 39 & YEAR <= 2021 ~ "Astoria & Long Island City",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 40 & YEAR <= 2021 ~ "Sunnyside & Woodside",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 41 & YEAR <= 2021 ~ "Jackson Heights & North Corona",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 42 & YEAR <= 2021 ~ "Elmhurst & South Corona",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 43 & YEAR <= 2021 ~ "Ridgewood, Glendale & Middle Village",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 44 & YEAR <= 2021 ~ "Forest Hills & Rego Park",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 45 & YEAR <= 2021 ~ "Flushing, Murray Hill & Whitestone",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 46 & YEAR <= 2021 ~ "Briarwood, Fresh Meadows & Hillcrest",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 47 & YEAR <= 2021 ~ "Richmond Hill & Woodhaven",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 48 & YEAR <= 2021 ~ "Howard Beach & Ozone Park",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 49 & YEAR <= 2021 ~ "Bayside, Douglaston & Little Neck",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 50 & YEAR <= 2021 ~ "Jamaica, Hollis & St Albans",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 51 & YEAR <= 2021 ~ "Queens Village, Cambria Heights & Rosedale",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 52 & YEAR <= 2021 ~ "Far Rockaway, Breezy Point & Broad Channel",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 53 & YEAR <= 2021 ~ "(North Shore) Port Richmond, Stapleton & Mariners Harbor",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 54 & YEAR <= 2021 ~ "(Mid-Island) New Springville & South Beach",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 55 & YEAR <= 2021 ~ "(South Shore) Tottenville, Great Kills & Annadale",
    ## New Geos
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 1 & YEAR >= 2022 ~ "Melrose, Mott Haven, Longwood, & Hunts Point",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 2 & YEAR >= 2022 ~ "Morrisania, Tremont, Belmont, & West Farms",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 3 & YEAR >= 2022 ~ "Highbridge & Concourse",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 4 & YEAR >= 2022 ~ "Morris Heights & Mount Hope",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 5 & YEAR >= 2022 ~ "Fordham, Bedford Park, & Norwood",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 6 & YEAR >= 2022 ~ "Riverdale, Kingsbridge, & Marble Hill",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 7 & YEAR >= 2022 ~ "Soundview & Parkchester",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 8 & YEAR >= 2022 ~ "Co-op City & Throgs Neck",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 9 & YEAR >= 2022 ~ "Pelham Parkway & Morris Park",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 10 & YEAR >= 2022 ~ "Wakefield, Williamsbridge, & Eastchester",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 11 & YEAR >= 2022 ~ "Williamsburg & Greenpoint",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 12 & YEAR >= 2022 ~ "Downtown Brooklyn & Fort Greene",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 13 & YEAR >= 2022 ~ "Bedford-Stuyvesant",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 14 & YEAR >= 2022 ~ "Bushwick",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 15 & YEAR >= 2022 ~ "East New York & Cypress Hills",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 16 & YEAR >= 2022 ~ "Park Slope & Carroll Gardens",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 17 & YEAR >= 2022 ~ "Sunset Park & Windsor Terrace",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 18 & YEAR >= 2022 ~ "Crown Heights (North)",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 19 & YEAR >= 2022 ~ "Crown Heights (South)",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 20 & YEAR >= 2022 ~ "Bay Ridge & Dyker Heights",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 21 & YEAR >= 2022 ~ "Bensonhurst & Bath Beach",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 22 & YEAR >= 2022 ~ "Borough Park & Kensington",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 23 & YEAR >= 2022 ~ "Coney Island & Brighton Beach",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 24 & YEAR >= 2022 ~ "Flatbush & Midwood",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 25 & YEAR >= 2022 ~ "Sheepshead Bay & Gravesend (East)",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 26 & YEAR >= 2022 ~ "Ocean Hill & Brownsville",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 27 & YEAR >= 2022 ~ "East Flatbush",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 28 & YEAR >= 2022 ~ "Canarsie & Flatlands",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 29 & YEAR >= 2022 ~ "Financial District & Greenwich Village",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 30 & YEAR >= 2022 ~ "Lower East Side & Chinatown",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 31 & YEAR >= 2022 ~ "Chelsea & Hells Kitchen",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 32 & YEAR >= 2022 ~ "Midtown, East Midtown, & Flatiron",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 33 & YEAR >= 2022 ~ "Upper West Side",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 34 & YEAR >= 2022 ~ "Upper East Side & Roosevelt Island",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 35 & YEAR >= 2022 ~ "Morningside Heights & Hamilton Heights",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 36 & YEAR >= 2022 ~ "Harlem",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 37 & YEAR >= 2022 ~ "East Harlem",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 38 & YEAR >= 2022 ~ "Washington Heights & Inwood",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 39 & YEAR >= 2022 ~ "Astoria & Queensbridge",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 40 & YEAR >= 2022 ~ "Long Island City, Sunnyside, & Woodside",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 41 & YEAR >= 2022 ~ "Jackson Heights & East Elmhurst",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 42 & YEAR >= 2022 ~ "Elmhurst & Corona",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 43 & YEAR >= 2022 ~ "Ridgewood, Maspeth, & Middle Village",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 44 & YEAR >= 2022 ~ "Forest Hills & Rego Park",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 45 & YEAR >= 2022 ~ "Flushing, Murray Hill, & Whitestone",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 46 & YEAR >= 2022 ~ "Fresh Meadows, Hillcrest, & Briarwood",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 47 & YEAR >= 2022 ~ "Kew Gardens, Richmond Hill, & Woodhaven",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 48 & YEAR >= 2022 ~ "South Ozone Park & Howard Beach",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 49 & YEAR >= 2022 ~ "Auburndale, Bayside, & Douglaston",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 50 & YEAR >= 2022 ~ "Jamaica, St. Albans, & Hollis",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 51 & YEAR >= 2022 ~ "Queens Village, Bellerose, & Rosedale",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 52 & YEAR >= 2022 ~ "The Rockaways",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 53 & YEAR >= 2022 ~ "(North Shore) Port Richmond, Stapleton & Mariners Harbor",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 54 & YEAR >= 2022 ~ "(Mid-Island) New Springville & South Beach",
    variable_name %in% c("CD_neighborhoods", "CD_neighborhood", "Neighborhood") & x == 55 & YEAR >= 2022 ~ "(South Shore) Tottenville, Great Kills & Annadale",
    
    ## mergedCD code & neighborhood names (reconciled 2010&2020 geos)
    variable_name == "mergedCD_code" & x == 1  ~ "BX 1 & 2",
    variable_name == "mergedCD_code" & x == 2  ~ "BX 3 & 6",
    variable_name == "mergedCD_code" & x == 3  ~ "BX 4",
    variable_name == "mergedCD_code" & x == 4  ~ "BX 5",
    variable_name == "mergedCD_code" & x == 5  ~ "BX 7",
    variable_name == "mergedCD_code" & x == 6  ~ "BX 8",
    variable_name == "mergedCD_code" & x == 7  ~ "BX 9",
    variable_name == "mergedCD_code" & x == 8  ~ "BX 10",
    variable_name == "mergedCD_code" & x == 9  ~ "BX 11",
    variable_name == "mergedCD_code" & x == 10  ~ "BX 12",
    variable_name == "mergedCD_code" & x == 11  ~ "BK 1",
    variable_name == "mergedCD_code" & x == 12  ~ "BK 2",
    variable_name == "mergedCD_code" & x == 13  ~ "BK 3",
    variable_name == "mergedCD_code" & x == 14  ~ "BK 4",
    variable_name == "mergedCD_code" & x == 15  ~ "BK 5",
    variable_name == "mergedCD_code" & x == 16  ~ "BK 6",
    variable_name == "mergedCD_code" & x == 17  ~ "BK 7",
    variable_name == "mergedCD_code" & x == 18  ~ "BK 8",
    variable_name == "mergedCD_code" & x == 19  ~ "BK 9",
    variable_name == "mergedCD_code" & x == 20  ~ "BK 10",
    variable_name == "mergedCD_code" & x == 21  ~ "BK 11",
    variable_name == "mergedCD_code" & x == 22  ~ "BK 12",
    variable_name == "mergedCD_code" & x == 23  ~ "BK 13",
    variable_name == "mergedCD_code" & x == 24  ~ "BK 14",
    variable_name == "mergedCD_code" & x == 25  ~ "BK 15",
    variable_name == "mergedCD_code" & x == 26  ~ "BK 16",
    variable_name == "mergedCD_code" & x == 27  ~ "BK 17",
    variable_name == "mergedCD_code" & x == 28  ~ "BK 18",
    variable_name == "mergedCD_code" & x == 29  ~ "MN 1 & 2",
    variable_name == "mergedCD_code" & x == 30  ~ "MN 3",
    variable_name == "mergedCD_code" & x == 3132  ~ "MN 4, 5, & 6",
    variable_name == "mergedCD_code" & x == 31.32  ~ "MN 4, 5, & 6",
    variable_name == "mergedCD_code" & x == 33  ~ "MN 7",
    variable_name == "mergedCD_code" & x == 34  ~ "MN 8",
    variable_name == "mergedCD_code" & x == 35  ~ "MN 9",
    variable_name == "mergedCD_code" & x == 36  ~ "MN 10",
    variable_name == "mergedCD_code" & x == 37  ~ "MN 11",
    variable_name == "mergedCD_code" & x == 38  ~ "MN 12",
    variable_name == "mergedCD_code" & x == 39  ~ "QN 1",
    variable_name == "mergedCD_code" & x == 40  ~ "QN 2",
    variable_name == "mergedCD_code" & x == 41  ~ "QN 3",
    variable_name == "mergedCD_code" & x == 42  ~ "QN 4",
    variable_name == "mergedCD_code" & x == 43  ~ "QN 5",
    variable_name == "mergedCD_code" & x == 44  ~ "QN 6",
    variable_name == "mergedCD_code" & x == 45  ~ "QN 7",
    variable_name == "mergedCD_code" & x == 46  ~ "QN 8",
    variable_name == "mergedCD_code" & x == 47  ~ "QN 9",
    variable_name == "mergedCD_code" & x == 48  ~ "QN 10",
    variable_name == "mergedCD_code" & x == 49  ~ "QN 11",
    variable_name == "mergedCD_code" & x == 50  ~ "QN 12",
    variable_name == "mergedCD_code" & x == 51  ~ "QN 13",
    variable_name == "mergedCD_code" & x == 52  ~ "QN 14",
    variable_name == "mergedCD_code" & x == 53  ~ "SI 1",
    variable_name == "mergedCD_code" & x == 54  ~ "SI 2",
    variable_name == "mergedCD_code" & x == 55  ~ "SI 3",
    
    variable_name == "mergedCD_neighborhood" & x == 1  ~ "Melrose, Mott Haven, Longwood, & Hunts Point",
    variable_name == "mergedCD_neighborhood" & x == 2  ~ "Morrisania, Tremont, Belmont, & West Farms",
    variable_name == "mergedCD_neighborhood" & x == 3  ~ "Highbridge & Concourse",
    variable_name == "mergedCD_neighborhood" & x == 4  ~ "Morris Heights & Mount Hope",
    variable_name == "mergedCD_neighborhood" & x == 5  ~ "Fordham, Bedford Park, & Norwood",
    variable_name == "mergedCD_neighborhood" & x == 6  ~ "Riverdale, Kingsbridge, & Marble Hill",
    variable_name == "mergedCD_neighborhood" & x == 7  ~ "Soundview & Parkchester",
    variable_name == "mergedCD_neighborhood" & x == 8  ~ "Co-op City & Throgs Neck",
    variable_name == "mergedCD_neighborhood" & x == 9  ~ "Pelham Parkway & Morris Park",
    variable_name == "mergedCD_neighborhood" & x == 10  ~ "Wakefield, Williamsbridge, & Eastchester",
    variable_name == "mergedCD_neighborhood" & x == 11  ~ "Williamsburg & Greenpoint",
    variable_name == "mergedCD_neighborhood" & x == 12  ~ "Downtown Brooklyn & Fort Greene",
    variable_name == "mergedCD_neighborhood" & x == 13  ~ "Bedford-Stuyvesant",
    variable_name == "mergedCD_neighborhood" & x == 14  ~ "Bushwick",
    variable_name == "mergedCD_neighborhood" & x == 15  ~ "East New York & Cypress Hills",
    variable_name == "mergedCD_neighborhood" & x == 16  ~ "Park Slope & Carroll Gardens",
    variable_name == "mergedCD_neighborhood" & x == 17  ~ "Sunset Park & Windsor Terrace",
    variable_name == "mergedCD_neighborhood" & x == 18  ~ "Crown Heights (North)",
    variable_name == "mergedCD_neighborhood" & x == 19  ~ "Crown Heights (South)",
    variable_name == "mergedCD_neighborhood" & x == 20  ~ "Bay Ridge & Dyker Heights",
    variable_name == "mergedCD_neighborhood" & x == 21  ~ "Bensonhurst & Bath Beach",
    variable_name == "mergedCD_neighborhood" & x == 22  ~ "Borough Park & Kensington",
    variable_name == "mergedCD_neighborhood" & x == 23  ~ "Coney Island & Brighton Beach",
    variable_name == "mergedCD_neighborhood" & x == 24  ~ "Flatbush & Midwood",
    variable_name == "mergedCD_neighborhood" & x == 25  ~ "Sheepshead Bay & Gravesend (East)",
    variable_name == "mergedCD_neighborhood" & x == 26  ~ "Ocean Hill & Brownsville",
    variable_name == "mergedCD_neighborhood" & x == 27  ~ "East Flatbush",
    variable_name == "mergedCD_neighborhood" & x == 28  ~ "Canarsie & Flatlands",
    variable_name == "mergedCD_neighborhood" & x == 29  ~ "Financial District & Greenwich Village",
    variable_name == "mergedCD_neighborhood" & x == 30  ~ "Lower East Side & Chinatown",
    variable_name == "mergedCD_neighborhood" & x == 3132  ~ "Chelsea, Hells Kitchen, Midtown,  & Flatiron",
    variable_name == "mergedCD_neighborhood" & x == 31.32  ~ "Chelsea, Hells Kitchen, Midtown,  & Flatiron",
    variable_name == "mergedCD_neighborhood" & x == 33  ~ "Upper West Side",
    variable_name == "mergedCD_neighborhood" & x == 34  ~ "Upper East Side & Roosevelt Island",
    variable_name == "mergedCD_neighborhood" & x == 35  ~ "Morningside Heights & Hamilton Heights",
    variable_name == "mergedCD_neighborhood" & x == 36  ~ "Harlem",
    variable_name == "mergedCD_neighborhood" & x == 37  ~ "East Harlem",
    variable_name == "mergedCD_neighborhood" & x == 38  ~ "Washington Heights & Inwood",
    variable_name == "mergedCD_neighborhood" & x == 39  ~ "Astoria & Queensbridge",
    variable_name == "mergedCD_neighborhood" & x == 40  ~ "Long Island City, Sunnyside, & Woodside",
    variable_name == "mergedCD_neighborhood" & x == 41  ~ "Jackson Heights & East Elmhurst",
    variable_name == "mergedCD_neighborhood" & x == 42  ~ "Elmhurst & Corona",
    variable_name == "mergedCD_neighborhood" & x == 43  ~ "Ridgewood, Maspeth, & Middle Village",
    variable_name == "mergedCD_neighborhood" & x == 44  ~ "Forest Hills & Rego Park",
    variable_name == "mergedCD_neighborhood" & x == 45  ~ "Flushing, Murray Hill, & Whitestone",
    variable_name == "mergedCD_neighborhood" & x == 46  ~ "Fresh Meadows, Hillcrest, & Briarwood",
    variable_name == "mergedCD_neighborhood" & x == 47  ~ "Kew Gardens, Richmond Hill, & Woodhaven",
    variable_name == "mergedCD_neighborhood" & x == 48  ~ "South Ozone Park & Howard Beach",
    variable_name == "mergedCD_neighborhood" & x == 49  ~ "Auburndale, Bayside, & Douglaston",
    variable_name == "mergedCD_neighborhood" & x == 50  ~ "Jamaica, St. Albans, & Hollis",
    variable_name == "mergedCD_neighborhood" & x == 51  ~ "Queens Village, Bellerose, & Rosedale",
    variable_name == "mergedCD_neighborhood" & x == 52  ~ "The Rockaways",
    variable_name == "mergedCD_neighborhood" & x == 53  ~ "(North Shore) Port Richmond, Stapleton & Mariners Harbor",
    variable_name == "mergedCD_neighborhood" & x == 54  ~ "(Mid-Island) New Springville & South Beach",
    variable_name == "mergedCD_neighborhood" & x == 55  ~ "(South Shore) Tottenville, Great Kills & Annadale",
    
    
    ## YearBuilt (based on corresponding YBL/YRBLT codeing, see fxn below)
    variable_name == "YearBuilt" & x == 0 ~ "Not Applicable (Group Quarters)",
    variable_name == "YearBuilt" & x == 1 ~ "1939 or earlier",
    variable_name == "YearBuilt" & x == 2 ~ "1940 to 1949",
    variable_name == "YearBuilt" & x == 3 ~ "1950 to 1959",
    variable_name == "YearBuilt" & x == 4 ~ "1960 to 1969",
    variable_name == "YearBuilt" & x == 5 ~ "1970 to 1979",
    variable_name == "YearBuilt" & x == 6 ~ "1980 to 1989",
    variable_name == "YearBuilt" & x == 7 ~ "1990 to 1999",
    variable_name == "YearBuilt" & x == 8 ~ "2000 to 2009",
    variable_name == "YearBuilt" & x == 9 ~ "2010 to 2019",
    variable_name == "YearBuilt" & x == 10 ~ "2020",
    variable_name == "YearBuilt" & x == 11 ~ "2021",
    variable_name == "YearBuilt" & x == 12 ~ "2022",
    
    ########## Poverty Unit Related variables
    
    ## PovType_PU
    variable_name == "PovType_PU" & x == 1 ~ "Family",
    variable_name == "PovType_PU" & x == 2 ~ "Unrelated Subfamily",
    variable_name == "PovType_PU" & x == 3 ~ "Unrelated Individual",
    
    ## PovRel
    variable_name == "PovRel" & x == 1 ~ "Head",
    variable_name == "PovRel" & x == 2 ~ "Spouse/Partner",
    variable_name == "PovRel" & x == 3 ~ "Child",
    variable_name == "PovRel" & x == 4 ~ "Other",
    variable_name == "PovRel" & x == 5 ~ "Dependent",
    
    ## REL_U_MHU
    variable_name == "REL_U_MHU" & x == 0 ~ "Reference Person",
    variable_name == "REL_U_MHU" & x == 1 ~ "Spouse",
    variable_name == "REL_U_MHU" & x == 2 ~ "Child",
    variable_name == "REL_U_MHU" & x == 3 ~ "Sibling",
    variable_name == "REL_U_MHU" & x == 4 ~ "Parent",
    variable_name == "REL_U_MHU" & x == 5 ~ "Grandchild",
    variable_name == "REL_U_MHU" & x == 6 ~ "Inlaw",
    variable_name == "REL_U_MHU" & x == 7 ~ "Other Relative",
    variable_name == "REL_U_MHU" & x == 8 ~ "Boarder",
    variable_name == "REL_U_MHU" & x == 9 ~ "Roommate",
    variable_name == "REL_U_MHU" & x == 10 ~ "Unmarried Partner",
    variable_name == "REL_U_MHU" & x == 11 ~ "Foster Child ",
    variable_name == "REL_U_MHU" & x == 12 ~ "Other Non-Relative",
    
    ## NEWFAM_TYPE
    variable_name == "NEWFAM_TYPE" & x == 0 ~ "Traditional Family/Single",
    variable_name == "NEWFAM_TYPE" & x == 1 ~ "Related Subfamily",
    variable_name == "NEWFAM_TYPE" & x == 2 ~ "Unrelated Subfamily",
    variable_name == "NEWFAM_TYPE" & x == 3 ~ "Unmarried Partner Subfamily",
    
    ## FamilyType_PU
    variable_name == "FamilyType_PU" & x == 1 ~ "Husband/Wife + child",
    variable_name == "FamilyType_PU" & x == 2 ~ "Husband/Wife no child",
    variable_name == "FamilyType_PU" & x == 3 ~ "Single Male + child",
    variable_name == "FamilyType_PU" & x == 4 ~ "Single Female + child",
    variable_name == "FamilyType_PU" & x == 5 ~ "Male unit head, no child",
    variable_name == "FamilyType_PU" & x == 6 ~ "Female unit head, no child",
    variable_name == "FamilyType_PU" & x == 7 ~ "Unrelated Indiv w/others",
    variable_name == "FamilyType_PU" & x == 8 ~ "Unrelated Indiv Alone",
    
    ## FamType (like FamilyType_PU but reflects elderly head/spouse)
    variable_name == "FamType" & x == 1 ~ "Two Parents Family",
    variable_name == "FamType" & x == 2 ~ "Sole Parent Family",
    variable_name == "FamType" & x == 3 ~ "Childless Married Couple",
    variable_name == "FamType" & x == 4 ~ "Single Living with relatives",
    variable_name == "FamType" & x == 5 ~ "Single nonelderly adults",
    variable_name == "FamType" & x == 6 ~ "Elderly Two Parents Family",
    variable_name == "FamType" & x == 7 ~ "Elderly Single Parent Family",
    variable_name == "FamType" & x == 8 ~ "Childless Elderly Married Couple",
    variable_name == "FamType" & x == 9 ~ "Elderly Living with relatives",
    variable_name == "FamType" & x == 10 ~ "Single Elderly",
    
    
    ## Total NYC
    x == -1 ~ "Sample Total",
    
    
    ######### TEMP/MISC Labels (breakout functions)
    variable_name == "school_level" & x == 1 ~ "Grade School",
    variable_name == "school_level" & x == 2 ~ "Post-Secondary School",
    variable_name == "school_level" & x == 3 ~ "Not Enrolled",
    
    variable_name == "work_status" & x == 1 ~ "Part-time",
    variable_name == "work_status" & x == 2 ~ "Full-time",
    variable_name == "work_status" & x == 3 ~ "No Work",
    
    variable_name == "sch_binary" & x == 1 ~ "Enrolled in School",
    variable_name == "sch_binary" & x == 2 ~ "NOT Enrolled in School",
    
    variable_name == "age_youth" & x == 1 ~ "Below 18",
    variable_name == "age_youth" & x == 2 ~ "18 - 24",
    variable_name == "age_youth" & x == 3 ~ "25 - 29",
    variable_name == "age_youth" & x == 4 ~ "30+",
    
    variable_name == "enrollXwork" & x == 1 ~ "Enrolled & works",
    variable_name == "enrollXwork" & x == 2 ~ "NOT enrolled & works",
    variable_name == "enrollXwork" & x == 3 ~ "Enrolled & NO work",
    variable_name == "enrollXwork" & x == 4 ~ "NOT enrolled & NO work",
    
    variable_name == "disccon" & x == 1 ~ "Disconnected",
    variable_name == "disccon" & x == 2 ~ "NOT Disconnected",
    
    variable_name == "age_range" & x == 1 ~ "16 - 24",
    variable_name == "age_range" & x == 2 ~ "25 - 29",
    
    variable_name == "dcYouth" & x == 1 ~ "Disconnected Youth",
    variable_name == "dcYouth" & x == 2 ~ "NOT Disconnected Youth",
    
    ### end
    
    
    TRUE ~ paste0(variable_name, "_", x))
  
  return(newVal)
  
}

###### Individual recode functions ######
## Age variables ###########################

fxn_age_recode <- function(AGEP){
  
  ## Variable: age_recode
  
  if(missing(AGEP)){ stop("ERROR: Missing AGEP for age_recode") } 
  
  x <- as.numeric(AGEP)
  
  newVal <- case_when(
    x < 5 ~ 1,
    x >= 5  & x < 10 ~ 2,
    x >= 10 & x < 15 ~ 3,
    x >= 15 & x < 20 ~ 4,
    x >= 20 & x < 25 ~ 5,
    x >= 25 & x < 30 ~ 6,
    x >= 30 & x < 35 ~ 7,
    x >= 35 & x < 40 ~ 8,
    x >= 40 & x < 45 ~ 9,
    x >= 45 & x < 50 ~ 10,
    x >= 50 & x < 55 ~ 11,
    x >= 55 & x < 60 ~ 12,
    x >= 60 & x < 65 ~ 13,
    x >= 65 & x < 70 ~ 14,
    x >= 70 & x < 75 ~ 15,
    x >= 75 & x < 80 ~ 16,
    x >= 80 & x < 85 ~ 17,
    x >= 85 & x < 90 ~ 18,
    x >= 90 ~ 19,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

fxn_age_3groups <- function(AGEP){
  
  ## Variable: age_3groups
  
  if(missing(AGEP)){ stop("ERROR: Missing AGEP for age_3groups") } 
  
  x <- as.numeric(AGEP)
  
  newVal <- case_when(
    x < 18 ~ 1,
    x >= 18 & x <= 64 ~ 2,
    x >= 65 ~ 3,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

fxn_AgeForReport <- function(AGEP){
  
  ## Variable: AgeForReport
  
  if(missing(AGEP)){ stop("ERROR: Missing AGEP for AgeForReport") } 
  
  x <- as.numeric(AGEP)
  
  newVal <- case_when(
    x < 18 ~ 1,
    x >= 18 & x <= 64 ~ 2,
    x >= 65 ~ 3,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

fxn_age_6groups <- function(AGEP){
  
  ## Variable: age_6groups
  
  if(missing(AGEP)){ stop("ERROR: Missing AGEP for age_6groups") } 
  
  x <- as.numeric(AGEP)
  
  newVal <- case_when(
    x < 16 ~ 1,
    x >= 16 & x <= 24 ~ 2,
    x >= 25 & x <= 40 ~ 3,
    x >= 41 & x <= 64 ~ 4,
    x >= 65 & x <= 80 ~ 5,
    x >= 81 ~ 6,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}


## HHT ################

fxn_hht_recode <- function(HHT){
  
  ## Variable: hht_recode
  
  if(missing(HHT)){ stop("ERROR: Missing HHT for hht_recode") } 
  
  x <- as.numeric(HHT)
  
  newVal <- case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 6 ~ 4,
    x == 5 ~ 5,
    x == 7 ~ 5,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

## ESR, Work experience, etc. ########

fxn_esr_recode <- function(ESR){
  
  ## Variable: esr_recode
  
  if(missing(ESR)){ stop("ERROR: Missing ESR for esr_recode") } 
  
  x <- as.numeric(ESR)
  
  newVal <- case_when(
    x == 1 ~ 1,
    x == 2 ~ 1,
    x == 4 ~ 1,
    x == 5 ~ 1,
    x == 3 ~ 2,
    x == 6 ~ 3,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}


fxn_WorkExp <- function(WKWN_orWKWpre2019, WKHP, YEAR){
  
  ## Variable: WorkExp
  ## NOTE: If YEAR < 2019, then WKWN is WKW
  
  ## ALSO NOTE: NA WKWN is categorized as 3 (no work)....i don't love this, but for now pair with a !is.na(ESR) filter
  
  if(missing(WKWN_orWKWpre2019)){ stop("ERROR: Missing WKWN (or WKW if pre2019) for WorkExp") }
  if(missing(WKHP)){stop("ERROR: Missing WKHP for WorkExp")}
  if(missing(YEAR)){stop("ERROR: Missing YEAR for WorkExp")}
  
  x <- as.numeric(WKWN_orWKWpre2019)
  y <- as.numeric(WKHP)
  
  newVal <- case_when(
    ## no wrkexp, applies all years
    YEAR >= 0 & is.na(x) ~ 3,   
    YEAR >= 0 & x == 0 ~ 3,
    YEAR >= 0 & is.na(y) ~ 3,
    
    ## other we levels
    YEAR >= 2019 & x > 49 & y > 34 ~ 1,  
    YEAR >= 2019 & x <= 49 & x > 0 & !is.na(y) ~ 2, 
    YEAR >= 2019 & !is.na(x) & y > 0 & y <= 34 ~ 2, 
    
    YEAR >= 2008 & YEAR <= 2018 & x == 1 & y > 34 ~ 1,  
    YEAR >= 2008 & YEAR <= 2018 & x >= 2 & !is.na(y) ~ 2, 
    YEAR >= 2008 & YEAR <= 2018 & !is.na(x) & y > 0 & y <= 34 ~ 2, 
    
    YEAR <= 2007 & x > 49 & y > 34 ~ 1,  
    YEAR <= 2007 & x <= 49 & x > 0 & !is.na(y) ~ 2, 
    YEAR <= 2007 & !is.na(x) & y > 0 & y <= 34 ~ 2,  
    
    TRUE ~ -9999999)
  
  return(newVal)
  
}



## Ethnicity variables ################

fxn_Ethnicity <- function(RAC1P, HISP){
  
  ## Variable: Ethnicity
  
  if(missing(RAC1P)){ stop("ERROR: Missing RAC1P for Ethnicity") }
  if(missing(HISP)){ stop("ERROR: Missing HISP for Ethnicity") }
  
  x <- as.numeric(RAC1P)
  y <- as.numeric(HISP)
  
  newVal <- case_when(
    !is.na(x) & y > 1 ~ 4,
    x == 1 & y == 1 ~ 1,
    x == 2 & y == 1 ~ 2,
    x == 6 & y == 1 ~ 3,
    x == 3 & y == 1 ~ 5,
    x == 4 & y == 1 ~ 5,
    x == 5 & y == 1 ~ 5,
    x == 7 & y == 1 ~ 5,
    x == 8 & y == 1 ~ 5,
    x == 9 & y == 1 ~ 5,
    is.na(x) | is.na(y) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

fxn_Ethnicity_recode <- function(RAC1P, HISP){
  
  ## Variable: Ethnicity_recode
  
  if(missing(RAC1P)){ stop("ERROR: Missing RAC1P for Ethnicity_recode") }
  if(missing(HISP)){ stop("ERROR: Missing HISP for Ethnicity_recode") }
  
  x <- as.numeric(RAC1P)
  y <- as.numeric(HISP)
  
  newVal <- case_when(
    !is.na(x) & y > 1 ~ 4,
    x == 1 & y == 1 ~ 1,
    x == 2 & y == 1 ~ 2,
    x == 6 & y == 1 ~ 3,
    x == 3 & y == 1 ~ 6,
    x == 4 & y == 1 ~ 6,
    x == 5 & y == 1 ~ 6,
    x == 7 & y == 1 ~ 5,
    x == 8 & y == 1 ~ 7,
    x == 9 & y == 1 ~ 8,
    is.na(x) | is.na(y) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}


## Education variables #########

## BIG AH NOTE: I defined EdAttain as 6 levels in a lot of my code! 
## ALSO, it's defined that way for control totals, so agg function reflects that (will eventually want to update)
## Watch out for errors now that fxn uses report/std 4levels!
fxn_EdAttain <- function(SCHL, YEAR){
  
  ## Variable: EdAttain
  ## NOTE: YEAR is optional. If not specified, assumes ACS coding of SCHL as of 2008
  
  if(missing(SCHL)){ stop("ERROR: Missing SCHL for EdAttain") }
  if(missing(YEAR)){ YEAR <- 2008}
  
  x <- as.numeric(SCHL)
  
  newVal <- case_when(
    YEAR >= 2008 & x >= 1 & x <= 15 ~ 1,  ## less than High School
    YEAR >= 2008 & x %in% c(16, 17) ~ 2,  ## High School Degree or equiv
    YEAR >= 2008 & x >= 18 & x <= 20 ~ 3,  ## Some collage
    YEAR >= 2008 & x >= 21 ~ 4,            ## Bach +
    
    YEAR %in% c(NA, 2000:2007) & x >= 1 & x <= 8 ~ 1,  ## less than High School
    YEAR %in% c(NA, 2000:2007) & x == 9 ~ 2,  ## High School Degree or equiv
    YEAR %in% c(NA, 2000:2007) & x >= 10 & x <= 12 ~ 3,  ## Some collage
    YEAR %in% c(NA, 2000:2007) & x >= 13 ~ 4,            ## Bach +
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

## EdAttain at 6 levels, good for 2008+ (AH: I usually called this "EdAttain" as of 2023.11.30)
## YEAR is optional. Defaults to assuming 2008+
fxn_EdAttain6 <- function(SCHL, YEAR){
  
  ## Variable: EdAttain6
  
  if(missing(SCHL)){ stop("ERROR: Missing SCHL for EdAttain6") }
  if(missing(YEAR)){YEAR = 2008}
  
  x <- as.numeric(SCHL)
  
  newVal <- case_when(
    YEAR >= 2008 & x == 1 ~ 1,  ## none completed
    YEAR >= 2008 & x > 1 & x <= 15 ~ 2,  ## Less than HS
    YEAR >= 2008 & x > 15 & x <= 17 ~ 3,  ## HS or equiv
    YEAR >= 2008 & x > 17 & x <= 19 ~ 4,  ## Some collage, no degree
    YEAR >= 2008 & x == 20 ~ 5,  ## Assoc
    YEAR >= 2008 & x >= 21 ~ 6,  ## Bach +
    YEAR %in% c(NA, 2000:2007) & x == 1 ~ 1,  ## none completed
    YEAR %in% c(NA, 2000:2007) & x > 1 & x <= 8 ~ 2,  ## Less than HS
    YEAR %in% c(NA, 2000:2007) & x == 9 ~ 3,  ## HS or equiv
    YEAR %in% c(NA, 2000:2007) & x %in% c(10, 11) ~ 4,  ## Some collage, no degree
    YEAR %in% c(NA, 2000:2007) & x == 12 ~ 5,  ## Assoc
    YEAR %in% c(NA, 2000:2007) & x >= 13 ~ 6,  ## Bach +
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

fxn_edu2 <- function(SCHL){
  
  ## Variable: edu2
  
  if(missing(SCHL)){ stop("ERROR: Missing SCHL for edu2") }
  
  x <- as.numeric(SCHL)
  
  newVal <- case_when(
    x <= 20 ~ 1,  ## lt BA
    x >= 21 ~ 6,  ## Bach +
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}


## Citizenship variables #######

fxn_CitizenStat <- function(CIT){
  
  ## Variable: CitizenStat
  
  if(missing(CIT)){ stop("ERROR: Missing SCHL for CitizenStat") }
  
  x <- as.numeric(CIT)
  
  newVal <- case_when(
    x == 1 ~ 1,
    x == 2 ~ 1,
    x == 3 ~ 1,
    x == 4 ~ 2,
    x == 5 ~ 3,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

## Industry & Occupation variables ########

fxn_IndShort <- function(INDP){
  
  ## Variable: IndShort
  
  if(missing(INDP)){ stop("ERROR: Missing INDP for IndShort") }
  
  x <- as.numeric(INDP)
  
  newVal <- case_when(
    x == 9920 ~ 0,   ## Unemployed And Last Worked 5 Years Ago Or Earlier Or Never Worked
    x %in% c(170:490,770) ~ 1,
    x %in% c(1070:3990) ~ 2,
    x %in% c(4070:4590) ~ 3,
    x %in% c(4670:5790) ~ 4,
    x %in% c(570:690,6070:6390) ~ 5,
    x %in% c(6470:6780) ~ 6,
    x %in% c(6870:7190) ~ 7,
    x %in% c(7270:7790) ~ 8,
    x %in% c(7860:8470) ~ 9,
    x %in% c(8560:8690) ~ 10,
    x %in% c(8770:9290) ~ 11,
    x %in% c(9370:9590) ~ 12,
    x %in% c(9670:9870) ~ 13,
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

fxn_occ_recode <- function(OCCP){
  
  ## Variable: occ_recode
  
  if(missing(OCCP)){ stop("ERROR: Missing OCCP for occ_recode") }
  
  x <- as.numeric(OCCP)
  
  newVal <- case_when(
    x >= 10 & x < 500 ~ 1,
    x >= 500 & x < 3700 ~ 2,
    x >= 3700 & x < 4700 ~ 3,
    x >= 4700 & x < 5000 ~ 4,
    x >= 5000 & x < 6000 ~ 5,
    x >= 6000 & x <= 7640 ~ 6,
    x >= 7700 & x <= 9760 ~ 7,
    x >= 9800 & x < 9920 ~ 8,
    x == 9920  ~ 9, ## Unemployed And Last Worked 5 Years Ago Or Earlier Or Never Worked
    ## NA note: N/A (less than 16 years old/NILF who last worked more than 5 years ago or never worked)
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}



## Geography variables ######

fxn_Boro <- function(PUMA, YEAR){
  
  ## Variable: Boro
  ## Updated to reflect 2020 geographies for 2022+ ACS data
  
  if(missing(PUMA)){ stop("ERROR: Missing PUMA for Boro") }
  if(missing(YEAR)){ stop("ERROR: Missing YEAR for Boro") }
  
  x <- as.numeric(PUMA)
  
  newVal <- case_when(
    ## 2010 geos (ACS <= 2021)
    x >= 3701 & x <= 3710 & YEAR <= 2021 ~ 1,
    x >= 4001 & x <= 4018 & YEAR <= 2021 ~ 2,
    x >= 3801 & x <= 3811 & YEAR <= 2021 ~ 3,
    x >= 4101 & x <= 4114 & YEAR <= 2021 ~ 4,
    x >= 3901 & x <= 3903 & YEAR <= 2021 ~ 5,
    
    ## 2020 geos (ACS >= 2022)
    x %in% c(4200:4299) & YEAR >= 2022 ~ 1,
    x %in% c(4300:4399) & YEAR >= 2022 ~ 2,
    x %in% c(4100:4199) & YEAR >= 2022 ~ 3,
    x %in% c(4400:4499) & YEAR >= 2022 ~ 4,
    x %in% c(4500:4599) & YEAR >= 2022 ~ 5,
    
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}

fxn_CD <- function(PUMA, YEAR){
  
  ## Variable: CD
  ## Updated to reflect 2020 geographies for 2022+ ACS data
  
  if(missing(PUMA)){ stop("ERROR: Missing PUMA for CD") }
  if(missing(YEAR)){ stop("ERROR: Missing YEAR for CD") }
  
  x <- as.numeric(PUMA)
  
  newVal <- case_when(
    
    ## See key/cross walk: /Users/annehill/Dropbox/NYCO_remote/PRU_2022_misc/PRU_2023.10.23/2022_scratchpad/geo_resources/NOTES_PUMAS_2010and2020.xlsx
    x == 3710 & YEAR <= 2021 ~ 1,
    x == 3705 & YEAR <= 2021 ~ 2,
    x == 3708 & YEAR <= 2021 ~ 3,
    x == 3707 & YEAR <= 2021 ~ 4,
    x == 3706 & YEAR <= 2021 ~ 5,
    x == 3701 & YEAR <= 2021 ~ 6,
    x == 3709 & YEAR <= 2021 ~ 7,
    x == 3703 & YEAR <= 2021 ~ 8,
    x == 3704 & YEAR <= 2021 ~ 9,
    x == 3702 & YEAR <= 2021 ~ 10,
    x == 4001 & YEAR <= 2021 ~ 11,
    x == 4004 & YEAR <= 2021 ~ 12,
    x == 4003 & YEAR <= 2021 ~ 13,
    x == 4002 & YEAR <= 2021 ~ 14,
    x == 4008 & YEAR <= 2021 ~ 15,
    x == 4005 & YEAR <= 2021 ~ 16,
    x == 4012 & YEAR <= 2021 ~ 17,
    x == 4006 & YEAR <= 2021 ~ 18,
    x == 4011 & YEAR <= 2021 ~ 19,
    x == 4013 & YEAR <= 2021 ~ 20,
    x == 4017 & YEAR <= 2021 ~ 21,
    x == 4014 & YEAR <= 2021 ~ 22,
    x == 4018 & YEAR <= 2021 ~ 23,
    x == 4015 & YEAR <= 2021 ~ 24,
    x == 4016 & YEAR <= 2021 ~ 25,
    x == 4007 & YEAR <= 2021 ~ 26,
    x == 4010 & YEAR <= 2021 ~ 27,
    x == 4009 & YEAR <= 2021 ~ 28,
    x == 3810 & YEAR <= 2021 ~ 29,
    x == 3809 & YEAR <= 2021 ~ 30,
    x == 3807 & YEAR <= 2021 ~ 31,
    x == 3808 & YEAR <= 2021 ~ 32,
    x == 3806 & YEAR <= 2021 ~ 33,
    x == 3805 & YEAR <= 2021 ~ 34,
    x == 3802 & YEAR <= 2021 ~ 35,
    x == 3803 & YEAR <= 2021 ~ 36,
    x == 3804 & YEAR <= 2021 ~ 37,
    x == 3801 & YEAR <= 2021 ~ 38,
    x == 4101 & YEAR <= 2021 ~ 39,
    x == 4109 & YEAR <= 2021 ~ 40,
    x == 4102 & YEAR <= 2021 ~ 41,
    x == 4107 & YEAR <= 2021 ~ 42,
    x == 4110 & YEAR <= 2021 ~ 43,
    x == 4108 & YEAR <= 2021 ~ 44,
    x == 4103 & YEAR <= 2021 ~ 45,
    x == 4106 & YEAR <= 2021 ~ 46,
    x == 4111 & YEAR <= 2021 ~ 47,
    x == 4113 & YEAR <= 2021 ~ 48,
    x == 4104 & YEAR <= 2021 ~ 49,
    x == 4112 & YEAR <= 2021 ~ 50,
    x == 4105 & YEAR <= 2021 ~ 51,
    x == 4114 & YEAR <= 2021 ~ 52,
    x == 3903 & YEAR <= 2021 ~ 53,
    x == 3902 & YEAR <= 2021 ~ 54,
    x == 3901 & YEAR <= 2021 ~ 55,
    ## 2020 geos needed for ACS years 2022+
    x == 4221 & YEAR >= 2022 ~ 1,
    x == 4263 & YEAR >= 2022 ~ 2,
    x == 4204 & YEAR >= 2022 ~ 3,
    x == 4205 & YEAR >= 2022 ~ 4,
    x == 4207 & YEAR >= 2022 ~ 5,
    x == 4208 & YEAR >= 2022 ~ 6,
    x == 4209 & YEAR >= 2022 ~ 7,
    x == 4210 & YEAR >= 2022 ~ 8,
    x == 4211 & YEAR >= 2022 ~ 9,
    x == 4212 & YEAR >= 2022 ~ 10,
    x == 4301 & YEAR >= 2022 ~ 11,
    x == 4302 & YEAR >= 2022 ~ 12,
    x == 4303 & YEAR >= 2022 ~ 13,
    x == 4304 & YEAR >= 2022 ~ 14,
    x == 4305 & YEAR >= 2022 ~ 15,
    x == 4306 & YEAR >= 2022 ~ 16,
    x == 4307 & YEAR >= 2022 ~ 17,
    x == 4308 & YEAR >= 2022 ~ 18,
    x == 4309 & YEAR >= 2022 ~ 19,
    x == 4310 & YEAR >= 2022 ~ 20,
    x == 4311 & YEAR >= 2022 ~ 21,
    x == 4312 & YEAR >= 2022 ~ 22,
    x == 4313 & YEAR >= 2022 ~ 23,
    x == 4314 & YEAR >= 2022 ~ 24,
    x == 4315 & YEAR >= 2022 ~ 25,
    x == 4316 & YEAR >= 2022 ~ 26,
    x == 4317 & YEAR >= 2022 ~ 27,
    x == 4318 & YEAR >= 2022 ~ 28,
    x == 4121 & YEAR >= 2022 ~ 29,
    x == 4103 & YEAR >= 2022 ~ 30,
    x == 4104 & YEAR >= 2022 ~ 31,
    x == 4165 & YEAR >= 2022 ~ 32,
    x == 4107 & YEAR >= 2022 ~ 33,
    x == 4108 & YEAR >= 2022 ~ 34,
    x == 4109 & YEAR >= 2022 ~ 35,
    x == 4110 & YEAR >= 2022 ~ 36,
    x == 4111 & YEAR >= 2022 ~ 37,
    x == 4112 & YEAR >= 2022 ~ 38,
    x == 4401 & YEAR >= 2022 ~ 39,
    x == 4402 & YEAR >= 2022 ~ 40,
    x == 4403 & YEAR >= 2022 ~ 41,
    x == 4404 & YEAR >= 2022 ~ 42,
    x == 4405 & YEAR >= 2022 ~ 43,
    x == 4406 & YEAR >= 2022 ~ 44,
    x == 4407 & YEAR >= 2022 ~ 45,
    x == 4408 & YEAR >= 2022 ~ 46,
    x == 4409 & YEAR >= 2022 ~ 47,
    x == 4410 & YEAR >= 2022 ~ 48,
    x == 4411 & YEAR >= 2022 ~ 49,
    x == 4412 & YEAR >= 2022 ~ 50,
    x == 4413 & YEAR >= 2022 ~ 51,
    x == 4414 & YEAR >= 2022 ~ 52,
    x == 4501 & YEAR >= 2022 ~ 53,
    x == 4502 & YEAR >= 2022 ~ 54,
    x == 4503 & YEAR >= 2022 ~ 55,
    
    is.na(x) ~ as.numeric(NA),
    TRUE ~ -9999999)
  
  return(newVal)
  
}


## Year Built ##############

## Need to check for ACS years pre-2012
## This fucntion is slightly different than others. Need to specify both YBL and YRBLT, even if one osn't included

fxn_YearBuilt <- function(YBL, YRBLT, YEAR){
  
  ## YBL: pre-2021
  ## YRBLT: 2021+
  
  ## Variable: YearBuilt
  
  if(missing(YBL)){YBL <- NA}
  if(missing(YRBLT)){YRBLT <- NA}
  
  YBL <- as.numeric(YBL)
  YRBLT <- as.numeric(YRBLT)
  
  newVal <- case_when(
    YEAR <= 2020 & YBL == 1 ~ 1, 
    YEAR <= 2020 & YBL == 2 ~ 2, 
    YEAR <= 2020 & YBL == 3 ~ 3, 
    YEAR <= 2020 & YBL == 4 ~ 4, 
    YEAR <= 2020 & YBL == 5 ~ 5, 
    YEAR <= 2020 & YBL == 6 ~ 6, 
    YEAR <= 2020 & YBL == 7 ~ 7, 
    YEAR <= 2020 & YBL == 8 ~ 8, 
    YEAR <= 2020 & YBL == 9 ~ 8, 
    YEAR <= 2020 & YBL == 10 ~ 8, 
    YEAR <= 2020 & YBL == 11 ~ 8, 
    YEAR <= 2020 & YBL == 12 ~ 8, 
    YEAR <= 2020 & YBL == 13 ~ 8, 
    YEAR <= 2020 & YBL == 14 ~ 9, 
    YEAR <= 2020 & YBL == 15 ~ 9, 
    YEAR <= 2020 & YBL == 16 ~ 9, 
    YEAR <= 2020 & YBL == 17 ~ 9, 
    YEAR <= 2020 & YBL == 18 ~ 9, 
    YEAR <= 2020 & YBL == 19 ~ 9, 
    YEAR <= 2020 & YBL == 20 ~ 9, 
    YEAR <= 2020 & YBL == 21 ~ 9, 
    YEAR <= 2020 & YBL == 22 ~ 9, 
    YEAR <= 2020 & YBL == 23 ~ 9, 
    YEAR <= 2020 & YBL == 24 ~ 10, 
    YEAR <= 2020 & is.na(YBL) ~ 0,  ## group quarters
    
    YEAR >= 2021 & YRBLT == 1939 ~ 1, 
    YEAR >= 2021 & YRBLT == 1940 ~ 2, 
    YEAR >= 2021 & YRBLT == 1950 ~ 3, 
    YEAR >= 2021 & YRBLT == 1960 ~ 4, 
    YEAR >= 2021 & YRBLT == 1970 ~ 5, 
    YEAR >= 2021 & YRBLT == 1980 ~ 6, 
    YEAR >= 2021 & YRBLT == 1990 ~ 7, 
    YEAR >= 2021 & YRBLT == 2000 ~ 8, 
    YEAR >= 2021 & YRBLT == 2010 ~ 9, 
    YEAR >= 2021 & YRBLT == 2020 ~ 10, 
    YEAR >= 2021 & YRBLT == 2021 ~ 11, 
    YEAR >= 2021 & YRBLT == 2022 ~ 12, 
    YEAR >= 2021 & is.na(YRBLT) ~ 0, ## GQ

    TRUE ~ -9999999)
  
  return(newVal)
  
}








#
## Income variables #####

fxn_inc <- function(INC, ADJINC){
  
  ## Variable: income variables <inc>_adj
  ## ACS income vars: PINCP, HINCP, SEMP, OIP, WAGP, PAP, SSIP, SSP, INTP, RETP
  ## If ACS year <= 2007, adjustment variable is ADJUST
  
  if(missing(INC)){ stop("ERROR: Missing ACS income variable (open function for options)") }
  if(missing(ADJINC)){ stop("ERROR: Missing adjustment variable") }
  
  newVal <- as.numeric(INC) * (as.numeric(ADJINC)/1000000)
  
  return(newVal)
}


###### CD to PUMA #######

fxn_cd2puma <- function(CD, YEAR){
  
  newVal <- case_when(
    CD == 1 & YEAR <= 2021 ~ 3710,
    CD == 2 & YEAR <= 2021 ~ 3705,
    CD == 3 & YEAR <= 2021 ~ 3708,
    CD == 4 & YEAR <= 2021 ~ 3707,
    CD == 5 & YEAR <= 2021 ~ 3706,
    CD == 6 & YEAR <= 2021 ~ 3701,
    CD == 7 & YEAR <= 2021 ~ 3709,
    CD == 8 & YEAR <= 2021 ~ 3703,
    CD == 9 & YEAR <= 2021 ~ 3704,
    CD == 10 & YEAR <= 2021 ~ 3702,
    CD == 11 & YEAR <= 2021 ~ 4001,
    CD == 12 & YEAR <= 2021 ~ 4004,
    CD == 13 & YEAR <= 2021 ~ 4003,
    CD == 14 & YEAR <= 2021 ~ 4002,
    CD == 15 & YEAR <= 2021 ~ 4008,
    CD == 16 & YEAR <= 2021 ~ 4005,
    CD == 17 & YEAR <= 2021 ~ 4012,
    CD == 18 & YEAR <= 2021 ~ 4006,
    CD == 19 & YEAR <= 2021 ~ 4011,
    CD == 20 & YEAR <= 2021 ~ 4013,
    CD == 21 & YEAR <= 2021 ~ 4017,
    CD == 22 & YEAR <= 2021 ~ 4014,
    CD == 23 & YEAR <= 2021 ~ 4018,
    CD == 24 & YEAR <= 2021 ~ 4015,
    CD == 25 & YEAR <= 2021 ~ 4016,
    CD == 26 & YEAR <= 2021 ~ 4007,
    CD == 27 & YEAR <= 2021 ~ 4010,
    CD == 28 & YEAR <= 2021 ~ 4009,
    CD == 29 & YEAR <= 2021 ~ 3810,
    CD == 30 & YEAR <= 2021 ~ 3809,
    CD == 31 & YEAR <= 2021 ~ 3807,
    CD == 32 & YEAR <= 2021 ~ 3808,
    CD == 33 & YEAR <= 2021 ~ 3806,
    CD == 34 & YEAR <= 2021 ~ 3805,
    CD == 35 & YEAR <= 2021 ~ 3802,
    CD == 36 & YEAR <= 2021 ~ 3803,
    CD == 37 & YEAR <= 2021 ~ 3804,
    CD == 38 & YEAR <= 2021 ~ 3801,
    CD == 39 & YEAR <= 2021 ~ 4101,
    CD == 40 & YEAR <= 2021 ~ 4109,
    CD == 41 & YEAR <= 2021 ~ 4102,
    CD == 42 & YEAR <= 2021 ~ 4107,
    CD == 43 & YEAR <= 2021 ~ 4110,
    CD == 44 & YEAR <= 2021 ~ 4108,
    CD == 45 & YEAR <= 2021 ~ 4103,
    CD == 46 & YEAR <= 2021 ~ 4106,
    CD == 47 & YEAR <= 2021 ~ 4111,
    CD == 48 & YEAR <= 2021 ~ 4113,
    CD == 49 & YEAR <= 2021 ~ 4104,
    CD == 50 & YEAR <= 2021 ~ 4112,
    CD == 51 & YEAR <= 2021 ~ 4105,
    CD == 52 & YEAR <= 2021 ~ 4114,
    CD == 53 & YEAR <= 2021 ~ 3903,
    CD == 54 & YEAR <= 2021 ~ 3902,
    CD == 55 & YEAR <= 2021 ~ 3901,
    ## 2020 geos (ACS 2022+)
    CD == 1 & YEAR >= 2022 ~ 4221,
    CD == 2 & YEAR >= 2022 ~ 4263,
    CD == 3 & YEAR >= 2022 ~ 4204,
    CD == 4 & YEAR >= 2022 ~ 4205,
    CD == 5 & YEAR >= 2022 ~ 4207,
    CD == 6 & YEAR >= 2022 ~ 4208,
    CD == 7 & YEAR >= 2022 ~ 4209,
    CD == 8 & YEAR >= 2022 ~ 4210,
    CD == 9 & YEAR >= 2022 ~ 4211,
    CD == 10 & YEAR >= 2022 ~ 4212,
    CD == 11 & YEAR >= 2022 ~ 4301,
    CD == 12 & YEAR >= 2022 ~ 4302,
    CD == 13 & YEAR >= 2022 ~ 4303,
    CD == 14 & YEAR >= 2022 ~ 4304,
    CD == 15 & YEAR >= 2022 ~ 4305,
    CD == 16 & YEAR >= 2022 ~ 4306,
    CD == 17 & YEAR >= 2022 ~ 4307,
    CD == 18 & YEAR >= 2022 ~ 4308,
    CD == 19 & YEAR >= 2022 ~ 4309,
    CD == 20 & YEAR >= 2022 ~ 4310,
    CD == 21 & YEAR >= 2022 ~ 4311,
    CD == 22 & YEAR >= 2022 ~ 4312,
    CD == 23 & YEAR >= 2022 ~ 4313,
    CD == 24 & YEAR >= 2022 ~ 4314,
    CD == 25 & YEAR >= 2022 ~ 4315,
    CD == 26 & YEAR >= 2022 ~ 4316,
    CD == 27 & YEAR >= 2022 ~ 4317,
    CD == 28 & YEAR >= 2022 ~ 4318,
    CD == 29 & YEAR >= 2022 ~ 4121,
    CD == 30 & YEAR >= 2022 ~ 4103,
    CD == 31 & YEAR >= 2022 ~ 4104,
    CD == 32 & YEAR >= 2022 ~ 4165,
    CD == 33 & YEAR >= 2022 ~ 4107,
    CD == 34 & YEAR >= 2022 ~ 4108,
    CD == 35 & YEAR >= 2022 ~ 4109,
    CD == 36 & YEAR >= 2022 ~ 4110,
    CD == 37 & YEAR >= 2022 ~ 4111,
    CD == 38 & YEAR >= 2022 ~ 4112,
    CD == 39 & YEAR >= 2022 ~ 4401,
    CD == 40 & YEAR >= 2022 ~ 4402,
    CD == 41 & YEAR >= 2022 ~ 4403,
    CD == 42 & YEAR >= 2022 ~ 4404,
    CD == 43 & YEAR >= 2022 ~ 4405,
    CD == 44 & YEAR >= 2022 ~ 4406,
    CD == 45 & YEAR >= 2022 ~ 4407,
    CD == 46 & YEAR >= 2022 ~ 4408,
    CD == 47 & YEAR >= 2022 ~ 4409,
    CD == 48 & YEAR >= 2022 ~ 4410,
    CD == 49 & YEAR >= 2022 ~ 4411,
    CD == 50 & YEAR >= 2022 ~ 4412,
    CD == 51 & YEAR >= 2022 ~ 4413,
    CD == 52 & YEAR >= 2022 ~ 4414,
    CD == 53 & YEAR >= 2022 ~ 4501,
    CD == 54 & YEAR >= 2022 ~ 4502,
    CD == 55 & YEAR >= 2022 ~ 4503,
    
    TRUE ~ as.numeric(NA)
    
  )
  
  return(newVal)
  
}

###### Breakout Functions ############

## Make a function to return nested breakdowns (counts & props, and means/medians if possible)

## APPLY FILTERS TO A TEMPORARY DF!!!!! And the do fxn on that filtered df
## Except for median fxn, in that case by default excludes 0 values

## Counts
fxn_breakout_count <-function(df, vars_for_breakdown, dfall, only_collapsed_labels, suppress_lt1000) {
  
  ## WHAT DO? Returns a nested breakout of the counts of persons for specified variables
  ## df --------------------> dataframe of primary sample of interest (filtered for any specific group values)
  ## vars_for_breakdown ----> vector of variable names to get counts by
  ## only_collapsed_labels -> defaults true, so only output a preformatted col of labels and breakout estimates
  ## dfall -----------------> dataframe of the entire NYC population. If it's the same as df, ignore this.
  ## suppress_lt1000 -------> defaults to true. suppresses any counts less than 1000
  
  ####### IMPORTAT NOTES:
  ## --- Function ONLY appropriate for person variables (PWGTP)
  ## --- "vars_for_breakdown" is a vector of variable names for breakdown. 
  ## ---      Order variables by prefered heirarchy of groupings
  ## --- APPLY FILTERS TO A TEMPORARY DF!!!!! And the do fxn on that filtered df
  ## --- Function expects a YEAR variable in df and will always group_by year
  ## --- If a breakout variable is a geography, YEAR muct exist!!!
  ## --- all_labels will be wonky if break out by more than 5 vars (very unlikely breakout anyways)
  
  ############# CHECKS 
  
  ## Check vars_for_breakdown specified
  if(missing(vars_for_breakdown)){ stop("ERROR: vars_for_breakdown argument not specified") } 
  if(missing(only_collapsed_labels)){ only_collapsed_labels <- T } 
  if(missing(dfall)){dfall <- df} 
  if(missing(suppress_lt1000)){suppress_lt1000 <- T} 
  ## to avoid created duplicate dfall if not needed, comment one above and uncomment two lines in agg loop below)
  
  ## Check variables exist in df
  check_vars_exist <- vars_for_breakdown %in% names(df)
  if( sum(check_vars_exist) != length(vars_for_breakdown) ){
    
    missing_var <- vars_for_breakdown[which(!vars_for_breakdown %in% names(df))]
    stop(paste0("ERROR: variable '", missing_var, "' does not exist in specified dataframe!!!"))
  }
  
  ## If variable is a geography, YEAR must exist!!!
  if( sum(c("Boro", "boro", "CD") %in%  vars_for_breakdown) > 0 & !"YEAR" %in% names(df)){
    
    print(stop("ERROR: variable 'YEAR' is required in dataframe if breaking out by geography!!!"))
  }
  
  ## If YEAR not in df, assume single year
  if(!"YEAR" %in% names(df)){
    
    print("WARNING: 'YEAR' does not exist in dataframe. Assuming a single year.")
    df$YEAR <- "NOT SPECIFIED"
    
  }
  
  
  ########## BEGIN
  list_years <- unique(df$YEAR) %>% as.numeric() %>% sort()
  full_grouping <- paste0(vars_for_breakdown, collapse = " x ")
  
  ## for each demo, check if label variables already exist (if not, create)
  for(v in 1:length(vars_for_breakdown)){ #v=1
    
    temp_var <- vars_for_breakdown[[v]]
    temp_var_label <- paste0(temp_var, "_label")
    
    ## if it exists, make sure it's a character
    if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) == "character"){
      ## next loop  
      next
    }else if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) != "character"){
      ## make as character
      df[,temp_var_label] <- as.character(df[,temp_var_label])
      dfall[,temp_var_label] <- as.character(dfdfall[,temp_var_label])
      
    }else if(!temp_var_label %in% names(df)){
      ## create label
      df[,temp_var_label] <- fxn_label(df[,temp_var], variable_name = temp_var, YEAR = df[,"YEAR"])
      #df[,temp_var_label] <- fxn_label(df[,temp_var], variable_name = temp_var, YEAR = df$YEAR)
      dfall[,temp_var_label] <- fxn_label(dfall[,temp_var], variable_name = temp_var, YEAR = dfall[,"YEAR"])
    }
    
  }
  
  vars_for_breakdown_labels <- paste0(vars_for_breakdown, "_label")
  
  ## Pre-filterd DF means subgroups will sum up to correspond with higher levels
  tempdf <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    group_by(YEAR, !!!syms(vars_for_breakdown), !!!syms(vars_for_breakdown_labels)) %>% 
    summarise(ct = sum(PWGTP)) %>% 
    ungroup() %>% 
    mutate(level = length(vars_for_breakdown),
           grouping = full_grouping) 
  
  ## create aggregation tables  
  for(v in 1: (length(vars_for_breakdown) - 1)){  ## v=1
    
    if(v==0){next}
    
    var_group <- vars_for_breakdown[c(1:v)]
    var_group_label <- paste0(var_group, "_label")
    var_level <- paste0(var_group, collapse = " x ")
    grouped_across <- vars_for_breakdown[c( (v+1):length(vars_for_breakdown))]
    
    ## Aggregated breakout
    if(length(vars_for_breakdown) > 1){
      tempdf_lv <-
        tempdf %>% 
        group_by(YEAR, !!!syms(var_group), !!!syms(var_group_label)) %>% 
        summarise(ct = sum(ct, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(level = v,
               grouping = var_level) %>% 
        select(level, grouping, YEAR,  !!!syms(var_group), !!!syms(var_group_label), ct)
    }else{
      tempdf_lv <- NULL
    }
    
    
    ## across all groups (to have at top of table)
    if(v == 1){
      
      ## sample-wide (entire, does NOT call filtered df, so NAs not excluded)
      tempdf_l99 <- dfall %>% 
        group_by(YEAR) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup() %>% 
        mutate(level = -99,
               grouping = "All Persons within Specified Universe")
      
      ## pre-filtered subset (i.e.df)
      tempdf_l0 <- df %>% 
        filter(if_any(vars_for_breakdown, ~ !is.na(.x))) %>%
        group_by(YEAR) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup() %>% 
        mutate(level = 0,
               grouping = "All Persons in Selected Breakout Categories (NA Excluded)")
      
      tbl_agg <- bind_rows(tempdf_l99, tempdf_l0, tempdf_lv)
      
    }else{
      
      tbl_agg <- bind_rows(tbl_agg, tempdf_lv)
      
    }
    
    
    
  }
  
  ## bind all, fill in labels for agg levels, and arrange table
  tbl_all <- bind_rows(tbl_agg, tempdf) %>% 
    mutate(id = row_number()) %>%   ## create unique identifier in case duplicate counts
    mutate_at(vars(vars_for_breakdown), ~ifelse(is.na(.), -88, .)) %>% ## -88 used here to identify agg levels
    mutate_at(vars(vars_for_breakdown), ~as.character(.)) %>% ## transform numeric into label to pivot below
    pivot_longer(!c(id, YEAR, ct, level, grouping)) %>% 
    mutate(value = ifelse(is.na(value), gsub("_label", "", name), value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate_at(vars(vars_for_breakdown), ~as.numeric(.)) %>% 
    arrange(YEAR) %>% 
    select(-id) %>% 
    ## Suppress counts if need be
    mutate(ct = ifelse(suppress_lt1000 == T & ct <1000, NA, ct)) %>% 
    pivot_wider(names_from = YEAR, values_from = ct) %>% 
    arrange(!!!syms(vars_for_breakdown), level)
  
  ## Year-2-Year comparisons: if multiple years, compare to latest year (change in count)
  ## (note, this set-up done when originally focusing on 2019/21/22 comp)
  tc_list <- NULL
  pc_list <- NULL
  if(length(list_years) > 1){
    
    ## note, loops backward so final comp is always earliest V latest year
    for(c in (length(list_years) -1):1){ # c=2
      
      max_yr <- max(list_years)
      min_yr <- min(list_years)
      comp_yr <- list_years[[c]]
      tc_name <-  paste0(max_yr, "_m_", comp_yr)   ## name of total change variable
      pc_name <- paste0("pc_", max_yr, "v", comp_yr)
      
      tbl_all[, tc_name] <- tbl_all[,as.character(max_yr)] - tbl_all[,as.character(comp_yr)] 
      tbl_all[, pc_name] <- tbl_all[,tc_name] / tbl_all[,as.character(comp_yr)] 
      
      ## list of comp vars (used in reordering cols below)
      if(c == length(list_years) -1){
        tc_list <- tc_name
        pc_list <- pc_name
      }else{
        tc_list <- c(tc_list, tc_name)
        pc_list <- c(pc_list, pc_name)
      }
      
      ## identify min_max comp yr to go at end
      if(length(list_years) > 1 & comp_yr == min_yr){
        
        last_tc_col <- tc_name
        last_pc_list <- pc_name
        
      }
      
    }
    
    ## May want to re-arrange comps if list of years > 3...
    if(length(list_years) > 1){
      
      tc_list <- c(sort(tc_list[1: (length(tc_list)-1) ]), tc_list[length(tc_list)])
      pc_list <- c(sort(pc_list[1: (length(pc_list)-1) ]), pc_list[length(pc_list)])
    }
  }
  
  ## rearrange columns and create final table
  sel_p1 <- names(tbl_all)[which(!names(tbl_all) %in% c(pc_list, tc_list) & !names(tbl_all) %in% as.character(list_years))]
  sel_p2 <- as.character(list_years)
  space_to_insert <- "---"
  max_level <- max(unique(tbl_all$level))
  
  ## variable lavel calls (works for up to 5/6)
  for(v in 1:6){
    
    if(v <= max_level){
      if(v != max_level){
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }else{
        assign(paste0("test_var_max"), paste0(vars_for_breakdown[[v]], "_label"))
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }
    }else{
      assign(paste0("test_var_", v), paste0(vars_for_breakdown[[max_level]], "_label"))
    }
    
    
  }
  
  tbl_form <- 
    tbl_all %>% 
    # prep some level labels
    rowwise() %>% 
    mutate(level_variable = ifelse(level >=1, vars_for_breakdown[[level]], ""),
           mult_ind = case_when(level == 0 ~ 1,
                                level == 1 ~ 2,
                                level > 1 & level < max_level ~ (level*2) + 1,
                                level == max_level ~ ((level-1)*2) + 2,
                                TRUE ~ 0),
           indents = case_when(level == -99 ~ "",
                               level >= 0 ~ paste0(rep(space_to_insert, mult_ind), collapse = ""), 
                               TRUE ~ "")) %>% 
    ungroup() %>% 
    mutate(level_variable = case_when(level_variable == "EdAttain" ~ "Educational Attainment",
                                      level_variable %in% c("Boro", "boro") ~ "Borough",
                                      level_variable %in% c("age_6groups", "age_3groups") ~ "Age Group",
                                      level_variable %in% c("ESR", "esr_recode") ~ "Employment Status",
                                      level_variable == "IndShort" ~ "Industry",
                                      level_variable == "occ_recode" ~ "Occupation",
                                      TRUE ~ level_variable)) %>%   
    rowwise() %>% 
    mutate(all_labels = case_when(level == -99 ~ grouping,
                                  level == 0 ~ paste0(".", indents, " ", grouping),
                                  level == max_level ~ paste0(".", indents, " ", !!sym(test_var_max)),
                                  level == 1 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_1)),
                                  level == 2 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level == 3 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level == 4 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level == 5 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  TRUE ~ "Error, check all_labels within fxn")) %>% 
    select(!!!syms(sel_p1), all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list)))
  
  ## final adjustments, remove extra cols if needed)
  if(only_collapsed_labels == F){
    tbl_fin <- tbl_form %>% ungroup()
  }else{
    tbl_fin <- tbl_form %>% 
      select(all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list))) %>% ungroup()
    
  }
  
  
  return(tbl_fin)
  
}


## NOTE: proportions given only for LAST characteristics in vars_for_breakdown
fxn_breakout_prop <-function(df, vars_for_breakdown, dfall, only_collapsed_labels, suppress_lt1000) {
  
  ## NOTE: proportions given only for LAST characteristic in vars_for_breakdown
  
  ## WHAT DO? Returns a nested breakout of the counts/proportions of persons for specified variables
  ## df --------------------> dataframe of primary sample of interest (filtered for any specific group values)
  ## vars_for_breakdown ----> vector of variable names to get counts by
  ## only_collapsed_labels -> defaults true, so only output a preformatted col of labels and breakout estimates
  ## dfall -----------------> dataframe of the entire NYC population. If it's the same as df, ignore this.
  ## suppress_lt1000 -------> defaults to true. suppresses any counts less than 1000
  
  ####### IMPORTAT NOTES:
  ## --- Function ONLY appropriate for person variables (PWGTP)
  ## --- "vars_for_breakdown" is a vector of variable names for breakdown. 
  ## ---      Order variables by prefered heirarchy of groupings
  ## --- APPLY FILTERS TO A TEMPORARY DF!!!!! And the do fxn on that filtered df
  ## --- Function expects a YEAR variable in df and will always group_by year
  ## --- If a breakout variable is a geography, YEAR muct exist!!!
  ## --- all_labels will be wonky if break out by more than 5 vars (very unlikely breakout anyways)
  
  ############# CHECKS 
  
  ## Check vvars_for_breakdown specified
  if(missing(vars_for_breakdown)){ stop("ERROR: vars_for_breakdown argument not specified") } 
  if(missing(only_collapsed_labels)){ only_collapsed_labels <- T } 
  if(missing(dfall)){dfall <- df} 
  if(missing(suppress_lt1000)){suppress_lt1000 <- T} 
  ## to avoid created duplicate dfall if not needed, comment one above and uncomment two lines in agg loop below)
  
  ## Check variables exist in df
  check_vars_exist <- vars_for_breakdown %in% names(df)
  if( sum(check_vars_exist) != length(vars_for_breakdown) ){
    
    missing_var <- vars_for_breakdown[which(!vars_for_breakdown %in% names(df))]
    stop(paste0("ERROR: variable '", missing_var, "' does not exist in specified dataframe!!!"))
  }
  
  ## If variable is a geography, YEAR must exist!!!
  if( sum(c("Boro", "boro", "CD") %in%  vars_for_breakdown) > 0 & !"YEAR" %in% names(df)){
    
    stop(paste0("ERROR: variable 'YEAR' is required in dataframe if breaking out by geography!!!"))
  }
  
  ## If YEAR not in df, assume single year
  if(!"YEAR" %in% names(df)){
    
    print("WARNING: 'YEAR' does not exist in dataframe. Assuming a single year.")
    df$YEAR <- "NOT SPECIFIED"
    
  }
  
  
  ########## BEGIN
  list_years <- unique(df$YEAR) %>% as.numeric() %>% sort()
  full_grouping <- paste0(vars_for_breakdown, collapse = " x ")
  
  ## for each demo, check if label variables already exist (if not, create)
  for(v in 1:length(vars_for_breakdown)){ #v=3
    
    temp_var <- vars_for_breakdown[[v]]
    temp_var_label <- paste0(temp_var, "_label")
    
    ## if it exists, make sure it's a character
    if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) == "character"){
      ## next loop  
      next
    }else if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) != "character"){
      ## make as character
      df[,temp_var_label] <- as.character(df[,temp_var_label])
      dfall[,temp_var_label] <- as.character(dfdfall[,temp_var_label])
      
    }else if(!temp_var_label %in% names(df)){
      ## create label
      df[,temp_var_label] <- fxn_label(df[,temp_var], variable_name = temp_var, YEAR = df[,"YEAR"])
      dfall[,temp_var_label] <- fxn_label(dfall[,temp_var], variable_name = temp_var, YEAR = dfall[,"YEAR"])
    }
    
  }
  
  vars_for_breakdown_labels <- paste0(vars_for_breakdown, "_label")
  
  ## tbl of props for last var
  prop_var <- vars_for_breakdown[length(vars_for_breakdown)]
  prop_var_label <- paste0(prop_var, "_label")
  
  ## create aggregation tables  (grouping slightly diff than iin count fxn)
  ## Note, function bit different from df count, but filters structured so subgroups sum up to higher levels
  for(v in 1: (length(vars_for_breakdown) - 1)){  ## v=1
    
    if(v==0){next} ## so doesn't break if only one demo var
    
    ## Aggregated breakout
    if(length(vars_for_breakdown) > 1){
      
      var_group <- c(vars_for_breakdown[c(1:v)], prop_var)
      var_group_label <- paste0(var_group, "_label")
      var_level <- paste0(var_group, collapse = " x ")
      grouped_across <- vars_for_breakdown[c( (v+1):length(vars_for_breakdown))]
      
      ## across all in var_group
      tempdf_lv <-df %>% 
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR, !!!syms( c(var_group, var_group_label))) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
        mutate(tot = sum(ct)) %>% 
        ungroup() %>% 
        mutate(prop = ct/tot) %>% 
        mutate(level = v,
               grouping = var_level) %>% 
        select(level, grouping, YEAR,  !!!syms(var_group), !!!syms(var_group_label), ct, prop)
      
      ## if var_group > 2 variables, get prop across current demo only
      if(length(var_group) >= 3){
        
        halfLvl_var_group <- c(vars_for_breakdown[c(v)], prop_var)
        halfLvl_var_group_label <- paste0(halfLvl_var_group, "_label")
        halfLvl_var_level <- paste0(halfLvl_var_group, collapse = " x ")
        
        tempdf_lv_half <-df %>% 
          filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
          group_by(YEAR, !!!syms(c(halfLvl_var_group, halfLvl_var_group_label))) %>% 
          summarise(ct = sum(PWGTP)) %>% 
          ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
          mutate(tot = sum(ct)) %>% 
          ungroup() %>% 
          mutate(prop = ct/tot) %>% 
          mutate(across(.cols = halfLvl_var_group, ~ ifelse(is.na(.), -88, .))) %>% 
          mutate(level = 1 + (0.1*v),
                 grouping = halfLvl_var_level) %>% 
          select(level, grouping, YEAR,  !!!syms(c(halfLvl_var_group, halfLvl_var_group_label)), ct, prop)
        
      }else{tempdf_lv_half <- NULL}
      
    }else{
      
      var_group <- vars_for_breakdown
      var_level <- vars_for_breakdown
      grouped_across <- "not applicable"
      
      tempdf_lv <- NULL
      tempdf_lv_half <- NULL
      
    }
    
    
    ## across all groups (to have at top of table)
    if(v == 1){
      
      ## Entire sample as per dfall (does NOT call filtered df, BUT NA values of outcome_var are excluded)
      tempdf_l99 <- dfall %>% 
        ## NA must be excluded or fxn is wonky
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR, !!!syms(c(prop_var, prop_var_label))) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
        mutate(tot = sum(ct)) %>% 
        ungroup() %>% 
        mutate(prop = ct/tot) %>% 
        mutate(level = -99,
               grouping = "All Persons in Specified Universe (Pre-set Filters & NA Outcome Values Excluded)") %>% 
        select(-tot)
      
      ## pre-filtered subset (i.e.df)
      tempdf_l0 <- df %>% 
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR, !!!syms(c(prop_var, prop_var_label))) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
        mutate(tot = sum(ct)) %>% 
        ungroup() %>% 
        mutate(prop = ct/tot) %>% 
        mutate(across(.cols = prop_var, ~ ifelse(is.na(.), -88, .))) %>% 
        mutate(level = 0,
               grouping = "All Persons in Selected Breakout Categories (NAs Excluded)") %>% 
        select(-tot)
      
      tbl_agg <- bind_rows(tempdf_l99, tempdf_l0, tempdf_lv, tempdf_lv_half)
      
    }else{
      
      tbl_agg <- bind_rows(tbl_agg, tempdf_lv, tempdf_lv_half)
      
    }
    
    
    
  }
  
  ## bind all and arrage table
  tbl_ct_prop <- tbl_agg %>% 
    mutate(id = row_number()) %>%   ## create unique identifier in case duplicate counts
    mutate_at(vars(vars_for_breakdown), ~ifelse(is.na(.), -88, .)) %>% ## -88 used here to identify agg levels
    mutate_at(vars(vars_for_breakdown), ~as.character(.)) %>% ## transform numeric into label to pivot below
    pivot_longer(!c(id, YEAR, ct, prop, level, grouping)) %>% 
    mutate(value = ifelse(is.na(value), gsub("_label", "", name), value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate_at(vars(vars_for_breakdown), ~as.numeric(.)) %>% 
    arrange(YEAR) %>% 
    mutate(ct = ifelse(suppress_lt1000 == T & ct <1000, NA, ct),
           prop = ifelse(suppress_lt1000 == T & ct <1000, NA, prop))
  
  ## separate counts & props
  tbl_ct <- tbl_ct_prop %>% 
    select(-id, -prop) %>% 
    pivot_wider(names_from = YEAR, values_from = ct) %>% 
    arrange(level, !!!syms(vars_for_breakdown))
  
  tbl_prop <- tbl_ct_prop %>% 
    select(-id, -ct) %>% 
    pivot_wider(names_from = YEAR, values_from = prop) %>% 
    arrange(level, !!!syms(vars_for_breakdown))
  
  ## Year-2-Year comparisons: if multiple years, compare to latest year (change in count)
  ## NOTE: gets change in count (tbl_ct) and percent POINT change (tbl_prop)
  tc_list <- NULL
  ppc_list <- NULL
  
  if(length(list_years) > 1){
    
    ## note, loops backward so final comp is always earliest V latest year
    for(c in (length(list_years) -1):1){ # c=2
      
      max_yr <- max(list_years)
      min_yr <- min(list_years)
      comp_yr <- list_years[[c]]
      tc_name <-  paste0(max_yr, "_m_", comp_yr)   ## name of total change variable
      ppc_name <- paste0("ppc_", max_yr, "v", comp_yr)
      
      tbl_ct[, tc_name] <- tbl_ct[,as.character(max_yr)] - tbl_ct[,as.character(comp_yr)] 
      tbl_prop[, ppc_name] <- tbl_prop[,as.character(max_yr)] - tbl_prop[,as.character(comp_yr)] 
      
      ## list of comp vars (used in reordering cols below)
      if(c == length(list_years) -1){
        tc_list <- tc_name
        ppc_list <- ppc_name
      }else{
        tc_list <- c(tc_list, tc_name)
        ppc_list <- c(ppc_list, ppc_name)
      }
      
      ## identify min_max comp yr to go at end
      if(length(list_years) > 1 & comp_yr == min_yr){
        
        last_tc_col <- tc_name
        last_ppc_list <- ppc_name
        
      }
      
    }
    
    ## May want to re-arrange comps if list of years > 3...
    if(length(list_years) > 3){
      
      tc_list <- c(sort(tc_list[1: (length(tc_list)-1) ]), tc_list[length(tc_list)])
      ppc_list <- c(sort(ppc_list[1: (length(ppc_list)-1) ]), ppc_list[length(ppc_list)])
    }
  }
  
  ## rearrange columns and create final table (note: p1 names same btw ct & prop tbl)
  sel_p1 <- names(tbl_prop)[which(!names(tbl_prop) %in% c(ppc_list, tc_list) & 
                                    !names(tbl_prop) %in% as.character(list_years) & 
                                    names(tbl_prop) != prop_var_label)]
  sel_p2 <- as.character(list_years)
  space_to_insert <- "---"
  max_level <- max(unique(tbl_prop$level))
  min_prop_var_val <- min(unique(tbl_prop[,prop_var]))
  
  if(length(vars_for_breakdown) > 1){
    arrangement_index <- c(1:(max_level -1))
  }else{arrangement_index <- 1}
  
  ## variable lavel calls (works for up to 5/6)
  for(v in 1:6){
    
    if(v <= max_level){
      if(v != max_level){
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }else{
        assign(paste0("test_var_max"), paste0(vars_for_breakdown[[v]], "_label"))
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }
    }else{
      assign(paste0("test_var_", v), paste0(vars_for_breakdown[[max_level]], "_label"))
      assign(paste0("test_var_", "0"), paste0(vars_for_breakdown[[max_level]], "_label"))
    }
    
    
  }
  
  ## props with formatted labels
  tbl_form_prop <- 
    tbl_prop %>% 
    # prep some level labels
    ungroup() %>% 
    mutate(level_remainder = level%%1,
           level_index = ifelse(level_remainder == 0, level, level_remainder/.1),
           level_index = as.numeric(round(level_index, 0)), ## NO idea why i had to round this for index below to work
           level_reorder = level_index + level_remainder,
           agg_order = ifelse(level <= 0, 1, 2)) %>% 
    rowwise() %>% 
    #distinct(level, level_remainder, level_index)
    mutate(level_variable = ifelse(level_index >= 1, vars_for_breakdown[[level_index]], ""),
           mult_ind = case_when(level == 0 ~ 1,
                                level == 1 ~ 2,
                                level > 1 & level < 2 ~ 2,
                                level >= 2 & level < max_level ~ (level*2) + 1,
                                level == max_level ~ ((level-1)*2) + 2,
                                TRUE ~ 0),
           indents = case_when(level == -99 ~ "",
                               level >= 0 ~ paste0(rep(space_to_insert, mult_ind), collapse = ""),
                               TRUE ~ "")) %>% 
    #distinct(level, level_remainder, level_index, level_variable)
    ungroup() %>% 
    mutate(level_variable = case_when(level_variable == "EdAttain" ~ "Educational Attainment",
                                      level_variable %in% c("age_6groups", "age_3groups") ~ "Age Group",
                                      level_variable %in% c("ESR", "esr_recode") ~ "Employment Status",
                                      level_variable == "IndShort" ~ "Industry",
                                      level_variable == "occ_recode" ~ "Occupation",
                                      TRUE ~ level_variable)) %>%   
    rowwise() %>% 
    mutate(all_labels = case_when(level == -99 ~ grouping,
                                  level == 0 ~ paste0(indents, " ", grouping),
                                  level == max_level ~ paste0(indents, " ", !!sym(test_var_max)),
                                  level == 1 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_1)),
                                  
                                  level > 1 & level < 2 & level != max_level & level_index == 2 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level > 1 & level < 2 & level != max_level & level_index == 3 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level > 1 & level < 2 & level != max_level & level_index == 4 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level > 1 & level < 2 & level != max_level & level_index == 5 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  
                                  level == 2 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level == 3 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level == 4 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level == 5 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  TRUE ~ "Error, check all_labels within fxn"),
           ## only labels for first row of prop value
           all_labels = ifelse(!!sym(prop_var) == min_prop_var_val, paste0(".", all_labels), ""),
           prop_label = !!sym(paste0(prop_var, "_label"))

    ) %>% 
    arrange(!!!syms(vars_for_breakdown[arrangement_index]), agg_order) %>% 
    #distinct(level, level_reorder, all_labels, grouping) 
    select(!!!syms(sel_p1), all_labels, prop_label, !!!syms(sel_p2), !!!syms(c(ppc_list)))
  
  names(tbl_form_prop)[which(names(tbl_form_prop) %in% sel_p2)] <- paste0(names(tbl_form_prop)[which(names(tbl_form_prop) %in% sel_p2)], "_prop")
  
  ## couts to join
  tbl_ct_toJoin <- tbl_ct %>% 
    select(level, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels, sel_p2, tc_list)))
  
  names(tbl_ct_toJoin)[which(names(tbl_ct_toJoin) %in% sel_p2)] <- paste0(names(tbl_ct_toJoin)[which(names(tbl_ct_toJoin) %in% sel_p2)], "_count")
  
  ## all together
  tbl_names_toJoin <- names(tbl_form_prop)[names(tbl_form_prop) %in% names(tbl_ct_toJoin)]
  tbl_form <-
    tbl_form_prop %>%   # names(tbl_form_prop)
    left_join(tbl_ct_toJoin, by = tbl_names_toJoin) 
  
  ## rearrange if only 1 bo
  if(length(vars_for_breakdown) == 1){
    tbl_form <- tbl_form %>% arrange(level)
  }
  
  last_var <- names(tbl_form)[length(names(tbl_form))]
  
  
  
  ## final adjustments, remove extra cols if needed)
  if(only_collapsed_labels == F){
    tbl_fin <- tbl_form %>% ungroup()
  }else{
    tbl_fin <- tbl_form %>% 
      select(grouping, all_labels,  prop_label:!!sym(last_var)) %>% ungroup()
    
  }
  
  
  return(tbl_fin)
  
}


## NOTE, doesn't include dfall or supression argument (run count function and use that to supress values)
fxn_breakout_median <-function(df, outcome_var, vars_for_breakdown, exclude_0, only_collapsed_labels) {
  
  ## WHAT DO? Returns a nested breakout of the median of a specified variable
  ## df --------------------> dataframe of primary sample of interest (filtered for any specific group values)
  ## outcome_var -----------> variable you want median of (string)
  ## vars_for_breakdown ----> vector of variable names to get counts by
  ## exclude_0 -------------> defaults TRUE (ecludes outcome values lte 0)
  ## only_collapsed_labels -> defaults true, so only output a preformatted col of labels and breakout estimates
  ## suppress_lt1000 -------> defaults to true. suppresses outcome if underlying counts less than 1000
  
  ####### IMPORTAT NOTES:
  ## --- Function ONLY appropriate for person variables (PWGTP), (IncHH_adj in beta, validity depends on breakout vars (need to be at HH level))
  ## --- "vars_for_breakdown" is a vector of variable names for breakdown. 
  ## ---      Order variables by prefered heirarchy of groupings
  ## --- APPLY FILTERS TO A TEMPORARY DF!!!!! And the do fxn on that filtered df
  ## --- Function expects a YEAR variable in df and will always group_by year
  ## --- If a breakout variable is a geography, YEAR muct exist!!!
  ## --- all_labels will be wonky if break out by more than 5 vars (very unlikely breakout anyways)
  
  ############# CHECKS 
  
  ## Check vvars_for_breakdown specified
  if(missing(vars_for_breakdown)){ stop("ERROR: vars_for_breakdown argument not specified") } 
  if(missing(outcome_var)){ stop("ERROR: outcome variable not specified")}
  if(missing(only_collapsed_labels)){ only_collapsed_labels <- T } 
  if(missing(exclude_0)){exclude_0 <- T} 

  
  ## Check variables exist in df
  check_vars_exist <- vars_for_breakdown %in% names(df)
  if( sum(check_vars_exist) != length(vars_for_breakdown) ){
    
    missing_var <- vars_for_breakdown[which(!vars_for_breakdown %in% names(df))]
    stop(paste0("ERROR: variable '", missing_var, "' does not exist in specified dataframe!!!"))
  }
  
  ## If variable is a geography, YEAR must exist!!!
  if( sum(c("Boro", "boro", "CD") %in%  vars_for_breakdown) > 0 & !"YEAR" %in% names(df)){
    
    stop(paste0("ERROR: variable 'YEAR' is required in dataframe if breaking out by geography!!!"))
  }
  
  ## If YEAR not in df, assume single year
  if(!"YEAR" %in% names(df)){
    
    print("WARNING: 'YEAR' does not exist in dataframe. Assuming a single year.")
    df$YEAR <- "NOT SPECIFIED"
    
  }
  
  ############################# Set outcome variable & weight
  if(outcome_var == "IncHH_adj"){
    
    df$outcome <- df$IncHH_adj
    df$weight <- df$WGTP
    
    ## identify & exclude non-ref persons
    df$rel_allYrs <- NA
    
    if(sum(c(2005:2009) %in% unique(df$YEAR)) > 0){
      df[df$YEAR %in% c(2005:2009), "rel_allYrs"] <- df[df$YEAR %in% c(2005:2009), "REL"]
    }
    
    if(sum(c(2010:2018) %in% unique(df$YEAR)) > 0){
      df[df$YEAR %in% c(2010:2018), "rel_allYrs"] <- df[df$YEAR %in% c(2010:2018), "RELP"]
    }
    
    if(sum(unique(df$YEAR) >= 2019) > 0){
      df[df$YEAR >= 2019, "rel_allYrs"] <- df[df$YEAR >= 2019, "RELSHIPP"]
    }
    
    df$ref_person <- ifelse(df$YEAR < 2019 & df$rel_allYrs == 0, 1,
                            ifelse(df$YEAR >= 2019 & df$rel_allYrs == 20, 1, 0))
    
    df <- df %>% filter(ref_person == 1)
    
  }else{
    
    df[,"outcome"] <- df[,outcome_var]
    df$weight <- df$PWGTP
    
  }
  
  ############ If need be, exclude values lte 0 (and create dfall as full sample ref)
  if(exclude_0 == T){ dfall <- df; df <- df %>% filter(outcome >=0)}
  
  ########## BEGIN
  list_years <- unique(df$YEAR) %>% as.numeric() %>% sort()
  full_grouping <- paste0(vars_for_breakdown, collapse = " x ")
  
  ## for each demo, check if label variables already exist (if not, create)
  for(v in 1:length(vars_for_breakdown)){ #v=1
    
    temp_var <- vars_for_breakdown[[v]]
    temp_var_label <- paste0(temp_var, "_label")
    
    ## if it exists, make sure it's a character
    if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) == "character"){
      ## next loop
      next
    }else if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) != "character"){
      ## make as character
      df[,temp_var_label] <- as.character(df[,temp_var_label])
    }else if(!temp_var_label %in% names(df)){
      ## create label
      df[,temp_var_label] <- fxn_label(df[,temp_var], variable_name = temp_var, YEAR = df[,"YEAR"])
    }
    
  }
  
  vars_for_breakdown_labels <- paste0(vars_for_breakdown, "_label")
  
  ## DF excluding only NA values
  ct_tempdf_all <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    group_by(YEAR, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels))) %>% 
    summarise(ct = sum(weight)) %>% 
    ungroup() %>% 
     mutate(level = length(vars_for_breakdown),
           grouping = full_grouping)
  
  ct_tempdf <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    filter(outcome > 0) %>% 
    group_by(YEAR, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels))) %>% 
    summarise(ct = sum(weight)) %>% 
    ungroup() %>% 
    mutate(level = length(vars_for_breakdown),
           grouping = full_grouping)
  
  dffilt <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    filter(outcome > 0)
  
  tempdf <- dffilt %>% 
    group_by(YEAR, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels))) %>% 
    summarise(median = weighted.median(outcome, weight)) %>% 
    ungroup() %>% 
    mutate(level = length(vars_for_breakdown),
           grouping = full_grouping)
  
  ## create aggregation tables  
  for(v in 1: (length(vars_for_breakdown) - 1)){  ## v=1
    
    if(v==0){next}
    
    var_group <- vars_for_breakdown[c(1:v)]
    var_group_label <- paste0(var_group, "_label")
    var_level <- paste0(var_group, collapse = " x ")
    grouped_across <- vars_for_breakdown[c( (v+1):length(vars_for_breakdown))]
    
    ## Aggregated breakout
    if(length(vars_for_breakdown) > 1){
      tempdf_lv <-
        dffilt %>% 
        group_by(YEAR, !!!syms(c(var_group, var_group_label))) %>% 
        summarise(median = weighted.median(outcome, weight)) %>%  
        ungroup() %>% 
        mutate(level = v,
               grouping = var_level) %>% 
        select(level, grouping, YEAR,  !!!syms(c(var_group, var_group_label)), median)
    }else{
      tempdf_lv <- NULL
    }
    
    
    ## across all groups (to have at top of table)
    if(v == 1){
      
      ## Sample as per dfall (NAs excluded but $0 included)
      tempdf_l99 <- df %>% 
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR) %>% 
        summarise(median = weighted.median(outcome, weight, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(level = -99,
               grouping = "All Persons in Specified Universe (Pre-set filters, excludes NA, includes 0)")
      
      ## pre-filtered subset (i.e.df)
      tempdf_l0 <- dffilt %>% 
        group_by(YEAR) %>% 
        summarise(median = weighted.median(outcome, weight, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(level = 0,
               grouping = "All Persons in Selected Breakout Categories (Pre-set filters, excludes NA and 0)")
      
      tbl_agg <- bind_rows(tempdf_l99, tempdf_l0, tempdf_lv)
      
    }else{
      
      tbl_agg <- bind_rows(tbl_agg, tempdf_lv)
      
    }
    
    
    
  }
  
  ## bind all and arrage table
  tbl_all <- bind_rows(tbl_agg, tempdf) %>% 
    mutate(id = row_number()) %>%   ## create unique identifier in case duplicate counts
    mutate_at(vars(vars_for_breakdown), ~ifelse(is.na(.), -88, .)) %>% ## -88 used here to identify agg levels
    mutate_at(vars(vars_for_breakdown), ~as.character(.)) %>% ## transform numeric into label to pivot below
    pivot_longer(!c(id, YEAR, median, level, grouping)) %>% 
    mutate(value = ifelse(is.na(value), gsub("_label", "", name), value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate_at(vars(vars_for_breakdown), ~as.numeric(.)) %>% 
    arrange(YEAR) %>% 
    select(-id) %>% 
    pivot_wider(names_from = YEAR, values_from = median) %>% 
    arrange(!!!syms(vars_for_breakdown), level)
  
  
  ## Year-2-Year comparisons: if multiple years, compare to latest year (change in med)
  ## (note, this set-up done when originally focusing on 2019/21/22 comp)
  tc_list <- NULL
  pc_list <- NULL
  if(length(list_years) > 1){
    
    ## note, loops backward so final comp is always earliest V latest year
    for(c in (length(list_years) -1):1){ # c=2
      
      max_yr <- max(list_years)
      min_yr <- min(list_years)
      comp_yr <- list_years[[c]]
      tc_name <-  paste0(max_yr, "_m_", comp_yr)   ## name of total change variable
      pc_name <- paste0("pc_", max_yr, "v", comp_yr)
      
      tbl_all[, tc_name] <- tbl_all[,as.character(max_yr)] - tbl_all[,as.character(comp_yr)] 
      tbl_all[, pc_name] <- tbl_all[,tc_name] / tbl_all[,as.character(comp_yr)] 
      
      ## list of comp vars (used in reordering cols below)
      if(c == length(list_years) -1){
        tc_list <- tc_name
        pc_list <- pc_name
      }else{
        tc_list <- c(tc_list, tc_name)
        pc_list <- c(pc_list, pc_name)
      }
      
      ## identify min_max comp yr to go at end
      if(length(list_years) > 1 & comp_yr == min_yr){
        
        last_tc_col <- tc_name
        last_pc_list <- pc_name
        
      }
      
    }
    
    ## May want to re-arrange comps if list of years > 3...
    if(length(list_years) > 1){
      
      tc_list <- c(sort(tc_list[1: (length(tc_list)-1) ]), tc_list[length(tc_list)])
      pc_list <- c(sort(pc_list[1: (length(pc_list)-1) ]), pc_list[length(pc_list)])
    }
  }
  
  ## rearrange columns and create final table
  sel_p1 <- names(tbl_all)[which(!names(tbl_all) %in% c(pc_list, tc_list) & !names(tbl_all) %in% as.character(list_years))]
  sel_p2 <- as.character(list_years)
  space_to_insert <- "---"
  max_level <- max(unique(tbl_all$level))
  
  ## variable lavel calls (works for up to 5/6)
  for(v in 1:6){
    
    if(v <= max_level){
      if(v != max_level){
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }else{
        assign(paste0("test_var_max"), paste0(vars_for_breakdown[[v]], "_label"))
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }
    }else{
      assign(paste0("test_var_", v), paste0(vars_for_breakdown[[max_level]], "_label"))
    }
    
    
  }
  
  tbl_form <- 
    tbl_all %>% 
    # prep some level labels
    rowwise() %>% 
    mutate(level_variable = ifelse(level >=1, vars_for_breakdown[[level]], ""),
           mult_ind = case_when(level == 0 ~ 1,
                                level == 1 ~ 2,
                                level > 1 & level < max_level ~ (level*2) + 1,
                                level == max_level ~ ((level-1)*2) + 2,
                                TRUE ~ 0),
           indents = case_when(level == -99 ~ "",
                               level >= 0 ~ paste0(rep(space_to_insert, mult_ind), collapse = ""), 
                               TRUE ~ "")) %>% 
    ungroup() %>% 
    mutate(level_variable = case_when(level_variable == "EdAttain" ~ "Educational Attainment",
                                      level_variable %in% c("age_6groups", "age_3groups") ~ "Age Group",
                                      level_variable %in% c("ESR", "esr_recode") ~ "Employment Status",
                                      level_variable == "IndShort" ~ "Industry",
                                      level_variable == "occ_recode" ~ "Occupation",
                                      TRUE ~ level_variable)) %>%   
    rowwise() %>% 
    mutate(all_labels = case_when(level == -99 ~ grouping,
                                  level == 0 ~ paste0(".", indents, " ", grouping),
                                  level == max_level ~ paste0(".", indents, " ", !!sym(test_var_max)),
                                  level == 1 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_1)),
                                  level == 2 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level == 3 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level == 4 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level == 5 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  TRUE ~ "Error, check all_labels within fxn")) %>% 
    select(!!!syms(sel_p1), all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list)))
  
  ## final adjustments, remove extra cols if needed)  only_collapsed_labels<-T
  if(only_collapsed_labels == F){
    tbl_fin <- tbl_form %>% ungroup()
  }else{
    tbl_fin <- tbl_form %>% 
      select(all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list))) %>% ungroup()
    
  }
  
  
  return(tbl_fin)
  
}

#

###### MODIFIED FOR CTs: Breakout Functions ############

## Like breakout functions above, except yr2yr comparison is sequential

## Counts
CT_fxn_breakout_count <-function(df, vars_for_breakdown, dfall, only_collapsed_labels, suppress_lt1000) {
  
  ## WHAT DO? Returns a nested breakout of the counts of persons for specified variables
  ## df --------------------> dataframe of primary sample of interest (filtered for any specific group values)
  ## vars_for_breakdown ----> vector of variable names to get counts by
  ## only_collapsed_labels -> defaults true, so only output a preformatted col of labels and breakout estimates
  ## dfall -----------------> dataframe of the entire NYC population. If it's the same as df, ignore this.
  ## suppress_lt1000 -------> defaults to true. suppresses any counts less than 1000
  
  ####### IMPORTAT NOTES:
  ## --- Function ONLY appropriate for person variables (PWGTP)
  ## --- "vars_for_breakdown" is a vector of variable names for breakdown. 
  ## ---      Order variables by prefered heirarchy of groupings
  ## --- APPLY FILTERS TO A TEMPORARY DF!!!!! And the do fxn on that filtered df
  ## --- Function expects a YEAR variable in df and will always group_by year
  ## --- If a breakout variable is a geography, YEAR muct exist!!!
  ## --- all_labels will be wonky if break out by more than 5 vars (very unlikely breakout anyways)
  
  ############# CHECKS 
  
  ## Check vars_for_breakdown specified
  if(missing(vars_for_breakdown)){ stop("ERROR: vars_for_breakdown argument not specified") } 
  if(missing(only_collapsed_labels)){ only_collapsed_labels <- T } 
  if(missing(dfall)){dfall <- df} 
  if(missing(suppress_lt1000)){suppress_lt1000 <- T} 
  ## to avoid created duplicate dfall if not needed, comment one above and uncomment two lines in agg loop below)
  
  ## Check variables exist in df
  check_vars_exist <- vars_for_breakdown %in% names(df)
  if( sum(check_vars_exist) != length(vars_for_breakdown) ){
    
    missing_var <- vars_for_breakdown[which(!vars_for_breakdown %in% names(df))]
    stop(paste0("ERROR: variable '", missing_var, "' does not exist in specified dataframe!!!"))
  }
  
  ## If variable is a geography, YEAR must exist!!!
  if( sum(c("Boro", "boro", "CD") %in%  vars_for_breakdown) > 0 & !"YEAR" %in% names(df)){
    
    print(stop("ERROR: variable 'YEAR' is required in dataframe if breaking out by geography!!!"))
  }
  
  ## If YEAR not in df, assume single year
  if(!"YEAR" %in% names(df)){
    
    print("WARNING: 'YEAR' does not exist in dataframe. Assuming a single year.")
    df$YEAR <- "NOT SPECIFIED"
    
  }
  
  
  ########## BEGIN
  list_years <- unique(df$YEAR) %>% as.numeric() %>% sort()
  full_grouping <- paste0(vars_for_breakdown, collapse = " x ")
  
  ## for each demo, check if label variables already exist (if not, create)
  for(v in 1:length(vars_for_breakdown)){ #v=3
    
    temp_var <- vars_for_breakdown[[v]]
    temp_var_label <- paste0(temp_var, "_label")
    
    ## if it exists, make sure it's a character
    if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) == "character"){
      ## next loop  
      next
    }else if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) != "character"){
      ## make as character
      df[,temp_var_label] <- as.character(df[,temp_var_label])
      dfall[,temp_var_label] <- as.character(dfdfall[,temp_var_label])
      
    }else if(!temp_var_label %in% names(df)){
      ## create label
      df[,temp_var_label] <- fxn_label(df[,temp_var], variable_name = temp_var, YEAR = df[,"YEAR"])
      dfall[,temp_var_label] <- fxn_label(dfall[,temp_var], variable_name = temp_var, YEAR = dfall[,"YEAR"])
    }
    
  }
  
  vars_for_breakdown_labels <- paste0(vars_for_breakdown, "_label")
  
  ## Pre-filterd DF means subgroups will sum up to correspond with higher levels
  tempdf <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    group_by(YEAR, !!!syms(vars_for_breakdown), !!!syms(vars_for_breakdown_labels)) %>% 
    summarise(ct = sum(PWGTP)) %>% 
    ungroup() %>% 
    mutate(level = length(vars_for_breakdown),
           grouping = full_grouping) 
  
  ## create aggregation tables  
  for(v in 1: (length(vars_for_breakdown) - 1)){  ## v=1
    
    if(v==0){next}
    
    var_group <- vars_for_breakdown[c(1:v)]
    var_group_label <- paste0(var_group, "_label")
    var_level <- paste0(var_group, collapse = " x ")
    grouped_across <- vars_for_breakdown[c( (v+1):length(vars_for_breakdown))]
    
    ## Aggregated breakout
    if(length(vars_for_breakdown) > 1){
      tempdf_lv <-
        tempdf %>% 
        group_by(YEAR, !!!syms(var_group), !!!syms(var_group_label)) %>% 
        summarise(ct = sum(ct, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(level = v,
               grouping = var_level) %>% 
        select(level, grouping, YEAR,  !!!syms(var_group), !!!syms(var_group_label), ct)
    }else{
      tempdf_lv <- NULL
    }
    
    
    ## across all groups (to have at top of table)
    if(v == 1){
      
      ## sample-wide (entire, does NOT call filtered df, so NAs not excluded)
      tempdf_l99 <- dfall %>% 
        group_by(YEAR) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup() %>% 
        mutate(level = -99,
               grouping = "All Persons within Specified Universe")
      
      ## pre-filtered subset (i.e.df)
      tempdf_l0 <- df %>% 
        filter(if_any(vars_for_breakdown, ~ !is.na(.x))) %>%
        group_by(YEAR) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup() %>% 
        mutate(level = 0,
               grouping = "All Persons in Selected Breakout Categories (NA Excluded)")
      
      tbl_agg <- bind_rows(tempdf_l99, tempdf_l0, tempdf_lv)
      
    }else{
      
      tbl_agg <- bind_rows(tbl_agg, tempdf_lv)
      
    }
    
    
    
  }
  
  ## bind all, fill in labels for agg levels, and arrange table
  tbl_all <- bind_rows(tbl_agg, tempdf) %>% 
    mutate(id = row_number()) %>%   ## create unique identifier in case duplicate counts
    mutate_at(vars(vars_for_breakdown), ~ifelse(is.na(.), -88, .)) %>% ## -88 used here to identify agg levels
    mutate_at(vars(vars_for_breakdown), ~as.character(.)) %>% ## transform numeric into label to pivot below
    pivot_longer(!c(id, YEAR, ct, level, grouping)) %>% 
    mutate(value = ifelse(is.na(value), gsub("_label", "", name), value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate_at(vars(vars_for_breakdown), ~as.numeric(.)) %>% 
    arrange(YEAR) %>% 
    select(-id) %>% 
    ## Suppress counts if need be
    mutate(ct = ifelse(suppress_lt1000 == T & ct <1000, NA, ct)) %>% 
    pivot_wider(names_from = YEAR, values_from = ct) %>% 
    arrange(!!!syms(vars_for_breakdown), level)
  
  ## Year-2-Year comparisons: sequential, but add 2019vlatest yr at end
  tc_list <- NULL
  pc_list <- NULL
  if(length(list_years) > 1){
    
    max_yr <- max(list_years)
    min_yr <- min(list_years)
    
    for(c in 2:length(list_years)){ # c=11
      
      curr_yr <- list_years[[c]]
      prior_yr <- list_years[[c-1]]
      tc_name <-  paste0(curr_yr, "_m_", prior_yr)   ## name of total change variable (current yr minus prior year)
      pc_name <- paste0("pc_", curr_yr, "v", prior_yr)
      
      tbl_all[, tc_name] <- tbl_all[,as.character(curr_yr)] - tbl_all[,as.character(prior_yr)] 
      tbl_all[, pc_name] <- tbl_all[,tc_name] / tbl_all[,as.character(prior_yr)] 
      
      ## list of comp vars (used in reordering cols below)
      if(c == 2){
        tc_list <- tc_name
        pc_list <- pc_name
      }else{
        tc_list <- c(tc_list, tc_name)
        pc_list <- c(pc_list, pc_name)
      }
      
      
      ## if last loop, check and add last V 2019 if (2019 in yr list and not the last comp)
      if(2019 %in% list_years & !grepl("2019",tc_name) & max_yr > 2019 & curr_yr == max_yr){
        
        comp_yr <- 2019
        tc19_name <-  paste0(curr_yr, "_m_", comp_yr)   ## name of total change variable (current yr minus prior year)
        pc19_name <- paste0("pc_", curr_yr, "v", comp_yr)
        
        tbl_all[, tc19_name] <- tbl_all[,as.character(curr_yr)] - tbl_all[,as.character(comp_yr)] 
        tbl_all[, pc19_name] <- tbl_all[,tc19_name] / tbl_all[,as.character(comp_yr)] 
        
        tc_list <- c(tc_list, tc19_name)
        pc_list <- c(pc_list, pc19_name)
      }

    }
    
  }
  
  ## rearrange columns and create final table
  sel_p1 <- names(tbl_all)[which(!names(tbl_all) %in% c(pc_list, tc_list) & !names(tbl_all) %in% as.character(list_years))]
  sel_p2 <- as.character(list_years)
  space_to_insert <- "---"
  max_level <- max(unique(tbl_all$level))
  
  ## variable lavel calls (works for up to 5/6)
  for(v in 1:6){
    
    if(v <= max_level){
      if(v != max_level){
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }else{
        assign(paste0("test_var_max"), paste0(vars_for_breakdown[[v]], "_label"))
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }
    }else{
      assign(paste0("test_var_", v), paste0(vars_for_breakdown[[max_level]], "_label"))
    }
    
    
  }
  
  tbl_form <- 
    tbl_all %>% 
    # prep some level labels
    rowwise() %>% 
    mutate(level_variable = ifelse(level >=1, vars_for_breakdown[[level]], ""),
           mult_ind = case_when(level == 0 ~ 1,
                                level == 1 ~ 2,
                                level > 1 & level < max_level ~ (level*2) + 1,
                                level == max_level ~ ((level-1)*2) + 2,
                                TRUE ~ 0),
           indents = case_when(level == -99 ~ "",
                               level >= 0 ~ paste0(rep(space_to_insert, mult_ind), collapse = ""), 
                               TRUE ~ "")) %>% 
    ungroup() %>% 
    mutate(level_variable = case_when(level_variable == "EdAttain" ~ "Educational Attainment",
                                      level_variable %in% c("Boro", "boro") ~ "Borough",
                                      level_variable %in% c("age_6groups", "age_3groups") ~ "Age Group",
                                      level_variable %in% c("ESR", "esr_recode") ~ "Employment Status",
                                      level_variable == "IndShort" ~ "Industry",
                                      level_variable == "occ_recode" ~ "Occupation",
                                      TRUE ~ level_variable)) %>%   
    rowwise() %>% 
    mutate(all_labels = case_when(level == -99 ~ grouping,
                                  level == 0 ~ paste0(".", indents, " ", grouping),
                                  level == max_level ~ paste0(".", indents, " ", !!sym(test_var_max)),
                                  level == 1 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_1)),
                                  level == 2 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level == 3 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level == 4 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level == 5 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  TRUE ~ "Error, check all_labels within fxn")) %>% 
    select(!!!syms(sel_p1), all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list)))
  
  ## final adjustments, remove extra cols if needed)
  if(only_collapsed_labels == F){
    tbl_fin <- tbl_form %>% ungroup()
  }else{
    tbl_fin <- tbl_form %>% 
      select(all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list))) %>% ungroup()
    
  }
  
  
  return(tbl_fin)
  
}


## NOTE: proportions given only for LAST characteristics in vars_for_breakdown
CT_fxn_breakout_prop <-function(df, vars_for_breakdown, dfall, only_collapsed_labels, suppress_lt1000) {
  
  ## NOTE: proportions given only for LAST characteristic in vars_for_breakdown
  
  ## WHAT DO? Returns a nested breakout of the counts/proportions of persons for specified variables
  ## df --------------------> dataframe of primary sample of interest (filtered for any specific group values)
  ## vars_for_breakdown ----> vector of variable names to get counts by
  ## only_collapsed_labels -> defaults true, so only output a preformatted col of labels and breakout estimates
  ## dfall -----------------> dataframe of the entire NYC population. If it's the same as df, ignore this.
  ## suppress_lt1000 -------> defaults to true. suppresses any counts less than 1000
  
  ####### IMPORTAT NOTES:
  ## --- Function ONLY appropriate for person variables (PWGTP)
  ## --- "vars_for_breakdown" is a vector of variable names for breakdown. 
  ## ---      Order variables by prefered heirarchy of groupings
  ## --- APPLY FILTERS TO A TEMPORARY DF!!!!! And the do fxn on that filtered df
  ## --- Function expects a YEAR variable in df and will always group_by year
  ## --- If a breakout variable is a geography, YEAR muct exist!!!
  ## --- all_labels will be wonky if break out by more than 5 vars (very unlikely breakout anyways)
  
  ############# CHECKS 
  
  ## Check vvars_for_breakdown specified
  if(missing(vars_for_breakdown)){ stop("ERROR: vars_for_breakdown argument not specified") } 
  if(missing(only_collapsed_labels)){ only_collapsed_labels <- T } 
  if(missing(dfall)){dfall <- df} 
  if(missing(suppress_lt1000)){suppress_lt1000 <- T} 
  ## to avoid created duplicate dfall if not needed, comment one above and uncomment two lines in agg loop below)
  
  ## Check variables exist in df
  check_vars_exist <- vars_for_breakdown %in% names(df)
  if( sum(check_vars_exist) != length(vars_for_breakdown) ){
    
    missing_var <- vars_for_breakdown[which(!vars_for_breakdown %in% names(df))]
    stop(paste0("ERROR: variable '", missing_var, "' does not exist in specified dataframe!!!"))
  }
  
  ## If variable is a geography, YEAR must exist!!!
  if( sum(c("Boro", "boro", "CD") %in%  vars_for_breakdown) > 0 & !"YEAR" %in% names(df)){
    
    stop(paste0("ERROR: variable 'YEAR' is required in dataframe if breaking out by geography!!!"))
  }
  
  ## If YEAR not in df, assume single year
  if(!"YEAR" %in% names(df)){
    
    print("WARNING: 'YEAR' does not exist in dataframe. Assuming a single year.")
    df$YEAR <- "NOT SPECIFIED"
    
  }
  
  
  ########## BEGIN
  list_years <- unique(df$YEAR) %>% as.numeric() %>% sort()
  full_grouping <- paste0(vars_for_breakdown, collapse = " x ")
  
  ## for each demo, check if label variables already exist (if not, create)
  for(v in 1:length(vars_for_breakdown)){ #v=3
    
    temp_var <- vars_for_breakdown[[v]]
    temp_var_label <- paste0(temp_var, "_label")
    
    ## if it exists, make sure it's a character
    if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) == "character"){
      ## next loop  
      next
    }else if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) != "character"){
      ## make as character
      df[,temp_var_label] <- as.character(df[,temp_var_label])
      dfall[,temp_var_label] <- as.character(dfdfall[,temp_var_label])
      
    }else if(!temp_var_label %in% names(df)){
      ## create label
      df[,temp_var_label] <- fxn_label(df[,temp_var], variable_name = temp_var, YEAR = df[,"YEAR"])
      dfall[,temp_var_label] <- fxn_label(dfall[,temp_var], variable_name = temp_var, YEAR = dfall[,"YEAR"])
    }
    
  }
  
  vars_for_breakdown_labels <- paste0(vars_for_breakdown, "_label")
  
  ## tbl of props for last var
  prop_var <- vars_for_breakdown[length(vars_for_breakdown)]
  prop_var_label <- paste0(prop_var, "_label")
  
  ## create aggregation tables  (grouping slightly diff than iin count fxn)
  ## Note, function bit different from df count, but filters structured so subgroups sum up to higher levels
  for(v in 1: (length(vars_for_breakdown) - 1)){  ## v=1
    
    if(v==0){next} ## so doesn't break if only one demo var
    
    ## Aggregated breakout
    if(length(vars_for_breakdown) > 1){
      
      var_group <- c(vars_for_breakdown[c(1:v)], prop_var)
      var_group_label <- paste0(var_group, "_label")
      var_level <- paste0(var_group, collapse = " x ")
      grouped_across <- vars_for_breakdown[c( (v+1):length(vars_for_breakdown))]
      
      ## across all in var_group
      tempdf_lv <-df %>% 
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR, !!!syms( c(var_group, var_group_label))) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
        mutate(tot = sum(ct)) %>% 
        ungroup() %>% 
        mutate(prop = ct/tot) %>% 
        mutate(level = v,
               grouping = var_level) %>% 
        select(level, grouping, YEAR,  !!!syms(var_group), !!!syms(var_group_label), ct, prop)
      
      ## if var_group > 2 variables, get prop across current demo only
      if(length(var_group) >= 3){
        
        halfLvl_var_group <- c(vars_for_breakdown[c(v)], prop_var)
        halfLvl_var_group_label <- paste0(halfLvl_var_group, "_label")
        halfLvl_var_level <- paste0(halfLvl_var_group, collapse = " x ")
        
        tempdf_lv_half <-df %>% 
          filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
          group_by(YEAR, !!!syms(c(halfLvl_var_group, halfLvl_var_group_label))) %>% 
          summarise(ct = sum(PWGTP)) %>% 
          ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
          mutate(tot = sum(ct)) %>% 
          ungroup() %>% 
          mutate(prop = ct/tot) %>% 
          mutate(across(.cols = halfLvl_var_group, ~ ifelse(is.na(.), -88, .))) %>% 
          mutate(level = 1 + (0.1*v),
                 grouping = halfLvl_var_level) %>% 
          select(level, grouping, YEAR,  !!!syms(c(halfLvl_var_group, halfLvl_var_group_label)), ct, prop)
        
      }else{tempdf_lv_half <- NULL}
      
    }else{
      
      var_group <- vars_for_breakdown
      var_level <- vars_for_breakdown
      grouped_across <- "not applicable"
      
      tempdf_lv <- NULL
      tempdf_lv_half <- NULL
      
    }
    
    
    ## across all groups (to have at top of table)
    if(v == 1){
      
      ## Entire sample as per dfall (does NOT call filtered df, BUT NA values of outcome_var are excluded)
      tempdf_l99 <- dfall %>% 
        ## NA must be excluded or fxn is wonky
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR, !!!syms(c(prop_var, prop_var_label))) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
        mutate(tot = sum(ct)) %>% 
        ungroup() %>% 
        mutate(prop = ct/tot) %>% 
        mutate(level = -99,
               grouping = "All Persons in Specified Universe (Pre-set Filters & NA Outcome Values Excluded)") %>% 
        select(-tot)
      
      ## pre-filtered subset (i.e.df)
      tempdf_l0 <- df %>% 
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR, !!!syms(c(prop_var, prop_var_label))) %>% 
        summarise(ct = sum(PWGTP)) %>% 
        ungroup(!!!syms(c(prop_var, prop_var_label))) %>% 
        mutate(tot = sum(ct)) %>% 
        ungroup() %>% 
        mutate(prop = ct/tot) %>% 
        mutate(across(.cols = prop_var, ~ ifelse(is.na(.), -88, .))) %>% 
        mutate(level = 0,
               grouping = "All Persons in Selected Breakout Categories (NAs Excluded)") %>% 
        select(-tot)
      
      tbl_agg <- bind_rows(tempdf_l99, tempdf_l0, tempdf_lv, tempdf_lv_half)
      
    }else{
      
      tbl_agg <- bind_rows(tbl_agg, tempdf_lv, tempdf_lv_half)
      
    }
    
    
    
  }
  
  ## bind all and arrage table
  tbl_ct_prop <- tbl_agg %>% 
    mutate(id = row_number()) %>%   ## create unique identifier in case duplicate counts
    mutate_at(vars(vars_for_breakdown), ~ifelse(is.na(.), -88, .)) %>% ## -88 used here to identify agg levels
    mutate_at(vars(vars_for_breakdown), ~as.character(.)) %>% ## transform numeric into label to pivot below
    pivot_longer(!c(id, YEAR, ct, prop, level, grouping)) %>% 
    mutate(value = ifelse(is.na(value), gsub("_label", "", name), value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate_at(vars(vars_for_breakdown), ~as.numeric(.)) %>% 
    arrange(YEAR) %>% 
    mutate(ct = ifelse(suppress_lt1000 == T & ct <1000, NA, ct),
           prop = ifelse(suppress_lt1000 == T & ct <1000, NA, prop))
  
  ## separate counts & props
  tbl_ct <- tbl_ct_prop %>% 
    select(-id, -prop) %>% 
    pivot_wider(names_from = YEAR, values_from = ct) %>% 
    arrange(level, !!!syms(vars_for_breakdown))
  
  tbl_prop <- tbl_ct_prop %>% 
    select(-id, -ct) %>% 
    pivot_wider(names_from = YEAR, values_from = prop) %>% 
    arrange(level, !!!syms(vars_for_breakdown))
  
  ## Year-2-Year comparisons: if multiple years, compare to latest year (change in count)
  ## NOTE: gets change in count (tbl_ct) and percent POINT change (tbl_prop)
  tc_list <- NULL
  ppc_list <- NULL
  
  if(length(list_years) > 1){
    
    max_yr <- max(list_years)
    min_yr <- min(list_years)
    
    for(c in 2:length(list_years)){ # c=11
      
      curr_yr <- list_years[[c]]
      prior_yr <- list_years[[c-1]]
      tc_name <-  paste0(curr_yr, "_m_", prior_yr)   ## name of total change variable
      ppc_name <- paste0("ppc_", curr_yr, "v", prior_yr)
      
      tbl_ct[, tc_name] <- tbl_ct[,as.character(curr_yr)] - tbl_ct[,as.character(prior_yr)] 
      tbl_prop[, ppc_name] <- tbl_prop[,as.character(curr_yr)] - tbl_prop[,as.character(prior_yr)] 
      
      
      ## list of comp vars (used in reordering cols below)
      if(c == 2){
        tc_list <- tc_name
        ppc_list <- ppc_name
      }else{
        tc_list <- c(tc_list, tc_name)
        ppc_list <- c(ppc_list, ppc_name)
      }
      
      
      ## if last loop, check and add last V 2019 if (2019 in yr list and not the last comp)
      if(2019 %in% list_years & !grepl("2019",tc_name) & max_yr > 2019 & curr_yr == max_yr){
        
        comp_yr <- 2019
        tc19_name <-  paste0(curr_yr, "_m_", comp_yr)   ## name of total change variable (current yr minus prior year)
        ppc19_name <- paste0("ppc_", curr_yr, "v", comp_yr)
        
        tbl_ct[, tc19_name] <- tbl_ct[,as.character(curr_yr)] - tbl_ct[,as.character(comp_yr)] 
        tbl_prop[, ppc19_name] <- tbl_prop[,as.character(curr_yr)] - tbl_prop[,as.character(comp_yr)] 
        
        tc_list <- c(tc_list, tc19_name)
        ppc_list <- c(ppc_list, ppc19_name)
      }
      
    }
    
  }

  ## rearrange columns and create final table (note: p1 names same btw ct & prop tbl)
  sel_p1 <- names(tbl_prop)[which(!names(tbl_prop) %in% c(ppc_list, tc_list) & 
                                    !names(tbl_prop) %in% as.character(list_years) & 
                                    names(tbl_prop) != prop_var_label)]
  sel_p2 <- as.character(list_years)
  space_to_insert <- "---"
  max_level <- max(unique(tbl_prop$level))
  min_prop_var_val <- min(unique(tbl_prop[,prop_var]))
  
  if(length(vars_for_breakdown) > 1){
    arrangement_index <- c(1:(max_level -1))
  }else{arrangement_index <- 1}
  
  ## variable lavel calls (works for up to 5/6)
  for(v in 1:6){
    
    if(v <= max_level){
      if(v != max_level){
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }else{
        assign(paste0("test_var_max"), paste0(vars_for_breakdown[[v]], "_label"))
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }
    }else{
      assign(paste0("test_var_", v), paste0(vars_for_breakdown[[max_level]], "_label"))
      assign(paste0("test_var_", "0"), paste0(vars_for_breakdown[[max_level]], "_label"))
    }
    
    
  }
  
  ## props with formatted labels
  tbl_form_prop <- 
    tbl_prop %>% 
    # prep some level labels
    ungroup() %>% 
    mutate(level_remainder = level%%1,
           level_index = ifelse(level_remainder == 0, level, level_remainder/.1),
           level_index = as.numeric(round(level_index, 0)), ## NO idea why i had to round this for index below to work
           level_reorder = level_index + level_remainder,
           agg_order = ifelse(level <= 0, 1, 2)) %>% 
    rowwise() %>% 
    #distinct(level, level_remainder, level_index)
    mutate(level_variable = ifelse(level_index >= 1, vars_for_breakdown[[level_index]], ""),
           mult_ind = case_when(level == 0 ~ 1,
                                level == 1 ~ 2,
                                level > 1 & level < 2 ~ 2,
                                level >= 2 & level < max_level ~ (level*2) + 1,
                                level == max_level ~ ((level-1)*2) + 2,
                                TRUE ~ 0),
           indents = case_when(level == -99 ~ "",
                               level >= 0 ~ paste0(rep(space_to_insert, mult_ind), collapse = ""),
                               TRUE ~ "")) %>% 
    #distinct(level, level_remainder, level_index, level_variable)
    ungroup() %>% 
    mutate(level_variable = case_when(level_variable == "EdAttain" ~ "Educational Attainment",
                                      level_variable %in% c("age_6groups", "age_3groups") ~ "Age Group",
                                      level_variable %in% c("ESR", "esr_recode") ~ "Employment Status",
                                      level_variable == "IndShort" ~ "Industry",
                                      level_variable == "occ_recode" ~ "Occupation",
                                      TRUE ~ level_variable)) %>%   
    rowwise() %>% 
    mutate(all_labels = case_when(level == -99 ~ grouping,
                                  level == 0 ~ paste0(indents, " ", grouping),
                                  level == max_level ~ paste0(indents, " ", !!sym(test_var_max)),
                                  level == 1 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_1)),
                                  
                                  level > 1 & level < 2 & level != max_level & level_index == 2 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level > 1 & level < 2 & level != max_level & level_index == 3 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level > 1 & level < 2 & level != max_level & level_index == 4 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level > 1 & level < 2 & level != max_level & level_index == 5 ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  
                                  level == 2 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level == 3 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level == 4 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level == 5 & level != max_level ~ paste0(indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  TRUE ~ "Error, check all_labels within fxn"),
           ## only labels for first row of prop value
           all_labels = ifelse(!!sym(prop_var) == min_prop_var_val, paste0(".", all_labels), ""),
           prop_label = !!sym(paste0(prop_var, "_label"))
           
    ) %>% 
    arrange(!!!syms(vars_for_breakdown[arrangement_index]), agg_order) %>% 
    #distinct(level, level_reorder, all_labels, grouping) 
    select(!!!syms(sel_p1), all_labels, prop_label, !!!syms(sel_p2), !!!syms(c(ppc_list)))
  
  names(tbl_form_prop)[which(names(tbl_form_prop) %in% sel_p2)] <- paste0(names(tbl_form_prop)[which(names(tbl_form_prop) %in% sel_p2)], "_prop")
  
  ## couts to join
  tbl_ct_toJoin <- tbl_ct %>% 
    select(level, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels, sel_p2, tc_list)))
  
  names(tbl_ct_toJoin)[which(names(tbl_ct_toJoin) %in% sel_p2)] <- paste0(names(tbl_ct_toJoin)[which(names(tbl_ct_toJoin) %in% sel_p2)], "_count")
  
  ## all together
  tbl_names_toJoin <- names(tbl_form_prop)[names(tbl_form_prop) %in% names(tbl_ct_toJoin)]
  tbl_form <-
    tbl_form_prop %>%   # names(tbl_form_prop)
    left_join(tbl_ct_toJoin, by = tbl_names_toJoin) 
  
  ## rearrange if only 1 bo
  if(length(vars_for_breakdown) == 1){
    tbl_form <- tbl_form %>% arrange(level)
  }
  
  last_var <- names(tbl_form)[length(names(tbl_form))]
  
  
  
  ## final adjustments, remove extra cols if needed)
  if(only_collapsed_labels == F){
    tbl_fin <- tbl_form %>% ungroup()
  }else{
    tbl_fin <- tbl_form %>% 
      select(grouping, all_labels,  prop_label:!!sym(last_var)) %>% ungroup()
    
  }
  
  
  return(tbl_fin)
  
}


## NOTE, doesn't include dfall or supression argument (run count function and use that to supress values)
CT_fxn_breakout_median <-function(df, outcome_var, vars_for_breakdown, exclude_0, only_collapsed_labels) {
  
  ## WHAT DO? Returns a nested breakout of the median of a specified variable
  ## df --------------------> dataframe of primary sample of interest (filtered for any specific group values)
  ## outcome_var -----------> variable you want median of (string)
  ## vars_for_breakdown ----> vector of variable names to get counts by
  ## exclude_0 -------------> defaults TRUE (ecludes outcome values lte 0)
  ## only_collapsed_labels -> defaults true, so only output a preformatted col of labels and breakout estimates
  ## suppress_lt1000 -------> defaults to true. suppresses outcome if underlying counts less than 1000
  
  ####### IMPORTAT NOTES:
  ## --- Function ONLY appropriate for person variables (PWGTP), (IncHH_adj in beta, validity depends on breakout vars (need to be at HH level))
  ## --- "vars_for_breakdown" is a vector of variable names for breakdown. 
  ## ---      Order variables by prefered heirarchy of groupings
  ## --- APPLY FILTERS TO A TEMPORARY DF!!!!! And the do fxn on that filtered df
  ## --- Function expects a YEAR variable in df and will always group_by year
  ## --- If a breakout variable is a geography, YEAR muct exist!!!
  ## --- all_labels will be wonky if break out by more than 5 vars (very unlikely breakout anyways)
  
  ############# CHECKS 
  
  ## Check vvars_for_breakdown specified
  if(missing(vars_for_breakdown)){ stop("ERROR: vars_for_breakdown argument not specified") } 
  if(missing(outcome_var)){ stop("ERROR: outcome variable not specified")}
  if(missing(only_collapsed_labels)){ only_collapsed_labels <- T } 
  if(missing(exclude_0)){exclude_0 <- T} 
  
  
  ## Check variables exist in df
  check_vars_exist <- vars_for_breakdown %in% names(df)
  if( sum(check_vars_exist) != length(vars_for_breakdown) ){
    
    missing_var <- vars_for_breakdown[which(!vars_for_breakdown %in% names(df))]
    stop(paste0("ERROR: variable '", missing_var, "' does not exist in specified dataframe!!!"))
  }
  
  ## If variable is a geography, YEAR must exist!!!
  if( sum(c("Boro", "boro", "CD") %in%  vars_for_breakdown) > 0 & !"YEAR" %in% names(df)){
    
    stop(paste0("ERROR: variable 'YEAR' is required in dataframe if breaking out by geography!!!"))
  }
  
  ## If YEAR not in df, assume single year
  if(!"YEAR" %in% names(df)){
    
    print("WARNING: 'YEAR' does not exist in dataframe. Assuming a single year.")
    df$YEAR <- "NOT SPECIFIED"
    
  }
  
  ############################# Set outcome variable & weight
  if(outcome_var == "IncHH_adj"){
    
    df$outcome <- df$IncHH_adj
    df$weight <- df$WGTP
    
    ## identify & exclude non-ref persons
    df$rel_allYrs <- NA
    
    if(sum(c(2005:2009) %in% unique(df$YEAR)) > 0){
      df[df$YEAR %in% c(2005:2009), "rel_allYrs"] <- df[df$YEAR %in% c(2005:2009), "REL"]
    }
    
    if(sum(c(2010:2018) %in% unique(df$YEAR)) > 0){
      df[df$YEAR %in% c(2010:2018), "rel_allYrs"] <- df[df$YEAR %in% c(2010:2018), "RELP"]
    }
    
    if(sum(unique(df$YEAR) >= 2019) > 0){
      df[df$YEAR >= 2019, "rel_allYrs"] <- df[df$YEAR >= 2019, "RELSHIPP"]
    }
    
    df$ref_person <- ifelse(df$YEAR < 2019 & df$rel_allYrs == 0, 1,
                            ifelse(df$YEAR >= 2019 & df$rel_allYrs == 20, 1, 0))
    
    df <- df %>% filter(ref_person == 1)
    
  }else{
    
    df[,"outcome"] <- df[,outcome_var]
    df$weight <- df$PWGTP
    
  }
  
  ############ If need be, exclude values lte 0 (and create dfall as full sample ref)
  if(exclude_0 == T){ dfall <- df; df <- df %>% filter(outcome >=0)}
  
  ########## BEGIN
  list_years <- unique(df$YEAR) %>% as.numeric() %>% sort()
  full_grouping <- paste0(vars_for_breakdown, collapse = " x ")
  
  ## for each demo, check if label variables already exist (if not, create)
  for(v in 1:length(vars_for_breakdown)){ #v=1
    
    temp_var <- vars_for_breakdown[[v]]
    temp_var_label <- paste0(temp_var, "_label")
    
    ## if it exists, make sure it's a character
    if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) == "character"){
      ## next loop
      next
    }else if(temp_var_label %in% names(df) && class(df[,temp_var_label][[1]]) != "character"){
      ## make as character
      df[,temp_var_label] <- as.character(df[,temp_var_label])
    }else if(!temp_var_label %in% names(df)){
      ## create label
      df[,temp_var_label] <- fxn_label(df[,temp_var], variable_name = temp_var, YEAR = df[,"YEAR"])
    }
    
  }
  
  vars_for_breakdown_labels <- paste0(vars_for_breakdown, "_label")
  
  ## DF excluding only NA values
  ct_tempdf_all <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    group_by(YEAR, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels))) %>% 
    summarise(ct = sum(weight)) %>% 
    ungroup() %>% 
    mutate(level = length(vars_for_breakdown),
           grouping = full_grouping)
  
  ct_tempdf <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    filter(outcome > 0) %>% 
    group_by(YEAR, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels))) %>% 
    summarise(ct = sum(weight)) %>% 
    ungroup() %>% 
    mutate(level = length(vars_for_breakdown),
           grouping = full_grouping)
  
  dffilt <- df %>% 
    filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
    filter(outcome > 0)
  
  tempdf <- dffilt %>% 
    group_by(YEAR, !!!syms(c(vars_for_breakdown, vars_for_breakdown_labels))) %>% 
    summarise(median = weighted.median(outcome, weight)) %>% 
    ungroup() %>% 
    mutate(level = length(vars_for_breakdown),
           grouping = full_grouping)
  
  ## create aggregation tables  
  for(v in 1: (length(vars_for_breakdown) - 1)){  ## v=1
    
    if(v==0){next}
    
    var_group <- vars_for_breakdown[c(1:v)]
    var_group_label <- paste0(var_group, "_label")
    var_level <- paste0(var_group, collapse = " x ")
    grouped_across <- vars_for_breakdown[c( (v+1):length(vars_for_breakdown))]
    
    ## Aggregated breakout
    if(length(vars_for_breakdown) > 1){
      tempdf_lv <-
        dffilt %>% 
        group_by(YEAR, !!!syms(c(var_group, var_group_label))) %>% 
        summarise(median = weighted.median(outcome, weight)) %>%  
        ungroup() %>% 
        mutate(level = v,
               grouping = var_level) %>% 
        select(level, grouping, YEAR,  !!!syms(c(var_group, var_group_label)), median)
    }else{
      tempdf_lv <- NULL
    }
    
    
    ## across all groups (to have at top of table)
    if(v == 1){
      
      ## Sample as per dfall (NAs excluded but $0 included)
      tempdf_l99 <- df %>% 
        filter_at(vars(vars_for_breakdown), all_vars(!is.na(.))) %>% 
        group_by(YEAR) %>% 
        summarise(median = weighted.median(outcome, weight, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(level = -99,
               grouping = "All Persons in Specified Universe (Pre-set filters, excludes NA, includes 0)")
      
      ## pre-filtered subset (i.e.df)
      tempdf_l0 <- dffilt %>% 
        group_by(YEAR) %>% 
        summarise(median = weighted.median(outcome, weight, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(level = 0,
               grouping = "All Persons in Selected Breakout Categories (Pre-set filters, excludes NA and 0)")
      
      tbl_agg <- bind_rows(tempdf_l99, tempdf_l0, tempdf_lv)
      
    }else{
      
      tbl_agg <- bind_rows(tbl_agg, tempdf_lv)
      
    }
    
    
    
  }
  
  ## bind all and arrage table
  tbl_all <- bind_rows(tbl_agg, tempdf) %>% 
    mutate(id = row_number()) %>%   ## create unique identifier in case duplicate counts
    mutate_at(vars(vars_for_breakdown), ~ifelse(is.na(.), -88, .)) %>% ## -88 used here to identify agg levels
    mutate_at(vars(vars_for_breakdown), ~as.character(.)) %>% ## transform numeric into label to pivot below
    pivot_longer(!c(id, YEAR, median, level, grouping)) %>% 
    mutate(value = ifelse(is.na(value), gsub("_label", "", name), value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate_at(vars(vars_for_breakdown), ~as.numeric(.)) %>% 
    arrange(YEAR) %>% 
    select(-id) %>% 
    pivot_wider(names_from = YEAR, values_from = median) %>% 
    arrange(!!!syms(vars_for_breakdown), level)
  
  
  ## Year-2-Year comparisons: if multiple years, compare to latest year (change in med)
  ## (note, this set-up done when originally focusing on 2019/21/22 comp)
  tc_list <- NULL
  pc_list <- NULL
  if(length(list_years) > 1){
    
    max_yr <- max(list_years)
    min_yr <- min(list_years)
    
    for(c in 2:length(list_years)){ # c=11
      
      curr_yr <- list_years[[c]]
      prior_yr <- list_years[[c-1]]
      tc_name <-  paste0(curr_yr, "_m_", prior_yr)   ## name of total change variable (current yr minus prior year)
      pc_name <- paste0("pc_", curr_yr, "v", prior_yr)
      
      tbl_all[, tc_name] <- tbl_all[,as.character(curr_yr)] - tbl_all[,as.character(prior_yr)] 
      tbl_all[, pc_name] <- tbl_all[,tc_name] / tbl_all[,as.character(prior_yr)] 
      
      ## list of comp vars (used in reordering cols below)
      if(c == 2){
        tc_list <- tc_name
        pc_list <- pc_name
      }else{
        tc_list <- c(tc_list, tc_name)
        pc_list <- c(pc_list, pc_name)
      }
      
      
      ## if last loop, check and add last V 2019 if (2019 in yr list and not the last comp)
      if(2019 %in% list_years & !grepl("2019",tc_name) & max_yr > 2019 & curr_yr == max_yr){
        
        comp_yr <- 2019
        tc19_name <-  paste0(curr_yr, "_m_", comp_yr)   ## name of total change variable (current yr minus prior year)
        pc19_name <- paste0("pc_", curr_yr, "v", comp_yr)
        
        tbl_all[, tc19_name] <- tbl_all[,as.character(curr_yr)] - tbl_all[,as.character(comp_yr)] 
        tbl_all[, pc19_name] <- tbl_all[,tc19_name] / tbl_all[,as.character(comp_yr)] 
        
        tc_list <- c(tc_list, tc19_name)
        pc_list <- c(pc_list, pc19_name)
      }
      
    }
    
  }
  
  ## rearrange columns and create final table
  sel_p1 <- names(tbl_all)[which(!names(tbl_all) %in% c(pc_list, tc_list) & !names(tbl_all) %in% as.character(list_years))]
  sel_p2 <- as.character(list_years)
  space_to_insert <- "---"
  max_level <- max(unique(tbl_all$level))
  
  ## variable lavel calls (works for up to 5/6)
  for(v in 1:6){
    
    if(v <= max_level){
      if(v != max_level){
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }else{
        assign(paste0("test_var_max"), paste0(vars_for_breakdown[[v]], "_label"))
        assign(paste0("test_var_", v), paste0(vars_for_breakdown[[v]], "_label"))
      }
    }else{
      assign(paste0("test_var_", v), paste0(vars_for_breakdown[[max_level]], "_label"))
    }
    
    
  }
  
  tbl_form <- 
    tbl_all %>% 
    # prep some level labels
    rowwise() %>% 
    mutate(level_variable = ifelse(level >=1, vars_for_breakdown[[level]], ""),
           mult_ind = case_when(level == 0 ~ 1,
                                level == 1 ~ 2,
                                level > 1 & level < max_level ~ (level*2) + 1,
                                level == max_level ~ ((level-1)*2) + 2,
                                TRUE ~ 0),
           indents = case_when(level == -99 ~ "",
                               level >= 0 ~ paste0(rep(space_to_insert, mult_ind), collapse = ""), 
                               TRUE ~ "")) %>% 
    ungroup() %>% 
    mutate(level_variable = case_when(level_variable == "EdAttain" ~ "Educational Attainment",
                                      level_variable %in% c("age_6groups", "age_3groups") ~ "Age Group",
                                      level_variable %in% c("ESR", "esr_recode") ~ "Employment Status",
                                      level_variable == "IndShort" ~ "Industry",
                                      level_variable == "occ_recode" ~ "Occupation",
                                      TRUE ~ level_variable)) %>%   
    rowwise() %>% 
    mutate(all_labels = case_when(level == -99 ~ grouping,
                                  level == 0 ~ paste0(".", indents, " ", grouping),
                                  level == max_level ~ paste0(".", indents, " ", !!sym(test_var_max)),
                                  level == 1 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_1)),
                                  level == 2 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_2)),
                                  level == 3 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_3)),
                                  level == 4 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_4)),
                                  level == 5 & level != max_level ~ paste0(".", indents, " ", level_variable, ": ",  !!sym(test_var_5)),
                                  TRUE ~ "Error, check all_labels within fxn")) %>% 
    select(!!!syms(sel_p1), all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list)))
  
  ## final adjustments, remove extra cols if needed)  only_collapsed_labels<-T
  if(only_collapsed_labels == F){
    tbl_fin <- tbl_form %>% ungroup()
  }else{
    tbl_fin <- tbl_form %>% 
      select(all_labels, !!!syms(sel_p2), !!!syms(c(tc_list, pc_list))) %>% ungroup()
    
  }
  
  
  return(tbl_fin)
  
}

#

###### num format #######
# 
fxn_format_nums <- function(number, is_money, is_prop, num_dec){  # number = 108479283

  ## number: numerical value to format
  ## is_money: if true, append with $, Defaults to false

  if (missing(is_money)){is_money <- F}
  if (missing(is_prop)){is_prop <- F}
  if (missing(num_dec)){num_dec <- 0}

  number <- as.numeric(number)

  to_round <- num_dec

  if(is_prop == F){

    new_num <- case_when(abs(number) < 1000 ~ as.character(round(number, 0)),
                         abs(number) >= 1000 & abs(number) < 1000000 ~ paste0(round(number/1000, to_round), "K"),
                         abs(number) >= 1000000 & abs(number) < 1000000000 ~ paste0(round(number/1000000, to_round), "M"),
                         abs(number) >= 1000000000 ~ paste0(round(number/1000000000, to_round), "B"),
                         TRUE ~ "??")

    new_num_formatted <- ifelse(is_money == T, paste0("$", new_num), new_num)

  }else{

    new_num <- round(number*100, to_round)

    new_num_formatted <- paste0(new_num, "%")
  }

  return(new_num_formatted)

}
