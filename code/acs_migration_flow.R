

library(tidyverse)
library(readxl)
library(here)
library(gt)

# MIGPUMA - Migration PUMA based on 2020 Census definition 
# identifies the geographic location where a respondent lived one year prior to the survey
#   bbbbb .N/A (person less than 1 year old/lived in same house 1 year ago) 
#   00001 .Did not live in the United States or in Puerto Rico one year ago
#   00002 .Lived in Puerto Rico one year ago and current residence is in the United States 
#   00100..81000 .Assigned Migration PUMA. Use with MIGSP. 

# MIGSP - Migration recode - State or foreign country code
# identifies the state or foreign country where a respondent previously lived
#   bbb  .N/A (person less than 1 year old/lived in same house 1 year ago)
#   001 to 072 US States including DC, Puerto Rico (with numbers missing)
#   109 to 501 Foreign Countries
#   555 .Other US Island Areas, Oceania, Not Specified, or At Sea

# FMIGSP - Migration state and foreign country allocation flag 
#   0 .No 
#   1 .Yes 

# POWPUMA - Place of work PUMA based on 2020 Census definition 
#   bbbbb .N/A (not a worker-not in the labor force, including .persons under 16 years; 
#       unemployed; civilian .employed, with a job not at work; Armed Forces, .with a job but not at work) 
#   00001 Did not work in the United States or in Puerto Rico 
#   00100..81000 Assigned Place of work PUMA. Use with POWSP. 

# POWSP - Place of work - State or foreign country recode
#   bbb .N/A (not a worker-not in the labor force, including persons .under 16 years; 
#       unemployed; employed, with a job not at .work; Armed Forces, with a job but not at work)
#   001 to 072 US States including DC, Puerto Rico (with numbers missing)
#   166 .Europe 
#   254 .Asia 
#   303 .Mexico 
#   399 .Americas, Not Specified 
#   555 .Other US Island Areas Not Specified, Africa, Oceania, at .Sea, or Abroad, Not Specified

# migsp_new - updated NA values to 36 to indicate NYS residency

# moved_flag 
#   0 Did not move in the last year
#   1 Changes residence from outside to NYS in the last year

acs22 <- read_csv("Z:/0-Source Data/ACS/2022/2022_ACS_NYC.csv")
acs_5yr_18_22 <- read_csv("Z:/0-Source Data/ACS/5_year_data/2018-2022/AH_5yr_withStdVars/2018_2022_5yrACS_StdVars.csv")


migration22 <- acs22 %>% 
  #select(-896) %>% 
  rename_with(tolower) %>% # make column names lowercase
  mutate(current_res = 36) %>% 
  select(1:13, # age
         current_res, migsp, migpuma, fmigsp, # migration
         powpuma, powsp, # working
         rac1p, rac2p, rac3p, racaian, racasn, racblk, racnh, racnum, racpi, racwht # race/ethnicity
         ) %>% 
  # checked the ratio of NAs to see if it matches the % that had the same residency
  #filter(is.na(migsp)) 
  mutate(migsp_new = ifelse(is.na(migsp) == TRUE,
                            36,
                            migsp),
         moved_flag = ifelse(migsp_new == current_res, 
                             0, 
                             1))

#########################
# Who is moving to NYC?
#########################

move22 <- migration22 %>% 
  filter(moved_flag == 1)

ggplot(move22, aes(x = agep)) +
  geom_histogram()

  


