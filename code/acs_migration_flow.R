

library(tidyverse)
library(readxl)
library(here)
library(gt)
library(tidycensus)

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


### Read in Data
source("C:/Users/lcatalan/OneDrive - NYC OTI/Documents/dev/gecko-plots/R/pru_plots.R")

acs18 <- read_csv("Z:/0-Source Data/ACS/2018/2018_ACS_NYC.csv") %>% 
  mutate(YEAR = 2018)
acs19 <- read_csv("Z:/0-Source Data/ACS/2019/2019_ACS_NYC.csv")
acs21 <- read_csv("Z:/0-Source Data/ACS/2021/2021_ACS_NYC.csv")
acs22 <- read_csv("Z:/0-Source Data/ACS/2022/2022_ACS_NYC.csv")
acs23 <- read_csv("Z:/0-Source Data/ACS/2023/2023_ACS_NYC.csv") %>% 
  rename(ST = STATE)
acs_5yr_18_22 <- read_csv("Z:/0-Source Data/ACS/5_year_data/2018-2022/AH_5yr_withStdVars/2018_2022_5yrACS_StdVars.csv")

### Create migration df to identify who moved to nyc
acs <- list(acs18, acs19, acs21, acs22, acs23)
years <- c(18, 19, 21, 22, 23)

for (i in 1:length(acs)) {
  
  df <- acs[[i]] %>% 
    rename_with(tolower) %>% # make column names lowercase
    mutate(current_res = 36) %>% 
    select(serialno, year, puma, st, adjinc, pwgtp, cit,
           agep, # age
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
  
  assign(paste0("mig", years[i]), df)
}

mig <- rbind(mig18, mig19, mig21, mig22, mig23)

#########################
# Who is moving to NYC?
#########################

# Age Distribution 2022

move22 <- mig22 %>% 
  filter(moved_flag == 1) %>% 
  uncount(pwgtp) # is there another way to apply weights?

ggplot(move22, aes(x = agep)) +
  geom_histogram(fill = "#f6871f") +
  pru_theme() +
  labs(title = "Distribution of Age of People Moving to NYC 2022",
       x = "Age",
       y = "Count")

# Age Distribution 2023
migration23 %>% 
  filter(moved_flag == 1) %>% 
  uncount(pwgtp) %>% 
  ggplot(aes(x = agep)) +
  geom_histogram(fill = "#f6871f") +
  pru_theme() +
  labs(title = "Distribution of Age of People Moving to NYC 2023",
       x = "Age",
       y = "Count")

  
# Race / Ethnicity 
eth_mig <- mig %>% 
  filter(moved_flag == 1) %>% 
  uncount(pwgtp) %>% 
  select(year, racaian, racasn, racblk, racnh, racpi, racwht) %>% 
  group_by(year) %>% 
  summarize(across(c(racaian, racasn, racblk, racnh, racpi, racwht), 
                   sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(!year) %>% 
  rename(ethnicity = name,
         count = value)

eth_in_plot <- eth_mig %>% 
  ggplot(aes(x = ethnicity, y = count/1000, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(title = "Race & Ethnicity of In-Migrants to NYC 2018-2023",
       x = "Ethnicity",
       y = "People (in thousands)",
       fill = "") +
  pru_theme() +
  pru_palette("dis6")

ggsave(here("outputs", "eth_in_plot.png"), plot = eth_in_plot, width = 5, height = 3, dpi = 300)

move22 %>% 
  #mutate(ct = row_number()) %>% 
  select(racaian, racasn, racblk, racnh, racpi, racwht) %>% 
  pivot_longer(cols = 1:6, 
               names_to = "ethnicity",
               values_to = "count") %>% 
  group_by(ethnicity) %>% 
  summarize(count = sum(count)) %>% 
  ggplot(aes(x = ethnicity, y = count/1000)) +
  geom_col() +
  labs(x = "Ethnicity",
       y = "People (in thousands)") +
  pru_theme() 

# Race / Ethnicity Part 2

dem <- move22 %>% 
  select(agep, rac1p) %>% 
  mutate(rac1p = factor(rac1p)) %>% 
  group_by(agep, rac1p) %>% 
  summarize(count = n(), .groups = 'drop')  # count occurrences of each agep, rac1p combo


move22 %>% 
  select(agep, rac1p) %>% 
  mutate(rac1p = factor(rac1p)) %>% 
  group_by(agep, rac1p) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  ggplot(aes(x = rac1p, y = count/1000)) +
  geom_col() +
  pru_theme() +
  labs(x = "Race",
       y = "People (in thousands)")
  

# Source of Migration

## Create a data frame with MIGSP codes and corresponding state/country names
migsp_mapping <- data.frame(
  migsp = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 
            18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 
            32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 
            47, 48, 49, 50, 51, 53, 54, 55, 56, 72, 109, 110, 111, 
            113, 114, 120, 134, 138, 139, 163, 164, 200, 207, 210, 
            214, 215, 217, 229, 231, 233, 235, 240, 242, 243, 245, 
            247, 251, 252, 253, 301, 303, 312, 313, 314, 317, 327, 
            329, 332, 333, 344, 362, 364, 365, 370, 373, 374, 414, 
            416, 427, 440, 467, 468, 469, 501, 555),
  state_country = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                    "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                    "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico", 
                    "France", "Germany", "Northern Europe, \nNot Specified", "Eastern Europe, Not Specified", 
                    "Western Europe or \nOther Europe, \nNot Specified", "Italy", "Spain", "United Kingdom, Excluding England", 
                    "England", "Russia", "Ukraine", "Afghanistan", "China, Hong Kong, \nMacau And Paracel Islands", 
                    "India", "Israel", "Japan", "Korea", "Nepal", "Pakistan", "Philippines", "Saudi Arabia", "Taiwan", 
                    "Thailand", "Turkey", "United Arab Emirates", "Vietnam", "Eastern Asia, Not Specified", 
                    "Western Asia, Not Specified", "South Central Asia or Asia, Not Specified", "Canada", "Mexico", 
                    "El Salvador", "Guatemala", "Honduras", "Central America, Not Specified", "Cuba", "Dominican Republic", 
                    "Haiti", "Jamaica", "Caribbean and North America, Not Specified", "Brazil", "Colombia", "Ecuador", 
                    "Peru", "Venezuela", "South America, Not Specified", "Egypt", "Ethiopia", "Kenya", "Nigeria", 
                    "Western Africa, \nNot Specified", "Other Africa, Not Specified", "Eastern Africa, Not Specified", 
                    "Australia", "Other US Island Areas, Oceania, Not Specified, or At Sea")
)

# View the data frame
print(migsp_mapping)


# Including states, Puerto Rico, and Other US Island Areas, Oceania, Not Specified, or At Sea
domestic <- move22 %>% 
  select(migsp_new, migpuma, fmigsp) %>% 
  filter(migsp_new <= 72 | migsp_new == 555) %>% 
  rename(migsp = migsp_new) %>% 
  left_join(migsp_mapping, by = "migsp") %>% 
  group_by(state_country) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  arrange(-count) %>% 
  mutate(ct = row_number()) %>% 
  select(ct, everything())


foreign <- move22 %>% 
  select(migsp_new, migpuma, fmigsp) %>% 
  filter(migsp_new >= 109 & migsp_new <= 501) %>% 
  rename(migsp = migsp_new) %>% 
  left_join(migsp_mapping, by = "migsp") %>% 
  group_by(state_country) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  arrange(-count) %>% 
  slice(-c(7, 9, 10, 13:15)) %>% 
  mutate(ct = row_number()) %>% 
  select(ct, everything())

top10 <- domestic %>% 
  slice(1:10) %>% 
  left_join(foreign, by = "ct") %>% 
  rename(domestic = state_country.x,
         foreign = state_country.y)

top10_22<- ggplot(top10) +
  # Foreign bars (negative values)
  geom_col(aes(x = reorder(ct, -count.y), y = -count.y, fill = "Foreign"), show.legend = TRUE) + 
  # Domestic bars (positive values)
  geom_col(aes(x = reorder(ct, -count.x), y = count.x, fill = "Domestic"), show.legend = TRUE) +
  geom_text(aes(x = reorder(ct, -count.x), y = count.x / 2, label = domestic), 
            color = "white", size = 3.25, fontface = "bold") +  # Labels for domestic bars
  geom_text(aes(x = reorder(ct, -count.y), y = -count.y / 2, label = foreign), 
            color = "white", size = 3.25, fontface = "bold") +  # Labels for foreign bars
  scale_y_continuous(
    breaks = seq(-8000, 20000, by = 4000),
    labels = function(x) abs(x)) +  # Custom breaks
  scale_fill_manual(
    values = c("Foreign" = "orange", 
               "Domestic" = "#710f11"),  # Custom colors
    name = "Migration Source"  # Legend title
  ) +
  labs(x = "",
       y = "People", 
       title = "Top 10 Foreign and Domestic Sources of In-Migration to NYC 2022") +
  # Flip the axis to make both bars 0-centered 
  coord_flip() + # Flip the axes if you want horizontal bars
  pru_theme() +
  theme(axis.text.y = element_blank())

ggsave(here("outputs", "source_in_plot.png"), plot = top10_22, width = 5, height = 3, dpi = 300)

