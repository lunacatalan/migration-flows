

library(tidyverse)
library(readxl)
library(here)
library(gt)
library(tidycensus)
library(ggridges)

# STATE -
# identifies the state where a resident currently lives

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

# load the acs_function.R file from Anne
source(here("code/acs_convenience_functions.r"))

# path to the raw file
# source("https://raw.githubusercontent.com/lunacatalan/gecko-plots/refs/heads/main/R/pru_plots.R")

source("C:/Users/lcatalan/OneDrive - NYC OTI/Documents/dev/gecko-plots/R/pru_plots.R")

### Create 5yr ACS file for NYS

### Read in the rds files created in code/acs_api_query.R
acs18 <- readRDS(here("data/acs/migration_acs_allStates_2018.rds")) %>% 
  mutate(YEAR = 2018)
acs19 <- readRDS(here("data/acs/migration_acs_allStates_2019.rds")) %>% 
  mutate(YEAR = 2019)
acs21 <- readRDS(here("data/acs/migration_acs_allStates_2021.rds")) %>% 
  mutate(YEAR = 2021)
acs22 <- readRDS(here("data/acs/migration_acs_allStates_2022.rds")) %>% 
  mutate(YEAR = 2022)
acs23 <- readRDS(here("data/acs/migration_acs_allStates_2023.rds")) %>% 
  mutate(YEAR = 2023) %>% 
  rename(ST = STATE)
acs_5yr_18_22 <- read_csv("Z:/0-Source Data/ACS/5_year_data/2018-2022/AH_5yr_withStdVars/2018_2022_5yrACS_StdVars.csv")

### Create loop to filter and label columns
# if MIGSP is 0 - person less than 1 year old/lived in same house 1 year ago) so set to ST value

acs <- list(acs18, acs19, acs21, acs22, acs23)
years <- c(18, 19, 21, 22, 23)

for (i in 1:length(acs)) {
  
  df <- acs[[i]] %>% 
    # convert columns to numeric values
    mutate(across(-1, as.numeric),
           previous_res = ifelse(MIGSP == 0, ST, 
                                 MIGSP),
           moved_flag = ifelse(previous_res!= ST, 
                               1, 
                               0),
           in_migration = ifelse(ST ==36 & moved_flag == 1,
                                 1,
                                 0),
           out_migration = ifelse(previous_res == 36 & moved_flag == 1, 
                                  1,
                                  0)) %>% 
    filter(in_migration == 1 | out_migration == 1) %>% 
    # create group labels for ethnicity, educational attainment, age, occupation, and working status
    mutate(Ethnicity = fxn_Ethnicity(RAC1P, HISP), 
           Ethnicity_label = fxn_label(Ethnicity),
           EdAttain = fxn_EdAttain(SCHL),
           EdAttain_label = fxn_label(EdAttain),
           EdAttain6 = fxn_EdAttain6(SCHL),
           EdAttain6_label = fxn_label(EdAttain6),
           AgeForReport = fxn_AgeForReport(AGEP),
           AgeForReport_label = fxn_label(AgeForReport, "age_3groups"),
           occ_recode = fxn_occ_recode(OCCP),
           occ_recode_label = fxn_label(occ_recode)) %>% 
    # is_working = case_when(ESR %in% c(3, 6) ~ 0,
    #                        ESR %in% c(1, 2, 4, 5) ~ 1,
    #                        is.na(ESR) ~ NA,
    #                        TRUE ~ -9999),
    # is_inSchool = ifelse(SCH %in% c(2, 3), 1, 0),
    # is_ageOOSOOW = ifelse(AGEP >= 16 & AGEP <= 24, 1, 0),
    # oosoow = ifelse(is_working == 0 & is_inSchool == 0 & is_ageOOSOOW == 1, 1, 0)) %>% 
    rename_with(tolower) %>% # make column names lowercase
    select("serialno","sporder","year", "pwgtp","wgtp","agep",
           "puma","migpuma",
           "st", "previous_res", "migsp", "in_migration", "out_migration",
           "ethnicity_label", "edattain_label", "ageforreport_label", "occ_recode_label")
  
  assign(paste0("mig", years[i]), df)
}

mig <- rbind(mig18, mig19, mig21, mig22, mig23)

#########################
# In-Migration and Out-Migration from NYC?
#########################

mig %>% 
  filter(out_migration == 1) %>% 
  mutate(year = as.factor(year)) %>% 
  uncount(pwgtp) %>% 
  ggplot(aes(x = agep)) +
  geom_histogram(fill = "#f6871f") +
  facet_wrap(~year, nrow = 3)+ 
  pru_theme() +
  labs(title = "Distribution of Age of Out-Migration NYC 2023",
       x = "Age",
       y = "Count")

mig %>% 
  filter(in_migration == 1) %>% 
  mutate(year = as.factor(year)) %>% 
  uncount(pwgtp) %>% 
  ggplot(aes(x = agep)) +
  geom_histogram(fill = "#f6871f") +
  facet_wrap(~year, nrow = 3)+ 
   pru_theme() +
  labs(title = "Distribution of Age of In-Migration NYC 2023",
       x = "Age",
       y = "Count")

#########################
# Occupations in vs out
#########################

occ <- mig %>% 
  select(year, occ_recode_label, in_migration, out_migration) %>% 
  group_by(year, occ_recode_label) %>% 
  pivot_longer(cols = c(3, 4),
               names_to = "migration", 
               values_to = "count") %>%
  ungroup() %>% 
  group_by(year, occ_recode_label, migration) %>% 
  summarise(count = sum(count)) %>% 
  filter(occ_recode_label != c("occ_recode_-9999999"))

ggplot(occ, aes(x = migration, y = count, fill = occ_recode_label)) +
  geom_col(position = "dodge") +
  facet_wrap(~year) +
  pru_theme()


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

  
# Age distribution 

age_mig <- mig %>% 
  select(year, hincp, moved_flag, pwgtp, agep) %>% 
  filter(moved_flag == 1) %>% 
  mutate(hincp = ifelse(hincp >500000,
                        500000,
                        hincp)) %>% 
  uncount(pwgtp)

age_plot <- age_mig %>% 
  ggplot(aes(x =agep)) +
  geom_histogram(fill = "#f6871f") +
  facet_wrap(~year, nrow = 3) +
  scale_x_continuous(labels = scales::label_comma()) +
  pru_theme() +
  labs(title = "Distribution of Age of In-Migrants to NYC",
       x = "Age",
       y = "Count")

ggsave(here("outputs", "age_dist_plot.png"), plot = age_plot, width = 5, height = 5, dpi = 300)

## Income
income_plot <- age_mig %>% 
  ggplot(aes(x = hincp)) +
  geom_histogram(fill = "#f6871f") +
  facet_wrap(~year, nrow = 3) +
  scale_x_continuous(labels = scales::label_comma()) +
  pru_theme() +
  labs(title = "Distribution of Age of In-Migrants to NYC",
       x = "Age",
       y = "Count")

ggsave(here("outputs", "inc_dist_plot.png"), plot = income_plot, width = 5, height = 5, dpi = 300)

age_mig <- mig %>% 
  select(year, moved_flag, pwgtp, agep, ethnicity_label) %>% 
  filter(moved_flag == 1) %>% 
  uncount(pwgtp) %>% 
  pivot_longer(cols = c(4:9),
               names_to = "race",
               values_to = "flag") %>% 
  filter(flag == 1) %>% 
  select(year, agep, race)

age_eth_plot <- mig %>% 
  filter(in_migration == 1) %>% 
  ggplot(aes(x = agep, y = ethnicity_label, fill = ethnicity_label)) +
  geom_density_ridges() +
  facet_wrap(~year, nrow = 3) +
  pru_theme() +
  labs(title = "Distribution of Age of in-Migrants to NYC",
       x = "Age",
       y = "Count") +
  pru_theme() +
  pru_palette("G2O")

ggsave(here("outputs", "age_eth_plot.png"), plot = age_eth_plot, width = 6, height = 6, dpi = 200)


  
# Race / Ethnicity 
eth_in_mig <- mig %>% 
  filter(in_migration == 1) %>% 
  select(year, pwgtp, ethnicity_label) %>% 
  uncount(pwgtp) %>% 
  group_by(year, ethnicity_label) %>% 
  dplyr::summarize(count = n(), .groups = 'drop') %>% 
  ungroup()

eth_out_mig <- mig %>% 
  filter(out_migration == 1) %>% 
  select(year, pwgtp, ethnicity_label) %>% 
  uncount(pwgtp) %>% 
  group_by(year, ethnicity_label) %>% 
  dplyr::summarize(count = n(), .groups = 'drop') %>% 
  ungroup()

eth_in_plot <- eth_in_mig %>% 
  ggplot(aes(x = ethnicity_label, y = count/1000, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(title = "Race & Ethnicity of In-Migrants to NYC 2018-2023",
       x = "Ethnicity",
       y = "People (in thousands)",
       fill = "") +
  pru_theme() +
  pru_palette("dis5")

eth_out_mig %>% 
  ggplot(aes(x = ethnicity_label, y = count/1000, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(title = "Race & Ethnicity of Out-Migrants to NYC 2018-2023",
       x = "Ethnicity",
       y = "People (in thousands)",
       fill = "") +
  pru_theme() +
  pru_palette("dis5")

ggsave(here("outputs", "eth_in_plot.png"), plot = eth_in_plot, width = 5, height = 3, dpi = 300)
  

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
                    "England", "Russia", "Ukraine", "Afghanistan", "China, Hong Kong, Macau And Paracel Islands", 
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

regions <- migsp_mapping %>% 
  filter(str_detect(state_country, "Not Specified"))

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
  filter(!state_country %in% regions$state_country) %>% 
  group_by(state_country) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  arrange(-count) %>% 
  #slice(-c(7, 9, 10, 13:15)) %>% 
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

ggsave(here("outputs", "source_in_plot.png"), plot = top10_22, width = 6, height = 4, dpi = 300)



domestic <- mig %>% 
  filter(moved_flag == 1) %>% 
  uncount(pwgtp) %>% 
  select(year, migsp_new, migpuma, fmigsp) %>% 
  filter(migsp_new <= 72 | migsp_new == 555) %>% 
  rename(migsp = migsp_new) %>% 
  left_join(migsp_mapping, by = "migsp") %>% 
  group_by(year, state_country) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  arrange(year,-count) %>% 
  group_by(year) %>% 
  slice_head(n = 5) %>% 
  mutate(ct = row_number()) %>% 
  select(ct, everything())

foreign <- mig %>% 
  filter(moved_flag == 1) %>% 
  uncount(pwgtp) %>% 
  select(year, migsp_new, migpuma, fmigsp) %>% 
  filter(migsp_new >= 109 & migsp_new <= 501) %>% 
  rename(migsp = migsp_new) %>% 
  left_join(migsp_mapping, by = "migsp") %>% 
  filter(!state_country %in% regions$state_country) %>% 
  group_by(year, state_country) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  arrange(year,-count) %>% 
  group_by(year) %>% 
  slice_head(n = 5) %>% 
  mutate(ct = row_number()) %>% 
  select(ct, everything())

top5 <- domestic %>%
  left_join(foreign, by = c("ct", "year")) %>% 
  rename(domestic = state_country.x,
         foreign = state_country.y)

# incorporating new years
top5 <- ggplot(top5) +
  # Foreign bars (negative values)
  geom_col(aes(x = interaction(ct, year), y = -count.y, fill = as.factor(year)), 
           show.legend = TRUE, position = "dodge", alpha = 0.5) + 
  # Domestic bars (positive values)
  geom_col(aes(x = interaction(ct, year), y = count.x, fill = as.factor(year)), 
           show.legend = TRUE, position = "dodge") +
  geom_text(aes(x = interaction(ct, year), y = count.x / 2, label = domestic), 
            color = "white", size = 3.25, fontface = "bold", position = position_dodge(width = 0.9)) +  # Labels for domestic bars
  geom_text(aes(x = interaction(ct, year), y = -count.y / 2, label = foreign), 
            color = "black", size = 3.25, fontface = "bold", position = position_dodge(width = 0.9)) +  # Labels for foreign bars
  scale_y_continuous(
    breaks = seq(-8000, 20000, by = 4000),
    labels = function(x) abs(x)) +  # Custom breaks
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
  labs(x = "",
       y = "People", 
       title = "Top 5 Foreign and Domestic Sources of In-Migration to NYC 2018-2023",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() + # Flip the axes if you want horizontal bars
  annotate("text", x = 26.5, y = 4000, label = "Domestic", size = 5, fontface = "bold") +
  annotate("text", x = 26.5, y = -2000, label = "Foreign", size = 5, fontface = "bold") +
  theme(axis.text.y = element_blank()) +
  pru_theme() +
  pru_palette("B2O")

ggsave(here("outputs", "source_in_plot_5yr.png"), plot = top5, width = 6, height = 4, dpi = 300)

