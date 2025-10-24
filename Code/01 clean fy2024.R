
################################################################################
# Cleaning pipleine for FY2021-2024 FOIA data
# Source: https://github.com/BloombergGraphics/2024-h1b-immigration-data
# Universe: all H-1B lottery winners who filed petitions

# Author: Jiaxin He (jiaxin@eig.org) and Sarah Eckhardt (sarah@eig.org)
# Date last edited: 10.20.2025

# Steps:
  # clean and combine 2015-2024 LCA entries
  # load 2021-2024 FOIA I-129 data and re-name variables for ease of use
  # filter for only lottery winners who filed petitions for H1-Bs
  # merge in LCA data to obtain SOC codes, LCA application addresses, and OES wage levels
  # clean wage data
  # identify potential duplicates
  # implement new wage level ranking rule as follows
  #   (1) merge with HUD zip code to county crosswalk
  #   (2) for those with missing zip codes, invalid zip codes, or low overlapping area zip codes, geocode using arcgis
  #   (3) link the geocoded addresses to counties by spatial geometry
  #   (4) merge the zip-based and geocoded counties and crosswalk counties to CBSA
  #   (5) merge CBSA to MSAs, manually correct for New England townships
  #   (6) crosswalk 2010 SOC occupation codes used by old LCA entries to 2018 SOCs
  #   (7) first pass on those without SOC occupations via string similarity on DOT codes and free string job titles
  #   (8) second pass on those without SOC occupations via machine learning on company names, degree fields, job titles, and DOT codes of entries with SOC codes
  #   (9) entries that still have no SOC matches cannot be merged with OFLC wage levels; use LCA wage levels for these and filter out those without LCA wage levels
  #   (10) adjust 2021-2023 wages by the ratio of average wage for the corresponding MSA-SOC combination in 2024 OES over the average wage for the given year-MSA-SOC combo
  #   (11) merge with 2024-2025 OFLC wage levels and determine wage levels with the time-adjusted I-129 wages
  #   (12) wherever the wage is lower than OFLC Level I lower bound, re-label as wage level I
  #   (13) wherever the LCA wage levels cannot be determined, fall back to LCA wage levels
  # remove extraneous variables and export cleaned data by year

################################################################################
################################################################################

rm(list = ls())

# load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(tools)
library(igraph)
library(stringr)
library(fuzzyjoin)
library(purrr)
library(tidygeocoder)
library(sf)
library(tidycensus)

# set project paths
user_path = ""

project_path = file.path(user_path, "h1b-npv-wage-ranking-simulations")
data_path = file.path(project_path, "Data")
foia_path = file.path(data_path, "FOIA Data")
lca_path = file.path(data_path, "LCA Data")
cleaned_path = file.path(project_path, "Cleaned Data")
dot_matching_path = file.path(project_path, "Code/DOT to SOC matching")

################################################################################
# read in LCA data
# pulling data back to 2015 for conservative purposes -- sometimes takes years between LCA and I-129 submission.
# file formats are quarterly 2020-2024, and annual 2015-2019
# note for user: this process may take time. skip 122-309, and uncomment 311
setwd(lca_path)
quarters = list()

for (i in (1:4)) {
  for (year in (2020:2024)) {
    print(i)
    print(year)
    
    file_name = paste0("LCA_Disclosure_Data_FY", year, "_Q",i,".xlsx")
    
    # load and clean
    file = read_xlsx(file_name) %>%
      filter(VISA_CLASS == "H-1B") %>%      # H-1Bs only
      select(CASE_NUMBER,
             CASE_STATUS,
             VISA_CLASS,
             DECISION_DATE,
             PW_WAGE_LEVEL,
             SOC_CODE,
             SOC_TITLE,
             WORKSITE_COUNTY,
             WORKSITE_STATE,
             WAGE_RATE_OF_PAY_FROM, 
             WAGE_RATE_OF_PAY_TO, 
             PREVAILING_WAGE,
             WAGE_UNIT_OF_PAY) %>%
      
      # clean up monetary variables
      mutate(across(c(WAGE_RATE_OF_PAY_FROM, WAGE_RATE_OF_PAY_TO, PREVAILING_WAGE),
                    ~(as.numeric(str_replace_all(., c("\\$"="", ","=""))))
      )) %>% mutate(FISCAL_YEAR = year)
    
    id = paste0(year,i)
    print(id)
    quarters [[id]] = file
  }
}

lca_2020_2024 = bind_rows(quarters)
lca_2020_2024 = lca_2020_2024 %>% distinct() # remove duplicates

# save progress in the event of crashes
save(lca_2020_2024, file = file.path(cleaned_path, "lca_2020_2024.RData"))

# read in earlier years, that are not in a quarterly format
# note that prevailing wage levels are not available before 2019
years = c()

for (year in c(2015:2019)) {
  print(year)
  
  file_name = paste0("H-1B_Disclosure_Data_FY", year,".xlsx")
  
  # load and clean
  if(year==2019) {
    file = read_xlsx(file_name) %>%
      filter(VISA_CLASS == "H-1B") %>%      # H-1Bs only
      select(CASE_NUMBER,
             CASE_STATUS,
             VISA_CLASS,
             DECISION_DATE,
             WORKSITE_COUNTY_1,
             PW_WAGE_LEVEL_1,
             WORKSITE_STATE_1,
             SOC_CODE,
             SOC_TITLE,
             WAGE_RATE_OF_PAY_FROM_1, 
             WAGE_RATE_OF_PAY_TO_1,
             PREVAILING_WAGE_1,
             WAGE_UNIT_OF_PAY_1) %>%
      rename(WAGE_RATE_OF_PAY_FROM = WAGE_RATE_OF_PAY_FROM_1,
             WAGE_RATE_OF_PAY_TO = WAGE_RATE_OF_PAY_TO_1,
             PREVAILING_WAGE = PREVAILING_WAGE_1,
             PW_WAGE_LEVEL = PW_WAGE_LEVEL_1,
             WAGE_UNIT_OF_PAY = WAGE_UNIT_OF_PAY_1,
             WORKSITE_COUNTY = WORKSITE_COUNTY_1,
             WORKSITE_STATE = WORKSITE_STATE_1)
  }else if(year %in% 2016:2018) {
    file = read_xlsx(file_name) %>%
      filter(VISA_CLASS == "H-1B") %>%      # H-1Bs only
      select(CASE_NUMBER,
             CASE_STATUS,
             VISA_CLASS,
             DECISION_DATE,
             SOC_CODE,
             SOC_NAME,
             WORKSITE_COUNTY,
             WORKSITE_STATE,
             WAGE_RATE_OF_PAY_FROM, 
             WAGE_RATE_OF_PAY_TO, 
             PREVAILING_WAGE,
             WAGE_UNIT_OF_PAY) %>% rename(SOC_TITLE = SOC_NAME)
  }else if(year == 2015){
    file = read_xlsx(file_name) %>%
      filter(VISA_CLASS == "H-1B") %>%
      select(CASE_NUMBER,
             CASE_STATUS,
             VISA_CLASS,
             DECISION_DATE,
             SOC_CODE,
             SOC_NAME,
             WORKSITE_COUNTY,
             WORKSITE_STATE,
             WAGE_RATE_OF_PAY,
             PREVAILING_WAGE,
             WAGE_UNIT_OF_PAY) %>%
      rename(SOC_TITLE = SOC_NAME) %>%
      mutate(WAGE_RATE_OF_PAY = str_trim(WAGE_RATE_OF_PAY)) %>%
      separate(
        WAGE_RATE_OF_PAY,
        into = c("WAGE_RATE_OF_PAY_FROM", "WAGE_RATE_OF_PAY_TO"),
        sep = "\\s*-\\s*", 
        fill = "right",
        remove = FALSE
      ) %>%
      mutate(
        # Remove commas and convert to numeric
        WAGE_RATE_OF_PAY_FROM = as.numeric(str_replace_all(WAGE_RATE_OF_PAY_FROM, ",", "")),
        WAGE_RATE_OF_PAY_TO   = as.numeric(str_replace_all(WAGE_RATE_OF_PAY_TO, ",", "")),
        # Replace missing ceilings with 0
        WAGE_RATE_OF_PAY_TO   = if_else(is.na(WAGE_RATE_OF_PAY_TO), 0, WAGE_RATE_OF_PAY_TO)
      ) %>%
      select(-WAGE_RATE_OF_PAY)
  }
  
  file = file %>%
    # clean up monetary variables
    mutate(across(c(WAGE_RATE_OF_PAY_FROM, WAGE_RATE_OF_PAY_TO, PREVAILING_WAGE),
                  ~(as.numeric(str_replace_all(., c("\\$"="", ","=""))))
    )) %>% mutate(FISCAL_YEAR = year)
  
  years[[year]] = file
}

# combine
lca_2015_2019 = bind_rows(years) %>% distinct()
lca_2015_2024 = bind_rows(lca_2015_2019, lca_2020_2024)
rm(lca_2015_2019, lca_2020_2024, years, quarters, file) # clean up

lca_2015_2024 = lca_2015_2024 %>%      
  mutate(AVG_PAY = case_when(
    is.na(WAGE_RATE_OF_PAY_FROM) & is.na(WAGE_RATE_OF_PAY_TO) ~  NA,
    is.na(WAGE_RATE_OF_PAY_FROM) & !is.na(WAGE_RATE_OF_PAY_TO) ~ WAGE_RATE_OF_PAY_TO,
    !is.na(WAGE_RATE_OF_PAY_FROM) & is.na(WAGE_RATE_OF_PAY_TO) ~ WAGE_RATE_OF_PAY_FROM,
    !is.na(WAGE_RATE_OF_PAY_FROM) & !is.na(WAGE_RATE_OF_PAY_TO) ~  (WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO)/2)) %>%
  
  # make time period adjustments. assuming 52 weeks per year, 40 hrs a week for full time
  mutate(ADJUSTED_PAY = case_when(
    WAGE_UNIT_OF_PAY== "Year" ~ AVG_PAY,
    WAGE_UNIT_OF_PAY== "Week" ~ AVG_PAY*52,
    WAGE_UNIT_OF_PAY == "Month" ~ AVG_PAY*12,
    WAGE_UNIT_OF_PAY == "Hour" ~ AVG_PAY*2080,
    WAGE_UNIT_OF_PAY == "Bi-Weekly" ~ AVG_PAY*26),
    
    WAGE_RATE_OF_PAY_FROM_annual = case_when(
      WAGE_UNIT_OF_PAY== "Year" ~ WAGE_RATE_OF_PAY_FROM,
      WAGE_UNIT_OF_PAY== "Week" ~ WAGE_RATE_OF_PAY_FROM*52,
      WAGE_UNIT_OF_PAY == "Month" ~ WAGE_RATE_OF_PAY_FROM*12,
      WAGE_UNIT_OF_PAY == "Hour" ~ WAGE_RATE_OF_PAY_FROM*2080,
      WAGE_UNIT_OF_PAY == "Bi-Weekly" ~ WAGE_RATE_OF_PAY_FROM*26),
    
    WAGE_RATE_OF_PAY_TO_annual = case_when(
      WAGE_UNIT_OF_PAY== "Year" ~ WAGE_RATE_OF_PAY_TO,
      WAGE_UNIT_OF_PAY== "Week" ~ WAGE_RATE_OF_PAY_TO*52,
      WAGE_UNIT_OF_PAY == "Month" ~ WAGE_RATE_OF_PAY_TO*12,
      WAGE_UNIT_OF_PAY == "Hour" ~ WAGE_RATE_OF_PAY_TO*2080,
      WAGE_UNIT_OF_PAY == "Bi-Weekly" ~ WAGE_RATE_OF_PAY_TO*26),
    
    PREVAILING_WAGE_ANNUAL = case_when(
      WAGE_UNIT_OF_PAY== "Year" ~ PREVAILING_WAGE,
      WAGE_UNIT_OF_PAY== "Week" ~ PREVAILING_WAGE*52,
      WAGE_UNIT_OF_PAY == "Month" ~ PREVAILING_WAGE*12,
      WAGE_UNIT_OF_PAY == "Hour" ~ PREVAILING_WAGE*2080,
      WAGE_UNIT_OF_PAY == "Bi-Weekly" ~ PREVAILING_WAGE*26)
  )

# flag the probable incorrectly coded wages
# note: electing to use a lower bound of 30,000

lca_2015_2024 = lca_2015_2024 %>%      
  mutate(WAGE_UNIT_FLAG_LCA = case_when(
    
    # use average pay to flag -- this will catch them.
    AVG_PAY < 500 &  WAGE_UNIT_OF_PAY !="Hour" ~ "likely hour",
    AVG_PAY > 1000 & WAGE_UNIT_OF_PAY == "Hour" ~ "too high for hour",
    AVG_PAY > 20000 & WAGE_UNIT_OF_PAY =="Week" ~ "too high for weeky",
    AVG_PAY < 1000 & WAGE_UNIT_OF_PAY == "Month" ~ "too low for month",
    AVG_PAY > 100000 & WAGE_UNIT_OF_PAY == "Month" ~ "too high for month",
    AVG_PAY < 30000 & WAGE_UNIT_OF_PAY == "Year" ~ "too low for year",           
    AVG_PAY > 1000000 & WAGE_UNIT_OF_PAY == "Year" ~ "suspiciously high for year",
    TRUE ~ "not flagged"
  ))


lca_2015_2024 = lca_2015_2024 %>% select(
  CASE_NUMBER, 
  lca_case_status = CASE_STATUS,
  lca_decision_date = DECISION_DATE,
  lca_adj_pay = ADJUSTED_PAY,
  lca_wage_unit_flag = WAGE_UNIT_FLAG_LCA,
  lca_wages_lower_bound = WAGE_RATE_OF_PAY_FROM_annual,
  lca_wages_upper_bound = WAGE_RATE_OF_PAY_TO_annual,
  PW_WAGE_LEVEL,
  SOC_CODE,
  SOC_TITLE,
  WORKSITE_COUNTY,
  WORKSITE_STATE
)

# save cleaned lca file.
save(lca_2015_2024, file = file.path(cleaned_path, "lca_2015_2024.RData"))

load(file.path(cleaned_path, "lca_2015_2024.RData"))

################################################################################
# pull in 2021-2024 H1-B FOIA data and clean names
setwd(foia_path)

fys <- bind_rows(
  read.csv("TRK_13139_FY2021.csv"),
  read.csv("TRK_13139_FY2022.csv"),
  read.csv("TRK_13139_FY2023.csv"),
  read.csv("TRK_13139_FY2024_single_reg.csv"),
  read.csv("TRK_13139_FY2024_multi_reg.csv")
)

fys <- fys %>%
  rename(
    registration_id	= bcn,
    registration_birth_country	= country_of_birth,
    registration_nationality	= country_of_nationality,
    registration_dob	= ben_date_of_birth,
    registration_birth_year	= ben_year_of_birth,
    registration_gender	= gender,
    registration_employer_name	= employer_name,
    registration_FEIN	= FEIN,
    registration_empl_addr = mail_addr,
    registration_empl_city = city,
    registration_empl_state	= state,
    registration_empl_zip	= zip,
    registration_agent_first	= agent_first_name,
    registration_agent_last	= agent_last_name,
    registration_lottery_year	= lottery_year,
    registration_status	= status_type,
    registration_duplicate	= ben_multi_reg_ind,
    petition_id	= RECEIPT_NUMBER,
    petition_recieve_date	= rec_date,
    petition_decision	= FIRST_DECISION,
    petition_decision_date	= first_decision_date,
    petition_employer_name	= i129_employer_name,
    petition_empl_addr	= PET_STREET,
    petition_empl_city	= PET_CITY,
    petition_empl_state	= PET_STATE,
    petition_empl_zip	= PET_ZIP,
    petition_request_class	= REQUESTED_CLASS,
    petition_classificiation_basis	= BASIS_FOR_CLASSIFICATION,
    petition_request_action	= REQUESTED_ACTION,
    petition_beneficiary_num	= NUMBER_OF_BENEFICIARIES,
    petition_beneficiary_gender	= BEN_SEX,
    petition_beneficiary_birth_country	= BEN_COUNTRY_OF_BIRTH,
    petition_beneficiary_classif	= BEN_CURRENT_CLASS,
    petition_beneficiary_dot_title = JOB_TITLE,
    petition_worksite_addr = WORKSITE_STREET,
    petition_worksite_city	= WORKSITE_CITY,
    petition_worksite_state	= WORKSITE_STATE,
    petition_worksite_zip	= WORKSITE_ZIP,
    petition_beneficiary_full_time	= FULL_TIME_IND,
    petition_beneficiary_wage	= WAGE_AMT,
    petition_beneficiary_wage_unit	= WAGE_UNIT,
    petition_classif_valid_start_date	= valid_from,
    petition_classif_valid_end_date	= valid_to,
    petition_n_employees	= NUM_OF_EMP_IN_US,
    petition_employer_h1b_dependent	= S1Q1A,
    petition_wilful_violator	= S1Q1B,
    petition_beneficiary_edu_code	= BEN_EDUCATION_CODE,
    petition_beneficiary_edu_defin	= ED_LEVEL_DEFINITION,
    petition_beneficiary_field	= BEN_PFIELD_OF_STUDY,
    petition_annual_pay	= BEN_COMP_PAID,
    petition_beneficiary_dot_occ_code	= DOT_CODE,
    petition_employer_naics	= NAICS_CODE,
    petition_h1b_type	= S3Q1,
    petition_beneficiary_off_site_work	= S4Q1,
    petition_beneficiary_protected_population	= T_U_VAWA_FLAG) %>%
  relocate(registration_lottery_year)

# drop duplicates and non-usable obs censored under FOIA
fys <- fys %>% distinct() %>% filter(registration_birth_country != "(b)(3) (b)(6) (b)(7)(c)")

# This step filters only for lottery winners who filed applications
# format DOL case numbers prior to linking with LCA applications
fys <- fys %>%
  filter(registration_status =="SELECTED" & petition_decision %in% c("Approved", "Denied")) %>%
  mutate(applicant_id = row_number()) %>%
  mutate(CASE_NUMBER = gsub("^(.)(.{3})(.{5})(.{6})$", "\\1-\\2-\\3-\\4", DOL_ETA_CASE_NUMBER)) %>%
  left_join(lca_2015_2024, by ="CASE_NUMBER") %>%
  relocate(applicant_id) %>% distinct(applicant_id, .keep_all = TRUE)

remove(lca_2015_2024)

################################################################################
# adjust wage data
# if pay is obviously hourly and not identified as such, convert to annual based on full/part time status
# assume full time is 40 hrs a week * 52 weeks a year, part time 20 hrs a week * 52 weeks a year

fys <- fys %>%
  mutate(petition_annual_pay = as.numeric(petition_annual_pay),
         petition_beneficiary_wage = as.numeric(petition_beneficiary_wage)) %>%
  mutate(petition_annual_pay_clean = ifelse(
    is.na(petition_annual_pay) & !is.na(petition_beneficiary_wage),
    petition_beneficiary_wage,
    petition_annual_pay
  )) %>%
  mutate(petition_annual_pay_clean = case_when(
    petition_annual_pay_clean < 10 ~ NA,
    petition_annual_pay_clean >= 10 & petition_annual_pay_clean < 500 & (petition_beneficiary_full_time != "N" | is.na(petition_beneficiary_full_time)) ~ petition_annual_pay_clean * 2080, # hourly
    petition_annual_pay_clean >= 10 & petition_annual_pay_clean < 500 & petition_beneficiary_full_time == "N" ~ petition_annual_pay_clean * 1040, # hourly
    TRUE ~ petition_annual_pay_clean
  )) %>%
  mutate(petition_annual_pay_clean = ifelse(
      petition_annual_pay_clean >= 500 & petition_annual_pay_clean < 30000,
      NA, petition_annual_pay_clean)) %>%
  mutate(petition_annual_pay_clean = case_when( # No valid I-129 wage, LCA wage available
    is.na(petition_annual_pay_clean) & !is.na(lca_adj_pay) ~ lca_adj_pay,
    petition_annual_pay_clean == as.numeric(petition_employer_naics) ~ lca_adj_pay,
    TRUE ~ petition_annual_pay_clean
  ))

wage_99th <- quantile(fys$petition_annual_pay_clean, 0.99, na.rm = TRUE)

# handle outliers
fys <- fys %>%
  mutate(
    flag_top1 = petition_annual_pay_clean >= wage_99th,
    foia_lca_pay_ratio = petition_annual_pay_clean/lca_adj_pay,
    foia_pay_wage_ratio = petition_annual_pay_clean/petition_beneficiary_wage
  ) %>%
  mutate(
    high_wage_adj = case_when(
      # if wages are in the top 99 percentile & are a multiple of the LCA wage, divide by the closest power of 10 to the I-129 to LCA wage ratio
      flag_top1 == TRUE & foia_lca_pay_ratio >= 9.5 ~ petition_annual_pay_clean / (10^round(log10(foia_lca_pay_ratio), 0)),
      # if I-129 annual pay is multiples of powers of 10 of I-129 wage, divide by that
      flag_top1 == TRUE & petition_beneficiary_wage >= 30000 &
        ((foia_pay_wage_ratio >= 9.5 & foia_pay_wage_ratio <= 10.5) |
           (foia_pay_wage_ratio >= 95 & foia_pay_wage_ratio <= 105) |
           (foia_pay_wage_ratio >= 950 & foia_pay_wage_ratio <= 1050)) ~ petition_annual_pay_clean / (10^round(log10(foia_pay_wage_ratio), 0)),
      # if I-129 wages are more than twice and less than 10 times the LCA wages, replace with LCA wages
      flag_top1 == TRUE & foia_lca_pay_ratio >= 2 & foia_lca_pay_ratio < 9.5 ~ lca_adj_pay,
      flag_top1 == TRUE & petition_annual_pay > 1000000 & !is.na(lca_adj_pay) & lca_adj_pay < 1000000 ~ lca_adj_pay,
      TRUE ~ NA
    ),
    low_wage_adj = ifelse(
      flag_top1 == FALSE & foia_lca_pay_ratio >= 0.105 & foia_lca_pay_ratio <= 0.95 &
        lca_adj_pay < 1000000 & (is.na(lca_wages_upper_bound) | lca_adj_pay <= lca_wages_upper_bound),
      lca_adj_pay, NA
    ),
    petition_annual_pay_flag = case_when(
      petition_annual_pay == petition_annual_pay_clean | petition_beneficiary_wage == petition_annual_pay_clean ~ 0,
      !is.na(high_wage_adj) | !is.na(low_wage_adj) ~ 1,
      TRUE ~ 1
    ),
    petition_annual_pay_clean = case_when(
      !is.na(high_wage_adj) ~ high_wage_adj,
      !is.na(low_wage_adj) ~ low_wage_adj,
      TRUE ~ petition_annual_pay_clean
    )
  ) %>%
  select(-c(flag_top1, high_wage_adj, low_wage_adj, foia_lca_pay_ratio, foia_pay_wage_ratio))

# Filter out those making over 8 million and those having no wage whatsoever
fys <- fys %>% filter(!is.na(petition_annual_pay_clean),
                      petition_annual_pay_clean < 8000000)

# Filter for unique cases based on LCA application case number
picked <- fys %>%
  filter(registration_duplicate == 1) %>%
  group_by(
    registration_lottery_year,
    DOL_ETA_CASE_NUMBER,
    petition_beneficiary_birth_country,
    registration_nationality,
    registration_birth_year,
    petition_beneficiary_gender,
    petition_beneficiary_edu_code,
    petition_beneficiary_field,
    petition_beneficiary_classif,
    petition_h1b_type,
    petition_request_class,
    petition_classificiation_basis,
    petition_request_action
  ) %>%
  mutate(
    .med  = median(petition_annual_pay_clean, na.rm = TRUE),
    .dist = abs(petition_annual_pay_clean - .med),
    .dist = if_else(is.na(.dist), Inf, .dist),  # NAs sort to the bottom
    .rand = runif(n())                           # random tie-breaker
  ) %>%
  arrange(.dist, .rand) %>%                      # median-closest first; ties random
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-.med, -.dist, -.rand)
fys <- fys %>%
  filter(registration_duplicate == 0) %>%
  bind_rows(picked)

################################################################################
# Additional variable cleaning: addresses, SOC titles.
      clean_addr_full <- function(street, city, state, zip) {
        street2 <- street %>%
          str_squish() %>%
          str_to_title() %>%
          str_replace_all(regex("\\bP\\.?O\\.? Box\\b", ignore_case = TRUE), "") %>%
          str_replace_all("#", " ")
        city2 <- str_to_title(city)
        state2 <- toupper(str_trim(state))
        zip2 <- str_extract(as.character(zip), "\\d{5}")
        paste0(street2, ", ", city2, ", ", state2, ifelse(!is.na(zip2), paste0(" ", zip2), ""))
      }
      
      replace_num_words <- function(x) {
        num_words <- c("one","two","three","four","five","six","seven","eight","nine","ten",
                       "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen",
                       "eighteen","nineteen","twenty")
        nums <- as.character(1:20)
        
        for (i in seq_along(num_words)) {
          x <- str_replace_all(x, paste0("\\b", num_words[i], "\\b"), nums[i])
        }
        return(x)
      }
      
      clean_addr <- function(addr) {
        addr <- tolower(addr)
        addr <- replace_num_words(addr)
        
        suffix_map <- c(
          " street" = " st",
          " avenue" = " ave",
          " boulevard" = " blvd",
          " road" = " rd",
          " drive" = " dr",
          " lane" = " ln",
          " court" = " ct",
          " place" = " pl",
          " square" = " sq",
          " terrace" = " ter",
          " parkway" = " pkwy",
          " highway" = " hwy"
        )
        for (suf in names(suffix_map)) {
          addr <- str_replace_all(addr, suf, suffix_map[suf])
        }
        addr <- str_squish(addr)
        addr <- str_replace_all(addr, ",,", ",")
        addr <- str_to_title(addr)
        
        return(addr)
      }

# Correct for those who wrote "same as part 1"
fys <- fys %>%
  mutate(petition_worksite_city = case_when(
    tolower(petition_worksite_addr) %in% c("same as part 1",
                                           "same address as part 1") ~ petition_empl_city,
    TRUE ~ petition_worksite_city
  ),
  petition_worksite_state = case_when(
    tolower(petition_worksite_addr) %in% c("same as part 1",
                                           "same address as part 1") ~ petition_empl_state,
    TRUE ~ petition_worksite_state
  ),
  petition_worksite_zip = case_when(
    tolower(petition_worksite_addr) %in% c("same as part 1",
                                           "same address as part 1") ~ petition_empl_zip,
    TRUE ~ petition_worksite_zip
  ),
  petition_worksite_addr = case_when(
    tolower(petition_worksite_addr) %in% c("same as part 1",
                                           "same address as part 1") ~ petition_empl_addr,
    TRUE ~ petition_worksite_addr
  ))

# Fix irregular addressees
fys <- fys %>%
  mutate(petition_po_box_flag = str_detect(coalesce(petition_worksite_addr, ""),
                                           regex("^\\s*p\\W*o\\W*box\\b", ignore_case = TRUE)),
         petition_worksite_addr =  replace_num_words(tolower(petition_worksite_addr)),
         petition_worksite_addr = clean_addr(petition_worksite_addr),
         
         petition_worksite_addr = str_to_title(petition_worksite_addr), 
         petition_worksite_city = str_to_title(petition_worksite_city), 
         petition_worksite_state = toupper(petition_worksite_state), 
         petition_worksite_zip = substr(str_pad(petition_worksite_zip, pad = "0", width = 5, side = "left"), 1, 5))

po_box_only <- fys %>% filter(petition_po_box_flag) %>%
  select(applicant_id, petition_empl_addr, petition_empl_city,
         petition_empl_state, petition_empl_zip,
         petition_worksite_addr, petition_worksite_city,
         petition_worksite_state, petition_worksite_zip, petition_po_box_flag) %>%
  mutate(petition_worksite_addr = case_when(
    petition_worksite_city == "Anahola" ~ "4333 Kukuihale Rd",
    petition_worksite_city == "Austin" ~ "1023 Springdale Rd",
    petition_worksite_city == "Aynor" ~ "709 N Main St",
    petition_worksite_city == "Bedford Hills" ~ "124 Davids Hill Rd",
    petition_worksite_city == "Blytheville" ~ "5929 East State Hwy 18",
    petition_worksite_city == "Boise" ~ "8000 S Federal Way",
    petition_worksite_city == "Bradford" ~ "101 Main Street",
    petition_worksite_city == "Brookville" ~ "175 Evans St",
    petition_worksite_city == "Buckingham" ~ "13115 W James Anderson Hwy",
    petition_worksite_city == "Clanton" ~ "114 Chilton Pl",
    petition_worksite_city == "Clearwater" ~ "830 County Rd 75",
    petition_worksite_city == "Colchester" ~ "480 Roosevelt Hwy",
    petition_worksite_city == "Colorado Springs" ~ "4615 Northpark Dr Ste 202",
    petition_worksite_city == "Columbus" ~ "52 E Gay St",
    petition_worksite_city == "Dallas" ~ "14785 Preston Rd Ste 370",
    petition_worksite_city == "Des Moines" ~ "7300 Westown Pkwy Ste 110",
    petition_worksite_city == "Duluth" ~ "3296 Summit Ridge Pkwy Ste 110",
    petition_worksite_city == "Fairfield" ~ "1891 Woolner Ave Ste G",
    petition_worksite_city == "Frisco" ~ "40 W Main St",
    petition_worksite_city == "Gig Harbor" ~ "3212 50th St Ct Ste 100",
    petition_worksite_city == "Grand Canyon" ~ "3551 Airport Rd",
    petition_worksite_city == "Hammond" ~ "19325 Hipark Blvd",
    petition_worksite_city == "Hanover" ~ "21 Great Hollow Road",
    petition_worksite_city == "Hartford" ~ "1 American Row",
    petition_worksite_city == "Henderson" ~ "2289 Ross Mill Rd",
    petition_worksite_city == "Honolulu" ~ "5554 Poola St",
    petition_worksite_city == "Mercerville" ~ "1741 Whitehorse Mercerville Rd",
    petition_worksite_city == "Minneapolis" ~ "400 S 4th St Ste 401",
    petition_worksite_city == "Moffett Field" ~ "Nasa Research Park Bldg 19 Ste 1078",
    petition_worksite_city == "Norwalk" ~ "45 Glover Ave",
    petition_worksite_city == "Pflugerville" ~ "1508 Dessau Ridge Ln Ste 703",
    petition_worksite_city == "Point Comfort" ~ "201 Formosa Dr",
    petition_worksite_city == "Rancho Cordova" ~ "11150 International Dr",
    petition_worksite_city == "Reading" ~ "2675 Morgantown Rd",
    petition_worksite_city == "Rogers" ~ "118 N 13th St",
    
    applicant_id == 5650 ~ "13254 Forkland Dr",
    applicant_id == 47407 ~ "6500 Meyer Way Suite 100",
    
    petition_worksite_addr == "Po Box 278642" ~ "2921 Stockton Blvd Ste 1810",
    petition_worksite_addr == "Po Box 276367" ~ "3131 Fite Cir",
    petition_worksite_addr == "P O Box 10462" ~ "60 Belvedere St",
    petition_worksite_addr == "P O Box 1328" ~ "809 Laurel Street, Ste 1328",
    petition_worksite_addr == "P O Box 2127" ~ "717 W Sprague Ave",
    petition_worksite_addr == "P O Box 305" ~ "772 Empire Expy",
    petition_worksite_addr == "P O Box 331" ~ "48 Ulm Vaughn Rd",
    petition_worksite_addr == "P O Box 368" ~ "1 American Sq Ste 368",
    petition_worksite_addr == "P.o.box 867 1107 Fair Oaks Ave." ~ "1107 Fair Oaks Ave",
    petition_worksite_addr == "Po Box 10016" ~ "1323 W Colton Ave Ste 105",
    petition_worksite_addr == "Po Box 10327" ~ "218 Elks Point Rd Ste 10327",
    petition_worksite_addr == "Po Box 136" ~ "One Sansome St Ste 1400",
    petition_worksite_addr == "Po Box 1415" ~ "1970 Montauk Hwy",
    petition_worksite_addr == "Po Box 1496" ~ "6 Sutton Square",
    petition_worksite_addr == "Po Box 15073" ~ "3609 Kermit Hwy",
    petition_worksite_addr == "Po Box 184" ~ "397 Williams St",
    petition_worksite_addr == "Po Box 2013" ~ "2552 Jefferson Ave",
    petition_worksite_addr == "Po Box 2128" ~ "Las Casas Street Lot 20 Bairoa Industrial Park",
    petition_worksite_addr == "Po Box 2338" ~ "300 Yorktown Plz Ste 315",
    petition_worksite_addr == "Po Box 2846" ~ "1701 Commerce St",
    petition_worksite_addr == "Po Box 287" ~ "5 Industrial Dr W",
    petition_worksite_addr == "Po Box 320250" ~ "983 University Ave Bldg A",
    petition_worksite_addr == "Po Box 35" ~ "7551 Hwy 50, 79th St NW House",
    petition_worksite_addr == "Po Box 3522" ~ "1 Capitol Mall",
    petition_worksite_addr == "Po Box 369" ~ "20 Noble Ct",
    petition_worksite_addr == "Po Box 3759" ~ "36 West 20th St 6th Floor",
    petition_worksite_addr == "Po Box 463 19260 Cr 46" ~ "19260 Co Rd 46",
    petition_worksite_addr == "Po Box 49" ~ "Nasa Research Park Bldg 19 Ste 1078",
    petition_worksite_addr == "Po Box 515381 Pmb 77107" ~ "12100 Wilshire Blvd Flr 8",
    petition_worksite_addr == "Po Box 540364" ~ "4848 S 180th St",
    petition_worksite_addr == "Po Box 56066" ~ "11363 San Jose Blvd Ste 100",
    petition_worksite_addr == "Po Box 6238" ~ "111 W Evelyn Ave Ste 101",
    petition_worksite_addr == "Po Box 651" ~ "4543 Route 27",
    petition_worksite_addr == "Po Box 70280" ~ "377 S Daniel Way",
    petition_worksite_addr == "Po Box 77490" ~ "699 Mississippi St Apt 108",
    petition_worksite_addr == "Po Box 880" ~ "601 Main St",
    petition_worksite_addr == "Po Box 9024051" ~ "CPM Plaza Corporate Office Park, 44 Road 20 Suite 201",
    
    petition_worksite_addr == "Po Box 14987" ~ "7200-10 Kit Creek Rd",
    str_detect(petition_worksite_addr, "12195") ~ "	3039 Cornwallis Rd",
    TRUE ~ str_to_title(clean_addr(replace_num_words(tolower(petition_empl_addr))))),
    
    petition_worksite_state = case_when(
      petition_worksite_city == "Aroda" ~ "NC",
      TRUE ~ petition_worksite_state),
    
    petition_worksite_zip = case_when(
      petition_worksite_city == "Aroda" ~ "27709",
      petition_worksite_city == "Pflugerville" ~ "78754",
      petition_worksite_city == "Research Triangle Pa" ~ "27709",
      TRUE ~ petition_worksite_zip),
    
    petition_worksite_city = case_when(
      petition_worksite_city == "Aroda" ~ "Durham",
      petition_worksite_city == "Mc Laughlin" ~ "McLaughlin",
      petition_worksite_city == "Mountain View" ~ "Moffett Field",
      petition_worksite_city == "Pflugerville" ~ "Austin",
      petition_worksite_city == "Research Triangle Pa" ~ "Durham",
      TRUE ~ petition_worksite_city),
    
    petition_po_box_flag = (petition_worksite_addr %in%
                              c("P.o. Box 944", "Po Box 27573", "Po Box 37779",
                                "Po Box 50039", "Po Box 58"))
  )

fys <- bind_rows(
  fys %>% filter(petition_po_box_flag == FALSE),
  fys %>% filter(petition_po_box_flag == TRUE) %>%
    select(-c(petition_worksite_addr, petition_worksite_city,
              petition_worksite_state, petition_worksite_zip,
              petition_po_box_flag)) %>%
    left_join(po_box_only %>% select(applicant_id, petition_worksite_addr,
                                     petition_worksite_city, petition_worksite_state,
                                     petition_worksite_zip, petition_po_box_flag),
              by = "applicant_id")
  )

fys <- fys %>%
  # construct full address
  mutate(petition_full_address = clean_addr_full(
    street = petition_worksite_addr,
    city   = petition_worksite_city,
    state  = petition_worksite_state,
    zip    = petition_worksite_zip
  )) %>%
  mutate(SOC_TITLE = str_to_title(SOC_TITLE)) #note that this combines the handful of 7 digit SOCs with 9 digit SOCS, where 7 and 9 have the same SOC name.

################################################################################
# Match zip codes to counties first
zip_cty_crosswalk <- read_xlsx(file.path(data_path, "Other Data/ZIP_COUNTY_122024.xlsx"))
zip_cty_crosswalk <- zip_cty_crosswalk %>%
  group_by(ZIP, USPS_ZIP_PREF_STATE) %>%
  slice(which.max(TOT_RATIO)) %>% ungroup() %>%
  select(ZIP, USPS_ZIP_PREF_STATE, COUNTY, TOT_RATIO) %>%
  rename(state = USPS_ZIP_PREF_STATE, petition_worksite_county_fips = COUNTY,
         matching_ratio_hud = TOT_RATIO)

fys <- fys %>% left_join(zip_cty_crosswalk, by = join_by("petition_worksite_zip" == "ZIP",
                                                         "petition_worksite_state" == "state")) %>%
  mutate(valid_zip_match = (matching_ratio_hud >= 0.9 & !is.na(matching_ratio_hud)))

################################################################################
# Geocode addresses that don't have a high-confidence zip-county match
fys_to_geocode <- fys %>% filter(!valid_zip_match) %>%
  select(applicant_id, petition_worksite_addr, petition_worksite_city,
         petition_worksite_county_fips, petition_worksite_state,
         petition_worksite_zip, petition_full_address) %>%
  ungroup()

chunks <- split(fys_to_geocode, ceiling(seq_along(fys_to_geocode$petition_full_address)/4000))

geocode_chunk <- function(chunk) {
  geocode(chunk, address = petition_full_address, method = "census",
          lat = latitude, long = longitude, full_results = TRUE)
}

geocoded_list <- map(chunks, geocode_chunk)
geocoded_all <- bind_rows(geocoded_list)

geocoded_all %>% summarise(
  n = n(),
  n_missing_lat = sum(is.na(latitude)),
  n_missing_lon = sum(is.na(longitude)),
  lat_min = min(latitude, na.rm = TRUE),
  lat_max = max(latitude, na.rm = TRUE),
  lon_min = min(longitude, na.rm = TRUE),
  lon_max = max(longitude, na.rm = TRUE)
) %>% print()

write.csv(geocoded_all, file.path(cleaned_path, "fys_geocoded_pre_xwalk.csv"),
          row.names = FALSE)

# Map using geometry for those with exact matches
geocoded_all <- read.csv(file.path(cleaned_path, "fys_geocoded_pre_xwalk.csv"))
geocode_valid <- geocoded_all %>% filter(!is.na(latitude))

# Identify possible errors
geocode_valid <- geocode_valid %>%
  mutate(lat_bad = !(latitude >= -90 & latitude <= 90),
         lon_bad = !(longitude >= -180 & longitude <= 180))
sum(geocode_valid$lat_bad)
sum(geocode_valid$lon_bad)

# Try using arcgis to geocode those Census can't identify
geocode_arcgis <- geocode(geocoded_all %>% filter(is.na(latitude)) %>% select(1:7),
                          address = petition_full_address,
                          method = "arcgis",
                          lat = latitude, long = longitude,
                          full_results = TRUE)
write.csv(geocode_arcgis, file.path(cleaned_path, "fys_geocoded_arcgis.csv"))

geocode_arcgis <- read.csv(file.path(cleaned_path, "fys_geocoded_arcgis.csv"))
geocode_valid <- bind_rows(
  geocode_valid %>% select(1:9, matched_address),
  geocode_arcgis %>% filter(score >= 84) %>%
    select(2:10, attributes.LongLabel) %>%
    rename(matched_address = attributes.LongLabel) %>%
    mutate(matched_address = str_replace(matched_address, ", USA", "")))

# download county geometries (cb=TRUE returns simpler coastlines)
counties_sf <- tigris::counties(cb = TRUE, year = 2024) %>% st_transform(4326)

# Add in county fips codes
successful_geocodes <- geocode_valid %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_join(., counties_sf %>% select(GEOID, NAME, STATEFP), left = TRUE) %>%
  st_drop_geometry() %>%
  select(applicant_id, GEOID) %>%
  rename(geocoded_county = GEOID)

# 99% of the addresses without valid zip code crosswalks are geocoded successfully
nrow(successful_geocodes) / nrow(fys_to_geocode)

# Merge back to the main data frame
remove(chunks, geocoded_list, geocode_arcgis)
fys <- fys %>%
  select(-matching_ratio_hud) %>%
  left_join(successful_geocodes, by = "applicant_id") %>%
  mutate(petition_worksite_county_fips = ifelse(is.na(geocoded_county),
                                                petition_worksite_county_fips,
                                                geocoded_county),
         valid_zip_geocode = valid_zip_match | !is.na(geocoded_county)) %>%
  select(-c(geocoded_county, valid_zip_match))

# 3 entries have no usable address in both I-129 and LCA, drop them
fys %>% filter(valid_zip_geocode == FALSE & is.na(WORKSITE_COUNTY))
fys <- fys %>% filter(valid_zip_geocode == TRUE | !is.na(WORKSITE_COUNTY))

################################################################################
# Crosswalk counties to MSAs
cty_msa_crosswalk <- read_xlsx(file.path(data_path, "Other Data/area_definitions_m2023.xlsx"))
cty_msa_crosswalk <- cty_msa_crosswalk %>%
  mutate(county_fips = paste0(`FIPS code`, `County code`),
         township_code = paste0(`FIPS code`, `Township code`)) %>%
  rename(MSA_code = `May 2023 MSA code`, MSA_name = `May 2023 MSA name`,
         state_abb = `State abbreviation`,
         county_town_name = `County name (or Township name for the New England states)`) %>%
  select(county_fips, township_code, county_town_name, state_abb, MSA_code, MSA_name) %>%
  mutate(county_town_name = str_to_title(county_town_name),
         county_town_name =  gsub(" Municipio", "", county_town_name),
         county_town_name = gsub(" Parish", "", county_town_name),
         county_town_name = gsub(" Borough", "", county_town_name),
         county_town_name = gsub("St. ", "St ", county_town_name),
         county_town_name = gsub("Ste. ", "St ", county_town_name),
         county_town_name = gsub(" Census Area", "", county_town_name),
         county_town_name = gsub(" Census  Area", "", county_town_name),
         county_town_name = ifelse(
           county_town_name == "Yellowstone National Park (Part)",
           "Yellowstone National Park",
           county_town_name
         ))

# Crosswalk counties to MSAs for those outside New England
fys_outside_NE <- fys %>%
  filter(!petition_worksite_state %in% c("MA", "CT", "RI", "ME", "NH", "VT")) %>%
  left_join(cty_msa_crosswalk %>% select(-township_code) %>%
              distinct(county_fips, .keep_all = TRUE),
            by = join_by("petition_worksite_county_fips" == "county_fips")) %>%
  select(-c(state_abb, county_town_name))

# Only entries outside NE without MSA matches are invalid counties or overseas territories
fys_outside_NE %>% filter(is.na(MSA_code)) %>%
  select(petition_full_address, petition_worksite_county_fips, valid_zip_geocode) %>%
  arrange(desc(valid_zip_geocode))

# Attempt to merge by town / city names in New England
cty_msa_xwalk_NE <- cty_msa_crosswalk %>%
  filter(state_abb %in% c("MA", "CT", "RI", "ME", "NH", "VT")) %>%
  mutate(county_town_name = gsub(" Town", "", county_town_name),
         county_town_name = gsub(" City", "", county_town_name)) %>%
  distinct(state_abb, county_town_name, .keep_all = TRUE)
fys_NE <- fys %>%
  filter(petition_worksite_state %in% c("MA", "CT", "RI", "ME", "NH", "VT")) %>%
  left_join(cty_msa_xwalk_NE,
            by = join_by("petition_worksite_state" == "state_abb",
                         "petition_worksite_city" == "county_town_name")) %>%
  select(-c(county_fips, township_code))

# Handle NE towns and cities without direct matches
fys_no_cty_msa_match <- fys_NE %>% filter(is.na(MSA_code))

zip_cbsa_crosswalk <- read_xlsx(file.path(data_path, "Other Data/ZIP_CBSA_122024.xlsx"))
zip_cbsa_crosswalk <- zip_cbsa_crosswalk %>%
  group_by(ZIP, USPS_ZIP_PREF_STATE) %>%
  slice(which.max(TOT_RATIO)) %>% ungroup() %>%
  select(ZIP, USPS_ZIP_PREF_STATE, CBSA, USPS_ZIP_PREF_CITY, TOT_RATIO) %>%
  rename(state_hud = USPS_ZIP_PREF_STATE, cbsa_hud = CBSA,
         cbsa_name_hud = USPS_ZIP_PREF_CITY, matching_ratio_hud = TOT_RATIO)

fys_no_cty_msa_fix <- fys_no_cty_msa_match %>%
  select(-c(MSA_code, MSA_name)) %>%
  left_join(zip_cbsa_crosswalk, by = join_by("petition_worksite_state" == "state_hud",
                                             "petition_worksite_zip" == "ZIP")) %>%
  mutate(cbsa_name_hud = str_to_title(cbsa_name_hud),
         cbsa_name_hud = case_when(
           petition_worksite_county_fips == "25025" |
             cbsa_name_hud == "Chestnut Hill" ~ "Boston",
           
           cbsa_name_hud == "Amston" ~ "Hebron",
           cbsa_name_hud == "Centerbrook" ~ "Essex",
           cbsa_name_hud == "Dennis Port" ~ "Dennis",
           cbsa_name_hud == "Devens" ~ "Ayer",
           cbsa_name_hud == "Essex Junction" ~ "Essex",
           cbsa_name_hud == "Florence" ~ "Northampton",
           cbsa_name_hud == "Greenville" ~ "Smithfield",
           cbsa_name_hud == "Hyannis" ~ "Barnstable",
           cbsa_name_hud == "Indian Orchard" ~ "Springfield",
           cbsa_name_hud == "Lakeville" ~ "Litchfield",
           petition_worksite_city == "Maiden" ~ "Malden",
           cbsa_name_hud == "Manchester" ~ "Manchester By The Sea",
           cbsa_name_hud == "Manville" ~ "Lincoln",
           cbsa_name_hud == "Mashantucket" ~ "Ledyard",
           cbsa_name_hud == "Mystic" ~ "Groton",
           cbsa_name_hud == "Needham Heights" ~ "Needham",
           cbsa_name_hud == "Northford" ~ "North Branford",
           cbsa_name_hud == "Old Greenwich" ~ "Greenwich",
           cbsa_name_hud == "Old Town" ~ "Penobscot",
           cbsa_name_hud == "Oakdale" ~ "Montville",
           cbsa_name_hud == "Pascoag" ~ "Burrillville",
           cbsa_name_hud == "Pinehurst" ~ "Billerica",
           cbsa_name_hud == "Plantsville" ~ "Southington",
           (cbsa_name_hud == "Riverside" & petition_worksite_state == "RI") |
             cbsa_name_hud == "Rumford" ~ "East Providence",
           cbsa_name_hud == "Riverside" & petition_worksite_state == "CT" ~ "Greenwich",
           cbsa_name_hud == "Rogers" ~ "Killingly",
           cbsa_name_hud == "Sandy Hook" ~ "Newtown",
           cbsa_name_hud == "Southport" ~ "Fairfield",
           cbsa_name_hud == "Saint Albans" ~ "St Albans",
           cbsa_name_hud == "Storrs Mansfield" ~ "Mansfield",
           cbsa_name_hud == "Unionville" ~ "Farmington",
           cbsa_name_hud == "Vernon Rockville" ~ "Vernon",
           cbsa_name_hud == "Wellesley Hills" ~ "Wellesley",
           cbsa_name_hud == "West Kingston" ~ "South Kingstown",
           cbsa_name_hud == "Woodstock Valley" ~ "Woodstock",
           cbsa_name_hud == "Whitinsville" ~ "Northbridge",
           cbsa_name_hud == "White River Junction" ~ "Hartford",
           cbsa_name_hud == "Willimantic" ~ "Windham",
           cbsa_name_hud == "Wyoming" ~ "Richmond",
           cbsa_name_hud %in% c("Foxboro", "Middleboro", "North Attleboro", "Tyngsboro") ~ str_replace(cbsa_name_hud, "boro", "borough"),
           str_detect(cbsa_name_hud, "Newton") | cbsa_name_hud == "Auburndale" |
             cbsa_name_hud == "Waban" | petition_worksite_city == "Auburndale" |
             petition_worksite_city == "Newton Center" ~ "Newton",
           
           cbsa_name_hud %in% c("North Billerica", "North Chelmsford", "North Clarendon", "North Dartmouth",
                                "North Dighton", "North Easton", "North Grafton", "North Granby",
                                "North Thetford") ~ str_replace(cbsa_name_hud, "North ", ""),
           cbsa_name_hud %in% c("West Chatham", "West Lebanon", "West Millbury", "West Yarmouth") ~ str_replace(cbsa_name_hud, "West ", ""),
           substr(cbsa_name_hud, 1, 5) == "East " ~ str_replace(cbsa_name_hud, "East ", ""),
           substr(cbsa_name_hud, 1, 6) == "South " ~ str_replace(cbsa_name_hud, "South ", ""),
           TRUE ~ cbsa_name_hud
         )) %>%
  left_join(cty_msa_xwalk_NE,
            by = join_by("petition_worksite_state" == "state_abb",
                         "cbsa_name_hud" == "county_town_name"))

# Combine back together
fys <- bind_rows(
  fys_outside_NE,
  fys_NE %>% filter(!is.na(MSA_code)),
  fys_no_cty_msa_fix %>% select(-c(cbsa_hud, cbsa_name_hud, matching_ratio_hud,
                                   county_fips, township_code))
)

# Fall back to LCA geographies
# Virgin Islands and Guam are the only ones left, we have covered everything
fys_LCA <- fys %>% filter(is.na(MSA_code)) %>%
  select(-c(MSA_code, MSA_name)) %>%
  mutate(WORKSITE_COUNTY = str_to_title(WORKSITE_COUNTY),
         WORKSITE_COUNTY = gsub(" County", "", WORKSITE_COUNTY),
         WORKSITE_COUNTY =  gsub(" Municipio", "", WORKSITE_COUNTY),
         WORKSITE_COUNTY = gsub(" Parish", "", WORKSITE_COUNTY),
         WORKSITE_COUNTY = gsub(" Borough", "", WORKSITE_COUNTY),
         WORKSITE_COUNTY = gsub("St. ", "St ", WORKSITE_COUNTY),
         WORKSITE_COUNTY = gsub(" Town", "", WORKSITE_COUNTY)) %>%
  left_join(cty_msa_crosswalk %>%
              mutate(county_town_name = gsub(" County", "", county_town_name)),
            by = join_by("WORKSITE_COUNTY" == "county_town_name",
                         "WORKSITE_STATE" == "state_abb")) %>%
  select(-c(county_fips, township_code))

fys <- bind_rows(fys %>% filter(!is.na(MSA_code)), fys_LCA)
remove(fys_NE, fys_outside_NE, fys_LCA, fys_no_cty_msa_fix, fys_no_cty_msa_match)

################################################################################
# Match DOT occupations with SOC occupations for those without SOC codes
library(fuzzyjoin)

# Normalize occupation titles: lowercase, trim, remove punctuation & multiple spaces
normalize_title <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

# Crosswalk 2010 SOC codes to 2018
soc_10_18_xwalk <- read_xlsx(file.path(data_path, "Other Data/soc_2010_to_2018_crosswalk.xlsx"),
                             skip = 8, sheet = "Sorted by 2010")
soc_10_18_xwalk <- soc_10_18_xwalk %>%
  filter(`2010 SOC Code` %in% (soc_10_18_xwalk %>%
                                 filter(`2010 SOC Code` != `2018 SOC Code`) %>%
                                 .$`2010 SOC Code`)) %>%
  mutate(SOC_title_18 = normalize_title(`2018 SOC Title`)) %>%
  select(SOC_10 = `2010 SOC Code`, SOC_18 = `2018 SOC Code`, SOC_title_18) %>%
  bind_rows(data.frame(
    SOC_10 = c("15-1034", "15-1035", "15-1037", "15-1053", "15-1054", "15-1055",
               "15-1295", "15-1296", "15-1297", "15-1217", "17-2074"),
    SOC_18 = c(rep("15-1252", 3),
               rep("15-1211", 3), 
               rep("15-1299", 3),
               "15-1212",
               "17-2071"),
    SOC_title_18 = c(rep("software developers", 3),
                     rep("computer systems analysts", 3),
                     rep("computer occupations all other", 3),
                     "information security analysts",
                     "electrical engineers")
  )) %>%
  group_by(SOC_10) %>% mutate(n_match_10 = n()) %>% ungroup()
soc_10_18_xwalk_single <- soc_10_18_xwalk %>% filter(n_match_10 == 1) %>% select(-c(n_match_10))
soc_10_18_xwalk_multi <- soc_10_18_xwalk %>% filter(n_match_10 != 1) %>% select(-c(n_match_10))

oflc_xwalk <- read.csv(file = file.path(data_path, "Other Data", "OFLC_Wages_2024-25", "xwalk_plus.csv"))
oflc_xwalk <- oflc_xwalk %>%
  select(TruncOnetCode, ONetTitle) %>%
  mutate(ONetTitle = normalize_title(ONetTitle)) %>%
  rename(SOC_18 = TruncOnetCode, SOC_TITLE = ONetTitle)

fys_xwalk <- fys %>%
  mutate(SOC_10 = substr(SOC_CODE, 1, 7),
         SOC_TITLE = normalize_title(SOC_TITLE)) %>%
  filter(registration_lottery_year < 2024,
         SOC_10 %in% soc_10_18_xwalk_multi$SOC_10) %>%
  distinct(SOC_10, SOC_TITLE) %>%
  left_join(oflc_xwalk, by = "SOC_TITLE")

fys_xwalk_matched <- fys_xwalk %>% filter(!is.na(SOC_18))
fys_xwalk_todo <- fys_xwalk %>% filter(is.na(SOC_18)) %>%
  mutate(.rid = row_number()) %>% select(-SOC_18) %>% relocate(.rid)

# Jaro–Winkler distance: 0 = exact match, 1 = far apart
fys_xwalk_todo <- stringdist_left_join(
  fys_xwalk_todo, oflc_xwalk, by = c("SOC_TITLE"),
  method = "jw", p = 0.1, max_dist = 0.35,  # tune max_dist as needed
  distance_col = "dist") %>%
  mutate(sim = 1 - dist) %>%
  group_by(.rid) %>%
  arrange(dist, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

fys_xwalk_best <- fys_xwalk_todo %>%
  filter(rank == 1) %>%
  rename(SOC_TITLE_fys = SOC_TITLE.x, best_SOC_TITLE_oflc = SOC_TITLE.y) %>%
  select(-rank) %>% arrange(desc(sim))

fys_xwalk_best <- fys_xwalk_best %>%
  mutate(
    SOC_18 = case_when(
      SOC_10 == "13-2051" ~ "13-2051",
      SOC_10 == "19-3031" ~ "19-3033",
      SOC_10 == "19-4041" ~ "19-4043",
      SOC_TITLE_fys == "investment underwriters" ~ "13-2053",
      SOC_TITLE_fys %in% c("electrical engineering technologists",
                           "electronics engineering technologists") ~ "17-3023",
      SOC_TITLE_fys == "electromechanical engineering technologists" ~ "17-3024",
      SOC_TITLE_fys == "pathologists" ~ "29-1222",
      SOC_TITLE_fys == "risk management specialists" ~ "13-2054",
      SOC_TITLE_fys == "it project managers" ~ "15-1299",
      TRUE ~ SOC_18
    ),
    best_SOC_TITLE_oflc = case_when(
      SOC_10 == "13-2051" ~ "financial and investment analysts",
      SOC_10 == "19-3031" ~ "clinical and counseling psychologists",
      SOC_10 == "19-4041" ~ "geological technicians except hydrologic technicians",
      SOC_TITLE_fys == "investment underwriters" ~ "insurance underwriters",
      SOC_TITLE_fys %in% c("electrical engineering technologists",
                           "electronics engineering technologists") ~ 
        "electrical and electronic engineering technologists and technicians",
      SOC_TITLE_fys == "electromechanical engineering technologists" ~ "electro mechanical and mechatronics technologists and technicians",
      SOC_TITLE_fys == "pathologists" ~ "physicians pathologists",
      SOC_TITLE_fys == "risk management specialists" ~ "financial risk specialists",
      SOC_TITLE_fys == "it project managers" ~ "information technology project managers",
      TRUE ~ best_SOC_TITLE_oflc
    ))

fys_xwalk <- bind_rows(
  fys_xwalk_matched %>% rename(SOC_title_10 = SOC_TITLE) %>%
    mutate(SOC_title_18 = SOC_title_10),
  fys_xwalk_best %>% select(2:5) %>% 
    rename(SOC_title_10 = SOC_TITLE_fys, SOC_title_18 = best_SOC_TITLE_oflc)
)

fys_soc_multi <- fys %>%
  mutate(SOC_CODE = substr(SOC_CODE, 1, 7),
         SOC_TITLE = normalize_title(SOC_TITLE)) %>%
  left_join(fys_xwalk, by = join_by("SOC_CODE" == "SOC_10",
                                    "SOC_TITLE" == "SOC_title_10"))

fys_soc_todo <- fys_soc_multi %>% filter(is.na(SOC_18)) %>%
  select(-c(SOC_18, SOC_title_18)) %>%
  left_join(soc_10_18_xwalk_single, by = join_by("SOC_CODE" == "SOC_10")) %>%
  mutate(SOC_18 = ifelse(is.na(SOC_18), SOC_CODE, SOC_18),
         SOC_title_18 = ifelse(is.na(SOC_title_18), SOC_TITLE, SOC_title_18))

fys <- bind_rows(
  fys_soc_multi %>% filter(!is.na(SOC_18)),
  fys_soc_todo
  ) %>%
  mutate(SOC_CODE = SOC_18, SOC_TITLE = SOC_title_18) %>%
  select(-c(SOC_18, SOC_title_18)) %>%
  mutate(SOC_CODE = substr(SOC_CODE, 1, 7), SOC_TITLE = normalize_title(SOC_TITLE)) %>%
  arrange(applicant_id)

remove(fys_soc_multi, fys_soc_todo)

# Filter out those with neither DOT code nor job title
fys_na_dot_job <- fys %>% filter(petition_beneficiary_dot_occ_code == "" &
                                   petition_beneficiary_dot_title == "")
nrow(fys_na_dot_job) / nrow(fys) # 0.8 percent have neither DOT code nor job title
nrow(fys_na_dot_job %>% filter(is.na(SOC_CODE))) # only 35 entries have no LCA fallback, safe to drop
fys <- fys %>% filter(!applicant_id %in% (fys_na_dot_job %>% filter(is.na(SOC_CODE)) %>% .$applicant_id))

# Clean DOT to SOC crosswalk
dot_soc_crosswalk <- read_xlsx(path = file.path(data_path, "Other Data/DOT_to_ONET_SOC.xlsx"), skip = 3)
dot_soc_crosswalk <- dot_soc_crosswalk %>%
  mutate(DOT_category = substr(`DOT Code`, 1, 3),
         dot_title_norm = normalize_title(`DOT Title`),
         soc_title_norm = normalize_title(`O*NET-SOC 2019 Title`)) %>%
  rename(SOC = `O*NET-SOC 2019 Code`) %>%
  select(DOT_category, dot_title_norm, SOC, soc_title_norm) %>%
  distinct(DOT_category, SOC, soc_title_norm, .keep_all = TRUE)

fys_na_soc <- fys %>%
  mutate(job_norm = normalize_title(petition_beneficiary_dot_title)) %>%
  filter(is.na(SOC_CODE)) %>%
  rename(DOT_category = petition_beneficiary_dot_occ_code) %>%
  select(applicant_id, DOT_category, job_norm,
         registration_employer_name, petition_beneficiary_field)

# Export for use in machine learning algorithm
write.csv(dot_soc_crosswalk,
          file = file.path(dot_matching_path, "DOT_SOC_xwalk.csv"),
          row.names = FALSE)
write.csv(fys %>%
            mutate(job_norm = normalize_title(petition_beneficiary_dot_title)) %>%
            filter(!is.na(SOC_CODE)) %>%
            rename(DOT_category = petition_beneficiary_dot_occ_code,
                   SOC = SOC_CODE, soc_title_norm = SOC_TITLE) %>%
            select(applicant_id, DOT_category, job_norm, SOC, soc_title_norm,
                   registration_employer_name, petition_beneficiary_field),
          file = file.path(dot_matching_path, "Applicants with SOC codes and titles.csv"),
          row.names = FALSE)
write.csv(fys_na_soc %>% filter(!is.na(job_norm), job_norm != ""),
          file = file.path(dot_matching_path, "Applicants without SOC codes.csv"),
          row.names = FALSE)

# First try matching using the DOT to SOC crosswalk, then try learning from existing data
setwd(dot_matching_path)

system('python3 dot_soc_xwalk_match.py')
xwalk_matched <- read.csv("Applicants with SOC codes (semantic xwalk).csv") %>%
  filter(!is.na(similarity_score), similarity_score != 0)
xwalk_matched <- xwalk_matched %>%
  filter(!predicted_SOC_title %in% c("curators", "chief sustainability officers",
                                     "biofuels biodiesel technology and product development managers",
                                     "camera operators television video and film",
                                     "biostatisticians"),
         !(predicted_SOC_title == "materials engineers" & str_detect(job_norm, "design engineer")),
         job_norm != "music teacher",
         similarity_score > mean(xwalk_matched$similarity_score)) %>%
  mutate(predicted_SOC = ifelse(
            str_detect(job_norm, "clinical laboratory scien"),
            "29-2011.00", predicted_SOC),
         predicted_SOC_title = ifelse(
            str_detect(job_norm, "clinical laboratory scien"),
            "medical and clinical laboratory technologists", predicted_SOC_title))

fys_na_soc <- fys_na_soc %>%
  filter(!applicant_id %in% xwalk_matched$applicant_id)
write.csv(fys_na_soc,
          file = file.path(dot_matching_path, "Applicants without xwalk matches.csv"),
          row.names = FALSE)

system('python3 dot_soc_matching.py')
fys_fitted <- read.csv(file = file.path(dot_matching_path, "Applicants with predicted SOC codes (learned from data).csv"))
fys_fitted <- fys_fitted %>% filter(alt1_prob >= 0.9) %>%
  select(applicant_id, predicted_SOC_code, predicted_SOC_title) %>%
  bind_rows(xwalk_matched %>% select(applicant_id, predicted_SOC, predicted_SOC_title) %>%
              rename(predicted_SOC_code = predicted_SOC))

# About 3,200 out of 4,900 have high confidence matches
# Keep those without good occupation matches as long as they have valid LCA wage level
fys <- fys %>%
  left_join(fys_fitted, by = "applicant_id") %>%
  mutate(SOC_CODE = ifelse(is.na(SOC_CODE),
                           substr(predicted_SOC_code, 1, 7),
                           SOC_CODE),
         SOC_TITLE = ifelse(is.na(SOC_TITLE),
                            predicted_SOC_title,
                            SOC_TITLE)) %>%
  select(-c(predicted_SOC_code, predicted_SOC_title)) %>%
  filter(!is.na(SOC_CODE) | !is.na(PW_WAGE_LEVEL)) %>%
  filter(!is.na(SOC_CODE) | !is.na(petition_full_address))

# Handle the mis-formatted SOC codes
fys_misform_SOC <- fys %>% filter(!str_detect(SOC_CODE, "-")) %>%
  select(applicant_id, SOC_CODE, SOC_TITLE) %>%
  left_join(oflc_xwalk, by = "SOC_TITLE") %>%
  mutate(SOC_18 = ifelse(is.na(SOC_18), "11-2011", SOC_18)) %>%
  select(applicant_id, SOC_18)
fys <- fys %>% left_join(fys_misform_SOC, by = "applicant_id") %>%
  mutate(SOC_CODE = ifelse(!is.na(SOC_18), SOC_18, SOC_CODE)) %>%
  select(-SOC_18)

################################################################################
# Merge in OFLC metro-occupation wages wherever possible
# Adjust 2021-2023 wages by OES mean growth rates

msa_soc_growth_adj <- fys %>%
  distinct(MSA_code, SOC_CODE) %>%
  filter(!is.na(SOC_CODE), !is.na(MSA_code)) %>%
  arrange(MSA_code, SOC_CODE) %>%
  mutate(SOC_OES = case_when(
    substr(SOC_CODE, 1, 6) == "13-102" ~ "13-1020",
    substr(SOC_CODE, 1, 6) == "13-202" ~ "13-2020",
    substr(SOC_CODE, 1, 6) == "17-214" ~ "17-2141",
    substr(SOC_CODE, 1, 6) == "29-201" ~ "29-2010",
    substr(SOC_CODE, 1, 6) == "31-112" ~ "31-1120",
    substr(SOC_CODE, 1, 6) == "47-409" ~ "47-4090",
    substr(SOC_CODE, 1, 6) == "53-104" & SOC_CODE != "53-1041" ~ "53-1047",
    SOC_CODE == "19-3032" ~ "19-3039",
    SOC_CODE %in% c("25-2055", "25-2056") ~ "25-2052",
    SOC_CODE %in% c("21-1011", "21-1014") ~ "21-1018",
    TRUE ~ SOC_CODE
  ))

oes_metro <- list()
oes_state <- list()
oes_nat <- list()
for(year in 2021:2024){
  oes_metro_annual <- bind_rows(
    read_xlsx(file.path(data_path, paste0("Other Data/OES/BOS_M", year, "_dl.xlsx")),
              sheet = paste0("BOS_M", year, "_dl")) %>%
      mutate(HOURLY = as.character(HOURLY)),
    read_xlsx(file.path(data_path, paste0("Other Data/OES/MSA_M", year, "_dl.xlsx")),
              sheet = paste0("MSA_M", year, "_dl")) %>%
      mutate(HOURLY = as.character(HOURLY))
  ) %>%
    select(AREA, AREA_TITLE, PRIM_STATE, OCC_CODE, OCC_TITLE, TOT_EMP, H_MEAN, A_MEAN, ANNUAL, HOURLY)
  
  oes_state_annual <- read_xlsx(file.path(data_path, paste0("Other Data/OES/state_M", year, "_dl.xlsx")), sheet = 1) %>%
    select(AREA, AREA_TITLE, PRIM_STATE, OCC_CODE, OCC_TITLE, TOT_EMP, H_MEAN, A_MEAN, ANNUAL, HOURLY)
  
  oes_nat_annual <- read_xlsx(file.path(data_path, paste0("Other Data/OES/national_M", year, "_dl.xlsx")), sheet = 1) %>%
    select(PRIM_STATE, OCC_CODE, OCC_TITLE, TOT_EMP, H_MEAN, A_MEAN, ANNUAL, HOURLY)
  
  oes_metro[[as.character(year)]] <- oes_metro_annual %>%
    mutate(
      A_MEAN = ifelse(!is.na(HOURLY),
                      as.numeric(H_MEAN) * 2080,
                      as.numeric(A_MEAN))) %>%
    select(-c(H_MEAN, ANNUAL, HOURLY)) %>%
    rename(mean_wage = A_MEAN, state_abb = PRIM_STATE)
  
  oes_state[[as.character(year)]] <- oes_state_annual %>%
    mutate(
      A_MEAN = ifelse(!is.na(HOURLY),
                      as.numeric(H_MEAN) * 2080,
                      as.numeric(A_MEAN))) %>%
    select(-c(H_MEAN, ANNUAL, HOURLY)) %>%
    rename(mean_wage = A_MEAN, state_abb = PRIM_STATE)
  
  oes_nat[[as.character(year)]] <- oes_nat_annual %>%
    mutate(
      A_MEAN = ifelse(!is.na(HOURLY),
                      as.numeric(H_MEAN) * 2080,
                      as.numeric(A_MEAN))) %>%
    select(-c(H_MEAN, ANNUAL, HOURLY)) %>%
    rename(mean_wage = A_MEAN)
}
remove(oes_metro_annual, oes_state_annual)

msa_soc_growth_adj <- msa_soc_growth_adj %>%
  left_join(oes_metro[["2021"]] %>% distinct(AREA, AREA_TITLE, state_abb), by = join_by("MSA_code" == "AREA")) %>%
  left_join(oes_metro[["2021"]], by = join_by("state_abb" == "state_abb", "MSA_code" == "AREA",
                                           "AREA_TITLE" == "AREA_TITLE", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2021` = mean_wage, emp_21 = TOT_EMP) %>%
  left_join(oes_metro[["2022"]] %>% select(-OCC_TITLE),
            by = join_by("state_abb" == "state_abb", "MSA_code" == "AREA",
                         "AREA_TITLE" == "AREA_TITLE", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2022` = mean_wage, emp_22 = TOT_EMP) %>%
  left_join(oes_metro[["2023"]] %>% select(-OCC_TITLE),
            by = join_by("state_abb" == "state_abb", "MSA_code" == "AREA",
                         "AREA_TITLE" == "AREA_TITLE", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2023` = mean_wage, emp_23 = TOT_EMP) %>%
  left_join(oes_metro[["2024"]] %>% select(-OCC_TITLE),
            by = join_by("state_abb" == "state_abb", "MSA_code" == "AREA",
                         "AREA_TITLE" == "AREA_TITLE", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2024` = mean_wage, emp_24 = TOT_EMP)

msa_soc_growth_valid <- msa_soc_growth_adj %>%
  filter(!is.na(`2021`), !is.na(`2022`), !is.na(`2023`), !is.na(`2024`),
         as.numeric(emp_21) >= 100, as.numeric(emp_22) >= 100,
         as.numeric(emp_23) >= 100, as.numeric(emp_24) >= 100) %>%
  mutate(adj_21 = `2024`/`2021`, adj_22 = `2024`/`2022`, adj_23 = `2024`/`2023`) %>%
  select(-c(`2021`, `2022`, `2023`, `2024`, emp_21, emp_22, emp_23, emp_24))

msa_soc_growth_state <- msa_soc_growth_adj %>%
  filter(is.na(`2021`) | is.na(`2022`) | is.na(`2023`) | is.na(`2024`) |
           as.numeric(emp_21) < 100 | as.numeric(emp_22) < 100 |
           as.numeric(emp_23) < 100 | as.numeric(emp_24) < 100) %>%
  select(-c(OCC_TITLE, `2021`, `2022`, `2023`, `2024`, emp_21, emp_22, emp_23, emp_24)) %>%
  left_join(oes_state[["2021"]] %>% select(-c(AREA, AREA_TITLE)),
            by = join_by("state_abb" == "state_abb", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2021` = mean_wage, emp_21 = TOT_EMP) %>%
  left_join(oes_state[["2022"]] %>% select(-c(AREA, AREA_TITLE, OCC_TITLE)),
            by = join_by("state_abb" == "state_abb", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2022` = mean_wage, emp_22 = TOT_EMP) %>%
  left_join(oes_state[["2023"]] %>% select(-c(AREA, AREA_TITLE, OCC_TITLE)),
            by = join_by("state_abb" == "state_abb", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2023` = mean_wage, emp_23 = TOT_EMP) %>%
  left_join(oes_state[["2024"]] %>% select(-c(AREA, AREA_TITLE, OCC_TITLE)),
            by = join_by("state_abb" == "state_abb", "SOC_OES" == "OCC_CODE")) %>%
  rename(`2024` = mean_wage, emp_24 = TOT_EMP)

msa_soc_growth_st_valid <- msa_soc_growth_state %>%
  filter(!is.na(`2021`), !is.na(`2022`), !is.na(`2023`), !is.na(`2024`)) %>%
  filter(!(as.numeric(emp_21) < 100 | as.numeric(emp_22) < 100 |
             as.numeric(emp_23) < 100 | as.numeric(emp_24) < 100)) %>%
  mutate(adj_21 = `2024`/`2021`, adj_22 = `2024`/`2022`, adj_23 = `2024`/`2023`) %>%
  select(-c(`2021`, `2022`, `2023`, `2024`, emp_21, emp_22, emp_23, emp_24))

msa_soc_growth_nat <- msa_soc_growth_state %>%
  filter(is.na(`2021`) | is.na(`2022`) | is.na(`2023`) | is.na(`2024`) |
           as.numeric(emp_21) < 100 | as.numeric(emp_22) < 100 |
           as.numeric(emp_23) < 100 | as.numeric(emp_24) < 100) %>%
  select(-c(OCC_TITLE, `2021`, `2022`, `2023`, `2024`, emp_21, emp_22, emp_23, emp_24)) %>%
  left_join(oes_nat[["2021"]] %>% select(-c(PRIM_STATE, TOT_EMP)), by = join_by("SOC_OES" == "OCC_CODE")) %>%
  rename(`2021` = mean_wage) %>%
  left_join(oes_nat[["2022"]] %>% select(-c(PRIM_STATE, OCC_TITLE, TOT_EMP)), by = join_by("SOC_OES" == "OCC_CODE")) %>%
  rename(`2022` = mean_wage) %>%
  left_join(oes_nat[["2023"]] %>% select(-c(PRIM_STATE, OCC_TITLE, TOT_EMP)), by = join_by("SOC_OES" == "OCC_CODE")) %>%
  rename(`2023` = mean_wage) %>%
  left_join(oes_nat[["2024"]] %>% select(-c(PRIM_STATE, OCC_TITLE, TOT_EMP)), by = join_by("SOC_OES" == "OCC_CODE")) %>%
  rename(`2024` = mean_wage) %>%
  distinct() %>%
  filter(!is.na(`2021`) | !is.na(`2022`) | !is.na(`2023`) | !is.na(`2024`)) %>%
  mutate(adj_21 = `2024`/`2021`, adj_22 = `2024`/`2022`, adj_23 = `2024`/`2023`) %>%
  select(-c(`2021`, `2022`, `2023`, `2024`))

msa_soc_growth_valid <- bind_rows(
  msa_soc_growth_valid,
  msa_soc_growth_st_valid,
  msa_soc_growth_nat
) %>% arrange(MSA_code, SOC_CODE) %>%
  mutate(adj_21 = ifelse(is.na(adj_21), adj_22^2/adj_23, adj_21),
         adj_22 = ifelse(is.na(adj_22), sqrt(adj_21*adj_23), adj_22),
         adj_23 = ifelse(is.na(adj_23), adj_22^2/adj_21, adj_23))

inflation_adj <- read.csv(file.path(data_path, "Other Data/PCEPI.csv"))
inflation_adj <- inflation_adj %>%
  filter(!is.na(PCEPI)) %>%
  mutate(observation_date = 2021:2024) %>%
  rename(year = observation_date)
inflation_adj$PCEPI <- last(inflation_adj$PCEPI)/inflation_adj$PCEPI

# Year adjust 2021-2023 by OES wage levels for each SOC-MSA combination to 2024
# Fall back to simple PCE inflation adjustment where no match can be found
fys <- fys %>%
  left_join(msa_soc_growth_valid %>% select(MSA_code, SOC_CODE, adj_21, adj_22, adj_23),
            by = join_by("MSA_code", "SOC_CODE")) %>%
  mutate(
    petition_annual_pay_24_dollars = case_when(
      registration_lottery_year == 2021 & !is.na(adj_21) ~ petition_annual_pay_clean * adj_21,
      registration_lottery_year == 2022 & !is.na(adj_22) ~ petition_annual_pay_clean * adj_22,
      registration_lottery_year == 2023 & !is.na(adj_23) ~ petition_annual_pay_clean * adj_23,
      registration_lottery_year == 2021 & is.na(adj_21) ~ petition_annual_pay_clean * (inflation_adj %>% filter(year == 2021) %>% .$PCEPI),
      registration_lottery_year == 2022 & is.na(adj_22) ~ petition_annual_pay_clean * (inflation_adj %>% filter(year == 2022) %>% .$PCEPI),
      registration_lottery_year == 2023 & is.na(adj_23) ~ petition_annual_pay_clean * (inflation_adj %>% filter(year == 2023) %>% .$PCEPI),
      TRUE ~ petition_annual_pay_clean),
    petition_annual_pay_24_dollars = round(petition_annual_pay_24_dollars, 0)
  )

wage_levels <- read.csv(file.path(data_path, "Other Data","OFLC_Wages_2024-25", "ALC_Export.csv")) %>%
  mutate(
    Level1_full = ifelse(Label !="Annual Wage",  Level1 * 2080, Level1),
    Level2_full = ifelse(Label !="Annual Wage",  Level2 * 2080, Level2),
    Level3_full = ifelse(Label !="Annual Wage",  Level3 * 2080, Level3),
    Level4_full = ifelse(Label !="Annual Wage",  Level4 * 2080, Level4),
    Average_full = ifelse(Label !="Annual Wage",  Average * 2080, Average),
    
    Level1_part = ifelse(Label !="Annual Wage",  Level1 * 1040, Level1),
    Level2_part = ifelse(Label !="Annual Wage",  Level2 * 1040, Level2),
    Level3_part = ifelse(Label !="Annual Wage",  Level3 * 1040, Level3),
    Level4_part = ifelse(Label !="Annual Wage",  Level4 * 1040, Level4),
    Average_part = ifelse(Label !="Annual Wage",  Average * 1040, Average),
  )

fys <- fys %>%
  mutate(SOC_CODE = substr(SOC_CODE, 1, 7),
         MSA_code = as.integer(MSA_code)) %>% 
  left_join(wage_levels, by = join_by("SOC_CODE" == "SocCode",
                                      "MSA_code" == "Area")) %>%
  mutate(WAGE_LEVEL_OFLC = case_when(
    (petition_beneficiary_full_time == "Y" | petition_beneficiary_full_time == "") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level1_full & petition_annual_pay_24_dollars < Level2_full ~ "I",
    (petition_beneficiary_full_time == "Y" | petition_beneficiary_full_time == "") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level2_full & petition_annual_pay_24_dollars < Level3_full ~ "II",
    (petition_beneficiary_full_time == "Y" | petition_beneficiary_full_time == "") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level3_full & petition_annual_pay_24_dollars < Level4_full ~ "III",
    (petition_beneficiary_full_time == "Y" | petition_beneficiary_full_time == "") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level4_full ~ "IV",
    (petition_beneficiary_full_time == "Y" | petition_beneficiary_full_time == "") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars < Level1_full ~ "Too Low",
    
    (petition_beneficiary_full_time == "N") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level1_part & petition_annual_pay_24_dollars < Level2_part ~ "I",
    (petition_beneficiary_full_time == "N") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level2_part & petition_annual_pay_24_dollars < Level3_part ~ "II",
    (petition_beneficiary_full_time == "N") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level3_part & petition_annual_pay_24_dollars < Level4_part ~ "III",
    (petition_beneficiary_full_time == "N") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars >= Level4_part ~ "IV",
    (petition_beneficiary_full_time == "N") & Label !="High Wage" &  Label !="No Leveled Wage" & petition_annual_pay_24_dollars < Level1_part ~ "Too Low",
    TRUE ~ NA)) %>%
  
  mutate(PW_WAGE_LEVEL = case_when(
    PW_WAGE_LEVEL == "Level I" ~ "I",
    PW_WAGE_LEVEL == "Level II" ~ "II",
    PW_WAGE_LEVEL == "Level III" ~ "III",
    PW_WAGE_LEVEL == "Level IV" ~ "IV",
    (is.na(PW_WAGE_LEVEL) | PW_WAGE_LEVEL == "N/A") &
      petition_annual_pay_24_dollars >= lca_wages_lower_bound ~ "I",
    TRUE ~ PW_WAGE_LEVEL)) %>%
  
  # Relabel all "Too Low" as "I", don't replcae with LCA level
  # Make sure territories that have NA OFLC wage levels fall back to LCA wage levels
  mutate(wage_level_combined = case_when(
    WAGE_LEVEL_OFLC == "Too Low" ~ "I",
    is.na(WAGE_LEVEL_OFLC) & !is.na(PW_WAGE_LEVEL) & PW_WAGE_LEVEL != "N/A" ~ PW_WAGE_LEVEL,
    TRUE ~ WAGE_LEVEL_OFLC)
  ) %>%
  
  filter(!is.na(wage_level_combined))

################################################################################
# Export cleaned data with SOC-MSA assignments for each year
fys_export <- fys %>%
  select(-c(applicant_id, registration_id, registration_dob, petition_id, registration_status)) %>%
  group_by(registration_lottery_year) %>%
  mutate(applicant_id = paste0(registration_lottery_year, "-", sprintf("%05d", row_number()))) %>%
  ungroup() %>%
  select(applicant_id,
         # Basic information
         1:29, petition_h1b_type,
         petition_classif_valid_start_date, petition_classif_valid_end_date,
         petition_n_employees, petition_employer_h1b_dependent, petition_wilful_violator,
         petition_beneficiary_off_site_work,
         petition_beneficiary_protected_population,
         CASE_NUMBER, lca_case_status, lca_decision_date,
         # Geographic information
         petition_worksite_addr, petition_worksite_city,
         petition_worksite_state, petition_worksite_zip,
         petition_full_address, petition_worksite_county_fips,
         MSA_code, MSA_name,
         # Occupation and education
         petition_employer_naics, SOC_CODE, SOC_TITLE,
         petition_beneficiary_dot_occ_code, petition_beneficiary_dot_title,
         petition_beneficiary_edu_code, petition_beneficiary_edu_defin, petition_beneficiary_field,
         # Wage information
         petition_beneficiary_full_time,
         petition_beneficiary_wage, petition_beneficiary_wage_unit,
         petition_annual_pay, lca_adj_pay, lca_wage_unit_flag,
         petition_annual_pay_clean, petition_annual_pay_flag,
         petition_annual_pay_24_dollars,
         # Wage levels
         lca_wages_lower_bound, PW_WAGE_LEVEL, 
         Level1_full, Average_full, WAGE_LEVEL_OFLC,
         wage_level_combined
    ) %>%
  rename(oflc_lvl1_fulltime = Level1_full,
         oflc_avg_fulltime = Average_full)

for(year in 2021:2024){
  write.csv(
    fys_export %>% filter(registration_lottery_year == year),
    file = file.path(cleaned_path, paste0("H1B_FY", year, "_Petitions_Cleaned.csv")),
    row.names = FALSE
  )
}
