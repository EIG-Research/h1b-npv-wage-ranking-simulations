
################################################################################
# Generating lifetime earning net present value (NPV) estimations from 2019-2023 5-year ACS
# Cleaning 2021-2023 Regional Price Parity data
# Author: Jiaxin He (jiaxin@eig.org)
# Date last edited: 10.20.2025

rm(list = ls())

# load libraries
library(readxl)
library(dplyr)
library(slider)
library(tidyr)
library(tools)
library(spatstat)
library(ipumsr)
library(tidycensus)
library(stringr)
library(purrr)

# set project paths
user_path = ""

project_path = file.path(user_path, "h1b-npv-wage-ranking-simulations")
data_path = file.path(project_path, "Data")
foia_path = file.path(data_path, "FOIA Data")
lca_path = file.path(data_path, "LCA Data")
cleaned_path = file.path(project_path, "Cleaned Data")

################################################################################
# Calculate NPV based on different methods
ddi_acs <- read_ipums_ddi(file.path(data_path, "Other Data/usa_00062.xml"))
acs <- read_ipums_micro(ddi_acs)

acs_age_wage <- acs %>% filter(AGE <= 71) %>%
  mutate(AGE = ifelse(AGE < 22, 22, AGE)) %>%
  group_by(YEAR, AGE) %>% 
  summarise(n_obs = n(), mean_wage = weighted.mean(INCWAGE, PERWT))

beta3 <- 1 / 1.03
beta7 <- 1 / 1.07

acs_npv_indices <- acs_age_wage %>%
  mutate(mean_wage_index = mean_wage / first(mean_wage)) %>%
  group_by(YEAR) %>%
  mutate(
    mean_w_next6 = slide_dbl(mean_wage_index, sum, .after = 6, .complete = TRUE) - mean_wage_index,
    mean_wage_6yr_proj = mean_w_next6 / mean_wage_index,
    mean_wage_6yr_proj = ifelse(is.infinite(mean_wage_6yr_proj), NA, mean_wage_6yr_proj)
  ) %>%
  ungroup() %>% filter(AGE <= 65) %>%
  select(-mean_w_next6) %>%
  group_by(YEAR) %>%
  mutate(mean_wage_npv_3pct = rev(accumulate(rev(mean_wage_index), ~ .y + beta3 * .x)),
         mean_wage_npv_7pct = rev(accumulate(rev(mean_wage_index), ~ .y + beta7 * .x))) %>%
  mutate(exp_lifetime_mean_3pct = mean_wage_npv_3pct/mean_wage_index,
         exp_lifetime_mean_7pct = mean_wage_npv_7pct/mean_wage_index)

max_npv_age <- acs_npv_indices %>% filter(YEAR == 2023)
max_npv_age <- data.frame(
  Method = c(
    "Mean wage and salary, 3% discount rate",
    "Mean wage and salary, 7% discount rate",
    "Expected earning over the next 6 years, as multiples of current year wage",
    "Expected lifetime earning, as multiples of current year wage, 3% discount rate",
    "Expected lifetime earning, as multiples of current year wage, 7% discount rate"
  ),
  `Peak NPV age` = c(
    max_npv_age$AGE[which.max(max_npv_age$mean_wage_npv_3pct)],
    max_npv_age$AGE[which.max(max_npv_age$mean_wage_npv_7pct)],
    max_npv_age$AGE[which.max(max_npv_age$mean_wage_6yr_proj)],
    max_npv_age$AGE[which.max(max_npv_age$exp_lifetime_mean_3pct)],
    max_npv_age$AGE[which.max(max_npv_age$exp_lifetime_mean_7pct)]
  )
)
max_npv_age

write.csv(acs_npv_indices,
          file.path(cleaned_path, "ACS 2021-23 NPV Projections.csv"),
          row.names = FALSE)

################################################################################
# Clean and export RPP
rpps <- read.csv(file.path(data_path, "Other Data/SARPP_STATE_2008_2023.csv"))
rpps_cleaned <- rpps %>% filter(LineCode == 1, GeoName != "United States") %>%
  rename(state_name = GeoName) %>%
  mutate(state_fips = substr(str_trim(GeoFIPS), 1, 2),
         state_abbr = ifelse(state_name %in% state.name,
                             state.abb[match(state_name, state.name)], 
                             ifelse(state_name == "District of Columbia", "DC", NA))) %>%
  select(state_fips, state_abbr, state_name, X2021, X2022, X2023) %>%
  pivot_longer(cols = -c(state_fips, state_abbr, state_name),
               names_to = "Year", values_to = "deflator") %>%
  mutate(Year = as.numeric(substr(Year, 2, 5)),
         deflator = 100/deflator) %>%
  relocate(Year) %>% arrange(Year, state_abbr)
  
write.csv(rpps_cleaned,
          file.path(cleaned_path, "RPP 2021-23 Deflators.csv"),
          row.names = FALSE)
