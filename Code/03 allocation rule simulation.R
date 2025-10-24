
################################################################################
# Simulating different H-1B allocation schemes 500 times and returning summary statistics
# Author: Jiaxin He (jiaxin@eig.org)
# Date last edited: 10.20.2025

rm(list = ls())

# load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(tools)
library(igraph)
library(stringr)
library(purrr)
library(sf)

# set project paths
user_path = ""

project_path = file.path(user_path, "h1b-npv-wage-ranking-simulations")
data_path = file.path(project_path, "Data")
foia_path = file.path(data_path, "FOIA Data")
lca_path = file.path(data_path, "LCA Data")
cleaned_path = file.path(project_path, "Cleaned Data")

################################################################################
setwd(cleaned_path)

# List of large IT outsourcers using the H1-B program, as described by Bloomberg
outsourcer_list <- c(
  "Infosys Limited",
  "Cognizant Technology Solutions US Corp",
  "Accenture LLP",
  "Tech Mahindra Americas Inc",
  "Tata Consultancy Services Limited",
  "Wipro Limited",
  "Mindtree Limited",
  "Capgemini America, Inc.",
  "Deloitte Consulting LLP",
  "HCL America Inc",
  "IBM Corporation",
  "Ernst & Young U.S. LLP",
  "WIPRO LIMITED",
  "Tech Mahindra Americas Inc.",
  "IBM Corp",
  "Snowstack LLC",
  "Larsen & Toubro Infotech Limited",
  "ATOS SYNTEL INC",
  "MindTree Limited",
  "LTIMINDTREE LIMITED",
  "MARVELOUS TECHNOLOGIES INC",
  "EVIDEN USA, INC.",
  "PRIMITIVE PARTNERS LLC",
  "Datics Inc",
  "Objects Experts LLC",
  "B3R Technologies LLC",
  "CloudNine Tek LLC",
  "Valuepro, Inc",
  "Zenspace IT LLC",
  "AVALANCHE TECHNOLOGIES LLC",
  "TECHSUFFICE LLC",
  "Aclat, Inc."
)

# Summarize the lottery / ranking winners
summarize_winners <- function(df) {
  df %>%
    summarise(
      year = year,
      avg_age = mean(registration_age, na.rm = TRUE),
      share_F1 = mean(petition_prev_status == "F1", na.rm = TRUE),
      share_F1_ba = mean(petition_beneficiary_edu_code == "F" & petition_prev_status == "F1", na.rm = TRUE),
      share_F1_grad = mean((petition_beneficiary_edu_code %in% c("G", "H", "I") | petition_h1b_type == "M") & # include "H" for professional degrees; assume all grad cap winners have grad degrees
                             petition_prev_status == "F1", na.rm = TRUE),
      share_no_prev_status = mean(petition_prev_status == "", na.rm = TRUE),
      share_outsourcers = mean(large_outsourcer == 1, na.rm = TRUE),
      
      # Wage level shares
      share_level1 = mean(wage_level_weight == 1, na.rm = TRUE),
      share_level2 = mean(wage_level_weight == 2, na.rm = TRUE),
      share_level3 = mean(wage_level_weight == 3, na.rm = TRUE),
      share_level4 = mean(wage_level_weight == 4, na.rm = TRUE),
      
      # Pay
      median_pay = median(petition_annual_pay_clean, na.rm=TRUE),
      min_pay = min(petition_annual_pay_clean, na.rm=TRUE),
      tenth_pct_pay = quantile(petition_annual_pay_clean, 0.10, na.rm = TRUE),
      ninety_pct_pay = quantile(petition_annual_pay_clean, 0.90, na.rm = TRUE),
      
      # Long term expected pay
      mean_6yr_pay_mean_wage_method = mean(proj_h1b_mean_wage_total, na.rm = TRUE),
      exp_lifetime_npv_discount_3pct = mean(petition_pay_npv_mean_3pct, na.rm = TRUE),
      exp_lifetime_npv_discount_7pct = mean(petition_pay_npv_mean_7pct, na.rm = TRUE),

      # Employer industry composition
      share_emp_manu = mean(emp_manu, na.rm = TRUE),
      share_emp_it = mean(emp_info_tech, na.rm = TRUE),
      
      # Geography
      state_count = list(table(petition_worksite_state))
    )
}

npv_adj_factors <- read.csv(file = file.path(cleaned_path, "ACS 2021-23 NPV Projections.csv"))
rpps <- read.csv(file.path(cleaned_path, "RPP 2021-23 Deflators.csv"))

total_regs_list <- list()
for(year in 2021:2024){
  if(year == 2024){
    total_regs_list[[as.character(year)]] <- bind_rows(
      read.csv(file.path(foia_path, "TRK_13139_FY2024_single_reg.csv")),
      read.csv(file.path(foia_path, "TRK_13139_FY2024_multi_reg.csv"))
    ) %>% filter(status_type %in% c("SELECTED", "CREATED", "ELIGIBLE")) %>% nrow()
  }else{
    total_regs_list[[as.character(year)]] <- read.csv(file.path(foia_path, paste0("TRK_13139_FY", year, ".csv"))) %>%
      filter(status_type %in% c("SELECTED", "CREATED", "ELIGIBLE")) %>% nrow()
  }
}

###########################################
#### Declare H1-B Allocation Functions ####
###########################################

####################### Lottery-based Systems #######################
## -----------------------------
## 1) Current lottery
## -----------------------------

current_lottery <- function(DT, cap_res, cap_unres){
  # Unreserved 65k from the whole pool
  winners_unres_current <- DT[, .SD[sample(.N, size = cap_unres, replace = FALSE)], by = sim_id]
  
  # Remove unreserved winners; keep only reserved pool
  remaining_current <- DT[!winners_unres_current, on = .(sim_id, synth_applicant_id)]
  remaining_current <- remaining_current[sample_flag == "reserved"]
  
  # Reserved 20k from remaining reserved
  winners_grad_current <- remaining_current[, .SD[sample(.N, size = cap_res, replace = FALSE)], by = sim_id]
  
  # All winners under current lottery:
  winners_current <- rbindlist(list(winners_unres_current, winners_grad_current), use.names = TRUE)
  remove(remaining_current, winners_unres_current, winners_grad_current)
  
  # Return summary statistics
  winners_current[
    , summarize_winners(.SD) %>%
      mutate(allocation_method = "current lottery") %>%
      relocate(year, allocation_method)
    , by = sim_id
  ]
}

## -----------------------------------------
## 2) Proposed rule
## -----------------------------------------

proposed_rule <- function(DT, cap_res, cap_unres){
  # Reserved 65k, wage level weighted 
  winners_unres_proposed <- DT[, .SD[sample(.N, size = cap_unres, prob = wage_level_weight, replace = FALSE)], by = sim_id]
  
  # Remove unreserved winners; keep only reserved pool
  remaining_proposed <- DT[!winners_unres_proposed, on = .(sim_id, synth_applicant_id)]
  remaining_proposed <- remaining_proposed[sample_flag == "reserved"]
  
  # Reserved 20k, wage level weighted
  winners_grad_proposed <- remaining_proposed[, .SD[sample(.N, size = cap_res, prob = wage_level_weight, replace = FALSE)], by = sim_id]
  
  # All winners under proposed rule:
  winners_proposed <- rbindlist(list(winners_unres_proposed, winners_grad_proposed), use.names = TRUE)
  remove(remaining_proposed, winners_unres_proposed, winners_grad_proposed)
  
  # Return summary statistics
  winners_proposed[
    , summarize_winners(.SD) %>%
      mutate(allocation_method = "proposed rule") %>%
      relocate(year, allocation_method)
    , by = sim_id
  ]
}

## -----------------------------------------
## 3) 2021 wage level ranking rule
## -----------------------------------------

greedy_wage_level_lottery <- function(DT, cap, weight_col = "wage_level_weight") {
  w <- DT[[weight_col]]
  
  # counts by wage level (exclude NA for the "full level" step)
  cnt <- data.table(w = w)[!is.na(w), .N, by = .(w)][order(-w)]
  cnt[, cumN := cumsum(N)]
  overflow_idx <- cnt[, which(cumN > cap)[1]]
  
  # Fully included levels (if any)
  include_full <- if (is.na(overflow_idx)) cnt$w else if (overflow_idx > 1) cnt$w[1:(overflow_idx - 1)] else cnt$w[0]
  sel_idx <- integer(0)
  if (length(include_full) > 0) {
    sel_idx <- which(!is.na(w) & w %in% include_full)
  }
  
  # Remaining slots
  slots_left <- cap - length(sel_idx)
  if (slots_left > 0) {
    if (is.na(overflow_idx)) {
      # All levels fit fully; nothing left to lottery (we already took them all)
      return(DT[sel_idx])
    } else {
      # Lottery ONLY within the highest remaining (overflow) level
      top_rem_level <- cnt$w[overflow_idx]
      cand_idx <- which(!is.na(w) & w == top_rem_level)
      if (length(cand_idx) > 0L) {
        extra <- sample(cand_idx, size = pmin(slots_left, length(cand_idx)), replace = FALSE)
        sel_idx <- c(sel_idx, extra)
      }
    }
  }
  
  DT[sel_idx]
}

final_rule_2021 <- function(DT, cap_res, cap_unres){
  winners_unreserved <- DT[
    , greedy_wage_level_lottery(.SD, cap_unres)   # greedy by levels, then random fill
    , by = sim_id
  ]
  
  remaining <- DT[!winners_unreserved, on = .(sim_id, synth_applicant_id)]
  remaining <- remaining[sample_flag == "reserved"]
  
  winners_reserved <- remaining[
    , greedy_wage_level_lottery(.SD, cap_res)     # same procedure on the reserved pool
    , by = sim_id
  ]
  
  # All winners under proposed rule:
  winners_final_21 <- rbindlist(list(winners_unreserved, winners_reserved), use.names = TRUE)
  remove(remaining, winners_unreserved, winners_reserved)
  
  # Return summary statistics
  winners_final_21[
    , summarize_winners(.SD) %>%
      mutate(allocation_method = "wage level rank & lottery, 2021 proposal") %>%
      relocate(year, allocation_method)
    , by = sim_id
  ]
}

####################### Wage-ranked based Systems #######################
top_n_rank <- function(DT, metric, n) {
  # NA metrics naturally fall to the end; tie-break by synth_applicant_id
  DT[order(sim_id, -get(metric), synth_applicant_id), .SD[seq_len(n)], by = sim_id]
}

rank_select <- function(DT, metric, cap_res, cap_unres) {
  
  # Pick top 65000 from the entire pool
  winners_unres <- DT[order(sim_id, -get(metric), synth_applicant_id),
                      .SD[seq_len(cap_unres)],
                      by = sim_id]
  
  # Remove winners; restrict to remaining reserved pool
  remaining <- DT[!winners_unres, on = .(sim_id, synth_applicant_id)]
  remaining <- remaining[sample_flag == "reserved"]
  
  # Reserved pass from remaining reserved pool
  winners_res <- remaining[order(sim_id, -get(metric), synth_applicant_id),
                           .SD[seq_len(cap_res)],
                           by = sim_id]
  
  # Combine winners
  winners_ranked <- rbindlist(list(winners_unres, winners_res), use.names = TRUE)
  remove(remaining, winners_res, winners_unres)
  
  # Return summary statistics
  winners_ranked[
    , summarize_winners(.SD) %>%
      mutate(allocation_method = case_when(
        metric == "petition_annual_pay_clean_rpp_adj" ~ "RPP adjusted wage ranking",
        metric == "petition_pay_npv_mean_3pct" ~ "NPV adjusted wage ranking, mean ACS wage 3% discount",
        metric == "petition_pay_npv_mean_7pct" ~ "NPV adjusted wage ranking, mean ACS wage 7% discount",
        metric == "petition_pay_demean_pct" ~ "wage ranking, demeaned by percentages",
      )) %>%
      relocate(year, allocation_method)
    , by = sim_id
  ]
}

# Initialize variables
i <- 1
simulation_results <- list()

for(year in 2021:2024){
  total_regs <- total_regs_list[[as.character(year)]]
  
  # Load cleaned data and filter out unrealistic age ranges
  fy_cleaned <- read.csv(paste0("H1B_FY", year, "_Petitions_Cleaned.csv")) %>%
    filter(petition_h1b_type %in% c("B", "M"),
           registration_birth_year <= (year - 18))
  fy_base_sample <- fy_cleaned %>%
    mutate(# OFLC wage-level based lottery weight under the admin's proposed rule
           wage_level_weight = case_when(
             wage_level_combined == "I" ~ 1,
             wage_level_combined == "II" ~ 2,
             wage_level_combined == "III" ~ 3,
             wage_level_combined == "IV" ~ 4,
             TRUE ~ NA),
           registration_age = year - registration_birth_year,
           large_outsourcer = as.integer(registration_employer_name %in% outsourcer_list),
           emp_info_tech = as.integer(substr(petition_employer_naics, 1, 4) == 5415),
           emp_manu = as.integer(substr(petition_employer_naics, 1, 1) == 3)) %>%
    rename(petition_prev_status = petition_beneficiary_classif) %>%
    filter(petition_decision == "Approved") %>%
    select(applicant_id,
           petition_h1b_type,
           registration_age,
           large_outsourcer,
           petition_prev_status,
           petition_beneficiary_edu_code,
           petition_beneficiary_field,
           emp_info_tech, emp_manu,
           petition_worksite_state,
           petition_annual_pay_clean,
           wage_level_weight)
  
  # Adjust with regional price parities
  if(year <= 2022){
    rpps_cur_yr <- rpps %>% filter(Year == year)
  }else if(year >= 2023){
    rpps_cur_yr <- rpps %>% filter(Year == 2023)
  }
  
  fy_base_sample <- fy_base_sample %>%
    left_join(rpps_cur_yr, by  = c("petition_worksite_state" = "state_abbr")) %>%
    mutate(petition_annual_pay_clean_rpp_adj = petition_annual_pay_clean * deflator) %>%
    select(-c(Year, state_fips, state_name, deflator))
  
  # Adjust with NPV, demean, and calculate projected 6-year & lifetime wages
  if(year <= 2022){
    npv_adj_cur_yr <- npv_adj_factors %>%
      filter(YEAR == year)
  }else if(year >= 2023){
    npv_adj_cur_yr <- npv_adj_factors %>%
      filter(YEAR == 2023)
  }
  npv_adj_cur_yr <- npv_adj_cur_yr %>%
    select(AGE, mean_wage, mean_wage_6yr_proj, exp_lifetime_mean_3pct, exp_lifetime_mean_7pct)
  fy_base_sample <- fy_base_sample %>%
    left_join(npv_adj_cur_yr, by = join_by("registration_age" == "AGE")) %>%
    mutate(
      mean_wage_6yr_proj = case_when(
        registration_age < 22 ~ first(npv_adj_cur_yr$mean_wage_6yr_proj),
        registration_age > 59 ~ last(npv_adj_cur_yr$mean_wage_6yr_proj),
        TRUE ~ mean_wage_6yr_proj),
      exp_lifetime_mean_3pct = case_when(
        registration_age < 22 ~ first(npv_adj_cur_yr$exp_lifetime_mean_3pct),
        registration_age > 59 ~ npv_adj_cur_yr$exp_lifetime_mean_3pct[38],
        TRUE ~ exp_lifetime_mean_3pct),
      exp_lifetime_mean_7pct = case_when(
        registration_age < 22 ~ first(npv_adj_cur_yr$exp_lifetime_mean_7pct),
        registration_age > 59 ~ npv_adj_cur_yr$exp_lifetime_mean_7pct[38],
        TRUE ~ exp_lifetime_mean_7pct),
      mean_wage = case_when(
        registration_age < 22 ~ first(npv_adj_cur_yr$mean_wage),
        registration_age > 65 ~ last(npv_adj_cur_yr$mean_wage),
        TRUE ~ mean_wage),
      
      petition_pay_demean_pct = petition_annual_pay_clean_rpp_adj/mean_wage - 1,
      
      petition_pay_npv_mean_3pct = petition_annual_pay_clean_rpp_adj * (exp_lifetime_mean_3pct/100),
      petition_pay_npv_mean_7pct = petition_annual_pay_clean_rpp_adj * (exp_lifetime_mean_7pct/100),
      
      proj_h1b_mean_wage_total = petition_annual_pay_clean * mean_wage_6yr_proj
    ) %>%
    select(-c(mean_wage, mean_wage_6yr_proj, exp_lifetime_mean_3pct, exp_lifetime_mean_7pct))
  
  # Split sample into graduate degree holders and non-graduate degree holders
  non_grad_sample <- fy_base_sample %>%
    filter(petition_h1b_type == "B" & !petition_beneficiary_edu_code %in% c("G", "H", "I")) %>%
    mutate(sample_flag = "unreserved")
  grad_sample <- fy_base_sample %>%
    filter(petition_h1b_type == "M" | (petition_h1b_type == "B" & petition_beneficiary_edu_code %in% c("G", "H", "I"))) %>%
    mutate(sample_flag = "reserved")
  grad_unreserv <- nrow(grad_sample %>% filter(petition_h1b_type != "M"))
  unreserve_total <- nrow(fy_base_sample %>% filter(petition_h1b_type != "M"))
  
  # Initiate synthetic registration pool size
  approval_rate <- nrow(fy_base_sample) / nrow(fy_cleaned)
  synth_reg_total <- round(total_regs * approval_rate, 0)
  synth_grad_total <- round(grad_unreserv/unreserve_total * synth_reg_total, 0)
  synth_non_grad_total <- synth_reg_total - synth_grad_total
  
  remove(fy_cleaned, fy_base_sample)
  
  ####################### Apply H1-B scenarios to simulated applications #######################
  set.seed(42)
  
  data.table::setDT(grad_sample)
  data.table::setDT(non_grad_sample)
  n_sim <- 500
  
  idx_g  <- sample.int(nrow(grad_sample),  synth_grad_total * n_sim, replace = TRUE)
  idx_ng <- sample.int(nrow(non_grad_sample), synth_non_grad_total * n_sim, replace = TRUE)
  
  synth <- rbindlist(list(grad_sample[idx_g][, sim_id := rep.int(seq_len(n_sim), synth_grad_total)],
                          non_grad_sample[idx_ng][, sim_id := rep.int(seq_len(n_sim), synth_non_grad_total)]),
                     use.names = TRUE, fill = TRUE)
  synth[, synth_applicant_id := seq_len(.N), by = sim_id]
  remove(idx_g, idx_ng, grad_sample, non_grad_sample)
  
  data.table::setDT(synth)
  data.table::setkey(synth, sim_id, synth_applicant_id)
  cap_unres <- 65000
  cap_res   <- 20000
  
  summaries_list <- rbindlist(
    c(
      list(
        current_lottery(synth, cap_res, cap_unres),
        proposed_rule(synth, cap_res, cap_unres),
        final_rule_2021(synth, cap_res, cap_unres)
      ),
      lapply(
        c(
          "petition_annual_pay_clean_rpp_adj",
          "petition_pay_npv_mean_3pct",
          "petition_pay_npv_mean_7pct",
          "petition_pay_demean_pct"
        ),
        function(m) rank_select(synth, m, cap_res, cap_unres)
      )
    ),
    use.names = TRUE,  # bind by column name
    fill      = TRUE   # allow methods to return different column sets
  )
  remove(synth)
  
  summaries_avg <- summaries_list %>%
    select(-sim_id) %>%
    mutate(state_count = map(state_count, ~{
      x <- .x
      if (is.null(x) || (length(x) == 1 && is.na(x))) return(setNames(numeric(0), character(0)))
      x <- unlist(x, recursive = TRUE, use.names = TRUE)
      # keep only entries with nonempty names
      keep <- !is.na(names(x)) & names(x) != ""
      x[keep]
    })
    ) %>%
    unnest_wider(state_count, names_repair = "check_unique") %>%
    group_by(allocation_method) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    summarise_all(~mean(., na.rm = TRUE)) %>%
    ungroup() %>% mutate_all(~replace(., is.na(.), 0))
  
  simulation_results[[i]] <- summaries_avg
  
  print(paste0("Year ", year, ", Iteration ", i, ", Batch Size ", n_sim))
  
  i <- i + 1
}

sim_results_yr <- bind_rows(simulation_results)

inflation_adj <- read.csv(file.path(data_path, "Other Data/PCEPI.csv"))
inflation_adj <- inflation_adj %>%
  filter(!is.na(PCEPI)) %>%
  mutate(observation_date = 2021:2024) %>%
  rename(year = observation_date)
inflation_adj$PCEPI <- last(inflation_adj$PCEPI)/inflation_adj$PCEPI

sim_results_final <- sim_results_yr %>%
  left_join(inflation_adj, by = "year") %>%
  mutate(median_pay = median_pay * PCEPI,
         tenth_pct_pay = tenth_pct_pay * PCEPI,
         ninety_pct_pay = ninety_pct_pay * PCEPI,
         mean_6yr_pay_mean_wage_method = mean_6yr_pay_mean_wage_method * PCEPI / 1000000,
         exp_lifetime_npv_discount_3pct = exp_lifetime_npv_discount_3pct * PCEPI / 10000,
         exp_lifetime_npv_discount_7pct = exp_lifetime_npv_discount_7pct * PCEPI / 10000
         ) %>%
  select(-c(year, PCEPI)) %>%
  group_by(allocation_method) %>%
  summarise_all(mean) %>%
  ungroup()

write.csv(bind_rows(sim_results_final),
          file.path(cleaned_path, paste0("simulation_output_it_", 500 * (i %/% 4), ".csv")))
