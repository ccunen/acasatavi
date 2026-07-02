##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets (without treatment group): KCCQ



####################################

library(tidyverse)
library(lubridate)

source("R/external/functions.R")
raw <- read_rds("data/raw/raw.rds")
items <- raw %>% pick("items")

# KCCQ
kc <- pick(raw,"kccq")

# Physical limitations score
pl_cols <- c("kccq1acd", "kccq1bcd", "kccq1ccd", "kccq1dcd", "kccq1ecd", "kccq1fcd")

kc <- kc %>%
  # 1. Turn "6" into NA, and convert factors → numeric
  mutate(
    across(
      all_of(pl_cols),
      ~ na_if(as.numeric(as.character(.x)), 6)
    )
  ) %>%
  # 2–4. Rowwise calculations
  rowwise() %>%
  mutate(
    n_nonmiss = sum(!is.na(c_across(all_of(pl_cols)))),          # count non-missing
    mean_raw  = if_else( # no score if more than 67% missing
      n_nonmiss >= 3,                                               # need < 4 missing → ≥ 3 non-missing
      mean(c_across(all_of(pl_cols)), na.rm = TRUE),
      NA_real_
    ),
    pl_score = ((mean_raw - 1) / 4) * 100                      # rescale
  ) %>%
  ungroup() %>%
  select(-n_nonmiss, -mean_raw)  # optional: drop intermediates

kc %>% select(all_of(pl_cols),pl_score)

# Symptom frequency score 
sf_cols <- c("kccq2acd","kccq3cd", "kccq4cd", "kccq5cd", "kccq6cd", "kccq7cd",
             "kccq8cd", "kccq9cd") #Self-efficacy domain: , "kccq10cd", "kccq11cd"

kc <- kc %>%
  # 0. Ensure these columns are factors
  mutate(
    across(
      all_of(sf_cols),
      ~ factor(.x)   # or factor(.x, levels = 1:k) if you need explicit levels
    )
  ) %>%
  mutate(
    # 1. Rescale each factor column to 0–100 based on its own number of levels
    across(
      all_of(sf_cols),
      ~ {
        k <- nlevels(.x)                        # number of factor levels in this column
        x_num <- as.numeric(as.character(.x))     # 1, 2, …, k
        ((x_num - 1) / (k - 1)) * 100             # rescale to 0–100
      }
    )
  ) %>%
  # 2. Rowwise mean of the rescaled values
  rowwise() %>%
  mutate(
    n_nonmiss = sum(!is.na(c_across(all_of(sf_cols)))),
    sf_score = if_else(
      n_nonmiss >= 2, # no score if more than 75% missing
      mean(c_across(all_of(sf_cols)), na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-n_nonmiss)

kc %>% select(all_of(sf_cols),sf_score)


# Quality of Life score
ql_cols <- c("kccq12acd", "kccq13acd", "kccq14cd")

kc <- kc %>%
  # 2–4. Rowwise calculations
  rowwise() %>%
  mutate(
    n_nonmiss = sum(!is.na(c_across(all_of(ql_cols)))),          # count non-missing
    mean_raw  = if_else( 
      n_nonmiss >= 0,   # no score if all are missing                                            
      mean(c_across(all_of(ql_cols)), na.rm = TRUE),
      NA_real_
    ),
    ql_score = ((mean_raw - 1) / 4) * 100                      # rescale
  ) %>%
  ungroup() %>%
  select(-n_nonmiss, -mean_raw)  # optional: drop intermediates

kc %>% select(all_of(ql_cols),ql_score)


# social limitations score
sl_cols <- c("kccq15acd", "kccq15bcd", "kccq15ccd", "kccq15dcd")

kc <- kc %>%
  # 1. Turn "6" into NA, and convert factors → numeric
  mutate(
    across(
      all_of(sl_cols),
      ~ na_if(as.numeric(as.character(.x)), 6)
    )
  ) %>%
  # 2–4. Rowwise calculations
  rowwise() %>%
  mutate(
    n_nonmiss = sum(!is.na(c_across(all_of(sl_cols)))),          # count non-missing
    mean_raw  = if_else( # no score if more than 67% missing
      n_nonmiss >= 2,                                               # need < 3 missing → ≥ 2 non-missing
      mean(c_across(all_of(sl_cols)), na.rm = TRUE),
      NA_real_
    ),
    sl_score = ((mean_raw - 1) / 4) * 100                      # rescale
  ) %>%
  ungroup() %>%
  select(-n_nonmiss, -mean_raw)  # optional: drop intermediates

kc %>% select(all_of(sl_cols),sl_score)

# Overall summary score
sum_cols <- c("pl_score", "sf_score", "ql_score","sl_score")

kc <- kc %>%
  # 2–4. Rowwise calculations
  rowwise() %>%
  mutate(
    n_nonmiss = sum(!is.na(c_across(all_of(sum_cols)))),          # count non-missing
    mean_raw  = if_else( 
      n_nonmiss >= 0,   # no score if all are missing                                            
      mean(c_across(all_of(sum_cols)), na.rm = TRUE),
      NA_real_
    ),
    sum_score = mean_raw                     
  ) %>%
  ungroup() %>%
  select(-n_nonmiss, -mean_raw)  # optional: drop intermediates

kc %>% select(all_of(sum_cols),sum_score)
# No NAs nin the overall score

kco <- kc %>% select(subjectid,eventname,sum_score)
kco <- kco %>% pivot_wider(names_from=eventname,values_from=sum_score)
colnames(kco)[3:4] <- c("mo12","prom_baseline")

# 1. Check for rows where both are non-NA and different
conflicts <- kco %>%
  filter(!is.na(baseline),
         !is.na(prom_baseline),
         baseline != prom_baseline)

if (nrow(conflicts) > 0) {
  warning("baseline and prom_baseline differ in ", nrow(conflicts), " rows.")
  # Optional: inspect them
  # print(conflicts)
}

# 2. Merge: fill in baseline from prom_baseline only when baseline is NA
kco <- kco %>%
  mutate(
    baseline = if_else(
      is.na(baseline) & !is.na(prom_baseline),
      prom_baseline,
      baseline
    )
  )
kco <- kco %>% select(-prom_baseline)
kco <- kco %>% mutate(change=mo12-baseline,
                      kccq_low=ifelse(mo12<45,1,0),
                      kccq_decr=ifelse(change < -10,1,0),
                      kccq_bad=ifelse((change < -10 | mo12<45),1,0))

#kco %>% filter(kccq_bad==1) %>% print(n=24)

readr::write_rds(kco, "data/td/kccq_td.rds")
