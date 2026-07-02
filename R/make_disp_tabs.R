#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

##############################
# Make tables for patient enrolment and disposition
# Input: adsl, dates, adherence, dv
# Output: 
# Trial population
# Patient disposition (reasons for discontinuation)
# Timing of end of study, withdrawals and follow ups
# Protocol deviations
# Adherence to allocated treatment
# Analysis populations

###############################

source("R/external/functions.R")

library(tidyverse)

raw <- read_rds("data/raw/raw.rds")
# protocol deviations
dv <- pick(raw,"dv")

dates <- read_rds("data/td/dates_end_td.rds")
adh <- read_rds("data/td/adherence_blind_td.rds")
adsl <- read_rds("data/ad/adsl.rds") # with shamrand
eff <- read_rds("data/td/cto_td.rds")
saf <- read_rds("data/td/cso_td.rds")

eff <- eff %>% left_join(adsl,by="subjectid")
saf <- saf %>% left_join(adsl,by="subjectid")
adh <- adh %>% left_join(adsl,by="subjectid")
  
##### Trial population

n_ass_screen <- NA # don't know
n_elig_screen <- NA # don't know
n_inelig_screen <- NA # don't know
n_elig_rand <- 359 # but one was later found to not fullfill inclusion/exclusion
n_elig_notrand <- NA #don't know
n_inelig_rand <- 1
n_lost <- sum(dates$eos_yn=="No",na.rm=T) # at the end there should be no NAs here
n_fas <- sum(adsl$fas==1)
n_fas_primary_eff <- sum(eff$fas==1 & !is.na(eff$halt)) # should increase when more reach end-of-study
n_fas_primary_saf <- sum(saf$fas==1 & !is.na(saf$safety))

consort_tab <- data.frame(description = c('Assessed at screening',
                                            'Eligible at screening', 
                                            'Ineligible at screening', 
                                            'Eligible and randomised', 
                                            'Eligible not randomised', 
                                            "Randomized but ineligible",
                                            # 'Received randomised allocation at day 0', 
                                            'Non-completers', 
                                            'Randomised and FAS', 
                                            'Randomised and FAS with co-primary efficacy endpoint',
                                            'Randomised and FAS with co-primary safety endpoint'), 
                            n = c(n_ass_screen,
                                  n_elig_screen,
                                  n_inelig_screen,
                                  n_elig_rand,
                                  n_elig_notrand,
                                  n_inelig_rand,
                                  # N.received,
                                  n_lost,
                                  n_fas,
                                  n_fas_primary_eff,
                                  n_fas_primary_saf))

##### Patient disposition (reasons for discontinuation)

# Total N per arm
arm_n <- adh %>%
  count(ran_trt, name = "N_arm") %>%
  select(arm=ran_trt,N_arm)

# Completed Yes/No (counts only)
comp_tab <- adh %>%
  group_by(ran_trt, eos_yn) %>%
  summarise(n = n(), .groups = "drop") %>%
  select(arm=ran_trt, row_label = eos_yn, val = n)
#comp_tab$row_label[is.na(comp_tab$row_label)] <- "Unfinished"

# Reasons for discontinuation (counts only, among completed == "No")
reason_tab <- adh %>%
  filter(eos_yn == "No") %>%
  group_by(ran_trt, eos_reas) %>%
  summarise(n = n(), .groups = "drop") %>%
  select(arm=ran_trt, row_label = eos_reas, val = n)

make_wide <- function(df) {
  df %>%
    select(arm, row_label, val) %>%
    pivot_wider(
      names_from  = arm,
      values_from = val
    ) %>%
    mutate(across(-row_label, ~ replace_na(., 0L)))  # <-- new line
}

comp_wide   <- make_wide(comp_tab)
reason_wide <- make_wide(reason_tab)

# Order Yes/No rows
comp_wide$row_label <- factor(comp_wide$row_label, levels = c("No", "Yes"))
comp_wide <- arrange(comp_wide, row_label)

# First row: number of randomised participants
n_row <- arm_n %>%
  transmute(
    row_label = "Number of randomised participants",
    arm, val = N_arm
  ) %>%
  make_wide()

# "Study completed" block
study_completed_block <- tibble(
  row_label = c("Study completed", as.character(comp_wide$row_label))
) %>%
  left_join(comp_wide, by = "row_label")

# "Reason for discontinuation" block
reason_block <- tibble(
  row_label = c("Reason for discontinuation",
                unique(reason_wide$row_label) |> as.character())
) %>%
  left_join(reason_wide, by = "row_label")

# Combine all blocks
body_tab <- bind_rows(
  n_row,
  study_completed_block,
  reason_block
)
body_tab

# Timing of end of study, withdrawals and follow ups 
adh$last_visit_date[is.na(adh$last_visit_date)] <- adh$eos_date.x[is.na(adh$last_visit_date)]
adh <- adh %>% mutate(length_st=last_visit_date-ran_date.x)

summary.fun <- function(x) {
  m <- median(x, na.rm = TRUE)
  mi <- min(x,na.rm=T)
  ma <- max(x,na.rm=T)
  summary <- paste0(m, ' (', mi, '-', ma, ')')
  return (summary)
}

overall <- adh %>% summarise(length = summary.fun(length_st))
bytreat <- adh %>% group_by(ran_trt) %>% 
  summarise(length = summary.fun(length_st))

overall_withdrawn <- adh %>% filter(eos_yn == 'No') %>%
  summarise(length = summary.fun(length_st))
bytreat_withdrawn  <- adh %>% filter(eos_yn == 'No') %>% 
  group_by(ran_trt) %>% summarise(length = summary.fun(length_st))

DOAC <- unlist(c(bytreat[1,2],bytreat_withdrawn[1,2]))
ASA <- unlist(c(bytreat[2,2],bytreat_withdrawn[2,2]))
Overall <- unlist(c(overall,overall_withdrawn))

time_tab <- tibble(dsc=c("All participants","Non-completers"),DOAC,ASA,Overall)

###### Protocol deviations
dv <- dv %>% select(subjectid, dvcat,dvplclas)
dv <- dv %>% left_join(adsl,by="subjectid")

minorPD <- dv %>% filter(dvplclas=="Not important") %>% 
  group_by(ran_trt,cat=dvcat) %>%
  summarize(n=n(),.groups = "drop_last")

majorPD <- dv %>% filter(dvplclas=="Important") %>% 
  group_by(ran_trt,cat=dvcat) %>%
  summarize(n=n(),.groups = "drop_last")

###### Adherence to allocated treatment 
# Number of patients stopping with study drug
# Number of patients switching
adh$changeTRT_all <- ifelse(adh$changeTRT>0 | adh$changeTRT_ku==T,1,0)
adh$changeTRT_all[which(is.na(adh$changeTRT_all) & !is.na(adh$eos_date.x))] <- 0
n_noChange <- sum(adh$changeTRT_all==0,na.rm=T)
n_switch_stop <- sum(adh$changeTRT_all==1,na.rm=T)
n_switch <- sum(adh$changeTRT>0,na.rm=T)
n_stop <- n_switch_stop-n_switch
#n_NA <- sum(is.na(adh$changeTRT_all))

adhSums <- adh %>% group_by(ran_trt) %>%
  summarize(n_noChange=sum(changeTRT_all==0,na.rm=T),
            n_switch_stop =sum(changeTRT_all==1,na.rm=T))
adhSums <- adhSums %>% 
  add_row(ran_trt = "Total", n_noChange = sum(adhSums$n_noChange), n_switch_stop = sum(adhSums$n_switch_stop))
adh_tab <- adhSums

###### Analysis sets

# at the end there should not be any NAs in FAS, SAS or PP. CHECK!
adh %>% filter(is.na(fas) | is.na(pp)) %>% print(n=28)

sets <- adh %>% group_by(ran_trt) %>%
  summarize(FAS=sum(fas==1),
            PP = sum(pp==1))
sets <- sets %>% 
  add_row(ran_trt = "Total", FAS = sum(sets$FAS),
          PP=sum(sets$PP))
set_tab <- sets

# Save tables

save(consort_tab,body_tab,time_tab,minorPD,majorPD,
     adh_tab,set_tab,file="data/res/disp_tab.RData")
