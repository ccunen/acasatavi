#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

##############################
# Make tables for baseline characteristics
# Input: adsl, baseline_td
# Output: 
# Demographics
# Baseline characteristics
# Table comparing lab values at baseline and 12 months?


###############################

sum_miss <- function(vec){
  sum(is.na(vec))
}

source("R/external/functions.R")

library(tidyverse)

baseline <- read_rds("data/td/baseline_td.rds")
eco_scr <- read_rds("data/td/preTAVI_eco_td.rds")
physlab12 <- read_rds("data/td/physlab12mo_td.rds")
adsl <- read_rds("data/ad/adsl.rds") # with shamrand
extra <- read_rds("data/td/baseline_extra_td.rds")

bsl <- baseline %>% left_join(adsl,by="subjectid")

# Put in preTAVI echo-parameters in baseline table
bsl_abs <- baseline %>% select(-c(ecodate:avrm))
bsl_abs <- left_join(bsl_abs,eco_scr,by="subjectid")

# Put in extra baseline data
extra <- extra %>% select(-c(site,sex,age,weight,diabetes,ch_obs_pulm))
bsl_abs <- bsl_abs %>% left_join(extra,by="subjectid")
bsl_abs <- bsl_abs %>% mutate(nyha3_4=factor(ifelse(nyha=="3"|nyha=="4",1,0)))
bsl_abs$score <- bsl_abs$score*100

# Demographics (table 1)
dm_table1 <- tribble(
  ~text, ~f, ~var, ~param,
  "Age (years)", "mean_sd", "age", list(digits = 1),
  "Sex, female", "pcto", "sex", list(level = "Female"),
  "NYHA functional class III or IV", "pcto", "nyha3_4", list(level = "1"),
  "Body mass index (kg/m^2)", "mean_sd", "bmi", list(digits = 1),
  "Coronary artery disease", "pcto", "cad", list(level = "1"),
  "Hypertension","pcto", "hypertension", list(level = "1"),
  "Diabetes mellitus", "pcto", "diabetes", list(level = "1"),
  "Previous stroke", "pcto", "prev_stroke", list(level = "1"),
  "Permanent pacemaker","pcto", "prev_pacemaker", list(level = "1"),
  "Chronic obstructive pulmonary disease", "pcto", "ch_obs_pulm", list(level = "1"),
  "EuroSCORE II (%)","mean_sd", "score", list(digits = 1),
  "Troponin T (ng/L)","median_iqr", "cardiacTroponin", list(digits = 0),
  "NT-proBNP (ng/L)","median_iqr", "NTProBNP", list(digits = 0),
  "Glomerular filtration rate (mL/min/1 1.73 m^2)","mean_sd", "GFR", list(digits = 1),
  "Pre-TAVI aortic valve parameters","empty", "", list(),
  "Left ventricular ejection fraction (%)","mean_sd", "lvef", list(digits = 1),
  "Aortic valve mean gradient (mmHg)","mean_sd", "avmg", list(digits = 1),
  "Aortic valvular area (cm^2)","mean_sd", "ava", list(digits = 1), # ask if the unit is correct!
  "Aortic annular area (mm^2)","mean_sd", "annular_area", list(digits = 1),
  "Aortic annular perimeter (mm)","mean_sd", "annular_perimeter_est", list(digits = 1),
  "Bicuspid aortic valve", "pcto", "bicuspid", list(level = "1"),
  "Valce-in-valve", "pcto", "ViV", list(level = "1"),
  "Procedural characteristics", "empty", "", list(),
  "Balloon-expanded valve", "pcto", "valve_type", list(level = "Balloon-expanded"),
  "Self-expandable valve", "pcto", "valve_type", list(level = "Self-expandable"),
  "Pre-dilatation", "pcto", "predilation", list(level = "1"),
  "Post-dilatation", "pcto", "postdilation", list(level = "1"),
)

bsl_abs <- bsl_abs %>% 
  mutate(overall = 1) %>% mutate(overall = as.factor(overall))

dm_table_abs <- dm_table1 %>% 
  mutate(data = list(bsl_abs),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl_abs$overall)) 

# Demographics additional to those in abstract + Medical history

dm_table <- tribble(
  ~text, ~f, ~var, ~param,
  "Age (years)", "mean_sd", "age", list(digits = 1),
  "Sex, n (%)", "empty", "", list(),
  "Male", "n_pct", "sex", list(level = "Male"),
  "Female", "n_pct", "sex", list(level = "Female"),
  "Hypertension, n (%)", "empty", "", list(),
  "Yes", "n_pct", "hypertension", list(level = "1"),
  "Missing", "missing_f", "hypertension", list(),
  "Coronary artery disease, n (%)", "empty", "", list(),
  "Yes", "n_pct", "cad", list(level = "1"),
  "Missing", "missing_f", "cad", list(),
  "Diabetes mellitus, n (%)", "empty", "", list(),
  "Yes", "n_pct", "diabetes", list(level = "1"),
  "Missing", "missing_f", "diabetes", list(),
  "Chronic obstructive pulmonary disease, n (%)", "empty", "", list(),
  "Yes", "n_pct", "ch_obs_pulm", list(level = "1"),
  "Missing", "missing_f", "ch_obs_pulm", list(),
  "Previous stroke, n (%)", "empty", "", list(),
  "Yes", "n_pct", "prev_stroke", list(level = "1"),
  "Missing", "missing_f", "prev_stroke", list(),
  "Permanent pacemaker", "empty", "", list(),
  "Yes", "n_pct", "prev_pacemaker", list(level = "1"),
  "Missing", "missing_f", "prev_pacemaker", list(),
  "Smoking status, n (%)", "empty", "", list(),
  "Never smoked", "n_pct", "smoke", list(level = "Never smoked"),
  "Has stopped", "n_pct", "smoke", list(level = "Previously smoked (more than 1 month)"),
  "Sometimes", "n_pct", "smoke", list(level = "Smokes occasionally"),
  "Daily", "n_pct", "smoke", list(level = "On a daily basis")
)

bsl <- bsl %>% filter(fas==1) %>% 
  mutate(overall = 1) %>% mutate(overall = as.factor(overall))


dm_table0 <- dm_table %>% 
  mutate(data = list(bsl),
         group = "ran_trt") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$ran_trt)) 

head0 <- bsl %>% 
  group_by(ran_trt) %>% 
  summarise(n=n()) %>% 
  mutate(txt = paste0(ran_trt, "(n=", n, ")")) %>% 
  select(txt) %>% 
  deframe

head0 <- c("Characteristic", head0)
colnames(dm_table0) <- head0


dm_table_overall <- dm_table %>% 
  mutate(data = list(bsl),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$overall)) 

head <- bsl %>% 
  group_by(overall) %>% 
  summarise(n=n()) %>% 
  mutate(txt = paste0("Overall (n=", n, ")")) %>% 
  select(txt) %>% 
  deframe

head <- c("Characteristic", head)
colnames(dm_table_overall) <- head


dm_baseline <- cbind(dm_table0, dm_table_overall) %>% select(-4)

# Vital signs and physical examination (including Frailty)

ph_table <- tribble(
  ~text, ~f, ~var, ~param,
  "Weight (kg)", "mean_sd", "weight", list(digits = 1),
  "Height (cm)", "mean_sd", "height", list(digits = 1),
  "Body mass index (kg/m2)", "mean_sd", "bmi", list(digits = 1),
  "Systolic blood pressure (mmHg)", "mean_sd", "systolicBP", list(digits = 1),
  "Diastolic blood pressure (mmHg)", "mean_sd", "diastolicBP", list(digits = 1),
  "Body temperature (C)", "mean_sd", "body_temp", list(digits = 1),
  "Pulse rate (beats/min)", "mean_sd", "pulse_rate", list(digits = 1),
  "Respiratory rate (breaths/min)", "mean_sd", "resp_rate", list(digits = 1),
  "Mini-Cog", "empty", "", list(),
  "Signs of cognitive impairment (0-2)", "n_pct", "cog_imp", list(level = "1"),
  "No cognitive impairment (3-5)", "n_pct", "cog_imp", list(level = "0"),
  "Missing", "missing_f", "cog_imp", list(),
  "Five times sit-to-stand", "empty", "", list(),
  "< 15 sec", "n_pct", "sts", list(level = "0"),
  ">= 15 sec", "n_pct", "sts", list(level = "1"),
  "Missing", "missing_f", "sts", list(),
  "Frailty, n (%)", "empty", "", list(),
  "Robust", "n_pct", "frailty_status", list(level = "robust"),
  "Pre-frail", "n_pct", "frailty_status", list(level = "pre-frail"),
  "Frail", "n_pct", "frailty_status", list(level = "frail"),
  "Missing", "n_pct", "frailty_status", list(level = "missing")
)


ph_tab0 <- ph_table %>% 
  mutate(data = list(bsl),
         group = "ran_trt") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$ran_trt)) 

colnames(ph_tab0) <- head0


ph_tab_overall <- ph_table %>% 
  mutate(data = list(bsl),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$overall)) 

colnames(ph_tab_overall) <- head


ph_tab <- cbind(ph_tab0, ph_tab_overall) %>% select(-4)


# Lab at baseline

om_table <- tribble(
  ~text, ~f, ~var, ~param,
  "Hemoglobin", "mean_sd", "hemoglobin", list(digits = 1),
  "Platelets", "mean_sd", "platelets", list(digits = 1),
  "Leukocytes", "median_iqr", "leukocytes", list(digits = 1),
  "International Normalized Ratio (INR)", "mean_sd", "INR", list(digits = 1),
  "Sodium", "mean_sd", "sodium", list(digits = 1),
  "Potassium", "mean_sd", "potassium", list(digits = 1),
  "ALT", "mean_sd", "ALT", list(digits = 1),
  "Cardiac troponin T", "median_iqr", "cardiacTroponin", list(digits = 1),
  "NT-proBNP", "median_iqr", "NTProBNP", list(digits = 1),
  "GFR", "mean_sd", "GFR", list(digits = 1),
  "Glucose", "mean_sd", "glucose", list(digits = 1),
  "Glycated haemoglobin (HbA1c)", "mean_sd", "glycaHaemoglobin", list(digits = 1),
  "Triglycerides", "mean_sd", "triglycerides", list(digits = 1),
  "Missing", "missing_f", "triglycerides", list(),
  "Total cholesterol", "mean_sd", "totchol", list(digits = 1),
  "HDL cholesterol", "mean_sd", "HDLchol", list(digits = 1),
  "LDL cholesterol", "mean_sd", "LDLchol", list(digits = 1),
  "Creatinine", "mean_sd", "creatinine", list(digits = 1),
  "Albumin", "mean_sd", "albumin", list(digits = 1),
  "Missing", "missing_f", "albumin", list(),
  "Bilirubin", "mean_sd", "bilirubin", list(digits = 1),
  "Missing", "missing_f", "bilirubin", list(),
  "CRP", "median_iqr", "CRP", list(digits = 1)

)


om_table0 <- om_table %>% 
  mutate(data = list(bsl),
         group = "ran_trt") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$ran_trt)) 

colnames(om_table0) <- head0


om_table_overall <- om_table %>% 
  mutate(data = list(bsl),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$overall)) 

colnames(om_table_overall) <- head


om_baseline <- cbind(om_table0, om_table_overall) %>% select(-4)


# Echo at baseline (pre-TAVI and post-TAVI)
eco_scr <- left_join(adsl,eco_scr,by="subjectid")
eco_scr <- eco_scr %>% filter(fas==1) %>% 
  mutate(overall = 1) %>% mutate(overall = as.factor(overall))

eco_table <- tribble(
  ~text, ~f, ~var, ~param,
  "Aortic valve mean gradient (mmHg)","mean_sd", "avmg", list(digits = 1),
  "LVOT (mm)",mean_sd,"lvot",list(digits = 1),
  "Presence of low-flow low-gradient","n_pct", "lflg", list(level = "Yes"),
  "Left ventricular ejection fraction (%)","mean_sd", "lvef", list(digits = 1),
  "Left ventricular global longitudinal strain (%)","mean_sd", "lvgls", list(digits = 1),
  "Stroke volume (mL)","mean_sd", "stvo", list(digits = 1),
  "Stroke volume index (mL per m^2)","mean_sd", "svi", list(digits = 1),
  "Aortic valvular area (cm^2)","mean_sd", "ava", list(digits = 1),
  "Aortic valve regurgitation","n_pct", "avr", list(level = "Yes"),
  "Missing echocardiography","missing_f", "lvef", list(),
)


eco1_table0 <- eco_table %>% 
  mutate(data = list(eco_scr),
         group = "ran_trt") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(eco_scr$ran_trt)) 

colnames(eco1_table0) <- head0


eco1_table_overall <- eco_table %>% 
  mutate(data = list(eco_scr),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(eco_scr$overall)) 

colnames(eco1_table_overall) <- head


eco_screening <- cbind(eco1_table0, eco1_table_overall) %>% select(-4)

# Echo at baseline (after TAVI)
eco_table2 <- tribble(
  ~text, ~f, ~var, ~param,
  "Aortic valve mean gradient (mmHg)","mean_sd", "avmg", list(digits = 1),
  "LVOT (mm)",mean_sd,"lvot",list(digits = 1),
  "Presence of low-flow low-gradient","n_pct", "lflg", list(level = "Yes"),
  "Left ventricular ejection fraction (%)","mean_sd", "lvef", list(digits = 1),
  "Left ventricular global longitudinal strain (%)","mean_sd", "lvgls", list(digits = 1),
  "Stroke volume (mL)","mean_sd", "stvo", list(digits = 1),
  "Stroke volume index (mL per m^2)","mean_sd", "svi", list(digits = 1),
  "Doppler velocity index","mean_sd", "dvi", list(digits = 1),
  "Aortic valvular area (cm^2)","mean_sd", "ava", list(digits = 1),
  "Aortic valve regurgitation","n_pct", "avr", list(level = "Yes"),
  "Missing echocardiography","missing_f", "lvef", list(),
)

eco2_table0 <- eco_table2 %>% 
  mutate(data = list(bsl),
         group = "ran_trt") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$ran_trt)) 

colnames(eco2_table0) <- head0


eco2_table_overall <- eco_table2 %>% 
  mutate(data = list(bsl),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$overall)) 

colnames(eco2_table_overall) <- head


eco_baseline <- cbind(eco2_table0, eco2_table_overall) %>% select(-4)


# TAVI at baseline
ta_table <- tribble(
  ~text, ~f, ~var, ~param,
  "TAVI valve type, n (%)", "empty", "", list(),
  "Balloon-expanded", "n_pct", "valve_type", list(level = "Balloon-expanded"),
  "Self-expandable", "n_pct", "valve_type", list(level = "Self-expandable"),
  "TAVI post-dilatation, n (%)", "empty", "", list(),
  "Yes", "n_pct", "tavi_post_dila", list(level = "Yes"),
  "No", "n_pct", "tavi_post_dila", list(level = "No"),
  "TAVI supra-valvular prothesis position, n (%)", "empty", "", list(),
  "Yes", "n_pct", "tavi_supra_pos", list(level = "Yes"),
  "No", "n_pct", "tavi_supra_pos", list(level = "No"),
  "Ascending aorta diameter (mm)", "mean_sd", "asc_aorta_diam", list(digits = 1),
  "Missing", "missing_f", "asc_aorta_diam", list()
)

ta_table0 <- ta_table %>% 
  mutate(data = list(bsl),
         group = "ran_trt") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$ran_trt)) 

colnames(ta_table0) <- head0


ta_table_overall <- ta_table %>% 
  mutate(data = list(bsl),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(bsl$overall)) 

colnames(ta_table_overall) <- head


tavi_baseline <- cbind(ta_table0, ta_table_overall) %>% select(-4)



# Other characteristics at 12 months
physlab12 <- physlab12 %>% left_join(adsl,by="subjectid")
physlab12 <- physlab12 %>% filter(fas==1) %>% 
  mutate(overall = 1) %>% mutate(overall = as.factor(overall))

om_table_overall_12 <- om_table %>% 
  mutate(data = list(physlab12),
         group = "overall") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(physlab12$overall))

head12 <- physlab12 %>% 
  group_by(overall) %>% 
  summarise(n=sum(!is.na(ageVis))) %>% 
  mutate(txt = paste0("Overall 12 months (n=", n, ")")) %>% 
  select(txt) %>% 
  deframe

head12 <- c("Characteristic", head12)
colnames(om_table_overall_12) <- head12

om_change <- cbind(om_table_overall,om_table_overall_12) %>% select(-3)

# Save tables
save(dm_table_abs,dm_baseline,ph_tab,om_baseline,eco_screening,eco_baseline,tavi_baseline,
     om_change,file="data/res/baseline_tab.RData")

