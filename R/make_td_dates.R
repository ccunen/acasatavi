#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets (without treatment group): dates

# structure of date dataset: 
# patient ID, site, date screening/inclusion, date of randomization
# date of withdrawal, reason for withdrawal
# end of study date, last date (0 withdraw or eos)
# dates of interviews: 3 mo, 6 mo, 9 mo, 12 mo and last intervju

####################################

library(tidyverse)
library(lubridate)

source("R/external/functions.R")
raw <- read_rds("data/raw/raw.rds")

# demographics (age, sex, site)
dm <- pick(raw,"dm")


# inclusion exclusion (screening dates etc)
ie <- pick(raw,"ie")

# randomisation date 
ran <- pick(raw,"ran")

# end of study
eos <- pick(raw,"eos")

# Clinical outcome
ku <- pick(raw,"ku")


# checks on ids
id_list <- ran %>% select(subjectid)

# check no duplicates
id_list <- as.character(id_list$subjectid)
sum(duplicated(id_list))

nn <- length(id_list)
# 360 patients were randomized

############################
# create dates db

# structure of dates data:
# patient ID, site,  date screening/inclusion, date of randomization
# date of withdrawal, reason for withdrawal
# end of study date, last date (0 withdraw or eos)
# dates of interviews: 3 mo, 6 mo, 9 mo, 12 mo and last intervju


dates0 <- ran %>% 
  select(subjectid,  site=sitename, ran_date = randat)
dates0$ran_dateTime <- dates0$ran_date
dates0$ran_date <- date(dates0$ran_date)
#tavi_dates <- tp %>% select(subjectid,date=eventdate) # more or less equal to the others too
ie_dates <- ie %>% select(subjectid,ie_date=iedat)
dm_dates <- dm %>% select(subjectid,dm_date=dmicdat) # date of informed consent, demographics
# most dm and ie dates are equal. For two patients ie was taken the day after dm
# randomisation dates are mostly equal to ie: but for two patients the randomisation was the day after ie

eos_tb <- eos %>% mutate(eos_yn   = eosyn,eos_date = eosdat, #completion/discontinuation date
                         eos_reas = eosreas,
                         death_date = eosdtdat,death_cause = eosdth)
eos_tb <- eos_tb %>% select(subjectid,eos_yn,eos_date,eos_reas,death_date,death_cause)
# not sure if I need the stuff about AEs and CMs??

eos_tb <- eos_tb %>% mutate(eos_reas = 
                              fct_recode(eos_reas, `Voluntary discontinuation by patient` = 
                                                    "Voluntary discontinuation: participating subjects are free to discontinue his/her participation in the study at any point in time, without prejudice to further treatment.",
                                         `Incorrect randomisation`="Incorrect randomisation, i.e. the subject does not meet the required inclusion/exclusion criteria for the study."))
levels(eos_tb$eos_reas)[grep("compliance",levels(eos_tb$eos_reas))]
levels(eos_tb$eos_reas)[grep("compliance",levels(eos_tb$eos_reas))] <- "Non-compliance"

# Find date and name of last visit
kuv <- ku %>% select(subjectid, visitname=eventname,visit_date=kudat)
kuv_wide <- kuv %>% pivot_wider(names_from = visitname,values_from=visit_date,
                                names_prefix="date")
colnames(kuv_wide) <- gsub(" ","_",colnames(kuv_wide))
kuv_wide <- kuv_wide %>% rowwise() %>% 
  mutate(last_visit_date=max(c_across(date3_months:date12_months),na.rm=T),
         last_visit_name=c("3mo","6mo","9mo","12mo")[which.max(c_across(date3_months:date12_months))])

dates <- left_join(dates0,ie_dates,by="subjectid")
dates <- left_join(dates,eos_tb,by="subjectid")
dates <- left_join(dates,kuv_wide,by="subjectid")

readr::write_rds(dates, "data/td/dates_end_td.rds")
