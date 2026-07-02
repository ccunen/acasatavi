##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

##############################
# Make the ADSL Subject-level analysis dataset (with shamrand)
# Input: adran, td_adherence, td_dates
# Output: adsl

# Contains: One record per subject
#           Treatment information: planned and actual treatments
#           Randomization 
#           Analysis population flags: SAS, SAS, PP
#         	Key dates (first/last treatment, study start/end - last visit date and time)

###############################


source("R/external/functions.R")

library(tidyverse)

dates <- read_rds("data/td/dates_end_td.rds")
adh <- read_rds("data/td/adherence_blind_td.rds")
ran <- read_rds("data/ad/adran.rds") # with shamrand

# Define FAS
# everyone (also the wrongly randomized one)

adh <- adh %>% mutate(fas=1)

# Define SAS - same as FAS (don't need any separate definition)
#adh <- adh %>% mutate(sas=ifelse(time_in_study>1,1,0))
#tablec(adh$sas) # 3 excluded from SAS

# Define per protocol population  --- CHECK this carefully

#Exclude:
# * non-completers that did not die (voluntary discontinuation only??)
# * participants with important protocol deviations: no-one probably
# * patients that change treatment group (KU? and MD data)
# * the one patient which was labelled as "Incorrectly randomised" and left the study after 2 days

adh$changeTRT[which(is.na(adh$changeTRT) & !is.na(adh$eos_date))] <- 0

discont <- c("Voluntary discontinuation by patient"," Major Protocol Deviation","Incorrect randomisation",
             "Subject lost to follow-up","Non-compliance","Other")
adh$non_completer_notDead <- ifelse(adh$eos_yn=="No" & adh$eos_reas%in%discont,1,0)
adh$changeTRT_all <- ifelse(adh$changeTRT>0 | adh$changeTRT_ku==T,1,0)
adh$changeTRT_all[which(is.na(adh$changeTRT_all) & adh$eos_reas=="Death")] <- 0
#adh %>% filter(changeTRT_all==1)  %>% select(subjectid,site,ran_date,eos_date,eos_yn,eos_reas,changeTRT,changeTRT_ku) %>% print(n=47)
adh$pd <- ifelse(adh$pd_important==T,1,0)
adh$pd[is.na(adh$pd)] <- 0

adh <- adh %>% mutate(pp=ifelse(non_completer_notDead==1 | changeTRT_all==1 | pd==1 ,0,1))
tablec(adh$pp)

# at the end there should not be any NAs in FAS, SAS or PP. CHECK!
adh %>% filter(is.na(fas) | is.na(pp)) %>% print(n=28)
# OK - all are finished

adho <- adh %>% select(subjectid,fas,pp)


# Join
adsl <- ran %>% select(!ran_dateTime)
adsl <- adsl %>% left_join(adho,by="subjectid")
dates0 <- dates %>% select(subjectid,eos_date,last_visit_date,last_visit_name)
adsl <- adsl%>% left_join(dates0,by="subjectid")

readr::write_rds(adsl, "data/ad/adsl.rds")
