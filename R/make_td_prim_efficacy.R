#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets (without treatment group): Efficacy co-primary endpoint

# Checking and combining information from:
# CT outcome (CT adjudicator): the classification of HALT
# Safety td: deaths

####################################

library(tidyverse)
library(lubridate)

source("R/external/functions.R")
raw <- read_rds("data/raw/raw.rds")

# randomisation  
ran <- pick(raw,"ran")
ran <- ran %>% select(subjectid,  site=sitename, ran_date = randat)
ran$ran_dateTime <- ran$ran_date
ran$ran_date <- date(ran$ran_date)

# Clinical outcome (CT adjudicator)
cto <- pick(raw,"cto")

# checks on ids
id_list <- ran %>% select(subjectid)

# check no duplicates
id_list <- as.character(id_list$subjectid)
sum(duplicated(id_list))

nn <- length(id_list)
# 360 patients were randomized

sum(duplicated(cto$subjectid))
tablec(cto$cthalt)

cto <- cto %>% select(subjectid, cthalt,ct_date=eventdate,ct_taken=ctperf,leaf_calc=ctlc,leaf_scle=ctls,valve_thromb=ctvt)
cto$halt <- ifelse(cto$cthalt=="No thickening","no",
                   ifelse(is.na(cto$cthalt),NA,"yes"))
tablec(cto$halt)

cto <- left_join(ran,cto,by="subjectid")

length(unique(cto$subjectid))
tablec(cto$halt)

# Add deaths
cso <- readr::read_rds("data/td/cso_td.rds")
cso <- cso %>% select(subjectid,death)

cto <- left_join(cto,cso,by="subjectid")

# Factorize:
cto <- cto %>% mutate(across(c(halt),as.factor))

readr::write_rds(cto, "data/td/cto_td.rds")


# Check date of HALT evaluation (did it happen later than 12 mon?)
time <- cto$ct_date-cto$ran_date
summary(time)

