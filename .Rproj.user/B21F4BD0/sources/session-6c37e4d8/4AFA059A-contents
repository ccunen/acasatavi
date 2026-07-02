#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets (without treatment group): AE



####################################

library(tidyverse)
library(lubridate)

source("R/external/functions.R")
raw <- read_rds("data/raw/raw.rds")
items <- raw %>% pick("items")

# randomisation date 
ran <- pick(raw,"ran")
dates0 <- ran %>% 
  select(subjectid,  site=sitename, ran_date = randat)
dates0$ran_dateTime <- dates0$ran_date
dates0$ran_date <- date(dates0$ran_date)

# ae
ae <- pick(raw,"ae")

# Medra
medra <- pick(raw,"meddra")

# checks on ids
id_list <- ran %>% select(subjectid)

# check no duplicates
id_list <- as.character(id_list$subjectid)
sum(duplicated(id_list))

nn <- length(id_list)
# 360 patients were randomized

ae_td <- ae %>% left_join(medra) %>% 
  select(-(eventid:designversion), -(siteseq:subjectseq)) %>% 
  labeliser() %>% 
  arrange(subjectid, aespid) %>%
  select(-c(aeongocd,aeaesiyncd,aesevincd,aesevextcd,aescongcd,aesdisabcd,aesdthcd,aeshospcd,
            aeslifecd,aesmiecd,aesercd,saeongocd,aeasarelcd,aeasaacncd,aeapirelcd,aeapiacncd,
            aeedorelcd,aeedoacncd,aerivrelcd,aerivacncd,aecontrt1cd,aecontrt2cd,aecontrt0cd,
            aeoutcd,saeasarecd,saeedorecd,saerivrecd,saeexpcd,saeapirecd,formid,formname,itemgroupid,
            itemgroupseq,itemid,itemname,dictinstance,version,codingscopedesc,codingscopelevel,
            codeseqnumber,llt_currency,interpretation))

ae_td <- ae_td %>%
  mutate(
    possible = rowSums(across(c(aeasarel, aeapirel, aeedorel, aerivrel),
                              ~ .x == "Possible")) > 0,
    probable = rowSums(across(c(aeasarel, aeapirel, aeedorel, aerivrel),
                              ~ .x == "Probable")) > 0,
    definite = rowSums(across(c(aeasarel, aeapirel, aeedorel, aerivrel),
                              ~ .x == "Definite")) > 0
  )
ae_td <- ae_td %>%
  mutate(
    s_possible = rowSums(across(c(saeasare, saeapire, saeedore, saerivre),
                              ~ .x == "Possible"),na.rm=T) > 0,
    s_probable = rowSums(across(c(saeasare, saeapire, saeedore, saerivre),
                              ~ .x == "Probable"),na.rm=T) > 0,
    s_definite = rowSums(across(c(saeasare, saeapire, saeedore, saerivre),
                              ~ .x == "Definite"),na.rm=T) > 0
  )

ae_td %>% filter(probable | s_probable) %>% select(subjectid,aespid,aestdat,aeser,aeasarel, aeapirel, aeedorel, aerivrel,
                                                   saeasare, saeapire, saeedore, saerivre,probable,s_probable) %>% print(n=43)
# Coding into causal categories does not see entirely consistent. Some AEs are coded as Possible for AE and Probable for SAE, for example.

ae_td$any_possible <- ifelse(ae_td$possible | ae_td$probable | ae_td$definite | 
                               ae_td$s_possible | ae_td$s_probable | ae_td$s_definite,1,0)
#ae_td %>% filter(any_possible==1) %>% select(subjectid,aespid,aestdat,aeser,aeasarel, aeapirel, aeedorel, aerivrel,
#                                                   saeasare, saeapire, saeedore, saerivre,probable,s_probable) %>% print(n=184)

ae_td <- ae_td %>% left_join(dates0[,c("subjectid","ran_date")],by="subjectid")

readr::write_rds(ae_td, "data/td/ae_td.rds")
