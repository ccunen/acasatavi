#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets (without treatment group): Hospitalisations



############################## 

source("R/external/functions.R")

library(tidyverse)

ae_td <- read_rds("data/td/ae_td.rds") 


# Hospitalisations for procedure- or valve related causes
pt_hosp_pace <- c("atrioventricular block",
                  "atrioventricular block first degree","atrioventricular block second degree")
pt_hosp_groin <- c("haematoma","haemorrhage")
pt_hosp <- c("atrial fibrillation","atrioventricular block complete","cardiac failure","pericardial effusion",
             "paravalvular regurgitation","prosthetic cardiac valve thrombosis","endocarditis",
             "post procedural haematoma","vascular pseudoaneurysm","cerebral artery embolism",
             "cerebral haemorrhage","cerebral infarction","cerebrovascular accident",
             "embolic cerebral infarction","transient ischaemic attack",
             "cardiac resynchronisation therapy","haemolytic anaemia")
hosp <- ae_td %>% filter(aeshosp=="Yes")
hosp$time_ae <- as.Date(hosp$aestdat)-hosp$ran_date
h1 <- hosp %>% filter(pt_name %in% pt_hosp_pace) %>% select(subjectid,saecom)
# all of these 6 got a pacemaker according to the narratives --> include in hospitalisations
h2 <- hosp %>% filter(pt_name %in% pt_hosp_groin) %>% select(subjectid,saecom)
# the first one of these had bleeding in the groin in relation to TAVI (from narrative) --> include
h3 <- hosp %>% filter(pt_name=="haemolytic anaemia")%>% select(subjectid,saecom)
# Seems to be TAVI related - ASK CI
h4 <- hosp %>% filter(pt_name=="device related infection")%>% select(subjectid,saecom)
# Seems not to be TAVI related - ASK cI
h5 <- hosp %>% filter(pt_name=="cardiac pacemaker insertion")%>% select(subjectid,saecom,aestdat,ran_date,time_ae) 
# pacemaker within 30 days of randomization is TAVI related

hosp2 <- hosp %>% filter((pt_name %in% pt_hosp) |
                           (pt_name %in% pt_hosp_pace & grepl("pacemaker",saecom)) |
                            (pt_name %in% pt_hosp_groin & grepl("femoral",saecom)) |
                           (pt_name=="cardiac pacemaker insertion" & (time_ae<30))) %>%
  select(subjectid,pt_name)

# Check in the KU - clinical outcomes form
raw <- read_rds("data/raw/raw.rds")
ku <- pick(raw,"ku")

kut <- ku %>% select(subjectid,bl_hospyn=ku2_0_6,hfail_hospyn=ku3_1,newTAVI_hospyn=ku3_2,
                     otherProc_yn=ku3_3,endo_yn=ku3_4,
                     ad_hospyn=ku4_0_3)
kutt <- kut %>% group_by(subjectid) %>% 
    summarise(bl_hosp=(sum(bl_hospyn=="Ja",na.rm=T)>0),
              hfail_hosp=(sum(hfail_hospyn=="Ja",na.rm=T)>0),
              newTAVI_hosp=(sum(newTAVI_hospyn=="Ja",na.rm=T)>0),
              otherProc_hosp=(sum(otherProc_yn=="Ja",na.rm=T)>0),
              endo_hosp=(sum(endo_yn=="Ja",na.rm=T)>0),
              ad_hosp=(sum(ad_hospyn=="Ja",na.rm=T)>0)) 

kutt$subjectid[which(kutt$bl_hosp)] %in% hosp2$subjectid
# some bleeding hospitalisations seem unrelated to TAVI

kutt$subjectid[which(kutt$hfail_hosp)] %in% hosp2$subjectid
# all heart failures are in the hospitalised group

kutt$subjectid[which(kutt$newTAVI_hosp)] %in% hosp2$subjectid
# all newTAVI are in the hospitalised group

kutt$subjectid[which(kutt$otherProc_hosp)] %in% hosp2$subjectid
# check one participant

kutt$subjectid[which(kutt$endo_hosp)] %in% hosp2$subjectid
# all endocarditis are in the hospitalised group

kutt$subjectid[which(kutt$ad_hosp)] %in% hosp2$subjectid
# check one participant - ok

readr::write_rds(hosp2, "data/td/hosp_td.rds")
