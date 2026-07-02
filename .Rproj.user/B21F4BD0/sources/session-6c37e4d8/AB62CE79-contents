#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets (without treatment group): key secondary endpoints



#################################### 

source("R/external/functions.R")

library(tidyverse)
 
raw <- read_rds("data/raw/raw.rds")

hosp <- read_rds("data/td/hosp_td.rds") # hospitalisations
saf <- read_rds("data/td/cso_td.rds") # death and stroke, endocarditis and TIA
eff <- read_rds("data/td/cto_td.rds") # HALT, valve thrombosis, leaflet calcification and sclerosis
kc <- read_rds("data/td/kccq_td.rds") # KCCQ
eos <- read_rds("data/td/dates_end_td.rds")

# Echocardiography at baseline and 12 months
eco <- pick(raw,"eco")

##### Clinical efficacy #####

# All-cause mortality + all stroke
ce <- saf %>% select(subjectid,site,ran_date,stroke,death)

# Hospitalisations for procedure- or valve related causes
hosp_pas <- unique(hosp$subjectid)
ce <- ce %>% mutate(hosp=ifelse(subjectid %in% hosp_pas,"yes","no"))

# KCCQ
ce <- ce %>% left_join(kc[,c("subjectid","kccq_low","kccq_decr","kccq_bad")],by="subjectid")

# Overall
# Efficacy=1 if all are "no" (NA if not all of them are "no") 
# Efficacy=0 if there is at least one "yes" (NAs don't matter)
ce$efficacy <- ifelse((ce$stroke=="no" & ce$death=="no" & ce$hosp=="no" &  ce$kccq_bad==0),1,
                      ifelse(ce$stroke=="yes" | ce$death=="yes" | ce$hosp=="yes" |  ce$kccq_bad==1,0,NA))
tablec(ce$efficacy)
attributes(ce$efficacy)$label <- "Clinical efficacy"

## Occurence of thromboembolic event: mi + stroke of any cause
thr <- saf %>% select(subjectid,stroke,mi)
thr$thr_event <- ifelse(thr$stroke=="yes" | thr$mi=="yes",1,
                      ifelse((thr$stroke=="no" & thr$mi=="no"),0,NA))
tablec(thr$thr_event)
# 12 missing because of death - what to do?
attributes(thr$thr_event)$label <- "Occurence of thromboembolic event"

## Occurence of bleeding events (varc-3 type 1,2,3,4) -> same as in safety composite
#bl <- saf %>% select(subjectid,varc)

##### All-cause mortality: cardiovascular + non-cardiovascular #####
de <- saf %>% select(subjectid,death)
eos <- eos %>% mutate(cardiovasc_cause=ifelse(death_cause=="Sudden or unwitnessed death" |
                                                death_cause=="Death of unknown cause" |
                                                death_cause=="Other",0,1))
de <- de %>%left_join(eos[,c("subjectid","cardiovasc_cause")],by="subjectid")

##### Hemodynamic valve deterioration #####

# Fixing some wrong values (0s are not possible). 
# Hardcoding is unfortunate, but due to time limitation it was not possible to fix these errors in Viedoc.
eco$ecava[which(eco$ecava==0)] <- NA

eco_hvd <- eco %>% filter(eventname=="baseline" | eventname=="12 months")  %>% 
  select(subjectid,eventname,mean_gradient=ecavmg,area=ecava,
         dopp=eco4,ar=ecoavr)
eco_hvd$eventname[eco_hvd$eventname=="12 months"] <- "12_months"
eco_hvd <- eco_hvd %>% 
  pivot_wider(names_from=eventname,values_from = c(mean_gradient,area,dopp,ar))
eco_hvd <- eco_hvd %>% mutate(delta_mean_gradient=mean_gradient_12_months-mean_gradient_baseline,
                              delta_area=(-1)*(area_12_months-area_baseline),
                              delta_dopp=(-1)*(dopp_12_months-dopp_baseline))
eco_hvd$delta_mean_gradient[eco_hvd$delta_mean_gradient<0] <- 0
eco_hvd$delta_area[eco_hvd$delta_area<0] <- 0
eco_hvd$delta_dopp[eco_hvd$delta_dopp<0] <- 0
# Round variables to avoid issue with floating point precision
eco_hvd$mean_gradient_baseline <- round(eco_hvd$mean_gradient_baseline)
eco_hvd$mean_gradient_12_months <- round(eco_hvd$mean_gradient_12_months)
eco_hvd$delta_mean_gradient <- round(eco_hvd$delta_mean_gradient)
eco_hvd$delta_area <- round(eco_hvd$delta_area,1)
eco_hvd$delta_dopp <- round(eco_hvd$delta_dopp,2)

eco_hvd <- eco_hvd %>% mutate(reldecrease_area=round(100*delta_area/area_baseline,2),
                              reldecrease_dopp=round(100*delta_dopp/dopp_baseline,2),
                              c1=case_when(mean_gradient_12_months<20 ~ 0, 
                                           mean_gradient_12_months>=20 & mean_gradient_12_months<30 ~ 1,
                                           mean_gradient_12_months>=30 ~ 2),
                              c2=case_when(delta_mean_gradient<10 ~ 0, 
                                           delta_mean_gradient>=10 & delta_mean_gradient<20 ~ 1,
                                           delta_mean_gradient>=20 ~ 2),
                              c3=case_when(delta_area<0.3 ~ 0, 
                                           delta_area>=0.3 & delta_area<0.6 ~ 1,
                                           delta_area>=0.6 ~ 2),
                              c4=case_when(reldecrease_area<25 ~ 0, 
                                           reldecrease_area>=25 & reldecrease_area<50 ~ 1,
                                           reldecrease_area>=50 ~ 2),
                              c5=case_when(delta_dopp<0.1 ~ 0, 
                                           delta_dopp>=0.1 & delta_dopp<0.2 ~ 1,
                                           delta_dopp>=0.2 ~ 2),
                              c6=case_when(reldecrease_dopp<20 ~ 0, 
                                           reldecrease_dopp>=20 & reldecrease_dopp<40 ~ 1,
                                           reldecrease_dopp>=40 ~ 2))
eco_hvd <- eco_hvd %>% mutate(hvd_stage2_ar=ifelse( (ar_baseline=="No" & ar_12_months=="Yes"),1,0),
                              hvd_stage23=case_when( (c1==0 | c2==0 | (c3==0 & c4==0 & c5==0 & c6==0))~ 0, 
                                                   (c1>0 & c2>0 & (c3>0 | c4>0 | c5>0 | c6>0)) ~ 1,
                                                   (is.na(sum(c1+c2+c3+c4+c5)))~ NA, # for now! can actually find stage even if there are some NAs
                                                   (c1==2 & c2==2 & (c3==2 | c4==2 | c5==2 | c6==2)) ~ 2),
                              hvd_23=ifelse(hvd_stage2_ar==1 | hvd_stage23>0,1,0))
tablec(eco_hvd$hvd_stage23)
#eco_hvd %>% filter(is.na(hvd_stage23)) %>% select(c1,c2,c3,c4,c5,c6) %>% print(n=24) all NAs - cannot find stage
#eco_hvd %>% filter(hvd_stage23>0) %>% select(c1,c2,c3,c4,c5,c6) %>% print(n=25)
hvd1 <- eff %>% select(subjectid,halt,leaf_calc,leaf_scle,valve_thromb)
hvd1 <- hvd1 %>% left_join(saf[,c("subjectid","endoc")],by="subjectid")
hvd1 <- hvd1 %>% mutate(SVDp=ifelse(halt=="yes" | leaf_calc=="Yes" | 
                                      leaf_scle=="Yes" | valve_thromb=="Yes" |
                                      endoc==T,1,0))
#tablec(hvd1$SVDp)
#hvd1 %>% filter(is.na(SVDp)) %>% select(halt,leaf_calc,leaf_scle,valve_thromb,endoc) %>% print(n=23)
#hvd1 %>% filter(SVDp==1) %>% select(halt,leaf_calc,leaf_scle,valve_thromb,endoc) %>% print(n=84)
eco_hvd <- eco_hvd %>% left_join(hvd1[,c("subjectid","SVDp")],by="subjectid")
eco_hvd <- eco_hvd %>% mutate(hvd_1=ifelse(hvd_23==0 & SVDp==1,1,0))

## Join and save
ksec <- ce %>% left_join(thr[,c("subjectid","mi","thr_event")],by="subjectid")
ksec <- ksec %>% left_join(de[,c("subjectid","cardiovasc_cause")],by="subjectid")
ksec <- ksec %>% left_join(eco_hvd[,c("subjectid","hvd_23","hvd_1")],by="subjectid")

readr::write_rds(ksec, "data/td/key_secondary_td.rds")


