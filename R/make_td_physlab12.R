#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets (without treatment group): phys and lab at 12 months


# structure of baseline data: 
# age, gender
# "weight", "height", "BMI", "SystolicBP", "DiastolicBP", 
#"Hemoglobin", "Plateles", "Leucocytes", "INR", 
#"Sodium", "Potassium", "ALT", "CardiacTroponin", "NTProBNP", 
#"Creatinine", "GRF", "Glucose", "GlycaHemoglobin", "Triglycerides", 
#"TotCholesterol", "HDLCholesterol", "LDLCholesterol", "Albumin",       
#"CRP", "Biluribin"



####################################

library(tidyverse)
library(lubridate)

source("R/external/functions.R")
raw <- read_rds("data/raw/raw.rds")

# randomisation date 
ran <- pick(raw,"ran")

# demographics (age, sex, site)
dm <- pick(raw,"dm")

# Physical examination
phys <- pick(raw,"pe")

# Lab results
lab <- pick(raw,"lb")

# checks on ids
id_list <- ran %>% select(subjectid)

# check no duplicates
id_list <- as.character(id_list$subjectid)
sum(duplicated(id_list))

nn <- length(id_list)
# 360 patients were randomized


############################
# create physlab12 data

# structure of dates data:
# age, gender
# "weight", "height", "BMI", "SystolicBP", "DiastolicBP", 
#"Hemoglobin", "Plateles", "Leucocytes", "INR", 
#"Sodium", "Potassium", "ALT", "CardiacTroponin", "NTProBNP", 
#"Creatinine", "GRF", "Glucose", "GlycaHemoglobin", "Triglycerides", 
#"TotCholesterol", "HDLCholesterol", "LDLCholesterol", "Albumin",       
#"CRP", "Biluribin"

# only include subjects that are randomized
# (should make no difference)
dm <- dm %>% filter(subjectid %in% id_list)
phys <- phys %>% filter(subjectid %in% id_list)
lab <- lab %>% filter(subjectid %in% id_list)

dm0 <- dm %>% select(subjectid,site=sitename, age=dmage, sex)
lab0 <- lab %>% filter(eventname == '12 months') %>% 
  select(subjectid, hemoglobin=lbhhbres, platelets=lbhplres, leukocytes=lbhlcres,
         INR=lbinrres, sodium=lbsodres, potassium=lbpotres, ALT=lbaltres,
         cardiacTroponin=lbcctres, NTProBNP=lbbnpres, creatinine=lbcreres,
         GFR=lbgfrres, glucose=lbglures, glycaHaemoglobin=lbhghre, 
         triglycerides=lbtrires, totchol=lbtchres, HDLchol=lbhdlres, LDLchol=lbldlres,
         albumin=lbalbres, CRP=lbcrpres, bilirubin=lbbilres)

phys0 <- phys %>% filter(eventname == '12 months')%>% 
  select(subjectid,ageVis=agevis, weight=peweight, height=peheight, 
         bmi=pebmi,systolicBP=pesysbp,diastolicBP=pediabp,smoke=pesmoke) 

# Joining
physlab12mo_td <- left_join(dm0,phys0,by="subjectid")
physlab12mo_td <- left_join(physlab12mo_td,lab0,by="subjectid")

readr::write_rds(physlab12mo_td, "data/td/physlab12mo_td.rds")
