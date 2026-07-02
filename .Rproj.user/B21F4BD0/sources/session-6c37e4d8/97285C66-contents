#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

# Make the tabulation datasets: euroscore II and extra baseline variables


####################################

library(tidyverse)
library(lubridate)

source("R/external/functions.R")


# Import coefficients
coefs <- read.csv2("data/euroscore2_coefs.csv",header=F)
coefs <- coefs[1:19,1:2]
coefs$V2 <- as.numeric(coefs$V2)
coefs <- coefs %>% pivot_wider(names_from=V1,values_from=V2)

# Define euroscore fucntion
euroscore_f <- function(data,coefs){
  sums <- (coefs$Constant + (data$age-59)*coefs$age + data$female*coefs$female +
             data$iddm*coefs$iddm+data$eca*coefs$eca + data$cpd*coefs$cpd+
             data$nm_mob*coefs$nm_mob + data$redo*coefs$redo +
             (data$urgency=="urgent")*coefs$urgency_urgent +
             (data$nyha=="2")*coefs$nyha2 + (data$nyha=="3")*coefs$nyha3 +
             (data$nyha=="4")*coefs$nyha4 + 
             (data$renal_imp=="moderate")*coefs$renal_imp_moderate +
             (data$renal_imp=="severe")*coefs$renal_imp_severe + 
             (data$LVfunc=="moderate")*coefs$lvfunc_Moderate +
             (data$LVfunc=="poor")*coefs$lvfunc_Poor +
             (data$LVfunc=="very poor")*coefs$`lvfunc_Very poor` +
             (data$phyper=="moderate PA")*coefs$phyper_moderate +
             (data$phyper=="severe PA")*coefs$phyper_severe )
  return(exp(sums)/(1+exp(sums)))
  
}

# Import extra data, baseline measurements and pre-TAVI echo
baseline <- read_rds("data/td/baseline_td.rds")
eco <- read_rds("data/td/preTAVI_eco_td.rds") # use pre-TAVI echo 
eco$lvef <- round(eco$lvef)
# most get a higher LVEF after TAVI
#eco2 <- baseline %>% select(subjectid,lvef)
#eco2 <- eco2 %>% left_join(eco[,c("subjectid","lvef")],by="subjectid")
#eco2$lvef.x[is.na(eco2$lvef.x)] <- eco2$lvef.y[is.na(eco2$lvef.x)] 
#eco2$lvef <- round(eco2$lvef.x)

ed <- read.csv2("data/Datakomplettering ACASA-TAVI all CC fixedTypos.csv")
ed <- ed[1:360,]
ed$bicuspid[ed$bicuspid=="."] <- NA
ed$nyha[ed$nyha=="."] <- NA
ed$nyha <- factor(ed$nyha,levels=c("1","2","3","4"))
ed$PA_sysPr[ed$PA_sysPr=="."] <- NA
ed$PA_sysPr[ed$PA_sysPr=="0"] <- NA
ed$PA_sysPr <- as.numeric(ed$PA_sysPr)
#id <- which(ed$nm_mob==50) # one patient with a typo
#ed[id,"PA_sysPr"] <- 50
#ed[id,"nm_mob"] <- 0
#ed[ed$no=="NO-01-181",c("predilation","postdilation")] <- c(1,0) # typo
#ed[ed$no=="NO-01-192",c("predilation","postdilation")] <- c(1,1) # typo

ed <- ed %>% mutate(eca=ifelse(per_kar==1 | caros==1,1,0),
                    nm_mob=ifelse(nm_mob==1,1,0),
                    redo=ifelse(redo==1,1,0),
                    urgency=factor(ifelse(urgent==1,"urgent","elective")))
ed$no <- tolower(ed$no)
colnames(ed)[1] <- "subjectid"
ed$PA_sysPr <- round(ed$PA_sysPr)
ed <- ed %>% mutate(phyper = case_when(is.na(PA_sysPr) | PA_sysPr<31 ~ "no", # assumes NA means OK pulmonary pressure
                                       PA_sysPr>=31 & PA_sysPr<56 ~ "moderate PA",
                                       PA_sysPr>=56 ~ "severe PA"),
                    phyper = factor(phyper,levels = c("no", "moderate PA", "severe PA")))



## Join and compute
euroscore <- baseline %>% select(subjectid,site,age,sex,weight,diabetes,ch_obs_pulm,creatinine)
euroscore <- euroscore %>% mutate(iddm=ifelse(diabetes=="1",1,0),
                                  cpd=ifelse(ch_obs_pulm=="1",1,0))
euroscore <- euroscore %>% left_join(ed,by="subjectid") %>%
  select(-c(valve,size_mm,annular_perimeter,PA_sysPr,per_kar,caros,type,urgent,
            lbbb:mi,prev_stroke:alcohol,pacemaker_before:TAVI.EOA))
eco <- eco %>% mutate(LVfunc = case_when(lvef>50 ~ "good",
                                     lvef>=31 & lvef<=50 ~ "moderate",
                                     lvef>=21 & lvef<=30 ~ "poor",
                                     lvef<=20 ~ "very poor"),
                      LVfunc = factor(LVfunc,levels = c("good", "moderate", "poor","very poor")))
euroscore <- euroscore %>% left_join(eco[,c("subjectid","LVfunc")],by="subjectid")
euroscore$crea <- euroscore$creatinine/88.4

euroscore <- euroscore %>% mutate(cc=(140-age)*weight/(72*crea))
euroscore$cc[euroscore$sex=="Female"] <- euroscore$cc[euroscore$sex=="Female"]*0.85
euroscore$cc <- round(euroscore$cc,2)
euroscore <- euroscore %>% mutate(renal_imp = case_when(cc>85 ~ "normal", 
                                       cc>=50 & cc<=85 ~ "moderate",
                                       cc<50 ~ "severe"),
                                  renal_imp = factor(renal_imp,levels = c("normal", "moderate", "severe")))
euroscore <- euroscore %>% mutate(female=ifelse(sex=="Female",1,0))

s2 <- euroscore_f(euroscore,coefs)
euroscore$score <- s2
#euroscore %>% select(age,sex,iddm,eca,cpd,nm_mob,redo,nyha,renal_imp,LVfunc,phyper,urgency,score)

readr::write_rds(euroscore, "data/td/baseline_extra_td.rds")
