## Primary safety endpoint: supplmentary analyses (except for hypothetical)

# Composite for switches
# Primary strata for switches


###############################

source("R/external/functions.R")

library(tidyverse)
library(marginaleffects)
library(broom) # for nice model tables


adsl <- read_rds("data/ad/adsl.rds") # with shamrand
baseline <- read_rds("data/td/baseline_td.rds")
saf <- read_rds("data/td/cso_td.rds")
adh <- read_rds("data/td/adherence_blind_td.rds")

adsl <- adsl %>% select(-c(site,ran_date))
baseline <- baseline %>% select(-site)

saf <- saf %>% left_join(adsl,by="subjectid")
adh2 <- adh %>% select(subjectid,switch_discont)
saf <- saf %>% left_join(adh2,by="subjectid")

saf_base <- saf %>% left_join(baseline,by="subjectid")

saf_base <- saf_base %>% mutate(smoke2 = 
                                  fct_collapse(smoke,
                                               smoker = c("On a daily basis", "Smokes occasionally")))

########### Composite strategy ####################
idm <- which(saf_base$switch_discont==1)
saf_base2 <- saf_base
saf_base2$safety[idm] <- 1


# Subset for modelling
saf_base_mod2 <- saf_base2 %>% select(safety,ran_trt,age,sex,cad,diabetes,hypertension,prev_stroke,GFR) #,frailty_status,bmi,euroscore
p <- ncol(saf_base_mod2)-1

idc <- which(apply(saf_base_mod2[, 3:(p+1)], 1, function(x) any(is.na(x))))
saf_base_mod2[idc,]

# Do something about covariate missing values (replace by median or most common class)
for (i in 3:(p+1)){
  vv <- unlist(saf_base_mod2[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    saf_base_mod2[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    saf_base_mod2[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}
saf_base_mod2[idc,]


# Checking all missing safety indicator
# no more missing with this strategy


### Adjusted analysis 
saf_mod_supp2 <- glm(safety~ran_trt+age+sex+cad+prev_stroke+
                       diabetes+hypertension+GFR,family="binomial",data=saf_base_mod2)
safety_supp2_diff <- avg_comparisons(saf_mod_supp2,variables=list(ran_trt = c("ASA", "DOAC")),
                                     equivalence=c(NA,0.119))


safety_supp2_ratio <- avg_comparisons(saf_mod_supp2,variables=list(ran_trt = c("ASA", "DOAC")),
                                      comparison="lnratioavg",transform=exp)

sum_events2 <- sum(saf_base_mod2$safety==1)

########### Primary strata strategy ####################
idm <- which(saf_base$switch_discont==1)
saf_base3 <- saf_base[-idm,]

# Subset for modelling
saf_base_mod3 <- saf_base3 %>% select(safety,ran_trt,age,sex,cad,diabetes,hypertension,prev_stroke,GFR) #,frailty_status,bmi,euroscore
p <- ncol(saf_base_mod3)-1

idc <- which(apply(saf_base_mod3[, 3:(p+1)], 1, function(x) any(is.na(x))))
saf_base_mod3[idc,]

# Do something about covariate missing values (replace by median or most common class)
for (i in 3:(p+1)){
  vv <- unlist(saf_base_mod3[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    saf_base_mod3[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    saf_base_mod3[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}
saf_base_mod3[idc,]


# Checking all missing safety indicator
# no more missing with this strategy


### Adjusted analysis 
saf_mod_supp3 <- glm(safety~ran_trt+age+sex+cad+prev_stroke+
                       diabetes+hypertension+GFR,family="binomial",data=saf_base_mod3)
safety_supp3_diff <- avg_comparisons(saf_mod_supp3,variables=list(ran_trt = c("ASA", "DOAC")),
                                     equivalence=c(NA,0.119))


safety_supp3_ratio <- avg_comparisons(saf_mod_supp3,variables=list(ran_trt = c("ASA", "DOAC")),
                                      comparison="lnratioavg",transform=exp)

sum_n3 <- nrow(saf_base_mod3)
sum_events3 <- sum(saf_base_mod3$safety==1)

sums_supp <- list(n_events2=sum_events2,n3=sum_n3,n_events3=sum_events3)

### Saving stuff

save(sums_supp,safety_supp2_diff,safety_supp2_ratio,safety_supp3_diff,
     safety_supp3_ratio,file="data/res/prim_saf_supp2_tab.RData")
