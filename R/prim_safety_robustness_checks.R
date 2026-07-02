## Primary safety endpoint: robustness to various treatment assignments

# No multiple imputation here - just best case imputation for the 6 with missing safety

###############################

source("R/external/functions.R")

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(marginaleffects)
library(broom)


adsl <- read_rds("data/ad/adsl.rds") # with shamrand
baseline <- read_rds("data/td/baseline_td.rds")
extra <- read_rds("data/td/baseline_extra_td.rds")
saf <- read_rds("data/td/cso_td.rds")

adsl <- adsl %>% select(-c(site,ran_date))
baseline <- baseline %>% select(-site)
extra <- extra %>% select(subjectid,euroscore=score)

saf <- saf %>% left_join(adsl,by="subjectid")

saf_base <- saf %>% left_join(baseline,by="subjectid")
saf_base <- saf_base %>% left_join(extra,by="subjectid")

saf_base <- saf_base %>% mutate(smoke2 = 
                                  fct_collapse(smoke,
                                               smoker = c("On a daily basis", "Smokes occasionally")))


# subset for modelling
saf_base_mod <- saf_base %>% select(safety,ran_trt,age,sex,cad,diabetes,hypertension,prev_stroke,GFR,euroscore) #,frailty_status,bmi
p <- ncol(saf_base_mod)-1

idc <- which(apply(saf_base_mod[, 3:(p+1)], 1, function(x) any(is.na(x))))
saf_base_mod[idc,]

# Do something about missing values... (replace by median or most common class)
for (i in 3:(p+1)){
  vv <- unlist(saf_base_mod[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    saf_base_mod[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    saf_base_mod[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}
saf_base_mod[idc,]



##### Adjusted analysis
idm <- which(is.na(saf_base_mod$safety))
saf_base_mod2 <- saf_base_mod
saf_base_mod2$safety[idm] <- 0

m0 <- glm(safety~ran_trt,family="binomial",data=saf_base_mod2)
diff0 <- avg_comparisons(m0,variables=list(ran_trt = c("ASA", "DOAC")),
                                     equivalence=c(NA,0.119))
m1 <- glm(safety~ran_trt+age+sex+cad+prev_stroke+
            diabetes+hypertension+GFR,family="binomial",data=saf_base_mod2)
m1$converged 

sim <- 10^3
mat <- data.frame(est0=rep(NA,sim),est1=NA,conv=NA)

for (i in 1:sim){
  nn <- 360
  ran_order <- sample(1:nn,nn,replace=F)
  saf_base_mod2$ran_trt <- saf_base_mod2$ran_trt[ran_order]
  
  
  m0 <- glm(safety~ran_trt,family="binomial",data=saf_base_mod2)
  d0 <- avg_comparisons(m0,variables=list(ran_trt = c("ASA", "DOAC")),
                           equivalence=c(NA,0.119))
  m1 <- glm(safety~ran_trt+age+sex+cad+prev_stroke+
              diabetes+hypertension+GFR,family="binomial",data=saf_base_mod2)
  d1 <- avg_comparisons(m1,variables=list(ran_trt = c("ASA", "DOAC")),
                        equivalence=c(NA,0.119))
  mat$est0[i] <- d0$estimate
  mat$est1[i] <- d1$estimate
  #if (abs(d0$estimate)>0.08) break
  mat$conv[i] <- m1$converged
}
table(mat$conv)
hist(mat$est0)
plot(mat$est0,mat$est1)

simres <- simulateResiduals(m1)
plot(simres)
