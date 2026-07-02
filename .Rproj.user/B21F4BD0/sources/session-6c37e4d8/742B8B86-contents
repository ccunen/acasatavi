#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

## Key secondary endpoints

# Descriptive statistics
# Analysis

###############################

source("R/external/functions.R")

library(tidyverse)
library(marginaleffects)
library(broom)


adsl <- read_rds("data/ad/adsl.rds") # with shamrand
ksec <- read_rds("data/td/key_secondary_td.rds")
saf <- read_rds("data/td/cso_td.rds") 
varc <- read_rds("data/td/varc_td.rds")

adsl <- adsl %>% select(-site,-ran_date)
saf <- saf %>% select(-site,-ran_date)

saf <- saf  %>% left_join(adsl,by="subjectid")
ksec <- ksec %>% left_join(adsl,by="subjectid")
varc <- varc %>% left_join(adsl,by="subjectid")
varc <- varc %>% left_join(saf[,c("subjectid","varc")],by="subjectid")

# HVD - Composite strategy for death
ksec <- ksec %>% mutate(hvd2=ifelse(hvd_23==1 | death=="yes",1,0))

# Covariates
baseline <- read_rds("data/td/baseline_td.rds")
baseline <- baseline %>% select(-site)

ksec_base <- ksec %>% left_join(baseline,by="subjectid")
ksec_base <- ksec_base %>% mutate(smoke2 = 
                                  fct_collapse(smoke,
                                               smoker = c("On a daily basis", "Smokes occasionally")))

saf_base <- saf %>% left_join(baseline,by="subjectid")
saf_base <- saf_base %>% mutate(smoke2 = 
                                  fct_collapse(smoke,
                                               smoker = c("On a daily basis", "Smokes occasionally")))
### Subsets for modelling ###

# Clinical efficacy
ce_base_mod <- ksec_base %>% select(efficacy,ran_trt,age,sex,cad,diabetes,hypertension,prev_stroke,GFR) 
p <- ncol(ce_base_mod)-1

idc <- which(apply(ce_base_mod[, 3:(p+1)], 1, function(x) any(is.na(x))))
ce_base_mod[idc,]

# Do something about covariate missing values (replace by median or most common class)
for (i in 3:(p+1)){
  vv <- unlist(ce_base_mod[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    ce_base_mod[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    ce_base_mod[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}
ce_base_mod[idc,]

# HVD 
hvd_base_mod <- ksec_base %>% select(hvd2,ran_trt,age,sex,cad,diabetes,hypertension,prev_stroke,GFR) 
p <- ncol(hvd_base_mod)-1

idc <- which(apply(hvd_base_mod[, 3:(p+1)], 1, function(x) any(is.na(x))))
hvd_base_mod[idc,]

# Do something about covariate missing values (replace by median or most common class)
for (i in 3:(p+1)){
  vv <- unlist(hvd_base_mod[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    hvd_base_mod[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    hvd_base_mod[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}
hvd_base_mod[idc,]

# Safety with TIA
saf2_base_mod <- saf_base %>% select(safety2,ran_trt,age,sex,cad,diabetes,hypertension,prev_stroke,GFR) 
p <- ncol(saf2_base_mod)-1

idc <- which(apply(saf2_base_mod[, 3:(p+1)], 1, function(x) any(is.na(x))))
saf2_base_mod[idc,]

# Do something about covariate missing values (replace by median or most common class)
for (i in 3:(p+1)){
  vv <- unlist(saf2_base_mod[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    saf2_base_mod[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    saf2_base_mod[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}
saf2_base_mod[idc,]


# Descriptive statistics
ceT <- ksec%>% group_by(ran_trt) %>% 
  summarise(missing=sum(is.na(efficacy)),
            composite0=sum(efficacy==0,na.rm=T),death=sum(death=="yes",na.rm=T),
            hospitalisation=sum(hosp=="yes",na.rm=T),
            kccq_low=sum(kccq_low==1,na.rm=T),
            kccq_decr=sum(kccq_decr==1,na.rm=T),
            composite1=sum(efficacy==1,na.rm=T), .groups = "drop_last") %>%
  mutate(n_tot=composite1+composite0,pct=paste(round(100*composite1/n_tot,1),"%",sep="")) %>%
  select(-c(n_tot))
ceT <- ceT %>% 
  add_row(ran_trt = "Overall", missing=sum(ceT$missing),
           composite0 = sum(ceT$composite0),death=sum(ceT$death),hospitalisation=sum(ceT$hospitalisation),
          kccq_low=sum(ceT$kccq_low),kccq_decr=sum(ceT$kccq_decr),composite1 = sum(ceT$composite1),
          pct=paste(round(100*sum(ceT$composite1)/(sum(ceT$composite1)+sum(ceT$composite0)),1),"%",sep=""))
ceT <- head(ceT) %>%  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "at") 
ceT$at[c(1:3,6:9)] <- c(" ","Missing","composite=0","KCCQ < 45","decline in KCCQ > 10","composite=1","composite=1, %")


thrT <- ksec%>% group_by(ran_trt) %>% 
  summarise(missing=sum(is.na(thr_event)),
            zeros=sum(thr_event==0,na.rm=T),ones=sum(thr_event==1,na.rm=T), .groups = "drop_last") %>%
  mutate(n_tot=zeros+ones,pct=paste(round(100*ones/n_tot,1),"%",sep="")) %>%
  select(-c(n_tot))
thrT <- thrT %>% 
  add_row(ran_trt = "Overall", missing=sum(thrT$missing),
          zeros = sum(thrT$zeros),ones = sum(thrT$ones),
          pct=paste(round(100*sum(thrT$ones)/(sum(thrT$ones)+sum(thrT$zeros)),1),"%",sep=""))
thrT <- head(thrT) %>%  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "at") 
thrT$at[1:5] <- c(" ","Missing","No thromboembolic event","With thromboembolic event","events, %")

varcT <- varc%>% group_by(ran_trt) %>% 
  summarise(missing=sum(is.na(varc)),zeros=sum(varc=="no",na.rm=T),
            ones=sum(varc=="yes",na.rm=T),
            varc1=sum(varc1=="yes",na.rm=T),varc2=sum(varc2=="yes",na.rm=T),
            varc3=sum(varc3=="yes",na.rm=T),varc4=sum(varc4=="yes",na.rm=T),
             .groups = "drop_last") %>%
  mutate(n_tot=zeros+ones,pct=paste(round(100*ones/n_tot,1),"%",sep="")) %>%
  select(-c(n_tot))
varcT <- varcT %>% 
  add_row(ran_trt = "Overall", missing=sum(varcT$missing),
          zeros = sum(varcT$zeros),ones = sum(varcT$ones),
          varc1=sum(varcT$varc1),varc2=sum(varcT$varc2),
          varc3=sum(varcT$varc3),varc4=sum(varcT$varc4),
          pct=paste(round(100*sum(varcT$ones)/(sum(varcT$ones)+sum(varcT$zeros)),1),"%",sep=""))
varcT <- head(varcT) %>%  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "at") 
varcT$at[1:9] <- c(" ","Missing","No bleeding events","VARC-3 bleeding events",
                   "Type 1","Type 2","Type 3","Type 4","VARC-3 bleeding events, %")

deathT <- ksec%>% group_by(ran_trt) %>% 
  summarise(missing=sum(is.na(death)),
            zeros=sum(death=="no",na.rm=T),ones=sum(death=="yes",na.rm=T),
            cardio=sum(cardiovasc_cause==1,na.rm=T), .groups = "drop_last") %>%
  mutate(n_tot=zeros+ones,pct=paste(round(100*ones/n_tot,1),"%",sep="")) %>%
  select(-c(n_tot))
deathT <- deathT %>% 
  add_row(ran_trt = "Overall", missing=sum(deathT$missing),
          zeros = sum(deathT$zeros),ones = sum(deathT$ones),
          cardio=sum(deathT$cardio),
          pct=paste(round(100*sum(deathT$ones)/(sum(deathT$ones)+sum(deathT$zeros)),1),"%",sep=""))
deathT <- head(deathT) %>%  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "at") 
deathT$at[1:6] <- c(" ","Missing","Alive at end-of-study","Deaths","Deaths from cardiovascular causes","Deaths, %")

hvdT <- ksec%>% group_by(ran_trt) %>% 
  summarise(missing=sum(is.na(hvd_23)),
            zeros=sum(hvd_23==0,na.rm=T),stage1=sum(hvd_1==1,na.rm=T),
            ones=sum(hvd_23==1,na.rm=T),stage3=0,hvdad=sum(hvd2==1,na.rm=T),
            .groups = "drop_last") %>%
  mutate(n_tot=zeros+ones,pct=paste(round(100*ones/n_tot,1),"%",sep=""),
         n_tot2=zeros+hvdad,pct2=paste(round(100*hvdad/n_tot2,1),"%",sep="")) %>%
  select(-c(n_tot,n_tot2))
hvdT <- hvdT %>% 
  add_row(ran_trt = "Overall", missing=sum(hvdT$missing),
          zeros = sum(hvdT$zeros),stage1=sum(hvdT$stage1),
          ones = sum(hvdT$ones),stage3=0,hvdad=sum(hvdT$hvdad),
          pct=paste(round(100*sum(hvdT$ones)/(sum(hvdT$ones)+sum(hvdT$zeros)),1),"%",sep=""),
          pct2=paste(round(100*sum(hvdT$hvdad)/(sum(hvdT$hvdad)+sum(hvdT$zeros)),1),"%",sep="")) %>%
  select(-c(hvdad))
hvdT <- head(hvdT) %>%  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "at") 
hvdT$at[1:8] <- c(" ","Missing","No HVD (stage 0 or 1)","Stage 1",
                  "HVD (stage 2 or 3)", "Stage 3","HVD (stage 2 or 3), %","HVD and death, %")

saf2T <- saf%>% group_by(ran_trt) %>% 
  summarise(missing=sum(is.na(safety2)),
            zeros=sum(safety2==0,na.rm=T),ones=sum(safety2==1,na.rm=T),
            bleeding=sum(varc=="yes",na.rm=T),death=sum(death=="yes",na.rm=T),
            mi=sum(mi=="yes",na.rm=T),stroke=sum(stroke=="yes",na.rm=T),
            tia=sum(tia==TRUE,na.rm=T),.groups = "drop_last") %>%
  mutate(n_tot=zeros+ones,pct=paste(round(100*ones/n_tot,1),"%",sep="")) %>%
  select(-c(n_tot))
saf2T <- saf2T %>% 
  add_row(ran_trt = "Overall", missing=sum(saf2T$missing),
          zeros = sum(saf2T$zeros),ones = sum(saf2T$ones),
          bleeding=sum(saf2T$bleeding),death=sum(saf2T$death),
          mi=sum(saf2T$mi),stroke=sum(saf2T$stroke),
          tia=sum(saf2T$tia),
          pct=paste(round(100*sum(saf2T$ones)/(sum(saf2T$ones)+sum(saf2T$zeros)),1),"%",sep=""))
saf2T <- head(saf2T) %>%  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "at") 
saf2T$at[c(1:4,10)] <- c(" ","Missing","No safety events","Safety events","Safety events, %")


##### Clinical efficacy analysis ##### 
library(mice)

dat_mice0 <- mice(ce_base_mod,maxit=0)
pred <- dat_mice0$predictorMatrix
pred[, "ran_trt"] <- 0 # to ensure that imputation model does not use treatment variable

dat_mice <- mice(ce_base_mod,m=20,printFlag = F,.Random.seed=14,
                 predictorMatrix = pred)
mod_mice <- with(dat_mice,glm(efficacy~ran_trt,family="binomial"))
ce_ratio <- avg_comparisons(mod_mice,variables=list(ran_trt = c("ASA", "DOAC")),
                                   comparison="lnratioavg",transform=exp)

ce_diff <- avg_comparisons(mod_mice,variables=list(ran_trt = c("ASA", "DOAC")))


# Diagnostics for imputation
plot(dat_mice) # should not have any trends
diagMI <- data.frame(dataset=rep(NA,21),pDOAC=NA,pASA=NA)
diagMI$dataset <- c("complete",paste(1:20))

for (i in 1:21){
  datai <- complete(dat_mice,action=(i-1)) 
  datai <- datai[!is.na(datai$efficacy),]
  diagMI$pDOAC[i] <- sum(datai$ran_trt=="DOAC"&datai$efficacy=="1")/sum(datai$ran_trt=="DOAC" )
  diagMI$pASA[i] <- sum(datai$ran_trt=="ASA"&datai$efficacy=="1")/sum(datai$ran_trt=="ASA")
}
diagMI_long <- pivot_longer(diagMI,cols=2:3)
diagMI_long$value <- as.numeric(diagMI_long$value)
ggplot(diagMI_long,aes(x=dataset,y=value,color=name,shape=name))+geom_point()
# Imputation model does not change rates much

##### Thromboembolic events analysis ##### 
library(contingencytables)
idm <- which(is.na(ksec$thr_event))
ksec$thr_event[idm] <- 0
thr_mod <- glm(thr_event~ran_trt,family="binomial",data=ksec)
thr_diff <- avg_comparisons(thr_mod,variables=list(ran_trt = c("ASA", "DOAC")))

mat <- matrix(c(sum(ksec$thr_event==1 & ksec$ran_trt=="DOAC"),sum(ksec$thr_event==1 & ksec$ran_trt=="ASA"),
                sum(ksec$thr_event==0 & ksec$ran_trt=="DOAC"),sum(ksec$thr_event==0 & ksec$ran_trt=="ASA")),2,2)
thr_diff_ne <- Newcombe_hybrid_score_CI_2x2(mat, alpha = 0.05)
thr_diff_ne <- data.frame(contrast="DOAC - ASA",estimate=thr_diff_ne$estimate,
                          conf.low=thr_diff_ne$lower,conf.high=thr_diff_ne$upper)

##### Varc3 bleeding events analysis ##### 
idm <- which(is.na(saf$varc))
saf$varc[idm] <- "no"
varc_mod <- glm(varc~ran_trt,family="binomial",data=saf)
varc_diff <- avg_comparisons(varc_mod,variables=list(ran_trt = c("ASA", "DOAC")))

mat <- matrix(c(sum(saf$varc=="yes" & saf$ran_trt=="DOAC"),sum(saf$varc=="yes" & saf$ran_trt=="ASA"),
                sum(saf$varc=="no" & saf$ran_trt=="DOAC"),sum(saf$varc=="no" & saf$ran_trt=="ASA")),2,2)
varc_diff_ne <- Newcombe_hybrid_score_CI_2x2(mat, alpha = 0.05)
varc_diff_ne <- data.frame(contrast="DOAC - ASA",estimate=varc_diff_ne$estimate,
                          conf.low=varc_diff_ne$lower,conf.high=varc_diff_ne$upper)

##### Mortality analysis ##### 
dth_mod <- glm(death~ran_trt,family="binomial",data=saf)
dth_diff <- avg_comparisons(dth_mod,variables=list(ran_trt = c("ASA", "DOAC")))

mat <- matrix(c(sum(saf$death=="yes" & saf$ran_trt=="DOAC"),sum(saf$death=="yes" & saf$ran_trt=="ASA"),
                sum(saf$death=="no" & saf$ran_trt=="DOAC"),sum(saf$death=="no" & saf$ran_trt=="ASA")),2,2)
dth_diff_ne <- Newcombe_hybrid_score_CI_2x2(mat, alpha = 0.05)
dth_diff_ne <- data.frame(contrast="DOAC - ASA",estimate=dth_diff_ne$estimate,
                          conf.low=dth_diff_ne$lower,conf.high=dth_diff_ne$upper)

##### HVD ##### Composite strategy for death
dat_mice0 <- mice(hvd_base_mod,maxit=0)
pred <- dat_mice0$predictorMatrix
pred[, "ran_trt"] <- 0 # to ensure that imputation model does not use treatment variable

dat_mice <- mice(hvd_base_mod,m=20,printFlag = F,.Random.seed=14,
                 predictorMatrix = pred)
mod_mice <- with(dat_mice,glm(hvd2~ran_trt,family="binomial"))
hvd_ratio <- avg_comparisons(mod_mice,variables=list(ran_trt = c("ASA", "DOAC")),
                            comparison="lnratioavg",transform=exp)

hvd_diff <- avg_comparisons(mod_mice,variables=list(ran_trt = c("ASA", "DOAC")))


# Diagnostics for imputation
plot(dat_mice) # should not have any trends
diagMI <- data.frame(dataset=rep(NA,21),pDOAC=NA,pASA=NA)
diagMI$dataset <- c("complete",paste(1:20))

for (i in 1:21){
  datai <- complete(dat_mice,action=(i-1)) 
  datai <- datai[!is.na(datai$hvd2),]
  diagMI$pDOAC[i] <- sum(datai$ran_trt=="DOAC"&datai$hvd2==1)/sum(datai$ran_trt=="DOAC" )
  diagMI$pASA[i] <- sum(datai$ran_trt=="ASA"&datai$hvd2==1)/sum(datai$ran_trt=="ASA")
}
diagMI_long <- pivot_longer(diagMI,cols=2:3)
diagMI_long$value <- as.numeric(diagMI_long$value)
ggplot(diagMI_long,aes(x=dataset,y=value,color=name,shape=name))+geom_point()
# Imputation model does not change rates much

##### Safety with TIA analysis ##### 
dat_mice <- mice(saf2_base_mod,m=20,printFlag = F,.Random.seed=14)
saf_mod_mice <- with(dat_mice,glm(safety2~ran_trt+age+sex+cad+prev_stroke+
                                    diabetes+hypertension+GFR,family="binomial"))
saf2_diff <- avg_comparisons(saf_mod_mice,variables=list(ran_trt = c("ASA", "DOAC")))


saf2_ratio <- avg_comparisons(saf_mod_mice,variables=list(ran_trt = c("ASA", "DOAC")),
                                     comparison="lnratioavg",transform=exp)


# Diagnostics for imputation
plot(dat_mice) # should not have any trends
diagMI <- data.frame(dataset=rep(NA,21),pDOAC=NA,pASA=NA)
diagMI$dataset <- c("complete",paste(1:20))

for (i in 1:21){
  datai <- complete(dat_mice,action=(i-1)) 
  datai <- datai[!is.na(datai$safety2),]
  diagMI$pDOAC[i] <- sum(datai$ran_trt=="DOAC"&datai$safety2=="1")/sum(datai$ran_trt=="DOAC" )
  diagMI$pASA[i] <- sum(datai$ran_trt=="ASA"&datai$safety2=="1")/sum(datai$ran_trt=="ASA")
}
diagMI_long <- pivot_longer(diagMI,cols=2:3)
diagMI_long$value <- as.numeric(diagMI_long$value)
ggplot(diagMI_long,aes(x=dataset,y=value,color=name,shape=name))+geom_point()
# Imputation model imputes a bit more events in the DOAC group 
# but not very much more than in the complete data



# Save
save(ceT,thrT,varcT,deathT,hvdT,saf2T,ce_ratio,ce_diff,
     thr_diff_ne,varc_diff_ne,dth_diff_ne,
     hvd_ratio,hvd_diff,saf2_ratio,saf2_diff,file="data/res/key_sec_tab.RData")
