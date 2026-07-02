# Model selection for primary safety endpoint 
# Only for my curiosity


###############################

source("R/external/functions.R")

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(marginaleffects)
library(broom)
library(glmnet) # for lasso pre-selection


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

# subset for model selection (do not include treatment since it is "uncorrelated" with the rest )
saf_base_mod <- saf_base %>% select(safety,age:sex,site,bmi:diastolicBP,body_temp:resp_rate,
                                    abnormal_find:asc_aorta_diam,avmg:lvef,stvo:avr,frailty_status:smoke2) #site,
# 53 potential covariates
p <- ncol(saf_base_mod)-1

idc <- which(apply(saf_base_mod[, 2:(p+1)], 1, function(x) any(is.na(x))))
saf_base_mod[idc,] %>% select(resp_rate,avmg:avr,euroscore) %>% print(n=20)

# Do something about missing values... (replace by median or most common clas)

for (i in 2:(p+1)){
  vv <- unlist(saf_base_mod[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    saf_base_mod[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    saf_base_mod[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}

# Drop missing safety rows: ok for model selection
id <- is.na(saf_base_mod$safety)
saf_base_mod <- saf_base_mod[!id,]

# Check factors
saf_base_mod$site <- factor(saf_base_mod$site)
saf_base_mod$abnormal_find <- factor(saf_base_mod$abnormal_find)
saf_base_mod$sex           <- factor(saf_base_mod$sex)
saf_base_mod$valve_type    <- factor(saf_base_mod$valve_type)
saf_base_mod$tavi_post_dila<- factor(saf_base_mod$tavi_post_dila)
saf_base_mod$tavi_supra_pos<- factor(saf_base_mod$tavi_supra_pos)
saf_base_mod$hypertension  <- factor(saf_base_mod$hypertension)
saf_base_mod$cad           <- factor(saf_base_mod$cad)
saf_base_mod$diabetes      <- factor(saf_base_mod$diabetes)
saf_base_mod$ch_obs_pulm   <- factor(saf_base_mod$ch_obs_pulm)
saf_base_mod$prev_stroke   <- factor(saf_base_mod$prev_stroke)
saf_base_mod$prev_pacemaker  <- factor(saf_base_mod$prev_pacemaker)
saf_base_mod$smoke2        <- factor(saf_base_mod$smoke2)
saf_base_mod$frailty_status        <- factor(saf_base_mod$frailty_status)
saf_base_mod$lflg        <- factor(saf_base_mod$lflg)
saf_base_mod$ntm        <- factor(saf_base_mod$ntm)
saf_base_mod$avr        <- factor(saf_base_mod$avr)

# Lasso pre-selection
X <- model.matrix(safety ~ ., data = saf_base_mod)[, -1]  # numeric matrix, no intercept
y_vec <- ifelse(saf_base_mod$safety=="1",1,0)

set.seed(1)
cvfit <- cv.glmnet(
  X, y_vec,
  family = "binomial",
  alpha  = 1
)
plot(cvfit)

# use lambda.1se to be conservative (fewer variables)
lambda_sel <- cvfit$lambda.min

coef_lasso <- coef(cvfit,s=lambda_sel)
sel_names  <- rownames(coef_lasso)[coef_lasso[,1] != 0]
sel_names  <- setdiff(sel_names, "(Intercept)")
sel_names

orig_vars <- setdiff(colnames(saf_base_mod), c("halt2"))

selected_vars <- orig_vars[
  sapply(orig_vars, function(v)
    any(grepl(paste0("^", v), sel_names))
  )
]

selected_vars


m2 <- glm(safety~NTProBNP+LDLchol+tavi_supra_pos+ntm, data = saf_base_mod, family = binomial)

m3 <- glm(safety~age+sex+cad+prev_stroke+diabetes+hypertension+GFR+euroscore, data = saf_base_mod, family = binomial)
m4 <- glm(safety~age+sex+cad+prev_stroke+diabetes+hypertension+GFR, data = saf_base_mod, family = binomial)

# Brier score
p_null <- mean(y_vec)         # null model predicts mean outcome
brier_null <- mean((y_vec - p_null)^2)

p_hat <- predict(m2, type = "response")  # predicted probabilities                         # or your original outcome vector
brier_score <- mean((y_vec - p_hat)^2)

brier_scaled <- 1 - brier_score / brier_null
brier_scaled
