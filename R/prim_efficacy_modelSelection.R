# Model selection for primary efficacy endpoint (HALT)
# Only do before unbliding


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
eff <- read_rds("data/td/cto_td.rds")
extra <- read_rds("data/td/baseline_extra_td.rds")

adsl <- adsl %>% select(-c(site,ran_date))
baseline <- baseline %>% select(-site)
extra <- extra %>% select(subjectid,euroscore=score)

eff <- eff %>% left_join(adsl,by="subjectid")
eff <- eff %>% mutate(halt2=as.factor(ifelse(halt=="yes" | death=="yes","yes","no")))

eff_base <- eff %>% left_join(baseline,by="subjectid")
eff_base <- eff_base %>% left_join(extra,by="subjectid")

eff_base <- eff_base %>% mutate(smoke2 = 
                                  fct_collapse(smoke,
                                               smoker = c("On a daily basis", "Smokes occasionally")))

# subset for model selection (do not include treatment since it is "uncorrelated" with the rest )
eff_base_mod <- eff_base %>% select(halt2:sex,site,bmi:diastolicBP,body_temp:resp_rate,
                                    abnormal_find:asc_aorta_diam,avmg:lvef,stvo:avr,frailty_status:smoke2) #site,
# 53 potential covariates
p <- ncol(eff_base_mod)-1

idc <- which(apply(eff_base_mod[, 2:(p+1)], 1, function(x) any(is.na(x))))
eff_base_mod[idc,] %>% select(resp_rate,avmg:avr,euroscore) %>% print(n=20)

# Do something about missing values... (replace by median or most common clas)

for (i in 2:(p+1)){
  vv <- unlist(eff_base_mod[,i])
  if (sum(is.na(vv))==0) next
  if (!is.factor(vv)){
    eff_base_mod[is.na(vv),i] <- median(vv,na.rm=T)
  }else{
    tt <- table(vv)
    eff_base_mod[is.na(vv),i] <- names(sort(tt,T)[1])
  }
}

# Drop missing HALT rows: ok for model selection
id <- is.na(eff_base_mod$halt2)
eff_base_mod <- eff_base_mod[!id,]

# Check factors
eff_base_mod$site <- factor(eff_base_mod$site)
eff_base_mod$abnormal_find <- factor(eff_base_mod$abnormal_find)
eff_base_mod$sex           <- factor(eff_base_mod$sex)
eff_base_mod$valve_type    <- factor(eff_base_mod$valve_type)
eff_base_mod$tavi_post_dila<- factor(eff_base_mod$tavi_post_dila)
eff_base_mod$tavi_supra_pos<- factor(eff_base_mod$tavi_supra_pos)
eff_base_mod$hypertension  <- factor(eff_base_mod$hypertension)
eff_base_mod$cad           <- factor(eff_base_mod$cad)
eff_base_mod$diabetes      <- factor(eff_base_mod$diabetes)
eff_base_mod$ch_obs_pulm   <- factor(eff_base_mod$ch_obs_pulm)
eff_base_mod$prev_stroke   <- factor(eff_base_mod$prev_stroke)
eff_base_mod$prev_pacemaker  <- factor(eff_base_mod$prev_pacemaker)
eff_base_mod$smoke2        <- factor(eff_base_mod$smoke2)
eff_base_mod$frailty_status        <- factor(eff_base_mod$frailty_status)
eff_base_mod$lflg        <- factor(eff_base_mod$lflg)
eff_base_mod$ntm        <- factor(eff_base_mod$ntm)
eff_base_mod$avr        <- factor(eff_base_mod$avr)

# Lasso pre-selection
X <- model.matrix(halt2 ~ ., data = eff_base_mod)[, -1]  # numeric matrix, no intercept
y_vec <- ifelse(eff_base_mod$halt2=="yes",1,0)

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

orig_vars <- setdiff(colnames(eff_base_mod), c("halt2"))

selected_vars <- orig_vars[
  sapply(orig_vars, function(v)
    any(grepl(paste0("^", v), sel_names))
  )
]

selected_vars

# Model selection
library(MASS)
form_sel <- as.formula(
  paste("halt2 ~", paste(selected_vars, collapse = " + "))
)

m0 <- glm(halt2 ~ 1,    data = eff_base_mod, family = binomial)
mf <- glm(form_sel, data = eff_base_mod, family = binomial)

m_step <- stepAIC(
  m0,
  scope     = list(lower = m0, upper = mf),
  direction = "both",
  trace     = TRUE
)

summary(m_step)
AIC(m_step)

m2 <- glm(halt2~bmi+euroscore+frailty_status, data = eff_base_mod, family = binomial)

# Brier score
p_null <- mean(y_vec)         # null model predicts mean outcome
brier_null <- mean((y_vec - p_null)^2)

p_hat <- predict(m_step, type = "response")  # predicted probabilities                         # or your original outcome vector
brier_score <- mean((y_vec - p_hat)^2)

brier_scaled <- 1 - brier_score / brier_null
brier_scaled
