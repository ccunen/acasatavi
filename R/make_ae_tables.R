#######################
##    ACASA-TAVI     ##
##    C. Cunen       ##
#######################

##############################
# Make tables for AEs
# Input: adsl, ae_td


###############################

source("R/external/functions.R")

library(tidyverse)

ae_td <- read_rds("data/td/ae_td.rds")
adsl <- read_rds("data/ad/adsl.rds") # with shamrand

# Joins
ae_all <- ae_td %>% full_join(adsl,by="subjectid")
ae_all <- ae_all %>% mutate(anyae=ifelse(is.na(aeterm),0,1),
                            sae=ifelse(anyae==1 & aeser=="Yes",1,0),
                            ae_p=ifelse(anyae==1 & any_possible==1,1,0),
                            sae_p=ifelse(sae==1 & any_possible==1,1,0),
                            susar=ifelse(sae==1 & saeexp=="Unexpected" & any_possible==1,1,0))

ae_all <- ae_all %>% filter(fas==1) %>% 
  mutate(overall = 1) %>% mutate(overall = as.factor(overall))


adae <- ae_all %>%
  group_by(subjectid) %>%
  mutate(
    n_ae = sum(anyae),
    one_ae = n_ae == 1,
    two_ae = n_ae == 2,
    three_plus_ae = n_ae > 2,
    anysae = max(sae),
    anysusar=max(susar)
  )

# Overall number of AEs and SAEs
arms <- c("DOAC", "ASA")

header_ae <- adae %>%
  group_by(ran_trt, subjectid) %>%
  summarise(n=n(), .groups = "drop_last") %>%
  group_by(ran_trt) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  ungroup() %>%
  mutate(armtxt = arms) %>%
  mutate(txt = paste0(armtxt, " (n=", n, ")")) %>%
  select(txt) %>%
  deframe


ae_summary_table <- tribble(
  ~text,  ~var, ~f,
  "Number of AEs", "anyae", "ae_N",
  "Number of patients with any AEs", "anyae", "ae_n_pct",
  "Number of patients with one AE", "one_ae", "ae_n_pct",
  "Number of patients with two AE", "two_ae", "ae_n_pct",
  "Number of patients with three or more AEs", "three_plus_ae", "ae_n_pct",
  "Number of SAEs", "sae", "ae_N",
  "Number of patients with any SAEs", "anysae","ae_n_pct",
  "Number of SUSARs", "susar", "ae_N"
)
# No SUSARs

ae_tab0 <- ae_summary_table %>%
  mutate(data = list(adae),
         group = "ran_trt",
         param = list(level = 1)) %>%
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>%
  mutate(id = map(res,names)) %>%
  unnest(c(res, id)) %>%
  mutate(id = paste0("txt", id)) %>%
  pivot_wider(values_from = res, names_from = id) %>%
  select(text, starts_with("txt"))

head <- c("text", header_ae)
colnames(ae_tab0) <- head


ae_tab_overall <- ae_summary_table %>% 
  mutate(data = list(adae),
         group = "overall",
         param = list(level = 1)) %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>%
  mutate(id = paste0("txt", id)) %>%
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, starts_with("txt")) 

head <- adae %>% 
  group_by(overall) %>% 
  summarise(n=length(unique(subjectid))) %>% 
  mutate(txt = paste0("Overall (n=", n, ")")) %>% 
  select(txt) %>% 
  deframe

head <- c("text", head)
colnames(ae_tab_overall) <- head


ae_table <- cbind(ae_tab0, ae_tab_overall) %>% select(-4)


# Overall number of AEs and SAEs with possible, probable or definite relation to study medication

adae_p <- ae_all %>%
  group_by(subjectid) %>%
  mutate(
    n_ae = sum(ae_p),
    one_ae = n_ae == 1,
    two_ae = n_ae == 2,
    three_plus_ae = n_ae > 2,
    anysae = max(sae_p),
    anysusar=max(susar)
  )


pae_summary_table <- tribble(
  ~text,  ~var, ~f,
  "Number of AEs", "ae_p", "ae_N",
  "Number of patients with any AEs", "ae_p", "ae_n_pct",
  "Number of patients with one AE", "one_ae", "ae_n_pct",
  "Number of patients with two AE", "two_ae", "ae_n_pct",
  "Number of patients with three or more AEs", "three_plus_ae", "ae_n_pct",
  "Number of SAEs", "sae_p", "ae_N",
  "Number of patients with any SAEs", "anysae","ae_n_pct",
  "Number of SUSARs", "susar", "ae_N"
)
# No SUSARs

pae_tab0 <- pae_summary_table %>%
  mutate(data = list(adae_p),
         group = "ran_trt",
         param = list(level = 1)) %>%
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>%
  mutate(id = map(res,names)) %>%
  unnest(c(res, id)) %>%
  mutate(id = paste0("txt", id)) %>%
  pivot_wider(values_from = res, names_from = id) %>%
  select(text, starts_with("txt"))

head <- c("text", header_ae)
colnames(pae_tab0) <- head


pae_tab_overall <- pae_summary_table %>% 
  mutate(data = list(adae_p),
         group = "overall",
         param = list(level = 1)) %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>%
  mutate(id = paste0("txt", id)) %>%
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, starts_with("txt")) 

head <- adae_p %>% 
  group_by(overall) %>% 
  summarise(n=length(unique(subjectid))) %>% 
  mutate(txt = paste0("Overall (n=", n, ")")) %>% 
  select(txt) %>% 
  deframe

head <- c("text", head)
colnames(pae_tab_overall) <- head


pae_table <- cbind(pae_tab0, pae_tab_overall) %>% select(-4)


# Adverse events of special interest (AESI)
adaesi <- adae %>% filter (aeaesiyn=="Yes") %>%
  select(subjectid,eventseq,aespid,soc=soc_name,pt=pt_name,ran_trt)
aesi_sum <- adaesi %>% group_by(soc,pt,ran_trt) %>%
  summarise(n = n(), .groups = "drop_last")%>%
  pivot_wider(names_from=ran_trt,values_from=n,names_sort=T,values_fill=0)

# Maximal intensity and causal relationships of AEs and SAEs to acetylsalicylic acid
tab <- table(ae_all$aesevext,ae_all$aeasarel)[,1:5]
asa_tab <- data.frame(tab)
colnames(asa_tab) <- c("intensity","causality","n")
asa_tab <- asa_tab %>% pivot_wider(names_from=causality,values_from=n)

# Maximal intensity and causal relationships of AEs and SAEs to apixaban
tab <- table(ae_all$aesevext,ae_all$aeapirel)[,1:5]
api_tab <- data.frame(tab)
colnames(api_tab) <- c("intensity","causality","n")
api_tab <- api_tab %>% pivot_wider(names_from=causality,values_from=n)

# Maximal intensity and causal relationships of AEs and SAEs to edoxaban
tab <- table(ae_all$aesevext,ae_all$aeedorel)[,1:5]
edo_tab <- data.frame(tab)
colnames(edo_tab) <- c("intensity","causality","n")
edo_tab <- edo_tab %>% pivot_wider(names_from=causality,values_from=n)

# Maximal intensity and causal relationships of AEs and SAEs to rivaroxaban
tab <- table(ae_all$aesevext,ae_all$aerivrel)[,1:5]
riv_tab <- data.frame(tab)
colnames(riv_tab) <- c("intensity","causality","n")
riv_tab <- riv_tab %>% pivot_wider(names_from=causality,values_from=n)

# AEs and SAEs by organ class and preferred term
adaes <- adae %>% filter (anyae==1) %>%
  select(subjectid,eventseq,aespid,sae,soc=soc_name,pt=pt_name,ran_trt)
ae_sum <- adaes %>% group_by(soc,pt,sae,ran_trt) %>%
  summarise(n = n(), .groups = "drop_last")%>%
  pivot_wider(names_from=c(sae,ran_trt),values_from=n,names_sort=T,values_fill=0)
# 0_DOAC and 0_ASA are AE only, 1_DOAC and 1_ASA are SAEs
ae_sum$soc <- replace(ae_sum$soc, which(duplicated(ae_sum$soc)), " ")

# Save tables
#aesi_sum,asa_tab,api_tab,edo_tab,riv_tab,
save(pae_table,ae_table,ae_sum,file="data/res/ae_tab.RData")
