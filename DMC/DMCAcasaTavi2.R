###########################
#     DMC Acasa Tavi     ##
#     Erica 02.03.23     ##
###########################





### this is the code to produce the third DMC report for
### the ACASA TAVI trial 
### first DMC was done after 20 patients have been included
### the second DMC includes baseline for all included patients and one year 
### measurements for those patients who have the 12-month visit
### for the third I did the same as the second
### added the HALT outcome (just overall, not by treatment arm)





# open files needed to produce descriptive tables 

DEMO <- read_excel(filename, 
                   sheet = "DM", skip = 1)

PHYS <- read_excel(filename, 
                   sheet = "PE", skip = 1)

LAB <- read_excel(filename, 
                   sheet = "LB", skip = 1)

RAN <- read_excel(filename, 
                  sheet = "RAN", skip = 1)

# check all treatment info
RAN <- RAN %>% filter(!is.na(RAN_TRT))

# only include subjects that are randomized
# (should make no difference)
ids <- RAN$SubjectId
DEMO <- DEMO %>% filter(SubjectId %in% ids)
PHYS <- PHYS %>% filter(SubjectId %in% ids)
LAB <- LAB %>% filter(SubjectId %in% ids)



# select and rename relevant variables and merge info
DEMO <- DEMO %>% select(SubjectId, DMAGE, SEX) %>% rename(Age = DMAGE, Sex = SEX) %>% mutate(Age = as.numeric(Age))
PHYS <- PHYS %>% select(SubjectId, EventName, AGEVIS, PEWEIGHT, PEHEIGHT, PEBMI, 
                        PESYSBP, PEDIABP) %>% 
        rename(AgeVis = AGEVIS, Weight = PEWEIGHT, 
               Height = PEHEIGHT, BMI = PEBMI, SystolicBP = PESYSBP,
               DiastolicBP = PEDIABP)

LAB <- LAB %>% select(SubjectId, EventName, LBHHBRES, LBHPLRES, LBHLCRES, LBINRRES,
                      LBSODRES, LBPOTRES, LBALTRES, LBCCTRES, LBBNPRES,
                      LBCRERES, LBGFRRES, LBGLURES, LBHGHRE, LBTRIRES, 
                      LBTCHRES, LBHDLRES, LBLDLRES, LBALBRES, LBCRPRES, 
                      LBBILRES) %>%
  rename(Hemoglobin = LBHHBRES, Plateles = LBHPLRES, Leucocytes = LBHLCRES, 
         INR = LBINRRES, Sodium = LBSODRES, Potassium = LBPOTRES,
         ALT = LBALTRES, CardiacTroponin = LBCCTRES, NTProBNP = LBBNPRES,
         Creatinine = LBCRERES, GRF = LBGFRRES, Glucose = LBGLURES, GlycaHemoglobin = LBHGHRE,
         Triglycerides =  LBTRIRES, 
         TotCholesterol = LBTCHRES, HDLCholesterol = LBHDLRES, LDLCholesterol = LBLDLRES, 
         Albumin = LBALBRES, CRP = LBCRPRES, Biluribin =LBBILRES)

RAN <- RAN %>% select(SubjectId, RAN_TRT) %>% rename(Treatment = RAN_TRT)




# lab and phys have baseline and 12 months measurements
# separate those!


# Screening visit
LAB0 <- LAB %>% filter(EventName == 'Screening')
PHYS0 <- PHYS %>% filter(EventName == 'Screening')

# One Year visit
LAB12 <- LAB %>% filter(EventName == '12 months')
PHYS12 <- PHYS %>% filter(EventName == '12 months')


# join different data sources                      
Baseline <- inner_join(DEMO, RAN, by = 'SubjectId')
Baseline2 <- inner_join(PHYS0, Baseline, by = 'SubjectId')
Baseline3 <- inner_join(Baseline2, LAB0, by = 'SubjectId')

# most variables are characters and need to be converted into numeric
BaselineNum <- Baseline3 %>% mutate_at(c("Weight", "Height", "BMI", "SystolicBP", "DiastolicBP", 
                                       "Hemoglobin", "Plateles", "Leucocytes", "INR", 
                                       "Sodium", "Potassium", "ALT", "CardiacTroponin", "NTProBNP", 
                                       "Creatinine", "GRF", "Glucose", "GlycaHemoglobin", "Triglycerides", 
                                       "TotCholesterol", "HDLCholesterol", "LDLCholesterol", "Albumin",       
                                       "CRP", "Biluribin")
  
  ,as.numeric)

BaselineNum$Treatment <- as.factor(BaselineNum$Treatment)


# same for the one year data
OneYear <- inner_join(DEMO, RAN, by = 'SubjectId')
OneYear2 <- inner_join(PHYS12, OneYear, by = 'SubjectId')
OneYear3 <- inner_join(OneYear2, LAB12, by = 'SubjectId')

# most variables are characters and need to be converted into numeric
OneYearNum <- OneYear3 %>% mutate_at(c("Weight", "Height", "BMI", "SystolicBP", "DiastolicBP", 
                                         "Hemoglobin", "Plateles", "Leucocytes", "INR", 
                                         "Sodium", "Potassium", "ALT", "CardiacTroponin", "NTProBNP", 
                                         "Creatinine", "GRF", "Glucose", "GlycaHemoglobin", "Triglycerides", 
                                         "TotCholesterol", "HDLCholesterol", "LDLCholesterol", "Albumin",       
                                         "CRP", "Biluribin")
                                       
                                       ,as.numeric)

OneYearNum$Treatment <- as.factor(OneYearNum$Treatment)

# create summary tables 

# report n, mean and SD of age and BMI
# report n and % for gender
# summarized by group and overall

# functions to extract summaries
mean_sd <- function(data, var, group, digits = 1) {
  var <- ensym(var)
  group <- ensym(group)
  data %>% 
    group_by(!!group) %>% 
    summarise(mean = mean(!!var, na.rm = TRUE), sd = sd(!!var, na.rm = TRUE), missing = sum(is.na(!!var))) %>% 
    mutate_at(vars(mean, sd), ~round(., digits = digits)) %>% 
    mutate(txt = paste0(mean, " (", sd, ")")) %>% 
    select(group, txt) %>% 
    deframe
}


n_pct <-  function(data, var, group, level = 1) {
  var <- ensym(var)
  group <- ensym(group)
  data %>% 
    filter(!is.na(!!var)) %>% 
    group_by(!!group, !!var) %>% 
    summarise(n = n()) %>% 
    group_by(!!group) %>% 
    mutate(tot = sum(n),
           pct = round(n/tot*100,digits = 1)) %>% 
    mutate(txt = paste0(n, " (", pct, "%)")) %>% 
    filter(!!var == !!level) %>% 
    ungroup %>% 
    select(group, txt) %>% 
    deframe
}

empty <- function(data, var, group, ...){
  group <- ensym(group)
  data %>% 
    group_by(!!group) %>% 
    summarise(n = n()) %>% 
    mutate(txt = "") %>% 
    select(group,txt) %>% 
    deframe
}

stats_exec <- function(f, data, var, group, ...){
  exec(f, data, var, group, !!!(...))
}
# define table format

dm_table <- tribble(
  ~text, ~f, ~var, ~param,
  "Age at enrolment (years)", "mean_sd", "Age", list(digits = 1), 
  "Sex, n (%)", "empty", "", list(),
  "Male", "n_pct", "Sex", list(level = "Male"),
  "Female", "n_pct", "Sex", list(level = "Female"),
  "BMI (kg/m2)", "mean_sd", "BMI", list(digits = 2),
  "Height (cm)", "mean_sd", "Height", list(digits = 2),
  "Weight (kg)", "mean_sd", "Weight", list(digits = 2),
  "Systolic blood pressure (mmHg)", "mean_sd", "SystolicBP", list(digits = 2),
  "Diastolic blood pressure (mmHg)", "mean_sd", "DiastolicBP", list(digits = 2),
  "Hemoglobin", "mean_sd", "Hemoglobin", list(digits = 2),
  "Plateles", "mean_sd", "Plateles", list(digits = 2),
  "INR", "mean_sd", "INR", list(digits = 2),
  "Sodium", "mean_sd", "Sodium", list(digits = 2),
  "Potassium", "mean_sd", "Potassium", list(digits = 2),
  "ALT", "mean_sd", "ALT", list(digits = 2),
  "CardiacTroponin", "mean_sd", "CardiacTroponin", list(digits = 2),
  "NTProBNP", "mean_sd", "NTProBNP", list(digits = 2),
  "Creatinine", "mean_sd", "Creatinine", list(digits = 2),
  "GRF", "mean_sd", "GRF", list(digits = 2),
  "Glucose", "mean_sd", "Glucose", list(digits = 2),
  "GlycaHemoglobin", "mean_sd", "GlycaHemoglobin", list(digits = 2),
  "Triglycerides", "mean_sd", "Triglycerides", list(digits = 2),
  "TotCholesterol", "mean_sd", "TotCholesterol", list(digits = 2),
  "HDLCholesterol", "mean_sd", "HDLCholesterol", list(digits = 2),
  "LDLCholesterol", "mean_sd", "LDLCholesterol", list(digits = 2),
  "Albumin", "mean_sd", "Albumin", list(digits = 2),
  "CRP", "mean_sd", "CRP", list(digits = 2),
  "Biluribin", "mean_sd", "Biluribin", list(digits = 2)
  
)



dm_table0 <- dm_table %>% 
  mutate(data = list(BaselineNum),
         group = "Treatment") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(BaselineNum$Treatment)) 



head0 <- BaselineNum %>% 
  group_by(Treatment) %>% 
  summarise(n=n()) %>% 
  mutate(txt = paste0(Treatment, "(N=", n, ")")) %>% 
  select(txt) %>% 
  deframe

head0 <- c("Characteristic", head0)


dm_table12 <- dm_table %>% 
  mutate(data = list(OneYearNum),
         group = "Treatment") %>% 
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>% 
  mutate(id = map(res,names)) %>% 
  unnest(c(res, id)) %>% 
  pivot_wider(values_from = res, names_from = id) %>% 
  select(text, levels(OneYearNum$Treatment)) 



head12 <- OneYearNum %>% 
  group_by(Treatment) %>% 
  summarise(n=n()) %>% 
  mutate(txt = paste0(Treatment, "(N=", n, ")")) %>% 
  select(txt) %>% 
  deframe

head12 <- c("Characteristic", head12)

colnames(dm_table0) <- head0
colnames(dm_table12) <- head12


# some plots on baseline characteristics

vars <- c('Age', 'BMI', 'Height', 'Weight', 'SystolicBP', 'DiastolicBP') 
          
vars2 <- c("Hemoglobin", "Plateles", "INR", "Sodium", "Potassium", "ALT", 
            "CardiacTroponin", "NTProBNP", "Creatinine", "GRF", 
            "Glucose", "GlycaHemoglobin", "Triglycerides", "TotCholesterol", 
            "HDLCholesterol", "LDLCholesterol", "Albumin", "CRP", "Biluribin")
 
plot_for_loop <- function(df, .y_var) {
  
  # convert strings to variable
  y_var <- sym(.y_var)
  # unquote variables using !! 
  ggplot(df, aes(x = SubjectId, y = !! y_var)) + 
    geom_point(aes(color = Treatment)) + 
    theme_bw()+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}

plot_list <- list()
for (ii in 1:length(vars)){
  plot_list[[ii]] <- plot_for_loop(BaselineNum, vars[ii])
} 

# Combine all plots
# create one unique legend for all plots
p_no_legend <- lapply(plot_list, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(plot_list[[1]] + theme(legend.position = "bottom"))

title <- cowplot::ggdraw() + cowplot::draw_label("Summary variables at screening", fontface = "bold")


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 2)


# separate plots for laboratory variables 
# 19 variables total
plot_list2 <- list()
for (ii in 1:6){
  plot_list2[[ii]] <- plot_for_loop(BaselineNum, vars2[ii])
} 

# Combine all plots
# create one unique legend for all plots
p_no_legend2 <- lapply(plot_list2, function(x) x + theme(legend.position = "none"))
legend2 <- cowplot::get_legend(plot_list2[[1]] + theme(legend.position = "bottom"))

title2 <- cowplot::ggdraw() + cowplot::draw_label("Laboratory variables at screening", fontface = "bold")


p_grid2 <- cowplot::plot_grid(plotlist = p_no_legend2, ncol = 2)

plot_list3 <- list()
for (ii in 1:6){
  plot_list3[[ii]] <- plot_for_loop(BaselineNum, vars2[6+ii])
} 

# Combine all plots
# create one unique legend for all plots
p_no_legend3 <- lapply(plot_list3, function(x) x + theme(legend.position = "none"))
legend3 <- cowplot::get_legend(plot_list3[[1]] + theme(legend.position = "bottom"))

title3 <- cowplot::ggdraw() + cowplot::draw_label("Laboratory variables at screening", fontface = "bold")


p_grid3 <- cowplot::plot_grid(plotlist = p_no_legend3, ncol = 2)

plot_list4 <- list()
for (ii in 1:7){
  plot_list4[[ii]] <- plot_for_loop(BaselineNum, vars2[12+ii])
} 

# Combine all plots
# create one unique legend for all plots
p_no_legend4 <- lapply(plot_list4, function(x) x + theme(legend.position = "none"))
legend4 <- cowplot::get_legend(plot_list4[[1]] + theme(legend.position = "bottom"))

title4 <- cowplot::ggdraw() + cowplot::draw_label("Laboratory variables at screening", fontface = "bold")


p_grid4 <- cowplot::plot_grid(plotlist = p_no_legend4, ncol = 2)


# One Year
# separate plots for laboratory variables 
# 19 variables total
plot_list12 <- list()
for (ii in 1:length(vars)){
  plot_list12[[ii]] <- plot_for_loop(OneYearNum, vars[ii])
} 

p_no_legend12 <- lapply(plot_list12, function(x) x + theme(legend.position = "none"))
legend12 <- cowplot::get_legend(plot_list12[[1]] + theme(legend.position = "bottom"))

title.12 <- cowplot::ggdraw() + cowplot::draw_label("Summary variables at one year", fontface = "bold")


p_grid.12 <- cowplot::plot_grid(plotlist = p_no_legend12, ncol = 2)


plot_list2.12 <- list()
for (ii in 1:6){
  plot_list2.12[[ii]] <- plot_for_loop(OneYearNum, vars2[ii])
} 

# Combine all plots
# create one unique legend for all plots
p_no_legend2.12 <- lapply(plot_list2.12, function(x) x + theme(legend.position = "none"))
legend2.12 <- cowplot::get_legend(plot_list2.12[[1]] + theme(legend.position = "bottom"))

title2.12 <- cowplot::ggdraw() + cowplot::draw_label("Laboratory variables at one year", fontface = "bold")


p_grid2.12 <- cowplot::plot_grid(plotlist = p_no_legend2.12, ncol = 2)

plot_list3.12 <- list()
for (ii in 1:6){
  plot_list3.12[[ii]] <- plot_for_loop(OneYearNum, vars2[6+ii])
} 

# Combine all plots
# create one unique legend for all plots
p_no_legend3.12 <- lapply(plot_list3.12, function(x) x + theme(legend.position = "none"))
legend3.12 <- cowplot::get_legend(plot_list3.12[[1]] + theme(legend.position = "bottom"))

title3.12 <- cowplot::ggdraw() + cowplot::draw_label("Laboratory variables at one year", fontface = "bold")


p_grid3.12 <- cowplot::plot_grid(plotlist = p_no_legend3.12, ncol = 2)

plot_list4.12 <- list()
for (ii in 1:7){
  plot_list4.12[[ii]] <- plot_for_loop(OneYearNum, vars2[12+ii])
} 

# Combine all plots
# create one unique legend for all plots
p_no_legend4.12 <- lapply(plot_list4.12, function(x) x + theme(legend.position = "none"))
legend4.12 <- cowplot::get_legend(plot_list4[[1]] + theme(legend.position = "bottom"))

title4.12 <- cowplot::ggdraw() + cowplot::draw_label("Laboratory variables at one year", fontface = "bold")


p_grid4.12 <- cowplot::plot_grid(plotlist = p_no_legend4.12, ncol = 2)




###########
## AEs 
###########


## load data on adverse events and medical coding
AE <- read_excel(filename, 
                 sheet = "AE", skip = 1)

MEDRA <- read_excel(filename, 
                    sheet = 'MedDRA', skip = 1)
# select only AEs
MEDRA <- MEDRA %>% filter(FormId== 'AE')

AE$AESERCD <- as.numeric(AE$AESERCD)

# this will include all subjects
AE0 <- full_join(AE, RAN, by = "SubjectId")
AE1 <- left_join(AE0, MEDRA, by = c("SubjectId", "EventSeq"))

# this will include only subjects with AEs
AE2 <- inner_join(AE, RAN, by = "SubjectId")
AE1.2 <- left_join(AE2, MEDRA, by = c("SubjectId", "EventSeq"))




total_n <- n_distinct(Baseline$SubjectId)

# functions to create AE tables
ae_n_pct <-  function(data, var, group, level = 1) {
  var <- ensym(var)
  group <- ensym(group)
  
  data %>% 
    group_by(SubjectId, !!group, !!var) %>% 
    summarise(n = sum(!!var)) %>% 
    group_by(!!group, !!var) %>% 
    summarise(n_ae = sum(n),
              n_pat = n()) %>% 
    group_by(!!group) %>% 
    mutate(N_pat = sum(n_pat),
           pct = round(n_pat/N_pat*100,digits = 1),
           txt = paste0(n_pat, " (", pct, "%)")) %>%
    filter(!!var %in% !!level) %>% 
    ungroup %>% 
    select(!!group, txt) %>% 
    deframe
}

ae_N_n_pct <-  function(data, var, group, level = 1) {
  var <- ensym(var)
  group <- ensym(group)
  
  data %>% 
    group_by(SubjectId, !!group) %>% 
    summarise(n = sum(!!var)) %>% 
    mutate(!!var := if_else(n==0, 0, 1)) %>% 
    group_by(!!group, !!var) %>% 
    summarise(n_ae = sum(n),
              n_pat = n()) %>% 
    group_by(!!group) %>% 
    mutate(N_pat = sum(n_pat),
           pct = round(n_pat/N_pat*100,digits = 1),
           txt = paste0("[", n_ae,"] ", n_pat, " (", pct, "%)")) %>%
    mutate(txt = if_else(n_ae == 0, "[0] 0 (0%)", txt)) %>% 
    filter(!!var %in% !!level) %>% 
    ungroup %>% 
    select(!!group, txt) %>% 
    deframe
}

stats_exec <- function(f, data, var, group, ...){
  exec(f, data, var, group, !!!(...))
}




AE1 <- AE1 %>%
  mutate(anyae = if_else(is.na(AETERM), 0, 1),
         sae = if_else(is.na(AETERM), 0, AESERCD)
  ) %>%   group_by(SubjectId) %>% 
  mutate(n_ae = sum(anyae),
         one_ae = n_ae == 1,
         two_ae = n_ae == 2,
         three_plus_ae = n_ae > 2,
         anysae = max(sae)) %>% 
  ungroup


AE1.2 <- AE1.2 %>%
  mutate(anyae = if_else(is.na(AETERM), 0, 1),
         sae = if_else(is.na(AETERM), 0, AESERCD)
  ) %>%   group_by(SubjectId) %>% 
  mutate(n_ae = sum(anyae),
         one_ae = n_ae == 1,
         two_ae = n_ae == 2,
         three_plus_ae = n_ae > 2,
         anysae = max(sae)) %>% 
  ungroup


arms <- c("ASA", "DOAC")


header_ae <- AE1 %>%
  group_by(Treatment, SubjectId) %>%
  summarise(n=n()) %>%
  group_by(Treatment) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(armtxt = arms) %>%
  mutate(txt = paste0(armtxt, " (N=", n, ")")) %>%
  select(txt) %>%
  deframe

header_ae1.2 <- AE1.2 %>%
  group_by(Treatment, SubjectId) %>%
  summarise(n=n()) %>%
  group_by(Treatment) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(armtxt = arms) %>%
  mutate(txt = paste0(armtxt, " (N=", n, ")")) %>%
  select(txt) %>%
  deframe



ae_summary_table <- tribble(
  ~text,  ~var, ~f,
  "Number of AEs", "anyae", "ae_N_n_pct",
  "Number of patients with any AEs?", "anyae", "ae_n_pct",
  "Number of patients with one AE", "one_ae", "ae_n_pct",
  "Number of patients with two AE", "two_ae", "ae_n_pct",
  "Number of patients with three or more AEs?", "three_plus_ae", "ae_n_pct",
  "Number of SAEs", "sae", "ae_N_n_pct",
  "Number of patients with any SAEs", "anysae","ae_n_pct"
)

 ae_summary_table <- ae_summary_table %>%
  mutate(data = list(AE1),
         group = "Treatment",
         param = list(level = 1)) %>%
  mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>%
  mutate(id = map(res,names)) %>%
  unnest(c(res, id)) %>%
  mutate(id = paste0("txt", id)) %>%
  pivot_wider(values_from = res, names_from = id) %>%
  select(text, starts_with("txt"))

 
 
 ae_summary_table1.2 <- tribble(
   ~text,  ~var, ~f,
   "Number of AEs", "anyae", "ae_N_n_pct",
   "Number of patients with any AEs?", "anyae", "ae_n_pct",
   "Number of patients with one AE", "one_ae", "ae_n_pct",
   "Number of patients with two AE", "two_ae", "ae_n_pct",
   "Number of patients with three or more AEs?", "three_plus_ae", "ae_n_pct",
   "Number of SAEs", "sae", "ae_N_n_pct",
   "Number of patients with any SAEs", "anysae","ae_n_pct"
 )
 
 ae_summary_table1.2 <- ae_summary_table1.2 %>%
   mutate(data = list(AE1.2),
          group = "Treatment",
          param = list(level = 1)) %>%
   mutate(res = pmap(list(f, data, var, group, param), stats_exec)) %>%
   mutate(id = map(res,names)) %>%
   unnest(c(res, id)) %>%
   mutate(id = paste0("txt", id)) %>%
   pivot_wider(values_from = res, names_from = id) %>%
   select(text, starts_with("txt"))
 
 
 

 
 ae_table_fns <- function(data, filtervar){
   
   filtervar = ensym(filtervar)
   
   data %>%
     group_by(Treatment) %>%
     mutate(N_pat = n_distinct(SubjectId)) %>%
    filter(!!filtervar == 1)  %>%
     group_by(SubjectId, Treatment, N_pat, soc_name, pt_name) %>%
     summarise(n_ae = n()) %>%
     filter(!is.na(pt_name)) %>%
     group_by(Treatment, N_pat, soc_name, pt_name) %>%
     summarise(n_pat = n(),
               n_ae = sum(n_ae)) %>%
     mutate(pct = round(n_pat/N_pat*100,digits = 1),
            txt = paste0("[", n_ae,"] ", n_pat, " (", pct, "%)"),
            arm = paste0("arm", Treatment)) %>%
     ungroup %>% select(arm,  soc_name, pt_name, txt) %>%
     pivot_wider(values_from = txt, names_from = arm) %>%
     mutate_at(vars(starts_with("arm")), ~if_else(is.na(.), "", .)) %>%
     arrange(soc_name, pt_name) %>%  group_by(soc2 = soc_name) %>%
     mutate(soc_name = if_else(row_number() != 1, "", soc_name)) %>% ungroup() %>% select(-soc2)
 }
 
 
 

 
 
 
# produce tables with preferred term
# the first one includes all AEs
# the second one only includes serious
 
 
 
tab_PT<-  AE1.2 %>%
   bind_rows(AE1.2, .id="added") %>%
   mutate(pt_name = if_else(added == 2, "#Total", pt_name)) %>%
   mutate(all = 1) %>%
   ae_table_fns("all")
 
 
tab_PT_SAE <- AE1.2 %>%
  bind_rows(AE1.2, .id="added") %>%
  mutate(pt_name = if_else(added == 2, "#Total", pt_name)) %>%
  ae_table_fns("AESERCD") 


 # Protocol Deviations

PD <- read_excel(filename, 
                 sheet = "DV", skip = 1)
PD <- inner_join(PD, RAN)
PDtab <- PD %>% select(SubjectId, DVCAT, Treatment)

# End of Study

#summary functions here
summary.fun <- function(x) {
  m <- median(x, na.rm = T)
  q1 <- quantile(x, prob = 0.25, na.rm = T)
  q2 <- quantile(x, prob = 0.75, na.rm = T)
  summary <- paste0(m, ' (', q1, '-', q2, ')')
  return(summary)
  
  }

EOS <-  read_excel(filename, 
                   sheet = "EOS", skip = 1)
RAN <- read_excel(filename, 
                  sheet = "RAN", skip = 1)

# check all treatment info
RAN <- RAN %>% filter(!is.na(RAN_TRT))
RAN <- RAN %>% select(SubjectId, RAN_TRT, EventDate) %>% 
  rename(Treatment = RAN_TRT, InclDate = EventDate)

EOS <- inner_join(EOS, RAN)
EOStab <- EOS %>% mutate(I = as.Date(InclDate)) %>% 
  mutate(E = as.Date(EOSDAT)) %>% mutate(length = E-I)

EOSsum <- EOStab %>% group_by(Treatment) %>% 
  summarise(LengthOfStudy = summary.fun(length))

# Safety outcome  

# define endpoint for each subject 
# varc3 events  + myocardial infarction + stroke + death


# VARC events data
SO <- read_excel(filename, 
                 sheet = "SO", skip = 1)

SO <- inner_join(SO, RAN)

varctab <- SO %>% select(SubjectId, SOVARC)


# only VARC of type 1,2,3 or 4 should be included
SO <- SO %>% filter(SOVARCCD != 5)


# infarction and stroke data
KU <- read_excel(filename, 
                 sheet = "KU", skip = 1)
KU <- inner_join(KU, RAN)

KU <- KU %>% select(SubjectId, KU1_3, KU1_3CD, KU1_3DAT, KU1_2, KU1_2CD, KU1_2DAT, Treatment, InclDate)



KUtb <- KU %>% filter(KU1_3CD == 1 | KU1_2CD == 1)
colnames(KUtb) <- c('SubjectId', 'Hjerteinfarkt','Hjerteinfarkt code','Hjerteinfarkt date', 'Hjerneslag', 'Hjerneslag code', 
                  'Hjerneslag date', 'Treatment', 'Inclusion Date')



# deaths
KU <- read_excel(filename, 
                 sheet = "KU", skip = 1)
KU <- inner_join(KU, RAN)

DE <- KU %>% select(SubjectId, KU1_0, KU1_0DAT,Treatment, InclDate)

DE <- DE %>% filter(KU1_0 == 'Ja')
colnames(DE) <- c('SubjectId', 'Death','Deat date', 'Treatment', 'Inclusion Date')

# check that it's the same info as in the AE data
DE2 <- AE1 %>% filter(AESDTH == 'Yes') %>% select(SubjectId, AESDTH, AEENDAT, AETERM, Treatment)
colnames(DE2) <- c('SubjectId', 'Death','Deat date','Term', 'Treatment')

DE2 <- inner_join(DE2, RAN)
#DEE <- rbind(DE, DE2)
#DEE <- DEE[!duplicated(DEE$SubjectID), ]

DEE <- DE2


# compute composity safety outcome as sum of the previous

CSO <- full_join(SO, KUtb, by = c("SubjectId", "Treatment"))
CSO <- full_join(CSO, DEE)
CSO <- CSO %>% select(SubjectId, Treatment, `Hjerteinfarkt code`, `Hjerneslag code`, 
                      Death, SOVARC)

CSO <- CSO %>% mutate(Hcode = as.numeric(`Hjerteinfarkt code`)) %>% mutate(Scode = as.numeric(`Hjerneslag code`)) %>%
  mutate(Dcode = ifelse(Death == 'Yes', 1, 0)) %>% mutate(VARCcode = ifelse(!is.na(SOVARC), 1, 0)) 


CSO[is.na(CSO$`Hjerteinfarkt code`), ]$Hcode <- 0
CSO[is.na(CSO$`Hjerneslag code`), ]$Scode <- 0
CSO[is.na(CSO$Death), ]$Dcode <- 0



CSO <- CSO %>% mutate(Safety = Hcode + Scode + Dcode + VARCcode)
CSO <- CSO %>% select(SubjectId, Treatment, Hcode, Scode, Dcode, VARCcode, Safety)


# compute number of HALT (not by treatment allocation)
CTO <- read_excel(filename, 
                   sheet = "CTO", skip = 1)
