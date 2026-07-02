## Primary safety endpoint (blinded)

# Plotting safety events, switching and discontinuations

###############################

source("R/external/functions.R")

library(tidyverse)

adsl <- read_rds("data/ad/adsl.rds") # with shamrand
adh <- read_rds("data/td/adherence_blind_td.rds")
saf <- read_rds("data/td/cso_td.rds")

adsl <- adsl %>% select(-site,-ran_date)
saf <- saf %>% left_join(adsl,by="subjectid")

# How may safety events happen after switching or discontinuation?
adh2 <- adh %>% select(subjectid,ran_date,eos_date,change_date,switch_discont,non_completer_notDead,reason)
adh_saf <- adh2 %>% left_join(saf[,c("subjectid","safety","event_date","ran_trt")],by="subjectid")

n_tot <- sum(adh_saf$safety==1,na.rm=T)
n_saf_switch <- sum(adh_saf$safety==1 & !is.na(adh_saf$change_date),na.rm=T)
n_saf_disc <- sum(adh_saf$safety==1 & is.na(adh_saf$change_date) & adh_saf$switch_discont==1,na.rm=T)
n_before <- sum(adh_saf$safety==1 & adh_saf$event_date <= adh_saf$change_date,na.rm=T)
n_after <- sum(adh_saf$safety==1 & adh_saf$event_date > adh_saf$change_date,na.rm=T)

saf_events <- list(total=n_tot,saf_switch=n_saf_switch,before=n_before,after=n_after,n_deviations=sum(adh_saf$switch_discont==1,na.rm=T),
                   n_dev_plus_saf=sum(adh_saf$safety==1 |adh_saf$switch_discont==1,na.rm=T))

# Tables describing participants with deviations from treatment
adhSums <- adh_saf %>% group_by(ran_trt) %>%
  summarize(n_noChange=sum(switch_discont==0,na.rm=T),
            n_switch_stop =sum(switch_discont==1 & reason=="switching/stopping",na.rm=T),
            n_early_disc =sum(switch_discont==1 & reason!="switching/stopping",na.rm=T))
adhSums <- adhSums %>% 
  add_row(ran_trt = "Overall", n_noChange = sum(adhSums$n_noChange), n_switch_stop = sum(adhSums$n_switch_stop),
          n_early_disc =sum(adhSums$n_early_disc))
dev_tab <- adhSums


### Make nice figure
# Order patients within each treatment by time in study
# Only patients where something happends
df3 <- adh_saf %>% filter(switch_discont==1 | safety==1) %>%
  mutate(
    # time since randomization in days
    dur_total   = as.numeric(eos_date   - ran_date),      # total duration
    dur_change  = as.numeric(change_date - ran_date),     # NA if no change
    dur_event   = as.numeric(event_date  - ran_date),     # NA if no event
  ) %>%
  group_by(ran_trt) %>%
  arrange(dur_total, .by_group = TRUE) %>%
  mutate(patient_order = row_number()) %>%
  ungroup()

# Drop patients which have not yet finished
df3 <- df3 %>% filter(!is.na(dur_total))

# Green segments
seg_green <- df3 %>%
  mutate(
    g_start = case_when(
      !is.na(dur_change) ~ 0,
      #is.na(dur_change) & switch_discont == 1 ~ NA_real_,   # no green
      TRUE ~ 0
    ),
    g_end = case_when(
      !is.na(dur_change) ~ dur_change,
      #is.na(dur_change) & switch_discont == 1 ~ NA_real_,   # no green
      TRUE ~ dur_total
    )
  ) %>%
  filter(!is.na(g_start), !is.na(g_end), g_end > g_start) %>%
  transmute(
    ran_trt,
    patient_order,
    x    = g_start,
    xend = g_end,
    color = "blue"
  )


# Red segments
seg_red <- df3 %>%
  mutate(
    r_start = case_when(
      !is.na(dur_change) ~ dur_change,
      
      TRUE ~ NA_real_   # no red
    ),
    r_end = case_when(
      !is.na(dur_change) ~ dur_total,
      TRUE ~ NA_real_   # no red
    )
  ) %>%
  filter(!is.na(r_start), !is.na(r_end), r_end > r_start) %>%
  transmute(
    ran_trt,
    patient_order,
    x    = r_start,
    xend = r_end,
    color = "red"
  )


segments <- bind_rows(seg_green, seg_red)

segments$xend <- pmin(365,segments$xend)

segments <- segments %>% arrange(ran_trt,patient_order)

events <- df3 %>%
  filter(!is.na(dur_event)) %>%
  mutate(type="safety") %>%
  select(ran_trt, patient_order, time=dur_event,type)

events2 <- df3 %>%
  filter(switch_discont==1 & reason!="switching/stopping") %>%
  mutate(type="discont") %>%
  select(ran_trt, patient_order, time=dur_total,type)

events <- bind_rows(events,events2)

selected <- ggplot() +
  geom_segment(
    data = segments,
    aes(x = x, xend = xend,
        y = patient_order, yend = patient_order,
        colour = color),
    linewidth = 1.2
  ) +  
  geom_point(
    data = events,
    aes(x = time, y = patient_order,shape=type),
    size = 2
  ) +
  scale_colour_manual(values = c(blue = "#d1e5f0", red = "#ca0020"),
                      label=c(blue="Time before switching or stopping", 
                              red="Time after switching or stopping")) +
  scale_shape_manual(name=" ",
                     values=c(discont=1,safety=17),
                     label=c(discont="Discontinuation",safety="Safety event"))+
  scale_y_continuous(breaks = NULL) +
  labs(
    x = "Days since randomization",
    y = "Patients (ordered by time in study)",
    colour = NULL
  ) +
  facet_wrap(~ ran_trt, ncol = 2, scales = "free_y") +
  theme_bw() +
  theme(text = element_text(size = 20),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "bottom"
  )

### Saving stuff

save(selected,saf_events,dev_tab,file="data/res/prim_saf_fig.RData")


### Full figure with all patients (not shown)
# df2 <- adh_saf %>%
#   mutate(
#     # time since randomization in days
#     dur_total   = as.numeric(eos_date   - ran_date),      # total duration
#     dur_change  = as.numeric(change_date - ran_date),     # NA if no change
#     dur_event   = as.numeric(event_date  - ran_date),     # NA if no event
#   ) %>%
#   group_by(ran_trt) %>%
#   arrange(dur_total, .by_group = TRUE) %>%
#   mutate(patient_order = row_number()) %>%
#   ungroup()
# 
# # Drop patients which have not yet finished
# df2 <- df2 %>% filter(!is.na(dur_total))
# 
# # Green segments
# seg_green <- df2 %>%
#   mutate(
#     g_start = case_when(
#       !is.na(dur_change) ~ 0,
#       TRUE ~ 0
#     ),
#     g_end = case_when(
#       !is.na(dur_change) ~ dur_change,
#       TRUE ~ dur_total
#     )
#   ) %>%
#   filter(!is.na(g_start), !is.na(g_end), g_end > g_start) %>%
#   transmute(
#     ran_trt,
#     patient_order,
#     x    = g_start,
#     xend = g_end,
#     color = "blue"
#   )
# 
# 
# # Red segments
# seg_red <- df2 %>%
#   mutate(
#     r_start = case_when(
#       !is.na(dur_change) ~ dur_change,
#       
#       TRUE ~ NA_real_   # no red
#     ),
#     r_end = case_when(
#       !is.na(dur_change) ~ dur_total,
#       TRUE ~ NA_real_   # no red
#     )
#   ) %>%
#   filter(!is.na(r_start), !is.na(r_end), r_end > r_start) %>%
#   transmute(
#     ran_trt,
#     patient_order,
#     x    = r_start,
#     xend = r_end,
#     color = "red"
#   )
# 
# 
# segments <- bind_rows(seg_green, seg_red)
# 
# segments$xend <- pmin(365,segments$xend)
# 
# segments <- segments %>% arrange(ran_trt,patient_order)
# 
# events <- df2 %>%
#   filter(!is.na(dur_event)) %>%
#   mutate(type="safety") %>%
#   select(ran_trt, patient_order, time=dur_event,type)
# 
# events2 <- df2 %>%
#   filter(switch_discont==1 & reason!="switching/stopping") %>%
#   mutate(type="discont") %>%
#   select(ran_trt, patient_order, time=dur_total,type)
# 
# events <- bind_rows(events,events2)
# #events <- events[events$time<=365,]
# 
# all <- ggplot() +
#   geom_segment(
#     data = segments,
#     aes(x = x, xend = xend,
#         y = patient_order, yend = patient_order,
#         colour = color),
#     linewidth = 1.2
#   ) +  
#   geom_point(
#     data = events,
#     aes(x = time, y = patient_order,shape=type),
#     size = 2
#   ) +
#   scale_colour_manual(values = c(blue = "#d1e5f0", red = "#ca0020"),
#                       label=c(blue="Time before switching or stopping", 
#                               red="Time after switching or stopping")) +
#   scale_shape_manual(name=" ",
#                      values=c(discont=1,safety=17),
#                      label=c(discont="Discontinuation",safety="Safety event"))+
#   scale_y_continuous(breaks = NULL) +
#   labs(
#     x = "Days since randomization",
#     y = "Patients (ordered by time in study)",
#     colour = NULL
#   ) +
#   facet_wrap(~ ran_trt, ncol = 2, scales = "free_y") +
#   theme_bw() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor   = element_blank(),
#     legend.position = "bottom"
#   )
