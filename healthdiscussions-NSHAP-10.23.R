#Author: Tess Wild
#Date: May 24, 2025

#load packages 
library(dplyr) # tools for manipulating data (e.g., select, mutate, %>%, left_join)
library(tidyr) # tools for manipulating data (e.g., pivot_wider)
library(lme4) #lmer for ICCS for vars 
library(ordinal) # clmm command 
library(gtsummary) # for decriptives table
library(flextable) # this lets me export the gtsummary table to Word
library(performance) #has icc() command
library(ggplot2) # create plots
library(here) # here command


##-------------LOAD IN ALL CORE DATA--------------------------------------------------------------

#load NSHAP data downloaded in R format - https://www.icpsr.umich.edu/web/NACDA/series/706

## wave 1
load(here("data", "20541-0001-Data.rda"))
core_data_w1 <- da20541.0001

load(here("data", "20541-0004-Data.rda"))
sn_data_w1 <- da20541.0004

## wave 2
load(here("data", "34921-0001-Data.rda"))
core_data_w2 <- da34921.0001

load(here("data", "34921-0005-Data.rda"))
sn_data_w2 <- da34921.0005
##-------------SELECT/CREATE VARIABLES FROM CORE DATA-----------------------------------------------------

### select study variables 
corew1_clean <- core_data_w1 %>% select(ID, AGE, CONDITNS_1, CONDITNS_3, CONDITNS_4, CONDITNS_5, CONDITNS_6, CONDITNS_7, CONDITNS_8, 
                                        CONDITNS_14, MNTLHLTH)

corew2_clean <- core_data_w2 %>% select(ID, AGE,ARTHRITIS, EMPHASTH, STROKE, CONDITNS_6, CONDITNS_7, OTHCAN, ALZHEIMER,
                                        DEMENTIA , MNTLHLTH)

### number of chronic health condition measure 

#### w1
corew1_clean <- corew1_clean %>%
  # recode items to numeric
  mutate(across(
    c(CONDITNS_1, CONDITNS_3, CONDITNS_4, CONDITNS_5,
      CONDITNS_6, CONDITNS_7, CONDITNS_8, CONDITNS_14),
    ~ case_when(
      . == "(0) no" ~ 0,
      . == "(1) yes" ~ 1,
      T ~ NA_real_
    )
  )) %>%
  # combine asthma and emphysema into one
  mutate(EMPHASTH = if_else(CONDITNS_3 == 1 | CONDITNS_4 == 1, 1, 0)) %>%
  rename(ARTHRITIS = CONDITNS_1) %>% rename(STROKE = CONDITNS_5) %>%
  rename(CONDITNS_83 = CONDITNS_8) %>% rename(OTHCAN = CONDITNS_14) %>%
  rowwise() %>%
  # create sum for num. chronic conditions
  rowwise() %>%
  mutate(num_conditions = sum(ARTHRITIS, EMPHASTH, STROKE,
                              CONDITNS_6, CONDITNS_7, CONDITNS_83, OTHCAN,
                                na.rm = T)) 

corew1_clean <- corew1_clean %>% select(-CONDITNS_3, -CONDITNS_4)

#### w2 
corew2_clean <- corew2_clean %>%
  # recode items to numeric
  mutate(across(
    c(ARTHRITIS, EMPHASTH, STROKE, CONDITNS_6, CONDITNS_7, OTHCAN, 
      ALZHEIMER, DEMENTIA ),
    ~ case_when(
      . == "(0) no" ~ 0,
      . == "(1) yes" ~ 1,
      T ~ NA_real_
    )
  )) %>%
  # combine dementia and alzheimer's into one
  mutate(CONDITNS_83 = if_else(ALZHEIMER == 1 | DEMENTIA == 1, 1, 0)) %>%
  rowwise() %>%
  # create sum for num. chronic conditions
  rowwise() %>%
  mutate(num_conditions = sum(ARTHRITIS, EMPHASTH, STROKE, CONDITNS_6, CONDITNS_7, OTHCAN, 
                             CONDITNS_83,
                              na.rm = T))

corew2_clean <- corew2_clean %>%
  select(-ALZHEIMER, -DEMENTIA)


### create wave indicator 
corew1_clean <- corew1_clean %>%
  mutate(wave = 1)

corew2_clean <- corew2_clean %>%
  mutate(wave = 2)

#recode mental health variable
corew1_clean <- corew1_clean %>%
  mutate(mental_health = as.integer(MNTLHLTH))
corew2_clean <- corew2_clean %>%
  mutate(mental_health = as.integer(MNTLHLTH))

### merge core datasets 

core_merged <- bind_rows(corew1_clean, corew2_clean)
core_merged <- core_merged %>% select(-MNTLHLTH, -ARTHRITIS, -EMPHASTH,
                                      -STROKE, -CONDITNS_6, -CONDITNS_7,
                                      -CONDITNS_83, -OTHCAN)

##-------------SELECT/CREATE VARIABLES FROM SOCIAL NETWORK DATA--------------------------

## Wave 1
sn_w1_clean <- sn_data_w1 %>%
  filter(SECTION %in% c("(1) a", "(2) b", "(3) c")) %>%
  select(ID, LINENO, HEALTHTALK, HOWCLOSE) %>%
  mutate(
    HEALTHTALK = as.integer(HEALTHTALK),
    HOWCLOSE = as.integer(HOWCLOSE),
    alter_id = if_else(
      !is.na(ID) & !is.na(LINENO),
      paste0(as.character(ID), "_", LINENO),
      NA_character_
    ),
    wave = 1
  )

## Wave 2
sn_w2_clean <- sn_data_w2 %>%
  filter(SECTION %in% c("(1) a", "(2) b", "(3) c")) %>%
  select(ID, LINENO, HEALTHTALK, HOWCLOSE, ROSTER_W1, LINENO_W1) %>%
  mutate(
    HEALTHTALK = as.integer(HEALTHTALK),
    HOWCLOSE = as.integer(HOWCLOSE),
    alter_id = case_when(
      ROSTER_W1 == "(1) yes" & !is.na(LINENO_W1) ~
        paste0(as.character(ID), "_", LINENO_W1),
      !is.na(ID) & !is.na(LINENO) ~
        paste0(as.character(ID), "_W2_", LINENO),
      TRUE ~ NA_character_
    ),
    wave = 2
  ) %>%
  select(-ROSTER_W1, -LINENO_W1)

## Combine waves
sn_merged <- bind_rows(sn_w1_clean, sn_w2_clean)

## unique roster entries 

network_records <- bind_rows(
  sn_data_w1 %>% mutate(wave = 1),
  sn_data_w2 %>% mutate(wave = 2)
) %>%
  filter(
    SECTION %in% c("(1) a", "(2) b", "(3) c"),
    !is.na(ID),
    !is.na(LINENO)
  ) %>%
  distinct(ID, wave, LINENO) %>%
  count(ID, wave, name = "n_alters")

## Include all core respondents, including those with zero recorded alters
alters_per_person <- bind_rows(
  core_data_w1 %>% distinct(ID) %>% mutate(wave = 1),
  core_data_w2 %>% distinct(ID) %>% mutate(wave = 2)
) %>%
  filter(!is.na(ID)) %>%
  left_join(network_records, by = c("ID", "wave")) %>%
  mutate(n_alters = coalesce(n_alters, 0L))

## check missing alter IDs
sn_merged %>%
  group_by(wave) %>%
  summarise(
    n_records = n(),
    n_missing_alter_id = sum(is.na(alter_id)),
    n_unique_alter_ids = n_distinct(alter_id, na.rm = TRUE),
    .groups = "drop"
  )

##-------------MERGE CORE & SN DATASETS -----------------------

merge_df <- sn_merged %>%
  left_join(core_merged, by = c("ID", "wave"))

### checking it

merge_df %>%
  select(ID, wave, alter_id, mental_health, num_conditions) %>%
  glimpse()

# check no rows were lost
nrow(sn_merged) == nrow(merge_df)  #TRUE


##--------------CALCULATE RETENTION RATE FOR CORE MERGED -------------------------------

retention_rate <- core_merged %>%
  group_by(ID) %>%
  summarise(has_w1 = any(wave == 1),
            has_w2 = any(wave == 2)) %>%
  summarise(
    total_wave1 = sum(has_w1),
    retained = sum(has_w1 & has_w2),
    retention_pct = 100 * retained / total_wave1
  )

retention_rate


##-------------PREPARE VARS FOR ANALYSIS-----------------------------------------------------------

### create person mean and within-person centered vars for age & closeness
merge_df <- merge_df %>%
  group_by(ID) %>%
  mutate(
    age_mean_b = mean(AGE, na.rm = T),  # between
    age_w = AGE - age_mean_b,      # within 
    closeness_mean_b = mean(HOWCLOSE, na.rm = T),  # between
    closeness_w = HOWCLOSE - closeness_mean_b,  # within 
    healthtalk_b = mean(as.numeric(HEALTHTALK), na.rm = T)
    ) %>%
  ungroup()

## create person means for number of health conditions and mental health rating

merge_df <- merge_df %>% 
  group_by(ID) %>%
  mutate(
    num_conditions_mean = mean(num_conditions, na.rm = T),
    mental_health_mean = mean(mental_health, na.rm=T)) %>%
  ungroup()

## create person means for network size 

network_size <- alters_per_person %>%
  rename(network_size = n_alters) %>%
  group_by(ID) %>%
  mutate(
    network_size_mean = mean(network_size, na.rm = TRUE)
  ) %>%
  ungroup()

## Merge network size into the alter-level dataset
merge_df <- merge_df %>%
  left_join(network_size, by = c("ID", "wave"))



##------------------DESCRIPTIVE STATISTICS-------------------------------------------

mean(core_merged$mental_health, na.rm = TRUE)
sd(core_merged$mental_health, na.rm = TRUE)
core_merged %>%
  summarise(
    n = sum(!is.na(mental_health)),
    mean_mental_health = mean(mental_health, na.rm = TRUE),
    sd_mental_health = sd(mental_health, na.rm = TRUE)
  )
core_merged %>%
  group_by(wave) %>%
  summarise(
    n = sum(!is.na(mental_health)),
    mean_mental_health = mean(mental_health, na.rm = TRUE),
    sd_mental_health = sd(mental_health, na.rm = TRUE)
  )

merge_df$HEALTHTALK <- as.numeric(merge_df$HEALTHTALK)
merge_df$HOWCLOSE <- as.numeric(merge_df$HOWCLOSE)

### health talk 
ICC_healthtalk <- lmer(HEALTHTALK ~ 1 + (1|ID), data = merge_df)
icc(ICC_healthtalk)

### how close
ICC_HOWCLOSE <- lmer(HOWCLOSE ~ 1 + (1|ID), data = merge_df)
icc(ICC_HOWCLOSE)


### means and SDs 
#### mean for each participant for each wave 
descrips <- merge_df %>% 
  group_by(ID, wave) %>%
  summarise(
    HEALTHTALK_mean = mean(HEALTHTALK, na.rm = T),
    HOWCLOSE_mean = mean(HOWCLOSE, na.rm = T),
    HEALTHTALK_sd = sd(HEALTHTALK, na.rm = T),
    HOWCLOSE_sd = sd(HOWCLOSE, na.rm = T),
    .groups="drop"
  )

### SN bivariate associations wave 1

age_w1 <- merge_df %>%
  filter(wave == 1) %>%
  select(ID, age_wave1 = AGE) %>%
  distinct()

descrips <- descrips %>%
  left_join(age_w1, by = "ID")

d1 <- descrips %>% filter(wave == 1)

cor.test(d1$age_wave1, d1$n_alters, use = "pairwise.complete.obs")
cor.test(d1$age_wave1, d1$HOWCLOSE_mean, use = "pairwise.complete.obs")
cor.test(d1$age_wave1, d1$HEALTHTALK_mean, use = "pairwise.complete.obs")

### SN bivariate associations wave 2

age_w2 <- merge_df %>%
  filter(wave == 2) %>%
  select(ID, age_wave2 = AGE) %>%
  distinct()

descrips <- descrips %>%
  left_join(age_w2, by = "ID")

d2 <- descrips %>% filter(wave == 2)

cor.test(d2$age_wave2, d2$n_alters, use = "pairwise.complete.obs")
cor.test(d2$age_wave2, d2$HOWCLOSE_mean, use = "pairwise.complete.obs")
cor.test(d2$age_wave2, d2$HEALTHTALK_mean, use = "pairwise.complete.obs")

## descriptives for alters at each wave
alters_summary <- alters_per_person %>%
  group_by(wave) %>%
  summarise(
    n_respondents = n(),
    n_zero = sum(n_alters == 0),
    mean_alters = mean(n_alters),
    sd_alters = sd(n_alters),
    median_alters = median(n_alters),
    min_alters = min(n_alters),
    max_alters = max(n_alters),
    .groups = "drop"
  )

alters_summary

## Distribution of network sizes
alters_distribution <- alters_per_person %>%
  count(wave, n_alters, name = "n_respondents") %>%
  arrange(wave, n_alters)

alters_distribution

ggplot(alters_distribution, aes(x = n_alters, y = n_respondents)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ wave, ncol = 1) +
  labs(
    title = "Number of Alters per Respondent",
    x = "Number of Alters",
    y = "Number of Respondents"
  ) +
  theme_minimal()

#### sample mean for alter characteristics each wave 

descrips_bywave <- descrips %>% 
  group_by(wave) %>%
  summarise(
    sample_mean_HEALTHTALK = mean(HEALTHTALK_mean, na.rm=T),  
    sample_sd_HEALTHTALK = sd(HEALTHTALK_sd, na.rm=T),
    sample_mean_HOWCLOSE = mean(HOWCLOSE_mean, na.rm=T),
    sample_sd_HOWCLOSE = sd(HOWCLOSE_sd, na.rm=T),
    .groups="drop"
  )

descrips_bywave

##-------------------ANALYSES---------------------------------------------------------------

merge_df$HEALTHTALK <- as.factor(merge_df$HEALTHTALK)
merge_df$ID <- as.factor(merge_df$ID)
merge_df$alter_id <- as.factor(merge_df$alter_id)

### create function for partial R^2s 

calculate_partial_r2s <- function(mod) {
  z_values <- coef(summary(mod))[
    names(mod$beta), # remove thresholds
    "z value",
    drop=TRUE
  ]
  df <- nobs(mod) - length(mod$beta)
  partial_r2 <- z_values^2 / (z_values^2 + df)
  return(
    partial_r2
  )
  
}


### unconditional model

clean_df <- merge_df %>%
  filter(!is.na(HEALTHTALK), !is.na(ID), !is.na(alter_id))

# unconditional model 
mod0 <- clmm(HEALTHTALK ~ 1 + (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod0) 

### get model ICCs 
vc_0 <- VarCorr(mod0)

id_var_0 <- as.numeric(vc_0$ID)
dyad_var_0 <- as.numeric(vc_0[["ID:alter_id"]])
resid_var_0 <- (pi^2) / 3  # fixed for logistic link

total_var_0 <- id_var_0 + dyad_var_0 + resid_var_0
icc_id_0 <- id_var_0 / total_var_0
icc_dyad_0 <- dyad_var_0 / total_var_0
cat("ICC for ID level: ", icc_id_0, "\n")
cat("ICC for dyad (ID:alter_id) level: ", icc_dyad_0, "\n")

# between person effects model

mod1 <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b +
             (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod1) 

# Between-person model
r2_partial_mod1 <- calculate_partial_r2s(mod1)


# extract random effect variances
vc_1 <- VarCorr(mod1)

id_var_1 <- as.numeric(vc_1$ID)
dyad_var_1 <- as.numeric(vc_1[["ID:alter_id"]])
resid_var_1 <- (pi^2) / 3  # fixed for logistic link

total_var_1 <- id_var_1 + dyad_var_1+ resid_var_1
icc_id_1<- id_var_1 / total_var_1
icc_dyad_1 <- dyad_var_1/ total_var_1
cat("ICC for ID level: ", icc_id_1, "\n")
cat("ICC for dyad (ID:alter_id) level: ", icc_dyad_1, "\n")


# within person effects added model 

mod2 <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b 
               + age_w + closeness_w +
               (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod2) 

r2_partials_mod2 <- calculate_partial_r2s(mod2)


# extract random effect variances
vc_2 <- VarCorr(mod2)

id_var_2 <- as.numeric(vc_2$ID)
dyad_var_2 <- as.numeric(vc_2[["ID:alter_id"]])
resid_var_2 <- (pi^2) / 3  # fixed for logistic link

total_var_2 <- id_var_2 + dyad_var_2+ resid_var_2
icc_id_2<- id_var_2 / total_var_2
icc_dyad_2 <- dyad_var_2/ total_var_2
cat("ICC for ID level: ", icc_id_2, "\n")
cat("ICC for dyad (ID:alter_id) level: ", icc_dyad_2, "\n")

# between person interaction 

clean_df <- clean_df %>%
  mutate(across(c(age_mean_b, closeness_mean_b, age_w, closeness_w),
                ~ scale(.), 
                .names = "{.col}_z"))

mod3 <- clmm(HEALTHTALK ~ age_mean_b_z + closeness_mean_b_z 
             + age_w_z + closeness_w_z + age_mean_b_z:closeness_mean_b_z +
               (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod3) 

r2_partial_mod3 <- calculate_partial_r2s(mod3)

# within person interaction 

mod4 <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b 
             + age_w + closeness_w + age_w:closeness_w +
               (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")

summary(mod4)

r2_partial_mod4 <- calculate_partial_r2s(mod4)


##-----------------ROBUSTNESS CHECKS----------------------------------------------------------

# between person effects model

mod1.robust <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b +
               num_conditions_mean + mental_health_mean + network_size_mean +
               (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod1.robust) 

r2_partial_mod1.robust <- calculate_partial_r2s(mod1.robust)



# within person effects added model 

mod2.robust <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b 
             + age_w + closeness_w + 
               num_conditions_mean + mental_health_mean + network_size_mean +
               (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod2.robust) 

r2_partial_mod2.robust <- calculate_partial_r2s(mod2.robust)


# between person interaction 


mod3.robust <- clmm(HEALTHTALK ~ age_mean_b_z + closeness_mean_b_z 
             + age_w_z + closeness_w_z + age_mean_b_z:closeness_mean_b_z +
               num_conditions_mean + mental_health_mean + network_size_mean +
               (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod3.robust) 

r2_partial_mod3.robust <- calculate_partial_r2s(mod3.robust)

# within person interaction 

mod4.robust <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b 
             + age_w + closeness_w + age_w:closeness_w  + 
               num_conditions_mean + mental_health_mean + network_size_mean +
               (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")

summary(mod4.robust)

r2_partial_mod4.robust <- calculate_partial_r2s(mod4.robust)

## -------------------------EXPLORATORY CROSS LEVEL INTERACTION ------------------------------------

# cross level interaction, between person age, within person closeness 

mod.crosslevel.int <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b 
                           + age_w + closeness_w + age_mean_b:closeness_w +
                             (1 | ID) + (1 | ID:alter_id), 
                           data = clean_df, 
                           link = "logit")

summary(mod.crosslevel.int)



mod.crosslevel.int.z <- clmm(HEALTHTALK ~ age_mean_b_z + closeness_mean_b_z 
                                   + age_w_z + closeness_w_z + age_mean_b_z:closeness_w_z +
                                     (1 | ID) + (1 | ID:alter_id), 
                                   data = clean_df, 
                                   link = "logit")

summary(mod.crosslevel.int.z)

### ---------- DESCRIPTIVES TABLE ----------

core_w1_selected <- core_data_w1 %>%
  select(ID, ETHGRP, EDUC, AGE, GENDER, MNTLHLTH) %>%
  left_join(
    corew1_clean %>% select(ID, num_conditions),
    by = "ID"
  ) %>%
  mutate(wave = 1)

core_w2_selected <- core_data_w2 %>%
  select(ID, ETHGRP, EDUC, AGE, GENDER, MNTLHLTH) %>%
  left_join(
    corew2_clean %>% select(ID, num_conditions),
    by = "ID"
  ) %>%
  mutate(wave = 2)

## merge for descriptives

merged_descriptives <- bind_rows(core_w1_selected, core_w2_selected)

merged_descriptives <- merged_descriptives %>%
  mutate(
    ETHGRP = as.factor(ETHGRP),
    EDUC = as.factor(EDUC),
    GENDER = as.factor(GENDER),
    wave = as.factor(wave),
    num_conditions = as.numeric(num_conditions),
    MNTLHLTH = as.numeric(MNTLHLTH)
  ) 

ids_in_model <- clean_df %>%
  distinct(ID, wave)

merged_descriptives <- as.data.frame(merged_descriptives)
ids_in_model <- as.data.frame(ids_in_model)

merged_descriptives$wave <- as.numeric(merged_descriptives$wave)
ids_in_model$wave <- as.numeric(ids_in_model$wave)

merged_descriptives_model <- merged_descriptives %>%
  inner_join(ids_in_model, by = c("ID", "wave"))

### table

tbl1 <- merged_descriptives_model %>%
  select(wave, ETHGRP, GENDER, EDUC, AGE, num_conditions, MNTLHLTH) %>%
  tbl_summary(
    by = wave,  
    type = list(num_conditions ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      ETHGRP ~ "Race/Ethnicity",
      GENDER ~ "Gender",
      EDUC ~ "Education",
      AGE ~ "Age",
      num_conditions ~ "Number of Conditions",
      MNTLHLTH ~ "Mental Health"
    ),
    digits = everything() ~ 1
  ) %>%
  bold_labels() %>%
  add_overall()

tbl1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "NSHAP_descriptives.docx")

# demographic covariates


demogr_df <- clean_df %>%
  left_join(
    core_data_w1 %>% 
      select(ID, GENDER, RACE_RECODE),
    by = "ID"
  )


mod1.demographics <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b + RACE_RECODE + GENDER + 
               (1 | ID) + (1 | ID:alter_id), 
             data = demogr_df, 
             link = "logit")
summary(mod1.demographics)

mod2.demographics <- clmm(HEALTHTALK ~ age_mean_b + closeness_mean_b 
             + age_w + closeness_w + RACE_RECODE + GENDER +
               (1 | ID) + (1 | ID:alter_id), 
             data = demogr_df, 
             link = "logit")
summary(mod2.demographics) 

# check other models with predictors standardized 

mod1.standard <- clmm(HEALTHTALK ~ age_mean_b_z + closeness_mean_b_z 
             +  (1 | ID) + (1 | ID:alter_id), 
             data = clean_df, 
             link = "logit")
summary(mod1.standard)

mod2.standard <- clmm(HEALTHTALK ~ age_mean_b_z + closeness_mean_b_z 
                      + age_w_z + closeness_w_z + 
                        (1 | ID) + (1 | ID:alter_id), 
                      data = clean_df, 
                      link = "logit")
summary(mod2.standard)

mod4.standard <- clmm(HEALTHTALK ~ age_mean_b_z + closeness_mean_b_z 
                      + age_w_z + closeness_w_z + age_w_z:closeness_w_z +
                        (1 | ID) + (1 | ID:alter_id), 
                      data = clean_df, 
                      link = "logit")
summary(mod4.standard)

## ------- EXPLORATORY EXPLICITLY DROP ALTERS NOT NAMED AT BOTH WAVES ----------
## alters named in BOTH waves

alters_both_waves <- sn_merged %>%
  mutate(
    ID = as.character(ID),
    alter_id = as.character(alter_id)
  ) %>%
  filter(!is.na(ID), !is.na(alter_id)) %>%
  group_by(ID, alter_id) %>%
  summarise(
    has_w1 = any(wave == 1),
    has_w2 = any(wave == 2),
    .groups = "drop"
  ) %>%
  filter(has_w1 & has_w2) %>%
  select(ID, alter_id)

merge_df_both <- merge_df %>%
  mutate(
    ID = as.character(ID),
    alter_id = as.character(alter_id)
  ) %>%
  inner_join(alters_both_waves, by = c("ID", "alter_id"))

merge_df_both <- merge_df_both %>%
  group_by(ID) %>%
  mutate(
    age_mean_b = mean(AGE, na.rm = TRUE),
    age_w = AGE - age_mean_b,
    closeness_mean_b = mean(HOWCLOSE, na.rm = TRUE),
    closeness_w = HOWCLOSE - closeness_mean_b,
    healthtalk_b = mean(as.numeric(HEALTHTALK), na.rm = TRUE),
    num_conditions_mean = mean(num_conditions, na.rm = TRUE),
    mental_health_mean = mean(mental_health, na.rm = TRUE)
  ) %>%
  ungroup()

clean_df_both <- merge_df_both %>%
  filter(
    !is.na(HEALTHTALK),
    !is.na(ID),
    !is.na(alter_id)
  ) %>%
  mutate(
    HEALTHTALK = as.factor(HEALTHTALK),
    ID = as.factor(ID),
    alter_id = as.factor(alter_id)
  )
clean_df_both <- clean_df_both %>%
  mutate(
    across(
      c(age_mean_b, closeness_mean_b, age_w, closeness_w),
      ~ as.numeric(scale(.)),
      .names = "{.col}_z"
    )
  )
#### RE RUN MODELS WITH RESTRICTED SAMPLE 

mod0.both <- clmm(
  HEALTHTALK ~ 1 +
    (1 | ID) + (1 | ID:alter_id),
  data = clean_df_both,
  link = "logit"
)

summary(mod0.both)

mod1.both <- clmm(
  HEALTHTALK ~ age_mean_b + closeness_mean_b +
    (1 | ID) + (1 | ID:alter_id),
  data = clean_df_both,
  link = "logit"
)

summary(mod1.both)

mod2.both <- clmm(
  HEALTHTALK ~ age_mean_b + closeness_mean_b +
    age_w + closeness_w +
    (1 | ID) + (1 | ID:alter_id),
  data = clean_df_both,
  link = "logit"
)

summary(mod2.both)

mod3.both <- clmm(
  HEALTHTALK ~ age_mean_b_z + closeness_mean_b_z +
    age_w_z + closeness_w_z +
    age_mean_b_z:closeness_mean_b_z +
    (1 | ID) + (1 | ID:alter_id),
  data = clean_df_both,
  link = "logit"
)

summary(mod3.both)

mod4.both <- clmm(
  HEALTHTALK ~ age_mean_b + closeness_mean_b +
    age_w + closeness_w + age_w:closeness_w +
    (1 | ID) + (1 | ID:alter_id),
  data = clean_df_both,
  link = "logit"
)

summary(mod4.both)

## -------------- VISUALIZATION -------------- 

library(emmeans)
library(ggplot2)
library(ggeffects)

close_seq <- seq(
min(clean_df$closeness_w, na.rm = TRUE),
max(clean_df$closeness_w, na.rm = TRUE),
length.out = 20
)

emm_close_prob <- emmeans(
  mod2,
  ~ HEALTHTALK | closeness_w,
  at = list(closeness_w = close_seq),
  mode = "prob"
)

age_vals <- quantile(
  clean_df$age_w,
  probs = c(0.10, 0.50, 0.90),
  na.rm = TRUE
)

emm_age <- emmeans(
  mod2,
  ~ closeness_w | HEALTHTALK * age_w,
  at = list(
    closeness_w = close_seq,
    age_w = as.numeric(age_vals)
  ),
  mode = "prob"
)


emm_df <- as.data.frame(emm_close_prob)
emm_age_df <- as.data.frame(emm_age)

# Check column names and confidence intervals
names(emm_df)
head(emm_df)

# Plot predicted probabilities
ggplot(
  emm_df,
  aes(
    x = closeness_w,
    y = prob,
    color = HEALTHTALK,
    fill = HEALTHTALK,
    group = HEALTHTALK
  )
) +
  geom_ribbon(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    x = "Within-person closeness (person-centered)",
    y = "Predicted probability",
    color = "Health talk level",
    fill = "Health talk level"
  ) +
  theme_classic()

### predicted probabilities with age included

ggplot(
  emm_age_df,
  aes(
    x = closeness_w,
    y = prob,
    color = HEALTHTALK,
    fill = HEALTHTALK,
    group = HEALTHTALK
  )
) +
  geom_ribbon(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~ age_w,
    labeller = label_both
  ) +
  labs(
    x = "Within-person closeness",
    y = "Predicted probability",
    color = "Health talk level",
    fill = "Health talk level"
  ) +
  theme_classic()
#### simple scatterplot for talk 

ggplot(merge_df, aes(x = age_mean_b, y = healthtalk_b)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm") +
  labs(
    x = "Age (Between Person)",
    y = "Mean Health Discussion"
  ) +
  theme_minimal()

ggplot(merge_df, aes(x = closeness_mean_b, y = healthtalk_b)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm") +
  labs(
    x = "Closeness (Between Person)",
    y = "Mean Health Discussion"
  ) +
  theme_minimal()

# Get predicted probabilities across age_w by closeness_w and interaction

pred <- ggpredict(
  mod4,
  terms = c("age_w", "closeness_w [-1, 0, 1]")
)

pred_df <- as.data.frame(pred)

ggplot(
  pred_df,
  aes(
    x = x,
    y = predicted,
    color = group,
    fill = group,
    group = group
  )
) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ response.level) +
  labs(
    x = "Within-person age",
    y = "Predicted probability",
    color = "Within-person closeness",
    fill = "Within-person closeness"
  ) +
  theme_classic()










