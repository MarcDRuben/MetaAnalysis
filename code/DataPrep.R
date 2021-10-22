# Clean & transform search data for meta-analysis

library(tidyverse)
library(readxl)
library(esc)

# All studies w/evaluable SMD
infile <- read_excel("./data/DatafileS1.xlsx", sheet = "SMD") 

# Selected outcome for evaluation; one per study
dat <- infile %>%
  filter(!is.na(SELECTED_d))

dat_h <- dat %>%
  # Absolute value effect size (ignore if AM or PM was better )
  mutate(d = abs(d),
         d_SEM = as.numeric(d_SEM),
         YEAR = as.numeric(YEAR),
         # Hedge's g estimate corrects small sample size bias in Cohen's-d
         g = hedges_g(d, PATIENTS),
         # Hedge's-g SEM, and range
         g_SEM = sqrt((G1_n+G2_n)/(G1_n*G2_n)+(g^2/(2*(G1_n+G2_n)))),
         g_SEM_UP = g + g_SEM,
         g_SEM_DN = g - g_SEM,
         # 95CI range
         g_CI95_UP = g + (1.96*g_SEM),
         g_CI95_DN = g - (1.96*g_SEM),
         # Author-drug ID
         ID = paste("(", REF, ")", "  ", "<B>", DRUG_MECHANISM_ABBREV, "<B>", sep = ""),
         REGISTERED = ifelse(is.na(PROSPECTIVE_REGISTRATION), "no", "yes"),
         DRUG_HALF_LIFE_BIN = case_when(
           DRUG_HALF_LIFE <= 10 ~ "short",
           TRUE ~ "long/unknown"),
         # Assign 'day' to morning and 'afternoon' to evening
         BETTER_TIME_EST = case_when(  
           BETTER_TIME == "Day" ~ "Morning",
           BETTER_TIME == "Afternoon" ~ "Evening",
           TRUE ~ BETTER_TIME),
         # group less common outcomes as 'other' 
         OUTCOME_TERM_GROUPS = ifelse(OUTCOME_TERM %in% c("airway patency", "blood pressure",
                                                          "lipid levels", "coagulation","thyroid function",
                                                          "pain"), OUTCOME_TERM, "other"),
         OUTCOME_TERM_GROUPS = factor(OUTCOME_TERM_GROUPS,
                                       levels = c("blood pressure", "airway patency", "lipid levels",
                                                  "coagulation", "thyroid function", "pain", "other")))
saveRDS(dat_h, "./data/meta.input.clean.rds")

dat_h_2k <- dat_h %>% 
  filter(YEAR >= 2000)
saveRDS(dat_h_2k, "./data/meta.input.clean.2k.rds")
