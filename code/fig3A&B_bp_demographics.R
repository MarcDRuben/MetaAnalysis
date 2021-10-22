# Nighttime blood pressure: impact of drug half-life, patient age, and patient sex

library(tidyverse)
library(readxl)
library(esc)
library(ggside)
library(patchwork)

#library(ggthemes)
#library(meta)
#library(dmetar)
#library(forcats)

# All studies & outcomes w/evaluable SMD
infile <- read_excel("./data/DatafileS1.xlsx", sheet = "SMD") %>%
  mutate(g = hedges_g(d, PATIENTS), # Hedge's g corrects small sample size bias
  g_SEM = sqrt((G1_n+G2_n)/(G1_n*G2_n)+(g^2/(2*(G1_n+G2_n))))) # g SEM

# BP studies & outcomes with evaluable SMD
htn <- infile %>%
  filter(DISEASE == "hypertension" & !is.na(g))
 
# HTN outcomes considered 
sys <- c("24 h SBP, mmHg", "Day SBP, mmHg", "Night SBP, mmHg")
dia <- c("24 h DBP, mmHg", "Day DBP, mmHg", "Night DBP, mmHg")

night_htn <- htn %>%
  filter(MEASURE %in% c(sys, dia)) %>% # Filter 3 HTN outcomes
  mutate(SYSTOL_DIASTOL = ifelse(MEASURE %in% sys, "systolic", "diastolic"),
         SYSTOL_DIASTOL = factor(SYSTOL_DIASTOL, level = c("systolic", "diastolic")),
         MEASURE_WINDOW = str_replace(MEASURE, " SBP, mmHg| DBP, mmHg", "")) %>%  # Clean up text
  select(PMID_DOI_NCTID, REF, DRUG, DRUG_HALF_LIFE, DRUG_MECHANISM_ABBREV,
         SYSTOL_DIASTOL, MEASURE_WINDOW, DESIGN_SUMMARY, PATIENTS, PCT_FEMALE,
         MEAN_AGE, RMD, BETTER_TIME, NSPAIN) %>%
  filter(SYSTOL_DIASTOL == "systolic" & MEASURE_WINDOW == "Night") # Only night systolic outcomes

# Density plot study features
labels <- c(
  `DRUG_HALF_LIFE` = "drug half-life (hrs)",
  `MEAN_AGE` = "mean age (yrs)",
  `PCT_FEMALE` = "percent female")

pA <- night_htn %>%
  select(PMID_DOI_NCTID, DRUG, PCT_FEMALE, MEAN_AGE, DRUG_HALF_LIFE, NSPAIN) %>%
  pivot_longer(cols = PCT_FEMALE:DRUG_HALF_LIFE, names_to = "var", values_to = "val") %>%
  ggplot(aes(x = val, color = NSPAIN)) +
  geom_density() +
  scale_color_manual(
    values = c("#999999", "#D55E00"), labels = c("no", "yes"),
    guide = guide_legend(
      title = "N.W. Spain",
      title.position = "top",
      legend.direction = "vertical",
      legend.box = "vertical",
      title.hjust = 0.5,
      title.vjust = 0.5,
      keywidth = 0.5,
      keyheight= 0.3)) +
  facet_wrap(~var, scales = "free", labeller = as_labeller(labels), nrow = 1) +
  labs(y = "density of studies") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8.5),
        legend.position = "none",
        legend.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        strip.text = element_text(size = 8, margin = margin(t = 3, b = 3.5)),
        strip.background = element_rect(size = 0.1))

# Study features versus RMD
pB <- night_htn %>%
  select(PMID_DOI_NCTID, DRUG, PCT_FEMALE, MEAN_AGE, DRUG_HALF_LIFE, RMD, NSPAIN) %>%
  pivot_longer(cols = PCT_FEMALE:DRUG_HALF_LIFE, names_to = "var", values_to = "val") %>%
  ggplot(aes(x = val, y = abs(RMD), color = NSPAIN)) +
  geom_point(size = 0.55) +
  geom_smooth(aes(group = NSPAIN), method = "gam",
              formula = y ~ s(x, bs = "cs", k = 5),
              size = 0.55) +
  scale_color_manual(
    values = c("#999999", "#D55E00"), labels = c("no", "yes"),
    guide = guide_legend(
      title = "N.W. Spain",
      title.position = "top",
      legend.direction = "vertical",
      legend.box = "vertical",
      title.hjust = 0.5,
      title.vjust = 0.5,
      keywidth = 0.5,
      keyheight= 0.3)) +
  facet_wrap(~var, scales = "free", labeller = as_labeller(labels), nrow = 1) +
  labs(y = "absolute RMD (mmHg)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8.5),
        legend.position = "none",
        legend.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        strip.text = element_blank())

comb <- pA + pB + plot_layout(nrow = 2)
ggsave("./figs/meta_Fig3B.pdf", comb, width = 4.2, height = 3)

