# Blood pressure studies

library(tidyverse)
library(readxl)
library(esc)
library(ggridges)
#library(ggExtra)
library(ggside)

# All studies & outcomes w/evaluable SMD
infile <- read_excel("./data/DatafileS1.xlsx", sheet = "SMD") %>%
  mutate(g = hedges_g(d, PATIENTS), # Hedge's g corrects small sample size bias
  g_SEM = sqrt((G1_n+G2_n)/(G1_n*G2_n)+(g^2/(2*(G1_n+G2_n)))),
  d_abs = abs(d)) # g SEM

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
         MEAN_AGE, RMD, SD_pool, d, d_abs, d_SEM, g, g_SEM, BETTER_TIME, NSPAIN) %>%
  filter(SYSTOL_DIASTOL == "systolic" & MEASURE_WINDOW == "Night") # Only night systolic outcomes

facet_labels <- c(
    `24 h` = "24 h SBP",
    `Day` = "daytime SBP",
    `Night` = "nighttime SBP")

# Variance vs RMD: marginal boxplots N.Spain vs. others 
pB <- ggplot(night_htn, aes(x = SD_pool, y = RMD, color = NSPAIN)) +
  geom_point(size = 1.25, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
  geom_xsideboxplot(aes(y = NSPAIN), orientation = "y", outlier.shape = NA) +
  scale_xsidey_discrete(label = c("", "")) +
  geom_ysideboxplot(aes(x = NSPAIN), orientation = "x", outlier.shape = NA) +
  scale_ysidex_discrete(label = c("", "")) +
  scale_color_manual(values = c("#999999", "#D55E00"), labels = c("no", "yes"),
                     guide = guide_legend(
                       title = "N.W.Spain",
                       title.position = "top", 
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth=0.4,
                       keyheight=0.3,
                       ncol = 1)) +
  labs(x = "study standard deviation", y = "raw mean difference (RMD), mmHg") +
  theme(axis.title.x = element_text(size = 8.5),
        axis.title.y = element_text(size = 8.5, margin = margin(r = 3)),
        axis.text = element_text(size = 8.5),
        plot.title = element_text(size = 8.5),
        legend.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        ggside.panel.scale = 0.25,
        legend.position = c(0.89, 0.92))
ggsave("./figs/meta_Fig3A.pdf", pB, height = 2.75, width = 2.75)


## Not used ##
# Variance versus SMD
pA <- night_htn %>%
  mutate(d_abs_cap = ifelse(d_abs > 1.5, 1.5, d_abs),
         SD_pool = ifelse(SD_pool < 5, 5, SD_pool)) %>% # Ceiling on d_abs for viz
  ggplot(aes(x = SD_pool, y = abs(d_abs_cap))) +
  geom_point(size = 0.75, aes(color = DRUG_MECHANISM_ABBREV)) +
  geom_smooth(method = "gam", size = 0.5, alpha = 0.5) +
  facet_wrap(~ MEASURE_WINDOW,  scales = "free_y",
             labeller = as_labeller(facet_labels)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
                     guide = guide_legend(
                       title = element_blank(),
                       title.position = "top",
                       legend.direction = "horizontal",
                       legend.box = "horizontal",
                       title.hjust = 0.5,
                       title.vjust = 0,
                       keywidth = 0.5,
                       keyheight= 0.3,
                       ncol = 1)) +
  labs(x = "study standard deviation", y = "absolute SMD") +
  theme_bw() +
  theme(axis.title = element_text(size = 8.5),
        axis.text = element_text(size = 8.5),
        legend.text = element_text(size = 8),
        legend.margin = margin(l = -5),
        strip.text = element_text(size = 8.5,
                                  margin = margin(t = 3, b = 3.5)),
        strip.background = element_rect(size = 0.1))
ggsave("./figs/meta_figS2A.pdf", pA, height = 2, width = 3.75)
