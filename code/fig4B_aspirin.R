# Aspirin studies for table

library(tidyverse)
library(readxl)
library(esc)
library(lemon)

# All studies & outcomes w/evaluable SMD
infile <- read_excel("./data/DatafileS1.xlsx", sheet = "SMD") %>%
  mutate(g = hedges_g(d, PATIENTS), # Hedge's g corrects small sample size bias
  g_SEM = sqrt((G1_n+G2_n)/(G1_n*G2_n)+(g^2/(2*(G1_n+G2_n))))) # g SEM

# Aspirin studies with evaluable SMD in coagulation
aspirin <- infile %>%
  filter(grepl("aspirin", DRUG, ignore.case = TRUE) &
           OUTCOME_TERM == "coagulation" &
           !is.na(g)) %>%
  mutate(endpoint_assay = case_when(
    grepl("ARU", MEASURE, ignore.case = TRUE) ~ "ARU, VerifyNow",
    grepl("thromboxane", MEASURE, ignore.case = TRUE) ~ "serum thromboxane B2",
    grepl("count", MEASURE, ignore.case = TRUE) ~ "platelet count",
    TRUE ~ "other"),
    endpoint_assay = factor(endpoint_assay, levels = c("ARU, VerifyNow",
                                                       "serum thromboxane B2",
                                                       "platelet count",
                                                       "other"))) %>%
  select(REF, PROSPECTIVE_REGISTRATION, DESIGN, PATIENTS, endpoint_assay, DECLARED_ENDPOINT, g, g_SEM)
 

# Plot absolute SMD for each specific endpoint assay of platelet inhibition
pA <- ggplot(aspirin, aes(x = endpoint_assay, y = g, shape = REF)) +
  geom_jitter(size = 1.95, width = 0.2, color = "#56B4E9", stroke = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
  coord_flex_cart(bottom = brackets_horizontal(tick.length = 0.2)) +
  labs(y = "SMD", title = "Aspirin effects for different endpoint assays of platelet inhibition") +
  scale_shape_manual(values = seq(0, 5, 1),
                     guide = guide_legend(
                       title = element_blank(),
                       title.position = "top", 
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth = 1,
                       keyheight = 1,
                       nrow = 1)) +
  #theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8.5, margin = margin(r = 3)),
        axis.text = element_text(size = 8.5),
        plot.title = element_text(size = 8.5, hjust = 0.5),
        legend.margin = margin(t = -25),
        legend.position = "bottom",
        legend.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        panel.grid.major.x = element_blank())
ggsave("./figs/meta_Fig4B.pdf", pA, height = 2.25, width = 7.2)


