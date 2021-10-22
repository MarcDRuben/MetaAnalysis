# Meta heterogeneity & outlier analysis

library(tidyverse)
library(metafor)
library(meta)
library(dmetar)
library(ggrepel)
library(ggtext)
library(patchwork)

# All studies w/evaluable SMD; one selected outcome per study
infile <- readRDS('./data/meta.input.clean.2k.rds')

# DerSimonian-Laird pooled effect
dsl <- metagen(g,
               g_SEM,
               data = infile,
               studlab = paste("(", REF, ") ", DRUG_MECHANISM_ABBREV, sep = ""),
               comb.fixed = FALSE,
               comb.random = TRUE,
               hakn = FALSE,
               prediction = TRUE,
               sm = "SMD")
dsl

# Leave-one-out analysis of heterogeneity and effect
inf.analysis <- InfluenceAnalysis(x = dsl, random = TRUE)
influence <- inf.analysis$Data %>%
  mutate(REF = gsub("Omitting ", "", Author)) %>%
  bind_cols(infile %>% select(c("ID", "PATIENTS", "OUTCOME_TERM_GROUPS", "NSPAIN", "DRUG_MECHANISM_ABBREV")))

# Methods
# dffits: in SDs how much the predicted pooled effect changes

# Annotate & clean for plot
dffits <- influence %>%
  #Impose ceiling & floor on dffits values for plot
  mutate(dffits = ifelse(dffits > 0.35, 0.35, ifelse(
    dffits < -0.3, -0.3, dffits)),
    idx = seq(1, nrow(.), 1),
    outlier = ifelse(
      dense_rank(dffits) <= 10, "low", ifelse(
      dense_rank(desc(dffits)) <= 10, "high", "no")))

pA <- ggplot(dffits, aes(x = reorder(idx, -dffits), y = dffits, group = 1)) +
  geom_point(aes(color = outlier), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#D55E00", "grey65"),
                     guide = guide_legend(
                       show_guide = FALSE)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.1, size = 2) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  labs(x = "study omitted", y = "change in pooled effect size",
       title = "leave-one-out analysis") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 9, margin = margin(b = -50), hjust = 0.5),
        legend.position = "none",
        axis.ticks = element_line(colour = "black", size = 0.1))

#facet labels
labels <- c(
  `high` = "large effect outliers",
  `low` = "small effect outliers")

# Top 10 outliers high & low
pB <- dffits %>%
  filter(outlier %in% c("low", "high")) %>%
  ggplot(aes(x = reorder(idx, -dffits), y = dffits,
                                  shape = NSPAIN, group = 1)) +
  geom_point(aes(color = OUTCOME_TERM_GROUPS), size = 2.25) +
  scale_color_manual(values = c("#E69F00", "#999999", "#009E73", "#56B4E9",
                                "#0072B2", "#D55E00", "#000000"),
                     guide = guide_legend(
                       title = "outcome measure",
                       title.position = "left", 
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth=0.4,
                       keyheight=0.3,
                       ncol = 2,
                       order = 1)) +
  scale_shape_manual(values = c(2, 17),
                     guide = guide_legend(
                       title = "N.W. Spain",
                       title.position = "left", 
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth=0.4,
                       keyheight=0.3,
                       ncol = 1,
                       order = 2)) +
  geom_text_repel(aes(label = DRUG_MECHANISM_ABBREV),
                  size = 1.95, max.overlaps = 1000,
                  min.segment.length = 0.1,
                  segment.color = 'grey70',
                  force_pull = 1) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  facet_wrap(~outlier, scales = "free", ncol = 2,
             labeller = as_labeller(labels)) +
  labs(x = "study omitted", y = "change in pooled effect size (dffits)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.margin = margin(b = -5, l = 0),
        strip.text = element_text(size = 9,
                                  margin = margin(t = 3, b = 3.5)),
        strip.background = element_rect(size = 0.1))

comb <- pA + pB + plot_layout(ncol = 2, widths = c(0.25, 1))
ggsave("./figs/meta_Fig2A.pdf", comb, height = 3.15, width = 7.2)

