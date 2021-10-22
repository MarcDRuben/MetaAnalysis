# Subgroup analysis: study design

library(tidyverse)
library(meta)
library(dmetar)

# All studies w/evaluable SMD; one selected outcome per study
infile <- readRDS('./data/meta.input.clean.2k.rds')

# DerSimonian-Laird pooled effect
dsl <- metagen(g,
               g_SEM,
               data = infile,
               studlab = ID,
               comb.fixed = FALSE,
               comb.random = TRUE,
               hakn = FALSE,
               prediction = TRUE,
               sm = "SMD")

# Subgroup analyses via mixed-effects model; random within subgroups, fixed between subgroups
# Design
sbgrp <- subgroup.analysis.mixed.effects(x = dsl, subgroups = infile$DESIGN)
out <- sbgrp$within.subgroup.results %>%
  rownames_to_column(var = "levels") %>%
  mutate(between_group_p = sbgrp$subgroup.analysis.results$p,
         between_group_Q = sbgrp$subgroup.analysis.results$Q,
         between_group_df = sbgrp$subgroup.analysis.results$df,
         subgroup = "all studies")

# Repeat but omit all Northern Spain studies
infile_NoSpain<- infile %>%
  filter(HERMIDA_AUTHORED != "YES")
dsl_NoSpain <- metagen(g,
                         g_SEM,
                         data = infile_NoSpain, 
                         studlab = ID,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         hakn = FALSE,
                         prediction = TRUE,
                         sm = "SMD")

sbgrp_NoSpain <- subgroup.analysis.mixed.effects(x = dsl_NoSpain, subgroups = infile_NoSpain$DESIGN)
out_NoSpain <- sbgrp_NoSpain$within.subgroup.results %>%
  rownames_to_column(var = "levels") %>%
  mutate(between_group_p = sbgrp_NoSpain$subgroup.analysis.results$p,
         between_group_Q = sbgrp_NoSpain$subgroup.analysis.results$Q,
         between_group_df = sbgrp_NoSpain$subgroup.analysis.results$df,
         subgroup = "omit N.Spain")

comb <- bind_rows(out, out_NoSpain)

# Plot labels
helper <- tibble(
  subgroup = c("all studies", "omit N.Spain"),
  SMD = c(dsl$TE.random, dsl_NoSpain$TE.random),
  k = c(dsl$k, dsl_NoSpain$k)
)

# Facet labels
labels <- c(
  `all studies` = paste0("all studies (n=", helper$k[helper$subgroup == "all studies"], ")"),
  `omit N.Spain` = paste0("omit N.Spain (n=", helper$k[helper$subgroup == "omit N.Spain"], ")"))
                         
pA <- ggplot(comb, aes(x = SMD , y = levels)) +
  geom_point(aes(color = levels), size = 2.4, shape = 0, stroke = 0.75) +
  facet_wrap(~ subgroup, nrow = 1, labeller = as_labeller(labels)) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9"),
                     guide = guide_legend(
                       title = "study design",
                       title.position = "left", 
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth=0.4,
                       keyheight=0.3,
                       ncol = 1,
                       order = 1)) +
  geom_linerange(aes(xmax = ULCI, xmin = LLCI), size = 0.4) +
  geom_vline(aes(xintercept = SMD), data = helper, color = "blue", size = 0.75, linetype = "dotted") +
  geom_text(aes(x = 0.55, y = 3.25, label = paste("p=", signif(between_group_p, digits = 2), sep = "")),
            size = 2.25) +
  geom_text(aes(label = paste0("n=", k)), vjust = 2.75, size = 2.1) +
  labs(x = "absolute SMD (95% CI)") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.margin = margin(b = -7),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        strip.text = element_text(size = 9,
                                  margin = margin(t = 3, b = 3.5)),
        strip.background = element_rect(size = 0.1))
ggsave("./figs/meta_figS1A.pdf", pA, height = 2.25, width = 2.65)

