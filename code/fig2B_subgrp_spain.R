# Subgroup analysis - Northern Spain studies

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

# Subgroup analyses via mixed-effects model: random within subgroups, fixed between subgroups

# Northern Spain versus 'Other' studies
subgrp <- subgroup.analysis.mixed.effects(x = dsl, subgroups = infile$NSPAIN)
subgrp_out <- subgrp$within.subgroup.results %>%
  rownames_to_column(var = "levels") %>%
  mutate(between_group_p = subgrp$subgroup.analysis.results$p,
         between_group_Q = subgrp$subgroup.analysis.results$Q,
         between_group_df = subgrp$subgroup.analysis.results$df,
         subgroup = "Northern Spain")

pA <- ggplot(subgrp_out, aes(x = SMD , y = levels)) +
  geom_point(aes(shape = levels), size = 2.25, stroke = 0.75) +
  scale_shape_manual(values = c(2, 17),
                     labels = c(paste0("no (n=", subgrp_out$k[subgrp_out$levels == "NO"],")"),
                                paste0("yes (n=", subgrp_out$k[subgrp_out$levels == "YES"], ")")),
                     guide = guide_legend(
                       title = "N.W.Spain",
                       title.position = "left", 
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth=0.4,
                       keyheight=0.3,
                       ncol = 1,
                       order = 1)) +
  geom_linerange(aes(xmax = ULCI, xmin = LLCI), size = 0.2) +
  geom_vline(aes(xintercept = dsl$TE.random), color = "blue", size = 0.75,
             linetype = "dotted") +
  geom_text(aes(x = 0.7, y = 1.5,
                label = paste("p=", signif(between_group_p, digits = 2), sep = "")), size = 2.5) +
  labs(x = "absolute SMD (95% CI)") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 8.5),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.margin = margin(b = -7),
        legend.text = element_text(size = 8.5),
        legend.title = element_text(size = 8.5))
ggsave("./figs/meta_Fig2B.pdf", pA, height = 2, width = 1.5)


