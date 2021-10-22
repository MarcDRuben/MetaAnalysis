# Meta forest plot

library(tidyverse)
library(metafor)
library(meta)
library(ggtext)
library(patchwork)
library(ggpmisc)

# All studies w/evaluable SMD; one selected outcome per study
infile <- readRDS('./data/meta.input.clean.2k.rds') 

# Estimate pooled effect by random effects model; DerSimonian-Laird estimator
dsl <- metagen(g,
               g_SEM,
               data = infile,
               studlab = ID,
               comb.fixed = FALSE,
               comb.random = TRUE,
               hakn = FALSE,
               prediction = TRUE,
               sm = "SMD")
dsl

# Optional: REML; another variance estimator; # Use different estimator algorithms: HKSJ; DS, REML
#m.gen_update <- update.meta(m.dsl, method.tau = "REML")

# Extract parameters from random effects model
dsl_fig <- tibble(ID = "pooled_effect_dsl",
              g = dsl$TE.random,
              g_SEM = dsl$seTE.random,
              g_CI95_UP = dsl$upper.random,
              g_CI95_DN = dsl$lower.random) %>%
  bind_rows(tibble(ID = "pred_dsl",
              g_CI95_UP = dsl$upper.predict, # prediction interval
              g_CI95_DN = dsl$lower.predict)) %>%
  mutate(idx = c(-1, -3)) # index

# Table with meta-variance estimates
het <- tibble(estimator = "DerSimonian-Laird",
              tau2 = paste0(signif(dsl$tau2, digits = 2),
                            " [",
                            signif(dsl$lower.tau2, digits = 2),
                            "-",
                            signif(dsl$upper.tau2, digits = 2),
                            "]"),
              I2 = paste0(signif(dsl$I2, digits = 2),
                            " [",
                            signif(dsl$lower.I2, digits = 2),
                            "-",
                            signif(dsl$upper.I2, digits = 2),
                            "]"),
              Q = signif(dsl$Q, digits = 2), pvalQ = signif(dsl$pval.Q, digits = 2)) %>%
  pivot_longer(estimator:pvalQ, names_to = "heterogeneity", values_to = "value",
               values_transform = list(value = as.character))


dat <- infile %>%
  select(ID, DRUG_MECHANISM_ABBREV, DISEASE, PATIENTS, OUTCOME_TERM_GROUPS, BETTER_TIME_EST, g,
         g_CI95_UP, g_CI95_DN) %>%
  arrange(g) %>%
  # Index by g
  mutate(idx = seq(1, nrow(.), 1)) %>%
  # Remove extreme outlier study for visualization
  filter(ID != "(Farah 2013)  <B>mixed AH<B>")

labels <- c(c("", "**<span style='font-size:9pt'>random effects model</span>**", "", ""), dat %>% pull(ID))
# Remove extreme outlier study for visualization
labels <- labels[1:80]

pA <- ggplot(dat, aes(x = g , y = idx)) +
  geom_point(aes(color = OUTCOME_TERM_GROUPS), stroke = 0.75, shape = 0, size = 0.6) +
  scale_color_manual(values = c("#E69F00", "#999999", "#009E73", "#56B4E9",
                                "#0072B2", "#D55E00", "#000000"),
                     guide = guide_legend(
                       override.aes = list(size = 3),
                       title = "outcome measure",
                       title.position = "top", 
                       title.hjust = 0.5,
                       title.vjust = 0,
                       keywidth = 0.6,
                       keyheight = 0.6)) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5)) +
  geom_vline(xintercept = dsl_fig$g[dsl_fig$ID == "pooled_effect_dsl"], color = "blue",
             size = 0.75, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "black", alpha = 0.10,
             size = 1) +
  geom_linerange(aes(xmax = g_CI95_UP, xmin = g_CI95_DN), alpha = 0.4) +
  geom_linerange(dat = dsl_fig[dsl_fig$ID == "pred_dsl", ],
                 aes(xmax = g_CI95_UP, xmin = g_CI95_DN), color = "red", size = 3) +
  geom_point(dat = dsl_fig[dsl_fig$ID == "pooled_effect_dsl", ],
             aes(x = g, y = idx), color = "blue", shape = 18, stroke = 1.5, size = 2) +
  scale_y_continuous(expand = c(0.025, 0.025), breaks = -3:76, labels = labels) +
  geom_text(x = 1.5, y = -1, label = paste("model estimate = ",
                                           signif(dsl_fig$g[dsl_fig$ID == "pooled_effect_dsl"],
                                                  digits = 2),
                                           " [", signif(dsl_fig$g_CI95_DN[dsl_fig$ID == "pooled_effect_dsl"],
                                                       digits = 2),
                                           "-",
                                           signif(dsl_fig$g_CI95_UP[dsl_fig$ID == "pooled_effect_dsl"],
                                                  digits = 2), 
                                           "]", sep = ""),
            color = "blue", size = 2.75) +
  geom_text(x = 2, y = -3, label = paste("prediction interval ",
                                            " [",
                                            signif(dsl_fig$g_CI95_DN[dsl_fig$ID == "pred_dsl"],
                                                                          digits = 2),
                                            "-",
                                            signif(dsl_fig$g_CI95_UP[dsl_fig$ID == "pred_dsl"],
                                                   digits = 2),
                                            "]",
                                            sep = ""),
            color = "red", size = 2.75) +
  geom_table(data = het, aes(x = 4, y = 25, label = list(het)), size = 2.5) +
  labs(x = "absolute SMD (95% CI)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
      axis.text.y = element_markdown(size = 5),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 9, hjust = 0.5),
      legend.position = c(0.75, 0.75),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 9))
ggsave("./figs/meta_Fig1C.pdf", pA, height = 6.3, width = 7)

