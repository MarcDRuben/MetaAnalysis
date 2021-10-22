# Scatter SMD vs SEM, indicate outcome measure & drug mechanism
# Exclude all blood pressure outcomes

library(tidyverse)
library(ggrepel)
library(ggalt)
library(patchwork)
library(meta)

# All studies w/evaluable SMD; one selected outcome per study
infile <- readRDS('./data/meta.input.clean.2k.rds')

dat <- infile %>%
  filter(OUTCOME_TERM_GROUPS != "blood pressure") %>%
  # Ceiling for visualization
  mutate(g_ceiling = ifelse(g > 1, 1, g),
         sem_ceiling = ifelse(g_SEM > 0.45, 0.45, g_SEM),
         set = "all")

# Drug-outcome pairs in > 1 study
dat_f <- dat %>%
  group_by(DRUG, OUTCOME_TERM) %>%
  filter(n() > 1) %>%
  mutate(set = "replicated",
         set_names = DRUG)

all <- bind_rows(dat, dat_f)

# Plot g vs sem for both sets
labels <- c(
    `all` = "all studies",
    `replicated` = "replicated studies")


pA <- ggplot(all, aes(g_ceiling, sem_ceiling,
                      color = OUTCOME_TERM_GROUPS)) +
  geom_point(aes(shape = BETTER_TIME_EST)) +
  geom_encircle(aes(g_ceiling, sem_ceiling, color = OUTCOME_TERM_GROUPS),
                data = dat_f,
                expand = 0, alpha = 0.35) +
  geom_text_repel(aes(label = set_names), size = 2.5) +
  scale_shape_manual(values = c(15, 3), labels = c("evening", "morning"),
                     guide = guide_legend(
                       title = "better treatment time",
                       title.position = "top", 
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth = 0.4,
                       keyheight = 0.3,
                       order = 2)) +
  scale_color_manual(values = c("#999999", "#009E73", "#56B4E9",
                                "#D55E00", "#0072B2", "#000000"),
                     guide = guide_legend(
                       title = element_blank(),
                       keywidth = 1,
                       keyheight = 1,
                       order = 1)) +
  scale_y_reverse() +
  scale_x_continuous(expand = c(0.05, 0.05)) +
  labs(x = "absolute SMD", y = "SEM") +
  facet_wrap(~set, nrow = 1, labeller = as_labeller(labels)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 8.5),
        legend.margin = margin(b = -7, r = 20),
        legend.title = element_text(size = 8.5),
        strip.text = element_text(
          size = 8.5,
          margin = margin(t = 3, b = 3.5)),
        strip.background = element_rect(size = 0.1),
        axis.title = element_text(size = 8.5),
        axis.text = element_text(size = 8.5))
ggsave("./figs/meta_Fig4A.pdf", pA, width = 7.2, height = 4)


# Estimate pooled effect by random effects model; DerSimonian-Laird estimator
dsl <- metagen(g,
               g_SEM,
               data = dat,
               studlab = ID,
               comb.fixed = FALSE,
               comb.random = TRUE,
               hakn = FALSE,
               prediction = TRUE,
               sm = "SMD")

# Meta-regress SMD onto drug half-life

# Study weights contributing to both random effects models
weights <- bind_rows(
  weights(dsl, comb.random = TRUE) %>%
    rownames_to_column("ID")) %>%
  left_join(dat[, c("ID", "DRUG_MECHANISM_ABBREV", "DRUG_HALF_LIFE", "g", "PATIENTS")], by = "ID")

# Regress effect on drug half-life
hl.reg <- metareg(dsl, ~DRUG_HALF_LIFE)

# Model coefficients
coef <- tibble(studies = c("all"),
               intrcpt = c(hl.reg$beta[1]),
               slope = c(hl.reg$beta[2]),
               p_value = c(hl.reg$pval[2]),
               r2 = c(hl.reg$R2),
               i2 = c(hl.reg$I2))

# Tables for plot
tb_all <- coef %>%
  mutate(slope = signif(slope, digits = 1),
         p_value = signif(p_value, digits = 1),
         i2 = signif(i2, digits = 1)) %>%
  select(-c("studies", "intrcpt"))

d <- tibble(x = 45, y = 1.25, tb = list(tb_all))

# Plot
pB <- weights %>%
  ggplot(aes(DRUG_HALF_LIFE, g)) +
  geom_point(aes(size = p.random), alpha = 0.4) +
  scale_size(range = c(0.25, 3),
             guide = guide_legend(
               title = "% weight \n in model",
               title.position = "top",
               tittle.hjust = 0.5,
               title.vjust = 0.5,
               keyheight = 0.3)) +
  geom_text_repel(aes(label = DRUG_MECHANISM_ABBREV),
                  size = 1.5, max.overlaps = 1000,
                  min.segment.length = 0.2,
                  segment.color = 'grey70',
                  force = 25) +
  geom_table(data = d, aes(x = x, y = y, label = tb), size = 2) +
  geom_abline(data = coef,
              aes(intercept = intrcpt, slope = slope), color = "blue") +
  labs(x = "drug half-life (hours)", y = "absolute SMD",
       title = paste0("studies w/known drug half-life, n=", hl.reg$k)) +
  theme_bw() +
  theme(axis.text = element_text(size = 8.5),
        axis.title = element_text(size = 8.5),
        plot.title = element_text(size = 8.5, hjust = 0.5),
        legend.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        legend.position = "right",
        legend.margin = margin(l = -7))
ggsave("./figs/meta_Fig4C.pdf", pB, height = 3.25, width = 4.5)
