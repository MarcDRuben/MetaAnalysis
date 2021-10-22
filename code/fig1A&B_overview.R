# Meta overview figures 1A & 1B

library(tidyverse)
library(viridis)
library(patchwork)
#library(lemon)

# All studies w/evaluable SMD
infile <- readRDS('./data/meta.input.clean.rds')

# Clean for plot
dat <- infile %>%
  mutate(CLAIM_P05_GROUP = ifelse(CLAIM_P05 == "none", "no", "yes"),
         post2k = ifelse(YEAR >= 2000, "yes", "no"))

# year vs study size
pA <- ggplot (dat, aes(x = YEAR, y = PATIENTS, shape = DESIGN_SUMMARY)) +
  geom_point(size = 1, alpha = 0.75, aes(color = post2k)) +
  scale_shape_manual(values = c(17, 2),
                     guide = guide_legend(title = element_blank(),
                                          keywidth=0.4,
                                          keyheight=0.3)) +
  scale_color_manual(values = c("grey", "black"), guide = FALSE) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000),  labels = (c(10, 100, "1K", "10K"))) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020),
                     guide = guide_axis(n.dodge = 2)) +
  labs(y = "study size (patients)") +
  theme_bw() +
  theme(legend.position = c(0.37, 0.87),
        legend.spacing.y = unit(-0.08, "cm"),
        legend.title = element_text(size = 7),
        legend.background = element_rect(fill=alpha('white', 0.5)),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_blank(),
        plot.margin = margin(0, 15, 0, 0))

# Plot SMDs by outcome groups
pB <- dat %>%
  # Studies from 2000
  filter(post2k == "yes") %>%
  # Negative g if morning was more favorable than evening, positive if evening better
  mutate(g = case_when(
    BETTER_TIME_EST == "Morning" ~ g*-1, 
    TRUE ~ g),
    # put upwads ceiling on SMD for visualization; affects only two blood pressure studies
    g_cap = ifelse(g > 1.5, 1.5, g)) %>%
  ggplot(aes(OUTCOME_TERM_GROUPS, g_cap,
             color = OUTCOME_TERM_GROUPS,
             shape = CLAIM_P05_GROUP,
             size = log10(PATIENTS))) +
  geom_jitter(width = 0.3, alpha = 0.7) +
  scale_shape_manual(values = c(1, 16),
                     guide = guide_legend(
                       title = "study reported \n difference (p < 0.05)",
                       title.position = "top", 
                       title.hjust = 0.5,
                       title.vjust = 0,
                       keywidth = 0.4,
                       keyheight = 0.3,
                       order = 1)) +
  scale_color_manual(values = c("#E69F00", "#999999", "#009E73", "#56B4E9",
                                "#0072B2", "#D55E00", "#000000"),
                     guide = FALSE) +
  scale_size(range = c(0.75, 3.5),
             guide = guide_legend(
               title = "study size (log10)",
               title.position = "top",
               tittle.hjust = 0.5,
               title.vjust = 0.5,
               keyheight = 0.3,
               order = 2)) +
  #coord_flex_cart(bottom = brackets_horizontal(tick.length = 0)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5, size = 0.75, linetype = "dotted") +
  labs(y = "standardized mean difference\n (SMD)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8, margin = margin(r = 1)),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, -5))
pAB <- pA + pB + plot_layout(nrow = 1, widths = c(0.25, 0.75))
ggsave("./figs/meta_Fig1A_B.pdf", pAB, width = 7.2, height = 2)

## Additional overview plots not in publication

# Study size vs SMD_SE (strong positive correlation)
p2 <- ggplot(dat, aes(x = PATIENTS, y = g_SEM, color = g)) +
  geom_point() +
  scale_x_log10() +
  scale_color_viridis()

# Study size vs SMD (no obvious correlation)
p3 <- ggplot(dat, aes(x = PATIENTS, y = g)) +
  geom_point() +
  scale_x_log10()

# Study size vs SD-pooled
p4 <- ggplot(dat, aes(x = PATIENTS, y = SD_pool, color = g)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis()

# gSEM in relation to p-value significance vs null
p5 <- dat %>%
  filter(CLAIM_P05_GROUP == "yes") %>%
  mutate(REF = reorder(ID, PATIENTS)) %>%
  ggplot(aes(x = g , y = ID)) +
  geom_point(aes(size = log10(PATIENTS)), stroke = 0.75, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.2),
                     guide=guide_axis(n.dodge = 2)) +
  scale_y_discrete(expand = c(-1, -1)) +
  geom_linerange(aes(xmax = g_CI95_UP, xmin = g_CI95_DN), alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 8))