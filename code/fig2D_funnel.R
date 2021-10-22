# Funnel plot analysis of small study bias

library(tidyverse)
library(dmetar)
library(meta)

# All studies w/evaluable SMD; one selected outcome per study
infile <- readRDS('./data/meta.input.clean.2k.rds')

# Random effects model; pooled SMD by DerSimonian-Laird estimator
dsl <- metagen(g,
               g_SEM,
               data = infile,
               studlab = ID,
               comb.fixed = FALSE,
               comb.random = TRUE,
               hakn = FALSE,
               prediction = TRUE,
               sm = "SMD")

# Eggars test for assymetry from pkg 'meta'
x <- metabias(dsl, method.bias = "linreg")

# Funnel plot
cis.data <- tibble(se.seq = seq(0, max(infile$g_SEM), 0.001),
              ll95 = dsl$TE.random-(1.96*se.seq),
              ul95 = dsl$TE.random+(1.96*se.seq))

pA <- infile %>%
  mutate(g = ifelse(g > 2.5, 2.5, g)) %>% # Ceiling on SMD for plotting; only affects two outlying points
  ggplot(aes(x = g, y = g_SEM)) +
  geom_point(aes(color = REGISTERED), size = 0.75, shape = 0) +
  scale_color_manual(values = c("#D55E00", "#009E73"),
                     guide = guide_legend(
                       title = "pre-registered",
                       title.position = "left",
                       direction = "vertical",
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       keywidth=0.4,
                       keyheight=0.3,
                       ncol = 1,
                       order = 2)) +
  scale_y_reverse() +
  labs(x = "absolute SMD", y = "SEM") +
  geom_vline(xintercept = dsl$TE.random, linetype = "dotted", color = "blue") +
  geom_line(aes(x = ll95, y = se.seq), data = cis.data, linetype = "dotted", color = "blue") + 
  geom_line(aes(x = ul95, y = se.seq), data = cis.data, linetype = "dotted", color = "blue") +
  annotate(geom = "text", x = Inf, y = -Inf,
           label = paste0("test of assymetry, ", "p=", signif(x$p.value, digits = 2)),
           hjust = 1, vjust = 3, size = 2.25) +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(b = -8, r = 20, l = -20),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))
ggsave("./figs/meta_Fig2D.pdf", pA, width = 2.45, height = 2.2)



# Not used
# Simulate 1000 studies of different sample sizes (from 1 to 1K) drawn from normal distribution w/fixed mean and sd 
#set.seed(1234)
sim <- tibble(n = sample.int(1000, 1000, replace = TRUE)) %>%
  mutate(g = map(n, ~rnorm(., mean = m.dsl$TE.random, sd = m.dsl$TE.random))) %>%
  unnest(g) %>%
  group_by(n) %>%
  summarise(mean = mean(g),
            sd = sd(g)) %>%
  mutate(sem = sd/sqrt(n))

cis.sim <- tibble(se.seq = seq(0, max(sim$sem), 0.001),
              ll95 = m.dsl$TE.random-(1.96*se.seq),
              ul95 = m.dsl$TE.random+(1.96*se.seq))
set.seed(1234)
pC <- ggplot(sim, aes(x = mean, y = sem)) +
  geom_point() +
  geom_vline(xintercept = m.dsl$TE.random, linetype = "dotted", color = "blue") +
  geom_line(aes(x = ll95, y = se.seq), data = cis.sim, linetype = "dotted", color = "blue") + 
  geom_line(aes(x = ul95, y = se.seq), data = cis.sim, linetype = "dotted", color = "blue") +
  scale_y_reverse()


## Using sample size as precision measure
pD <- ggplot(infile, aes(x = g, y = 1/sqrt(PATIENTS))) +
  geom_point(aes(color = DESIGN)) +
  scale_x_continuous(limits = c(-.1, 1.5)) +
  scale_y_reverse() +
  geom_vline(xintercept = m.dsl$TE.random, linetype = "dotted", color = "blue")

