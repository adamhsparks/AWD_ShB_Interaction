# Histogram plot of LShB_rating data ----------------------------------------
ggplot(RAW_data, aes(x = LEAF_ShB)) +
  geom_histogram(aes(fill = YEAR), stat = "count") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Leaf Sheath Blight Severity Rating")
ggsave("graphs/01_LShB_rating.png", width = 6, height = 4)


# Histogram plot of TShB_rating data ----------------------------------------
ggplot(RAW_data, aes(x = TIL_ShB)) +
  geom_histogram(aes(fill = YEAR), stat = "count") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Severity Rating")
ggsave("graphs/02_TShB_rating.png", width = 6, height = 4)


# Density plot of TShB_incidence data ----------------------------------------
ggplot(RAW_data, aes(x = TShB_incidence)) +
  geom_density(aes(fill = YEAR, colour = YEAR)) +
  geom_rug() +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Incidence") +
theme(axis.text.x = element_text(size = 8,
                                 angle = 45,
                                 hjust = 1))
ggsave("graphs/03_TShB_incidence.png", width = 6, height = 4)

# Histogram plot of green leaf data -------------------------------------------
ggplot(RAW_data, aes(x = GL, linetype = YEAR)) +
  geom_histogram(aes(fill = YEAR), position = "dodge") +
  xlab("Count") +
  ggtitle("Green Leaves per Tiller")
ggsave("graphs/04_GL_value.png", width = 6, height = 4)


# Histogram plot plot of dead leaf data ---------------------------------------
ggplot(RAW_data, aes(x = DL, linetype = YEAR)) +
  geom_histogram(aes(fill = YEAR), position = "dodge") +
  xlab("Count") +
  ggtitle("Dry Leaves per Tiller")
ggsave("graphs/05_DL_value.png", width = 6, height = 4)


# Dot plot of leaf sheath blight severity data --------------------------------
ggplot(RAW_data, aes(x = ASMT, y = LEAF_ShB)) +
  geom_point(position = position_jitter(width = 0.05),
             aes(colour = YEAR), size = 0.75) +
  xlab("Assessment") +
  ylab("Rating") +
  ggtitle("Leaf Sheath Blight Severity at Each Assessment") +
  facet_grid(. ~ TRT) +
  theme(strip.text.x = element_text(angle = 90))
ggsave("graphs/06_LShB_severity_over_time.png", width = 6, height = 4)


# Dot plot of tiller sheath blight severity data ------------------------------
ggplot(RAW_data, aes(x = ASMT, y = TIL_ShB)) +
  geom_point(position = position_jitter(width = 0.05),
             aes(colour = YEAR), size = 0.75) +
  xlab("Assessment") +
  ylab("Rating") +
  facet_grid(. ~ TRT) +
  ggtitle("Tiller Sheath Blight Severity at Each Assessment") +
  theme(strip.text.x = element_text(angle = 90))
ggsave("graphs/07_TShB_severity_over_time.png", width = 6, height = 4)


# Line graph of tiller sheath blight incidence data ---------------------------
ggplot(RAW_data, aes(x = ASMT, y = TShB_incidence,
                     group = TShB_incidence)) +
  geom_point(position = position_jitter(width = 0.05),
             aes(colour = YEAR), size = 0.75) +
  xlab("Assessment") +
  ylab("Incidence") +
  facet_grid(. ~ TRT) +
  ggtitle("Tiller Sheath Blight Severity Rating Over Time") +
  theme(strip.text.x = element_text(angle = 90)) +
  xlab("Assessment") +
  ylab("Incidence") +
  ggtitle("Tiller Sheath Blight at Each Assessment")
ggsave("graphs/08_TShB_incidence_over_time.png", width = 6, height = 4)


# Violin/dot plots of tiller sheath blight incidence data ----------------------
ggplot(RAW_data, aes(x = TRT, y = TShB_incidence)) +
  geom_point(position = position_jitter(width = 0.2),
             aes(colour = YEAR), size = 0.1) +
  geom_violin(fill = NA) +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1)) +
  xlab("Treatment") +
  ylab("Incidence per Hill") +
  ggtitle("Tiller Sheath Blight Incidence")
ggsave("graphs/09_TShB_incidence_violin_plot.png", width = 6, height = 4)


# Box/dotplots of tiller sheath blight incidence AUDPS data --------------------

# Reorder factors for easier interpretation of plots
AUDPS$NRTE <-
  forcats::fct_relevel(AUDPS$NRTE,
                       "N0",
                       "N100",
                       "N120",
                       "N60",
                       "N180")

ggplot(AUDPS, aes(x = NRTE, y = TShB_inc_AUDPS)) +
  geom_boxplot(aes(fill = YEAR)) +
  geom_point() +
  xlab("Treatment") +
  ylab("AUDPS") +
  ggtitle("Tiller Sheath Blight Incidence")
ggsave("graphs/10_TShB_inc_AUDPS_NRTE_boxplot.png", width = 6, height = 4)


ggplot(AUDPS, aes(x = WMGT, y = TShB_inc_AUDPS)) +
  geom_boxplot(aes(fill = YEAR)) +
  geom_point() +
  xlab("Treatment") +
  ylab("AUDPS") +
  facet_grid(. ~ YEAR) +
  ggtitle("Tiller Sheath Blight Incidence")
ggsave("graphs/11_TShB_inc_AUDPS_WMGT_boxplot.png", width = 6, height = 4)


# Box/dotplots of leaf sheath blight severity AUDPS data -----------------------
ggplot(AUDPS, aes(x = NRTE, y = TShB_percent_AUDPS)) +
  geom_boxplot(aes(fill = YEAR)) +
  geom_point() +
  xlab("Treatment") +
  ylab("AUDPS") +
  ggtitle("Tiller Sheath Blight Severity")
ggsave("graphs/12_TShB_severity_AUDPS_boxplot.png", width = 6, height = 4)

ggplot(AUDPS, aes(x = WMGT, y = TShB_inc_AUDPS)) +
  geom_boxplot(aes(fill = YEAR)) +
  geom_point() +
  xlab("Treatment") +
  ylab("AUDPS") +
  facet_grid(. ~ YEAR) +
  ggtitle("Tiller Sheath Blight Severity")
ggsave("graphs/13_TShB_severity_AUDPS_boxplot.png", width = 6, height = 4)


# Box/dotplots of leaf sheath blight severity AUDPS ----------------------------
ggplot(AUDPS, aes(x = NRTE, y = LShB_percent_AUDPS)) +
  geom_boxplot(aes(fill = YEAR)) +
  geom_point() +
  xlab("Treatment") +
  ylab("AUDPS") +
  ggtitle("Leaf Sheath Blight Severity")
ggsave("graphs/14_LShB_severity_AUDPS_NRTE_boxplot.png", width = 6, height = 4)


ggplot(AUDPS, aes(WMGT, y = LShB_percent_AUDPS)) +
  geom_boxplot(aes(fill = YEAR)) +
  geom_point() +
  xlab("Treatment") +
  ylab("AUDPS") +
  facet_grid(. ~ YEAR) +
  ggtitle("Leaf Sheath Blight Severity")
ggsave("graphs/15_LShB_seveity_AUDPS_WMGT_boxplot.png", width = 6, height = 4)

# Line graph of leaf sheath blight severity data -------------------------------
RAW_data %>% group_by(YEAR, WMGT, NRTE, ASMT) %>%
  summarize(value = mean(PERC_LEAF_ShB)) %>%
  ggplot(aes(x = ASMT,
             y = value,
             color = NRTE,
             group = NRTE)) +
  geom_line() +
  xlab("Assessment") +
  ylab("Severity (%)") +
  facet_grid(WMGT ~ YEAR) +
  ggtitle("Leaf Sheath Blight")
ggsave("graphs/16_LShB_progress_curves.png", width = 6, height = 4)

# Line graph of tiller sheath blight severity AUDPS data ----------------------
RAW_data %>% group_by(YEAR, WMGT, NRTE, ASMT) %>%
  summarize(value = mean(PERC_TIL_ShB)) %>%
  ggplot(aes(x = ASMT,
             y = value,
             color = NRTE,
             group = NRTE)) +
  geom_line() +
  xlab("Assessment") +
  ylab("Severity (%)") +
  facet_grid(WMGT ~ YEAR) +
  ggtitle("Tiller Sheath Blight")
ggsave("graphs/17_TShB_progress_curves.png", width = 6, height = 4)


# Line graph of tiller sheath blight incidence AUDPS data ---------------------
RAW_data %>% group_by(YEAR, WMGT, NRTE, ASMT) %>%
  summarize(value = mean(TShB_incidence)) %>%
  ggplot(aes(x = ASMT,
             y = value,
             color = NRTE,
             group = NRTE)) +
  geom_line() +
  xlab("Assessment") +
  ylab("Incidence (%)") +
  facet_grid(WMGT ~ YEAR) +
  ggtitle("Tiller Sheath Blight")
ggsave("graphs/18_TShB_incidence_progress_curves.png", width = 6, height = 4)
