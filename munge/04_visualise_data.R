# histogram plot of LShB_rating data ----------------------------------------
ggplot(RAW_data, aes(x = LEAF_ShB)) +
  geom_histogram(aes(fill = YEAR), stat = "count") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Leaf Sheath Blight Severity Rating")
ggsave("graphs/LShB_rating.png", width = 6, height = 4)

# histogram plot of TShB_rating data ----------------------------------------
ggplot(RAW_data, aes(x = TIL_ShB)) +
  geom_histogram(aes(fill = YEAR), stat = "count") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Severity Rating")
ggsave("graphs/TShB_rating.png", width = 6, height = 4)


# density plot of TShB_incidence data ----------------------------------------
ggplot(RAW_data, aes(x = TShB_incidence)) +
  geom_density(aes(fill = YEAR, colour = YEAR)) +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Incidence") +
theme(axis.text.x = element_text(size = 8,
                                 angle = 45,
                                 hjust = 1))
ggsave("graphs/TShB_incidence.png", width = 6, height = 4)


# histogram plot of green leaf data ----------------------------------------------
ggplot(RAW_data, aes(x = GL, linetype = YEAR)) +
  geom_histogram(aes(fill = YEAR), position = "dodge") +
  xlab("Count") +
  ggtitle("Green Leaves per Tiller")
ggsave("graphs/GL_value.png", width = 6, height = 4)

# histogram plot plot of dead leaf data -----------------------------------------------
ggplot(RAW_data, aes(x = DL, linetype = YEAR)) +
  geom_histogram(aes(fill = YEAR), position = "dodge") +
  xlab("Count") +
  ggtitle("Dry Leaves per Tiller")
ggsave("graphs/DL_value.png", width = 6, height = 4)

# dot plot of leaf sheath blight severity data ---------------------------------
ggplot(RAW_data, aes(x = ASMT, y = LEAF_ShB)) +
  geom_point(position = position_jitter(width = 0.05),
             aes(colour = YEAR), size = 0.75) +
  xlab("Assessment") +
  ylab("Rating") +
  ggtitle("Leaf Sheath Blight Severity at Each Assessment") +
  facet_grid(. ~ TRT) +
  theme(strip.text.x = element_text(angle = 90))
ggsave("graphs/LShB_severity_over_time.png", width = 6, height = 4)

# dot plot of tiller sheath blight severity data ------------------------------
ggplot(RAW_data, aes(x = ASMT, y = TIL_ShB)) +
  geom_point(position = position_jitter(width = 0.05),
             aes(colour = YEAR), size = 0.75) +
  xlab("Assessment") +
  ylab("Rating") +
  facet_grid(. ~ TRT) +
  ggtitle("Tiller Sheath Blight Severity at Each Assessment") +
  theme(strip.text.x = element_text(angle = 90))
ggsave("graphs/TShB_severity_over_time.png", width = 6, height = 4)

# line plot of tiller sheath blight incidence data ---------------------------------
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
ggsave("graphs/TShB_incidence_over_time.png", width = 6, height = 4)

# violin/dot plots of tiller sheath blight incidence data ----------------------
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
ggsave("graphs/TShB_Incidence_plot.png", width = 6, height = 4)

# dotplots of tiller sheath blight incidence AUDPS data ------------------------
ggplot(AUDPS, aes(x = as.factor(TRT), y = TShB_inc_AUDPS)) +
  geom_point(aes(colour = YEAR, shape = REP), size = 2) +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1)) +
  xlab("Treatment") +
  ylab("AUDPS") +
  ggtitle("Tiller Sheath Blight Incidence")
ggsave("graphs/TShB_inc_AUDPS_dotplot.png", width = 6, height = 4)

# dotplots of tiller sheath blight severity AUDPS data ------------------------
ggplot(AUDPS, aes(x = as.factor(TRT), y = TShB_sev_AUDPS)) +
  geom_point(aes(colour = YEAR, shape = REP), size = 2) +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1)) +
  xlab("Treatment") +
  ylab("AUDPS") +
  ggtitle("Tiller Sheath Blight Severity")
ggsave("graphs/TShB_sev_AUDPS_dotplot.png", width = 6, height = 4)

# dotplots of leav sheath blight severity AUDPS data ------------------------
ggplot(AUDPS, aes(x = as.factor(TRT), y = LShB_sev_AUDPS)) +
  geom_point(aes(colour = YEAR, shape = REP), size = 2) +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1)) +
  xlab("Treatment") +
  ylab("AUDPS") +
  ggtitle("Leaf Sheath Blight Severity")
ggsave("graphs/LShB_sev_AUDPS_dotplot.png", width = 6, height = 4)
