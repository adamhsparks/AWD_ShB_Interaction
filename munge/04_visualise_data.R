# density plot of LShB_rating data ----------------------------------------
ggplot(RAW_data, aes(x = LEAF_SHB, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Leaf Sheath Blight Severity Rating") +
  theme_tufte()
ggsave("graphs/LShB_rating.png", width = 6, height = 4)

# density plot of TShB_rating data ----------------------------------------
ggplot(RAW_data, aes(x = TIL_SHB, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Severity Rating") +
  theme_tufte()
ggsave("graphs/TShB_rating.png", width = 6, height = 4)


# density plot of TShB_incidence data ----------------------------------------
ggplot(RAW_data, aes(x = TShB_incidence, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Incidence") +
  theme_tufte() +
theme(axis.text.x = element_text(size = 8,
                                 angle = 45,
                                 hjust = 1))
ggsave("graphs/TShB_incidence.png", width = 6, height = 4)


# density plot of green leaf data ----------------------------------------------
ggplot(RAW_data, aes(x = GL, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  xlab("Count") +
  ggtitle("Green Leaf Count") +
  theme_tufte()
ggsave("graphs/GL_value.png", width = 6, height = 4)

# density plot of dead leaf data -----------------------------------------------
ggplot(RAW_data, aes(x = DL, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("Dry Leaf Count") +
  theme_tufte()
ggsave("graphs/DL_value.png", width = 6, height = 4)

# line plot of leaf sheath blight severity data --------------------------------
ggplot(RAW_data, aes(x = ASMT, y = LEAF_SHB,
                     group = LEAF_SHB)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = as.factor(YEAR)),
               size = 1) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Year") +
  xlab("Assessment") +
  ylab("Leaf Sheath Blight Rating") +
  ggtitle("Leaf Sheath Blight Severity Over Time") +
  theme_tufte()
ggsave("graphs/LShB_severity_over_time.png", width = 6, height = 4)

# line plot of tiller sheath blight severity data ------------------------------
ggplot(RAW_data, aes(x = ASMT, y = TIL_SHB,
                     group = TIL_SHB)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = as.factor(YEAR)),
               size = 1) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Year") +
  xlab("Assessment") +
  theme_tufte() +
  ggtitle("Tiller Sheath Blight Severity Over Time")
ggsave("graphs/TShB_severity_over_time.png", width = 6, height = 4)

# line plot of tiller sheath blight incidence data ---------------------------------
ggplot(RAW_data, aes(x = ASMT, y = TShB_incidence,
                     group = TShB_incidence)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = as.factor(YEAR)),
               size = 1) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Year") +
  xlab("Assessment") +
  theme_tufte() +
  ggtitle("Tiller Sheath Blight Incidence Over Time")
ggsave("graphs/TShB_incidence_over_time.png", width = 6, height = 4)

# boxplots of tiller sheath blight incidence data ------------------------------
ggplot(RAW_data, aes(x = as.factor(TRT), y = TShB_incidence)) +
  geom_boxplot(aes(fill = YEAR, colour = YEAR)) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Year") +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year",
                              option = "A") +
  theme_tufte() +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1)) +
  xlab("Treatment") +
  ylab("Tiller Sheath Blight Incidence per Hill") +
  ggtitle("Tiller Sheath Blight Incidence")
ggsave("graphs/TShB_Incidence_boxplot.png", width = 6, height = 4)

