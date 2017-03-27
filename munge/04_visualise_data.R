# density plot of LShB_rating_mean data ----------------------------------------
ggplot(RAW_data, aes(x = LShB_rating_mean, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
                   alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Leaf Sheath Blight Severity Rating") +
  theme_tufte()
ggsave("graphs/LShB_rating_mean.png", width = 4, height = 4)

# density plot of LShB_rating_sd data ------------------------------------------
ggplot(RAW_data, aes(x = LShB_rating_sd, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Leaf Sheath Blight Severity Rating Std Dev.") +
  theme_tufte() +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1))
ggsave("graphs/LShB_rating_sd.png", width = 4, height = 4)

# density plot of TShB_rating_mean data ----------------------------------------
ggplot(RAW_data, aes(x = TShB_rating_mean, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Severity Rating") +
  theme_tufte()
ggsave("graphs/TShB_rating_mean.png", width = 4, height = 4)

# density plot of TShB_rating_sd data ------------------------------------------
ggplot(RAW_data, aes(x = TShB_rating_sd, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  ggtitle("Tiller Sheath Blight Severity Rating Std Dev.") +
  theme_tufte()
ggsave("graphs/TShB_rating_sd.png", width = 4, height = 4)

# density plot of green leaf data ----------------------------------------------
ggplot(RAW_data, aes(x = GL_value_mean, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  xlab("Count") +
  ggtitle("Green Leaf Values") +
  theme_tufte()
ggsave("graphs/GL_value.png", width = 4, height = 4)

# density plot of dead leaf data -----------------------------------------------
ggplot(RAW_data, aes(x = DL_value_mean, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("DL_value") +
  theme_tufte()
ggsave("graphs/DL_value.png", width = 4, height = 4)

# line plot of leaf sheath blight AUDPS data -----------------------------------
ggplot(RAW_data, aes(x = ASMT, y = LShB_rating_mean, group = LShB_rating_mean)) +
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
ggsave("graphs/LShB_over_time.png", width = 4, height = 4)

# line plot of tiller sheath blight AUDPS data ---------------------------------
ggplot(RAW_data, aes(x = ASMT, y = GL_value_mean, group = GL_value_mean)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = as.factor(YEAR)),
               size = 1) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Year") +
  xlab("Assessment") +
  theme_tufte()
ggsave("graphs/TShB_over_time.png", width = 4, height = 4)

# density plot of leaf sheath blight AUDPS data --------------------------------
ggplot(AUDPS, aes(x = LShB_AUDPS, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  xlab("Area Under Disease Progress Stairs") +
  ggtitle("Leaf Sheath Blight AUDPS") +
  theme_tufte()
ggsave("graphs/LShB_AUDPS_density.png", width = 4, height = 4)

# density plot of tiller sheath blight AUDPS data ------------------------------
ggplot(AUDPS, aes(x = TShB_AUDPS, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  facet_grid(WMGT ~ NRTE) +
  xlab("Area Under Disease Progress Stairs") +
  ggtitle("Tiller Sheath Blight AUDPS") +
  theme_tufte()
ggsave("graphs/TShB_AUDPS_density.png", width = 4, height = 4)

# boxplots of leaf sheath blight AUDPS data ------------------------------------
ggplot(AUDPS, aes(x = TRT, y = LShB_AUDPS)) +
  geom_boxplot(aes(fill = YEAR, colour = YEAR)) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Year") +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  theme_tufte() +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1)) +
  xlab("Treatment") +
  ylab("Area Under Disease Progress Stairs (AUDPS)") +
  ggtitle("Leaf Sheath Blight Severity")
  ggsave("graphs/LShB_AUDPS_boxplot.png", width = 4, height = 4)

# boxplots of tiller sheath blight AUDPS data ----------------------------------
ggplot(AUDPS, aes(x = TRT, y = TShB_AUDPS)) +
  geom_boxplot(aes(fill = YEAR, colour = YEAR)) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Year") +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  theme_tufte() +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1)) +
  xlab("Treatment") +
  ylab("Area Under Disease Progress Stairs (AUDPS)") +
  ggtitle("Tiller Sheath Blight Severity")
ggsave("graphs/TShB_AUDPS_boxplot.png", width = 4, height = 4)
