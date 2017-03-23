# density plot of LShB_rating data ---------------------------------------------
ggplot(RAW_data, aes(x = LShB_rating, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
                   alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("Leaf Sheath Blight Severity Rating") +
  theme_tufte()
ggsave("graphs/LShB_rating.png", width = 4, height = 4)

# density plot of TShB_rating data ---------------------------------------------
ggplot(RAW_data, aes(x = TShB_rating, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("Tiller Sheath Blight Severity Rating") +
  theme_tufte()
ggsave("graphs/TShB_rating.png", width = 4, height = 4)

# density plot of green leaf data ----------------------------------------------
ggplot(RAW_data, aes(x = GL_value, linetype = as.factor(YEAR))) +
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
ggplot(RAW_data, aes(x = DL_value, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("DL_value") +
  theme_tufte()
ggsave("graphs/DL_value.png", width = 4, height = 4)

# line plot of leaf sheath blight AUDPS data -----------------------------------
ggplot(RAW_data, aes(x = ASMT, y = LShB_rating, group = LShB_rating)) +
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


ggplot(RAW_data, aes(x = ASMT, y = GL_value, group = GL_value)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = TRT),
               size = .5) +
  stat_summary(fun.y = "mean", geom = "point",
               aes(group = factor(TRT),
                   colour = TRT),
               size = 1) +
  ylab("Green Leaves (count)") +
  xlab("Assessment") +
  facet_grid(. ~ YEAR) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Treatment") +
  theme_tufte()

ggplot(RAW_data, aes(x = ASMT, y = DL_value, group = DL_value)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = TRT),
               size = .5) +
  stat_summary(fun.y = "mean", geom = "point",
               aes(group = factor(TRT),
                   colour = TRT),
               size = 1) +
  ylab("Dead Leaves (count)") +
  xlab("Assessment") +
  facet_grid(. ~ YEAR) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Treatment") +
  theme_tufte()

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
