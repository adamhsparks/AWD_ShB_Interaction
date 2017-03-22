# Density plots of raw data -------------------------------------------------------

# check density plot of LShB_rating data
ggplot(AWD, aes(x = LShB_rating, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
                   alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("LShB_rating") +
  theme_tufte()
ggsave("graphs/LShB_rating.png", width = 4, height = 4)

ggplot(AWD, aes(x = TShB_rating, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("TShB_rating") +
  theme_tufte()
ggsave("graphs/TShB_rating.png", width = 4, height = 4)

ggplot(AWD, aes(x = GL_value, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("GL_value") +
  theme_tufte()
ggsave("graphs/GL_value.png", width = 4, height = 4)

ggplot(AWD, aes(x = DL_value, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("DL_value") +
  theme_tufte()
ggsave("graphs/DL_value.png", width = 4, height = 4)

ggplot(AUDPS, aes(x = LShB_rating, linetype = as.factor(YEAR))) +
  geom_density(aes(fill = as.factor(YEAR)),
               alpha = 0.5) +
  viridis::scale_fill_viridis(discrete = TRUE,
                              name = "Year") +
  scale_linetype(name = "Year") +
  ggtitle("LShB_AUDPS") +
  theme_tufte()
ggsave("graphs/LShB_AUDPS.png", width = 4, height = 4)

ggplot(AWD, aes(x = ASMT, y = LShB_rating, group = LShB_rating)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = as.factor(YEAR)),
               size = 1) +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Year") +
  theme_tufte()
ggsave("graphs/LShB_over_time.png", width = 4, height = 4)

ggplot(AUDPS, aes(x = TShB_rating, linetype = as.factor(YEAR))) +
  geom_density() +
  ggtitle("TShB_AUDPS") +
  ggtitle("TShB Rating Over Time") +
  theme_tufte()
ggsave("graphs/TShB_AUDPS.png", width = 4, height = 4)

AWD_LShB_SE <-
  AWD %>% ddply(c("YEAR", "ASMT", "TRT"), summarise,
                N    = length(LShB_rating),
                mean = mean(LShB_rating),
                sd   = sd(LShB_rating),
                se   = sd / sqrt(N))

AWD_LShB_SE <- mutate(AWD_LShB_SE, ciMult = qt(.95 / 2 + 0.5, N - 1))
AWD_LShB_SE <- mutate(AWD_LShB_SE, ci = se * ciMult)


ggplot(AWD_LShB_SE, aes(x = ASMT, y = mean, group = mean)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = TRT),
               size = .5) +
  stat_summary(fun.y = "mean", geom = "point",
               aes(group = factor(TRT),
                   colour = TRT),
               size = 1) +
  xlab("Assesment Number") +
  ylab("Rating") +
  ggtitle("Tiller Sheath Blight Severity") +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Treatment") +
  facet_grid(. ~ YEAR) +
  theme_tufte()
ggsave("graphs/LShB_over_time.png", width = 4, height = 4)

AWD_TShB_SE <-
  AWD %>% ddply(c("YEAR", "ASMT", "TRT"), summarise,
                N    = length(TShB_rating),
                mean = mean(TShB_rating),
                sd   = sd(TShB_rating),
                se   = sd / sqrt(N))

AWD_TShB_SE <- mutate(AWD_TShB_SE, ciMult = qt(.95 / 2 + 0.5, N - 1))
AWD_TShB_SE <- mutate(AWD_TShB_SE, ci = se * ciMult)


ggplot(AWD_TShB_SE, aes(x = ASMT, y = mean, group = mean)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = TRT),
               size = .5) +
  stat_summary(fun.y = "mean", geom = "point",
               aes(group = factor(TRT),
                   colour = TRT),
               size = 1) +
  xlab("Assesment Number") +
  ylab("Rating") +
  ggtitle("Tiller Sheath Blight Severity") +
  viridis::scale_color_viridis(discrete = TRUE,
                               name = "Treatment") +
  scale_linetype(name = "Treatment") +
  facet_grid(. ~ YEAR) +
  theme_tufte()
ggsave("graphs/TShB_over_time.png", width = 4, height = 4)


ggplot(AWD, aes(x = ASMT, y = GL_value, group = GL_value)) +
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

ggplot(AWD, aes(x = ASMT, y = DL_value, group = DL_value)) +
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

# boxplots ---------------------------------------------------------------------

ggplot(AUDPS, aes(x = TRT, y = LShB_rating)) +
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
  ylab("Leaf Sheath Blight Rating")
ggsave("graphs/LShB_AUDPS.png", width = 4, height = 4)


ggplot(AUDPS, aes(x = TRT, y = TShB_rating)) +
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
  ylab("Tiller Sheath Blight Rating")
ggsave("graphs/TShB_AUDPS.png", width = 4, height = 4)

