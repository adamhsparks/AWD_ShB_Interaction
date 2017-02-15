# Density plots of raw data -------------------------------------------------------
AWD <- AWD[AWD$ASMT != 5, ]
# check density plot of LShB_rating data
ggplot(AWD, aes(x = LShB_rating)) +
  geom_density() +
  ggtitle("LShB_rating") +
  theme_tufte()
ggsave("graphs/LShB_rating.png", width = 4, height = 4)

ggplot(AWD, aes(x = TShB_rating)) +
  geom_density() +
  ggtitle("TShB_rating") +
  theme_tufte()
ggsave("graphs/TShB_rating.png", width = 4, height = 4)

ggplot(AWD, aes(x = GL_value)) +
  geom_density() +
  ggtitle("GL_value") +
  theme_tufte()
ggsave("graphs/GL_value.png", width = 4, height = 4)

ggplot(AWD, aes(x = DL_value)) +
  geom_density() +
  ggtitle("DL_value") +
  theme_tufte()
ggsave("graphs/DL_value.png", width = 4, height = 4)

ggplot(AUDPS, aes(x = LShB_rating)) +
  geom_density() +
  ggtitle("LShB_AUDPS") +
  theme_tufte()
ggsave("graphs/LShB_AUDPS.png", width = 4, height = 4)

ggplot(AWD, aes(x = ASMT, y = LShB_rating, group = LShB_rating)) +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = factor(TRT),
                   colour = TRT,
                   linetype = TRT),
               size = 1) +
  theme_tufte()
  ggsave("graphs/LShB_over_time.png", width = 4, height = 4)

ggplot(AUDPS, aes(x = TShB_rating)) +
  geom_density() +
  ggtitle("TShB_AUDPS") +
  ggtitle("TShB Rating Over Time") +
  theme_tufte()
ggsave("graphs/TShB_AUDPS.png", width = 4, height = 4)

AWD_LShB_SE <-
  AWD %>% ddply(c("ASMT", "TRT"), summarise,
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
  ggtitle("Leaf Sheath Blight Severity") +
  theme_tufte()
ggsave("graphs/LShB_over_time.png", width = 4, height = 4)

AWD_TShB_SE <-
  AWD %>% ddply(c("ASMT", "TRT"), summarise,
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
  theme_tufte()

# boxplots ---------------------------------------------------------------------

ggplot(AUDPS, aes(x = TRT, y = LShB_rating)) +
  geom_rangeframe() +
  geom_tufteboxplot() +
  theme_tufte()
ggsave("graphs/LShB_AUDPS.png", width = 4, height = 4)


ggplot(AUDPS, aes(x = TRT, y = TShB_rating)) +
  geom_rangeframe() +
  geom_tufteboxplot() +
  theme_tufte()
ggsave("graphs/TShB_AUDPS.png", width = 4, height = 4)

