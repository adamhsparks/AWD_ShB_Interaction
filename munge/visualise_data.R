# Density plots of raw data -------------------------------------------------------

# check density plot of LShB_rating data
ggplot(AWD, aes(x = LShB_rating)) +
  geom_density() +
  ggtitle("LShB_rating") +
  theme_tufte()
ggsave("graphs/LShB_rating.png")

ggplot(AWD, aes(x = TShB_rating)) +
  geom_density() +
  ggtitle("TShB_rating") +
  theme_tufte()
ggsave("graphs/TShB_rating.png")

ggplot(AWD, aes(x = GL_value)) +
  geom_density() +
  ggtitle("GL_value") +
  theme_tufte()
ggsave("graphs/GL_value.png")

ggplot(AWD, aes(x = DL_value)) +
  geom_density() +
  ggtitle("DL_value") +
  theme_tufte()
ggsave("graphs/DL_value.png")

ggplot(AUDPS, aes(x = LShB_rating)) +
  geom_density() +
  ggtitle("LShB_AUDPS") +
  theme_tufte()
ggsave("graphs/LShB_AUDPS.png")

ggplot(AUDPS, aes(x = TShB_rating)) +
  geom_density() +
  ggtitle("TShB_AUDPS") +
  theme_tufte()
ggsave("graphs/TShB_AUDPS.png")

# boxplots ---------------------------------------------------------------------

ggplot(AUDPS, aes(x = TRT, y = LShB_rating)) +
  geom_rangeframe() +
  geom_tufteboxplot() +
  theme_tufte()
ggsave("graphs/LShB_AUDPS.png")


ggplot(AUDPS, aes(x = TRT, y = TShB_rating)) +
  geom_rangeframe() +
  geom_tufteboxplot() +
  theme_tufte()
ggsave("graphs/TShB_AUDPS.png")
