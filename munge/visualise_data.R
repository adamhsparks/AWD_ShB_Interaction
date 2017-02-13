# Histograms of raw data -------------------------------------------------------
# check histogram of NTShB data
ggplot(AWD, aes(x = NTShB)) +
  geom_density() +
  facet_grid(. ~ YEAR) +
  ggtitle("NTShB") +
theme_minimal()
ggsave("graphs/NTShB.png")

# check histogram of LShB_rating data
ggplot(AWD, aes(x = LShB_rating)) +
  geom_density() +
  facet_grid(. ~ YEAR) +
  ggtitle("LShB_rating")
ggsave("graphs/LShB_rating.png")

ggplot(AWD, aes(x = TShB_rating)) +
  geom_density() +
  facet_grid(. ~ YEAR) +
  ggtitle("TShB_rating") +
theme_minimal()
ggsave("graphs/TShB_rating.png")

ggplot(AWD, aes(x = GL_value)) +
  geom_density() +
  facet_grid(. ~ YEAR) +
  ggtitle("GL_value") +
theme_minimal()
ggsave("graphs/GL_value.png")

ggplot(AWD, aes(x = DL_value)) +
  geom_density() +
  facet_grid(. ~ YEAR) +
  ggtitle("DL_value") +
theme_minimal()
ggsave("graphs/DL_value.png")

ggplot(AUDPS, aes(x = LShB_rating)) +
  geom_density() +
  ggtitle("LShB_AUDPS") +
  theme_minimal()
ggsave("graphs/LShB_AUDPS.png")

ggplot(AUDPS, aes(x = TShB_rating)) +
  geom_density() +
  ggtitle("TShB_AUDPS") +
  theme_minimal()
ggsave("graphs/TShB_AUDPS.png")

# Violinplots ---------------------------------------------------------------------

ggplot(AUDPS, aes(x = as.factor(TRT), y = LShB_rating)) +
  geom_violin(aes(fill = WMGT, colour = WMGT)) +
  theme_minimal()
ggsave("graphs/LShB_AUDPS.png")


ggplot(AUDPS, aes(x = TRT, y = TShB_rating)) +
  geom_violin(aes(fill = WMGT, colour = WMGT)) +
theme_minimal()
ggsave("graphs/TShB_AUDPS.png")
