# Histograms of raw data -------------------------------------------------------
# check histogram of NTShB data
ggplot(AWD, aes(x = NTShB)) +
  geom_histogram() +
  facet_grid(. ~ YEAR) +
  ggtitle("NTShB")
ggsave("graphs/NTShB.png")

# check histogram of LShB_rating data
ggplot(AWD, aes(x = LShB_rating)) +
  geom_histogram() +
  facet_grid(. ~ YEAR) +
  ggtitle("LShB_rating")
ggsave("graphs/LShB_rating.png")

ggplot(AWD, aes(x = TShB_rating)) +
  geom_histogram() +
  facet_grid(. ~ YEAR) +
  ggtitle("TShB_rating")
ggsave("graphs/TShB_rating.png")

ggplot(AWD, aes(x = GL_value)) +
  geom_histogram() +
  facet_grid(. ~ YEAR) +
  ggtitle("GL_value")
ggsave("graphs/GL_value.png")

ggplot(AWD, aes(x = DL_value)) +
  geom_histogram() +
  facet_grid(. ~ YEAR) +
  ggtitle("DL_value")
ggsave("graphs/DL_value.png")

# Boxplots ---------------------------------------------------------------------
ggplot(AUDPS, aes(x = as.factor(TRT), y = LShB_rating)) +
  geom_boxplot()
ggsave("graphs/LShB_AUDPS.png")

ggplot(AUDPS, aes(x = as.factor(TRT), y = TShB_rating)) +
  geom_boxplot()
ggsave("graphs/TShB_AUDPS.png")
