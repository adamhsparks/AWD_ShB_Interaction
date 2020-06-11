if (!require("pacman")) {
  install.packages("pacman")
}
library("pacman")

# requires jags to be installed, e.g.
# > brew install jags
p_load("magrittr", "tidyverse", "rjags", "readxl")

if (!require("BayesianFirstAid")) {
  remotes::install_github("rasmusab/bayesian_first_aid")
  library("Bayesian_First_Aid")
}

# create a notin function
`%notin%` = function(x, y)
  !(x %in% y)

l <- list.files(pattern = ".xlsx$",
                path = "data-raw/data",
                full.names = TRUE)
f <- map(
  l,
  read_excel,
  sheet = 13,
  skip = 4,
  col_names = c(
    "year",
    "month",
    "day",
    "radiation",
    "sun_hours",
    "rainfall",
    "et",
    "tmax",
    "tmin",
    "tavg",
    "ea",
    "es",
    "rh",
    "wndspd",
    "wnddir"
  ),
  na = c("*", "")
) %>%
  bind_rows() %>%
  mutate(YYYYMMDD = paste(year, month, day, sep = "-")) %>%
  mutate(YYYYMMDD = as.Date(YYYYMMDD, format = "%Y-%b-%d")) %>%
  filter(month %in% c("Jan", "Feb", "Mar", "Apr")) %>%
  filter(year != "2014")

# Rainfall ---------------------------------------------------------------------

ggplot(f, aes(x = YYYYMMDD,
              y = rainfall,
              group = year)) +
  geom_line(colour = "blue") +
  ylab("Precipitation (mm)") +
  xlab("Date") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  facet_wrap(. ~ year, scales = "free_x")

# Temperature ------------------------------------------------------------------

ggplot(f, aes(x = YYYYMMDD,
              y = tavg,
              group = year)) +
  geom_line() +
  geom_line(aes(y = tmin), col = "blue") +
  geom_line(aes(y = tmax), col = "red") +
  ylab("Temperature (˚C)") +
  xlab("Date") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  facet_wrap(. ~ year, scales = "free_x")

# RH ---------------------------------------------------------------------------

ggplot(f, aes(x = YYYYMMDD,
              y = rh,
              group = year)) +
  geom_line(colour = "blue") +
  ylab("Relative Humidity (%)") +
  xlab("Date") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  facet_wrap(. ~ year, scales = "free_x")

str(f)
summary(f)

# Statistical tests ------------------------------------------------------------

# check if the weather variables are different between seasons

# avg T
tavg_2015 <- unlist(filter(f, year == "2015")["tavg"])
tavg_2016 <- unlist(filter(f, year == "2016")["tavg"])

bayes.t.test(tavg_2015, tavg_2016)

# RH
rh_2015 <- unlist(filter(f, year == "2015")["rh"])
rh_2016 <- unlist(filter(f, year == "2016")["rh"])

bayes.t.test(rh_2015, rh_2016)

# rainfall
rain_2015 <- unlist(filter(f, year == "2015")["rainfall"])
rain_2016 <- unlist(filter(f, year == "2016")["rainfall"])

bayes.t.test(rain_2015, rain_2016)
