library(magrittr)
library(BayesianFirstAid)
`%notin%` = function(x,y) !(x %in% y)

l <- list.files(path = "~/Downloads/Fwd__Weather_data",
                full.names = TRUE)
f <- purrr::map(l, readxl::read_excel,
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
                na = c("*", "")) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(YYYYMMDD = paste(year, month, day, sep = "-")) %>%
  dplyr::mutate(YYYYMMDD = as.Date(YYYYMMDD, format = "%Y-%b-%d")) %>%
  dplyr::filter(month %in% c("Jan", "Feb", "Mar", "Apr")) %>%
  dplyr::filter(year != "2014")

# Rainfall ---------------------------------------------------------------------

ggplot2::ggplot(f, ggplot2::aes(x = YYYYMMDD,
                                y = rainfall,
                                group = year)) +
  ggplot2::geom_line(colour = "blue") +
  ggplot2::ylab("Precipitation (mm)") +
  ggplot2::xlab("Date") +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(colour = "black"),
                 strip.background = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_blank()) +
ggplot2::facet_wrap(. ~ year, scales = "free_x")

# Temperature ------------------------------------------------------------------

ggplot2::ggplot(f, ggplot2::aes(x = YYYYMMDD,
                                y = tavg,
                                group = year)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(y = tmin), col = "blue") +
  ggplot2::geom_line(ggplot2::aes(y = tmax), col = "red") +
  ggplot2::ylab("Temperature (˚C)") +
  ggplot2::xlab("Date") +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(colour = "black"),
                 strip.background = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_blank()) +
  ggplot2::facet_wrap(. ~ year, scales = "free_x")

# RH ---------------------------------------------------------------------------

ggplot2::ggplot(f, ggplot2::aes(x = YYYYMMDD,
                                y = rh,
                                group = year)) +
  ggplot2::geom_line(colour = "blue") +
  ggplot2::ylab("Relative Humidity (%)") +
  ggplot2::xlab("Date") +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(colour = "black"),
                 strip.background = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_blank()) +
  ggplot2::facet_wrap(. ~ year, scales = "free_x")

str(f)
summary(f)

# Statistical tests ------------------------------------------------------------

# check if the weather variables are different between seasons

# avg T
tavg_2015 <- unlist(dplyr::filter(f, year == "2015")["tavg"])
tavg_2016 <- unlist(dplyr::filter(f, year == "2016")["tavg"])

bayes.t.test(tavg_2015, tavg_2016)

# RH
rh_2015 <- unlist(dplyr::filter(f, year == "2015")["rh"])
rh_2016 <- unlist(dplyr::filter(f, year == "2016")["rh"])

bayes.t.test(rh_2015, rh_2016)

# rainfall
rain_2015 <- unlist(dplyr::filter(f, year == "2015")["rainfall"])
rain_2016 <- unlist(dplyr::filter(f, year == "2016")["rainfall"])

bayes.t.test(rain_2015, rain_2016)
