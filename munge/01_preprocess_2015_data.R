# Preprocess 2015 data for all assessments
#

files <-
  list.files("data", pattern = "^DS2015_Raw", full.names = TRUE)

reformat <- function(files) {
  x <- read_csv(files)
  x[is.na(x)] <- 0

  # ensure that all leaf sheath blight observations are numeric, not character
  x <-
    x %>% mutate_each(funs(as.numeric), starts_with("SL"))

  x <-
    x %>%
    group_by(TRT, REP, SMPL) %>%
    gather(LShB, LShB_rating, starts_with("SL")) %>%
    gather(TShB, TShB_rating, starts_with("SHB")) %>%
    gather(GL, GL_value, starts_with("GL")) %>%
    gather(DL, DL_value, starts_with("DL")) %>%
    summarise_each(funs(mean),
                   NTIL,
                   NTShB,
                   LShB_rating,
                   TShB_rating,
                   GL_value,
                   DL_value)

  x[, 3:8] <- round(x[, 3:8], 2)

  if (files == "data/DS2015_Raw_22DAI.csv") {
    DATE <- rep(as.Date("2015-02-12", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(1, times = nrow(x))
    visit <- data.frame(DATE, ASMT)
  } else if (files == "data/DS2015_Raw_35DAI.csv") {
    DATE <- rep(as.Date("2015-02-20", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(2, times = nrow(x))
    visit <- data.frame(DATE, ASMT)
  } else if (files == "data/DS2015_Raw_49DAI.csv") {
    DATE <- rep(as.Date("2015-03-05", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(3, times = nrow(x))
    visit <- data.frame(DATE, ASMT)
  } else if (files == "data/DS2015_Raw_62DAI.csv") {
    DATE <- rep(as.Date("2015-03-19", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(4, times = nrow(x))
    visit <- data.frame(DATE, ASMT)
  } else if (files == "data/DS2015_Raw_83DAI.csv") {
    DATE <- rep(as.Date("2015-04-01", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(5, times = nrow(x))
    visit <- data.frame(DATE, ASMT)
  }
}

DS2015 <- ldply(.data = files, .fun = reformat)

DS2015 <-
  DS2015 %>% select(DATE,
                    ASMT,
                    REP,
                    TRT,
                    WMGT,
                    NRTE,
                    SMPL,
                    HILL,
                    HGHT,
                    NTIL,
                    NTShB,
                    NSHB,
                    everything())

# write CSV to cache -----------------------------------------------------------
write_csv(DS2015, "./cache/AWD_2015_Data.csv")
