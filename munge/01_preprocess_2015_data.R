# Preprocess 2015 data for all assessments

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
    group_by(REP, WMGT, NRTE, HILL, SMPL) %>%
    gather(LShB_A, LShB_rating_A, starts_with("SLA")) %>%
    gather(LShB_B, LShB_rating_B, starts_with("SLB")) %>%
    gather(LShB_C, LShB_rating_C, starts_with("SLC")) %>%
    gather(LShB_D, LShB_rating_D, starts_with("SLD")) %>%
    gather(LShB_E, LShB_rating_E, starts_with("SLE")) %>%
    gather(LShB_F, LShB_rating_F, starts_with("SLF")) %>%
    gather(TShB, TShB_rating, starts_with("SHB")) %>%
    gather(GL, GL_value, starts_with("GL")) %>%
    gather(DL, DL_value, starts_with("DL")) %>%
    summarise_each(funs(mean),
                   NTIL,
                   NTShB,
                   LShB_rating_A,
                   LShB_rating_B,
                   LShB_rating_C,
                   LShB_rating_D,
                   LShB_rating_E,
                   LShB_rating_F,
                   TShB_rating,
                   GL_value,
                   DL_value)
  x <-
    x %>%
    group_by(REP, WMGT, NRTE) %>%
    summarise_each(funs(mean),
                   NTIL,
                   NTShB,
                   LShB_rating_A,
                   LShB_rating_B,
                   LShB_rating_C,
                   LShB_rating_D,
                   LShB_rating_E,
                   LShB_rating_F,
                   TShB_rating,
                   GL_value,
                   DL_value)

  x[, 4:14] <- round(x[, 4:14], 2)

  if (files == "data/DS2015_Raw_22DAI.csv") {
    DATE <- rep(as.Date("2015-02-12", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(1, times = nrow(x))
    visit <- data.frame(DATE, ASMT, x)
  } else if (files == "data/DS2015_Raw_35DAI.csv") {
    DATE <- rep(as.Date("2015-02-20", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(2, times = nrow(x))
    visit <- data.frame(DATE, ASMT, x)
  } else if (files == "data/DS2015_Raw_49DAI.csv") {
    DATE <- rep(as.Date("2015-03-05", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(3, times = nrow(x))
    visit <- data.frame(DATE, ASMT, x)
  } else if (files == "data/DS2015_Raw_62DAI.csv") {
    DATE <- rep(as.Date("2015-03-19", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(4, times = nrow(x))
    visit <- data.frame(DATE, ASMT, x)
  } else if (files == "data/DS2015_Raw_83DAI.csv") {
    DATE <- rep(as.Date("2015-04-01", origin = "1970-01-01"),
                times = nrow(x))
    ASMT <- rep(5, times = nrow(x))
    visit <- data.frame(DATE, ASMT, x)
  }
}

DS2015 <- ldply(.data = files, .fun = reformat)

DS2015$TRT <- NA
DS2015$TRT[which(DS2015$WMGT == "FLD" & DS2015$NRTE == "N0")] = "FLD_N0"
DS2015$TRT[which(DS2015$WMGT == "FLD" & DS2015$NRTE == "N1")] = "FLD_N1"
DS2015$TRT[which(DS2015$WMGT == "FLD" & DS2015$NRTE == "N2")] = "FLD_N2"
DS2015$TRT[which(DS2015$WMGT == "AWD" & DS2015$NRTE == "N0")] = "AWD_N0"
DS2015$TRT[which(DS2015$WMGT == "AWD" & DS2015$NRTE == "N1")] = "AWD_N1"
DS2015$TRT[which(DS2015$WMGT == "AWD" & DS2015$NRTE == "N2")] = "AWD_N2"

DS2015 <-
  as_tibble(DS2015) %>% dplyr::select(DATE,
                                      ASMT,
                                      WMGT,
                                      NRTE,
                                      REP,
                                      TRT,
                                      NTIL,
                                      NTShB,
                                      LShB_rating_A,
                                      LShB_rating_B,
                                      LShB_rating_C,
                                      LShB_rating_D,
                                      LShB_rating_E,
                                      LShB_rating_F,
                                      TShB_rating,
                                      GL_value,
                                      DL_value)

DS2015$YEAR <- year(DS2015$DATE)


# write CSV to cache -----------------------------------------------------------
write_csv(DS2015, "./cache/AWD_2015_Data.csv")
