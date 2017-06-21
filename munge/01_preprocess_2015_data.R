# Preprocess 2015 data for all assessments

files <-
  list.files("data", pattern = "^DS2015_Raw", full.names = TRUE)

reformat <- function(files) {
  x <- read_csv(files)
  x[is.na(x)] <- 0

  x <- arrange(x, REP, TRT)

  # add plot numbers
  x$PLOT <- rep(1:24, each = 18)

  # ensure that all leaf sheath blight observations are numeric ----------------
  x <-
    x %>% mutate_each(funs(as.numeric), starts_with("SL"))

  # calculate tiller sheath blight incidence -----------------------------------
  x <-
    x %>%
    mutate(TShB_incidence = NTShB / NTIL)

  x$TShB_incidence <- round(x$TShB_incidence, 2)

  TShB_incidence <-
    x %>% select(REP,
                        TRT,
                        WMGT,
                        NRTE,
                        SMPL,
                        HILL,
                        HGHT,
                        NTIL,
                        NTShB,
                        TShB_incidence)

  # Tiller 1 -------------------------------------------------------------------
  TIL1 <-
    x %>%
    select(TRT,
                  REP,
                  WMGT,
                  SMPL,
                  NRTE,
                  HILL,
                  TIL1,
                  GL1,
                  DL1,
                  SHB1,
                  SLA1,
                  SLB1,
                  SLC1,
                  SLD1,
                  SLE1,
                  SLF1) %>%
    gather(LEAF,
           LEAF_ShB,
           SLA1:SLF1)

  TIL1$LEAF[TIL1$LEAF == "SLA1"] <- 1
  TIL1$LEAF[TIL1$LEAF == "SLB1"] <- 2
  TIL1$LEAF[TIL1$LEAF == "SLC1"] <- 3
  TIL1$LEAF[TIL1$LEAF == "SLD1"] <- 4
  TIL1$LEAF[TIL1$LEAF == "SLE1"] <- 5
  TIL1$LEAF[TIL1$LEAF == "SLF1"] <- 6

  TIL1 <- rename(TIL1, GL = GL1)
  TIL1 <- rename(TIL1, DL = DL1)
  TIL1 <- rename(TIL1, TIL = TIL1)
  TIL1 <- rename(TIL1, TIL_ShB = SHB1)

  # Tiller 2 -------------------------------------------------------------------

  TIL2 <-
    x %>%
    select(TRT,
                  REP,
                  WMGT,
                  SMPL,
                  NRTE,
                  HILL,
                  TIL2,
                  GL1,
                  DL1,
                  SHB1,
                  SLA1,
                  SLB1,
                  SLC1,
                  SLD1,
                  SLE1,
                  SLF1) %>%
    gather(LEAF,
           LEAF_ShB,
           SLA1:SLF1)

  TIL2$LEAF[TIL2$LEAF == "SLA1"] <- 1
  TIL2$LEAF[TIL2$LEAF == "SLB1"] <- 2
  TIL2$LEAF[TIL2$LEAF == "SLC1"] <- 3
  TIL2$LEAF[TIL2$LEAF == "SLD1"] <- 4
  TIL2$LEAF[TIL2$LEAF == "SLE1"] <- 5
  TIL2$LEAF[TIL2$LEAF == "SLF1"] <- 6

  TIL2 <- rename(TIL2, GL = GL1)
  TIL2 <- rename(TIL2, DL = DL1)
  TIL2 <- rename(TIL2, TIL = TIL2)
  TIL2 <- rename(TIL2, TIL_ShB = SHB1)

  # Tiller 3 -------------------------------------------------------------------

  TIL3 <-
    x %>%
    select(TRT,
                  REP,
                  WMGT,
                  SMPL,
                  NRTE,
                  HILL,
                  TIL3,
                  GL1,
                  DL1,
                  SHB1,
                  SLA1,
                  SLB1,
                  SLC1,
                  SLD1,
                  SLE1,
                  SLF1) %>%
    gather(LEAF,
           LEAF_ShB,
           SLA1:SLF1)

  TIL3$LEAF[TIL3$LEAF == "SLA1"] <- 1
  TIL3$LEAF[TIL3$LEAF == "SLB1"] <- 2
  TIL3$LEAF[TIL3$LEAF == "SLC1"] <- 3
  TIL3$LEAF[TIL3$LEAF == "SLD1"] <- 4
  TIL3$LEAF[TIL3$LEAF == "SLE1"] <- 5
  TIL3$LEAF[TIL3$LEAF == "SLF1"] <- 6

  TIL3 <- rename(TIL3, GL = GL1)
  TIL3 <- rename(TIL3, DL = DL1)
  TIL3 <- rename(TIL3, TIL = TIL3)
  TIL3 <- rename(TIL3, TIL_ShB = SHB1)
  # Tiller 4 -------------------------------------------------------------------

  TIL4 <-
    x %>%
    select(TRT,
                  REP,
                  WMGT,
                  SMPL,
                  NRTE,
                  HILL,
                  TIL4,
                  GL1,
                  DL1,
                  SHB1,
                  SLA1,
                  SLB1,
                  SLC1,
                  SLD1,
                  SLE1,
                  SLF1) %>%
    gather(LEAF,
           LEAF_ShB,
           SLA1:SLF1)

  TIL4$LEAF[TIL4$LEAF == "SLA1"] <- 1
  TIL4$LEAF[TIL4$LEAF == "SLB1"] <- 2
  TIL4$LEAF[TIL4$LEAF == "SLC1"] <- 3
  TIL4$LEAF[TIL4$LEAF == "SLD1"] <- 4
  TIL4$LEAF[TIL4$LEAF == "SLE1"] <- 5
  TIL4$LEAF[TIL4$LEAF == "SLF1"] <- 6

  TIL4 <- rename(TIL4, GL = GL1)
  TIL4 <- rename(TIL4, DL = DL1)
  TIL4 <- rename(TIL4, TIL = TIL4)
  TIL4 <- rename(TIL4, TIL_ShB = SHB1)

  y <- rbind(TIL1, TIL2, TIL3, TIL4)
  x <- left_join(TShB_incidence, y)

  # Add dates ------------------------------------------------------------------

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

# Run reformat function for all 2015 files -------------------------------------
DS2015 <- map_df(.x = files, .f = reformat)

# Replace "N*" with a number for NRTE ------------------------------------------
DS2015$NRTE[which(DS2015$NRTE == "N0")] <- 0
DS2015$NRTE[which(DS2015$NRTE == "N1")] <- 100
DS2015$NRTE[which(DS2015$NRTE == "N2")] <- 120

# Add treatment numbers --------------------------------------------------------
DS2015$TRT <- NA
DS2015$TRT[which(DS2015$WMGT == "FLD" &
                   DS2015$NRTE == 0)] = "FLD_N0"
DS2015$TRT[which(DS2015$WMGT == "FLD" &
                   DS2015$NRTE == 100)] = "FLD_N100"
DS2015$TRT[which(DS2015$WMGT == "FLD" &
                   DS2015$NRTE == 120)] = "FLD_N120"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 0)] = "AWD_N0"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 100)] = "AWD_N100"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 120)] = "AWD_N120"

DS2015$YEAR <- year(DS2015$DATE)

DS2015$PLOT <- rep(1:24, each = 432)

# Arrange columns --------------------------------------------------------------
DS2015 <-
  DS2015 %>% select(
    YEAR,
    DATE,
    ASMT,
    REP,
    PLOT,
    TRT,
    SMPL,
    HILL,
    TIL,
    LEAF,
    GL,
    DL,
    TShB_incidence,
    TIL_ShB,
    LEAF_ShB,
    WMGT,
    NRTE
  )

DS2015 <- as_tibble(arrange(DS2015, ASMT, PLOT))

# Create new columns of dates to calcluate AUDPS -------------------------------
#2015
DATE_1_2015 <- DATE_2_2015 <- DS2015$DATE
DATE_1_2015[which(DATE_1_2015 == min(DATE_1_2015))] = NA
DATE_2_2015[which(DATE_2_2015 == max(DATE_2_2015))] = NA

DATE_1 <- na.omit(DATE_1_2015)
DATE_2 <- na.omit(DATE_2_2015)

DAYS_2015 <- time_length(DATE_1 - DATE_2, unit = "day")
DS2015$DAYS <-
  c(rep(0, times = (nrow(DS2015) - length(DAYS_2015))), DAYS_2015)

rm(files, reformat, DATE_1_2015, DATE_2_2015, DAYS_2015)

# write CSV to cache -----------------------------------------------------------
write_csv(DS2015, "./cache/AWD_2015_Data.csv")
