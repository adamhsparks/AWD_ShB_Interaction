# Preprocess 2016 data for all assessments
#
# 1. Cleanup date format and fill empty dates
# 2. Fill empty NTIL (for same hill, not missing values)
# 3. Fill empty NTShB (for same hill, not missing values)

# First assessment -------------------------------------------------------------
DS2016_A1 <- read_csv("data/DS2016_1.csv")
# Fill missing values in NTIL and NTShB
DS2016_A1 <-
  DS2016_A1 %>% fill(NTIL)
DS2016_A1 <-
  DS2016_A1 %>% fill(NTShB)

# Fill dates
DS2016_A1[, 1] <- as.Date("2016-03-15", origin = "1970-01-01")
ASMT <- rep(1, nrow(DS2016_A1))
DS2016_A1 <- cbind(ASMT, DS2016_A1)

DS2016_A1 <- select(DS2016_A1, DATE, everything())

# Second assessement -----------------------------------------------------------
DS2016_A2 <- read_csv("data/DS2016_2.csv")
DS2016_A2 <-
  DS2016_A2 %>% fill(NTIL)
DS2016_A2 <-
  DS2016_A2 %>% fill(NTShB)

# Fill dates
DS2016_A2[, 1] <- as.Date("2016-03-29", origin = "1970-01-01")
ASMT <- rep(2, nrow(DS2016_A2))
DS2016_A2 <- cbind(ASMT, DS2016_A2)

DS2016_A2 <- select(DS2016_A2, DATE, everything())


# Third assessement ------------------------------------------------------------
DS2016_A3 <- read_csv("data/DS2016_3.csv")
DS2016_A3 <-
  DS2016_A3 %>% fill(NTIL)
DS2016_A3 <-
  DS2016_A3 %>% fill(NTShB)

# Fill dates
DS2016_A3[, 1] <- as.Date("2016-04-12", origin = "1970-01-01")
ASMT <- rep(3, nrow(DS2016_A3))
DS2016_A3 <- cbind(ASMT, DS2016_A3)

DS2016_A3 <- select(DS2016_A3, DATE, everything())

# Fourth assessment ------------------------------------------------------------
DS2016_A4 <- read_csv("data/DS2016_4.csv")
DS2016_A4 <-
  DS2016_A4 %>% fill(NTIL)
DS2016_A4 <-
  DS2016_A4 %>% fill(NTShB)

# Fill dates
DS2016_A4[, 1] <- as.Date("2016-04-26", origin = "1970-01-01")
ASMT <- rep(4, nrow(DS2016_A4))
DS2016_A4 <- cbind(ASMT, DS2016_A4)

DS2016_A4 <- select(DS2016_A4, DATE, everything())

# Combine all assessments ------------------------------------------------------
DS2016 <- rbind(
  as.data.frame(DS2016_A1),
  as.data.frame(DS2016_A2),
  as.data.frame(DS2016_A3),
  as.data.frame(DS2016_A4)
)


if (DS2016$WMGT == "FLD" && DS2016$NRTE == "N1") {
  DS2016$TRT <- "T2"
}

if (DS2016$WMGT == "FLD" && DS2016$NRTE == "N2") {
  DS2016$TRT <- "T3"
}


if (DS2016$WMGT == "AWD" && DS2016$NRTE == "N1") {
  DS2016$TRT <- "T5"
}

if (DS2016$WMGT == "AWD" && DS2016$NRTE == "N2") {
  DS2016$TRT <- "T6"
}

DS2016 <- rename(DS2016, NSHB = NSHShB)
DS2016 <- rename(DS2016, SLA = ShBL1)
DS2016 <- rename(DS2016, SLB = ShBL2)
DS2016 <- rename(DS2016, SLC = ShBL3)
DS2016 <- rename(DS2016, SLD = ShBL4)
DS2016 <- rename(DS2016, SLE = ShBL5)
DS2016 <- rename(DS2016, SLF = ShBL6)

DS2016[, 17:19] <- as.numeric(DS2016[, 17:19])

DS2016[is.na(DS2016)] <- 0
DS2016$HGHT <- NA
DS2016$SHB <- NA

DS2016 <-
  DS2016 %>%
  group_by(TRT, REP) %>%
  gather(LShB, LShB_rating, starts_with("SL")) %>%
  gather(TShB, TShB_rating, starts_with("SHB")) %>%
  gather(GL, GL_value, starts_with("GL")) %>%
  gather(DL, DL_value, starts_with("DL")) %>%
  summarise_each(funs(mean),
                 HGHT,
                 NTIL,
                 NTShB,
                 LShB_rating,
                 TShB_rating,
                 GL_value,
                 DL_value)

DS2016 <-
  DS2016 %>% select(DATE,
                    ASMT,
                    REP,
                    TRT,
                    SMPL,
                    HGHT,
                    NTIL,
                    NTShB,
                    everything())

DS2016 <- rename(DS2016, GL_value = GL)
DS2016 <- rename(DS2016, DL_value = DL)

# write CSV to cache -----------------------------------------------------------
write_csv(DS2016, "./cache/AWD_2016_Data.csv")

# eos
