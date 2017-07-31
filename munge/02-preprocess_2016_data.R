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
DS2016_A1$ASMT <- rep(1, nrow(DS2016_A1))
DS2016_A1$PLOT <- rep(1:16, each = 72)


# Second assessement -----------------------------------------------------------
DS2016_A2 <- read_csv("data/DS2016_2.csv")
DS2016_A2 <-
  DS2016_A2 %>% fill(NTIL)
DS2016_A2 <-
  DS2016_A2 %>% fill(NTShB)

# Fill dates
DS2016_A2[, 1] <- as.Date("2016-03-29", origin = "1970-01-01")
DS2016_A2$ASMT <- rep(2, nrow(DS2016_A2))
DS2016_A2$PLOT <- rep(1:16, each = 72)


# Third assessement ------------------------------------------------------------
DS2016_A3 <- read_csv("data/DS2016_3.csv")
DS2016_A3 <-
  DS2016_A3 %>% fill(NTIL)
DS2016_A3 <-
  DS2016_A3 %>% fill(NTShB)

# Fill dates
DS2016_A3[, 1] <- as.Date("2016-04-12", origin = "1970-01-01")
DS2016_A3$ASMT <- rep(3, nrow(DS2016_A3))
DS2016_A3$PLOT <- rep(1:16, each = 72)

# Fourth assessment ------------------------------------------------------------
DS2016_A4 <- read_csv("data/DS2016_4.csv")
DS2016_A4 <-
  DS2016_A4 %>% fill(NTIL)
DS2016_A4 <-
  DS2016_A4 %>% fill(NTShB)

# Fill dates
DS2016_A4[, 1] <- as.Date("2016-04-26", origin = "1970-01-01")
DS2016_A4$ASMT <- rep(4, nrow(DS2016_A4))
DS2016_A4$PLOT <- rep(1:16, each = 72)

# Combine all assessments ------------------------------------------------------
DS2016 <- rbind(
  as.data.frame(DS2016_A1),
  as.data.frame(DS2016_A2),
  as.data.frame(DS2016_A3),
  as.data.frame(DS2016_A4)
)

# Set any NA values to 0, per Dr. Castilla on 060317 ---------------------------
# "We used to put zeroes on the data sheets, but Dr. Savary later suggested that
# we leave the cells blank because it has caused difficulties in proof reading
# the data. We also found more mistakes in data encoding when there are a lot
# of zeroes."
# Set any NA values to 0, per Dr. Castilla on 060317 ---------------------------

DS2016[is.na(DS2016)] <- 0

# Replace "Flooded" with "FLD" -------------------------------------------------
DS2016$WMGT[which(DS2016$WMGT == "Flooded")] = "FLD"

# Replace "N*" with a number for NRTE ------------------------------------------
DS2016$NRTE[which(DS2016$NRTE == "N1")] = 60
DS2016$NRTE[which(DS2016$NRTE == "N2")] = 180

# Add treatment numbers --------------------------------------------------------
DS2016$TRT <- NA
DS2016$TRT[which(DS2016$WMGT == "FLD" &
                   DS2016$NRTE == 60)] = "FLD_N60"
DS2016$TRT[which(DS2016$WMGT == "FLD" &
                   DS2016$NRTE == 180)] = "FLD_N180"
DS2016$TRT[which(DS2016$WMGT == "AWD" &
                   DS2016$NRTE == 60)] = "AWD_N60"
DS2016$TRT[which(DS2016$WMGT == "AWD" &
                   DS2016$NRTE == 180)] = "AWD_N180"

DS2016 <-
  DS2016 %>%
  gather(
    LEAF,
    LEAF_ShB,
    starts_with("ShBL"),
    -DATE,
    -ASMT,
    -PLOT,
    -TRT,
    -REP,
    -WMGT,
    -SMPL,
    -NRTE,
    -HILL,
    -NTIL,
    -GL,
    -DL,
    -NTShB,
    -NSHShB,
    -TIL
  )

# reclassify LEAF column to leaf numbers 1...6

DS2016$LEAF[DS2016$LEAF == "ShBL1"] <- 1
DS2016$LEAF[DS2016$LEAF == "ShBL2"] <- 2
DS2016$LEAF[DS2016$LEAF == "ShBL3"] <- 3
DS2016$LEAF[DS2016$LEAF == "ShBL4"] <- 4
DS2016$LEAF[DS2016$LEAF == "ShBL5"] <- 5
DS2016$LEAF[DS2016$LEAF == "ShBL6"] <- 6

# before classifying incidence, set any values where there are more infected
# tillers in the hill than there are tillers to NA for both columns

DS2016$NTIL[DS2016$NTIL < DS2016$NTShB] <- NA
DS2016$NTShB[is.na(DS2016$NTIL)] <- NA

# calculate tiller sheath blight incidence
DS2016 <-
  DS2016 %>%
  group_by(DATE, ASMT, REP, PLOT, TRT, WMGT, NRTE, SMPL, HILL) %>%
  mutate(TShB_incidence = NTShB / NTIL)

DS2016$TShB_incidence <- round(DS2016$TShB_incidence, 2)

DS2016 <- rename(DS2016, TIL_ShB = NSHShB)

DS2016$YEAR <- year(DS2016$DATE)
DS2016$LEAF_ShB <- as.numeric(DS2016$LEAF_ShB)

DS2016 <-
  DS2016 %>% dplyr::select(
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

DS2016 <- as_tibble(arrange(DS2016, ASMT, PLOT))

# 2016
DATE_1_2016 <- DATE_2_2016 <- DS2016$DATE
DATE_1_2016[which(DATE_1_2016 == min(DATE_1_2016))] <- NA
DATE_2_2016[which(DATE_2_2016 == max(DATE_2_2016))] <- NA

DATE_1 <- na.omit(DATE_1_2016)
DATE_2 <- na.omit(DATE_2_2016)

DAYS_2016 <- time_length(DATE_1 - DATE_2, unit = "day")
DS2016$DAYS <-
  c(rep(0, nrow(DS2016) - length(DAYS_2016)), DAYS_2016)

rm(
  DS2016_A1,
  DS2016_A2,
  DS2016_A3,
  DS2016_A4,
  DATE_1,
  DATE_1_2016,
  DATE_2,
  DATE_2_2016,
  DAYS_2016
)

DS2016 <-
  DS2016 %>% dplyr::select(
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
    NRTE,
    DAYS
  )

# write CSV to cache -----------------------------------------------------------
write_csv(DS2016, "./cache/AWD_2016_Data.csv")

# eos
