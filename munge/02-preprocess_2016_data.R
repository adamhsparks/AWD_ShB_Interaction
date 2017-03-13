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

DS2016_A1 <- dplyr::select(DS2016_A1, DATE, everything())

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

DS2016_A2 <- dplyr::select(DS2016_A2, DATE, everything())


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

DS2016_A3 <- dplyr::select(DS2016_A3, DATE, everything())

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

DS2016_A4 <- dplyr::select(DS2016_A4, DATE, everything())

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

# Add treatment numbers --------------------------------------------------------
DS2016$TRT <- NA
DS2016$TRT[which(DS2016$WMGT == "FLD" & DS2016$NRTE == "N1")] = "FLD_N1"
DS2016$TRT[which(DS2016$WMGT == "FLD" & DS2016$NRTE == "N2")] = "FLD_N2"
DS2016$TRT[which(DS2016$WMGT == "AWD" & DS2016$NRTE == "N1")] = "AWD_N1"
DS2016$TRT[which(DS2016$WMGT == "AWD" & DS2016$NRTE == "N2")] = "AWD_N2"

# Rename columns for consistency with 2015 -------------------------------------
DS2016 <- dplyr::rename(DS2016, TShB_rating = NSHShB)
DS2016 <- dplyr::rename(DS2016, SLA = ShBL1)
DS2016 <- dplyr::rename(DS2016, SLB = ShBL2)
DS2016 <- dplyr::rename(DS2016, SLC = ShBL3)
DS2016 <- dplyr::rename(DS2016, SLD = ShBL4)
DS2016 <- dplyr::rename(DS2016, SLE = ShBL5)
DS2016 <- dplyr::rename(DS2016, SLF = ShBL6)

# convert columns 17:19 to numeric from character and change NA to 0 -----------
cols <- c(17:19)
DS2016[, cols] = apply(DS2016[, cols], 2, function(x)
  as.numeric(as.character(x)))

DS2016[is.na(DS2016)] <- 0

DS2016 <-
  DS2016 %>%
  group_by(DATE, ASMT, REP, TRT, WMGT, NRTE, SMPL, HILL) %>%
  gather(LShB, LShB_rating, starts_with("SL")) %>%
  gather(GL, GL_value, starts_with("GL")) %>%
  gather(DL, DL_value, starts_with("DL")) %>%
  summarise_each(funs(mean),
                 NTIL,
                 NTShB,
                 LShB_rating,
                 TShB_rating,
                 GL_value,
                 DL_value)

DS2016 <-
  DS2016 %>%
  group_by(DATE, ASMT, REP, TRT, WMGT, NRTE) %>%
  summarise_each(funs(mean),
                 NTIL,
                 NTShB,
                 LShB_rating,
                 TShB_rating,
                 GL_value,
                 DL_value)

# arrange columns to be in same order in both data frames
DS2016 <-
  DS2016 %>% dplyr::select(DATE,
                           ASMT,
                           WMGT,
                           NRTE,
                           REP,
                           TRT,
                           NTIL,
                           NTShB,
                           LShB_rating,
                           TShB_rating,
                           GL_value,
                           DL_value)

cols <- c(7:12)
DS2016[, cols] = apply(DS2016[, cols], 2, function(x)
  round(x, 2))

DS2016$YEAR <- year(DS2016$DATE)

# write CSV to cache -----------------------------------------------------------
write_csv(DS2016, "./cache/AWD_2016_Data.csv")

# eos
