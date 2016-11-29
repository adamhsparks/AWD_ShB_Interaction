# Preprocess 2016 data for all assessments
#
# 1. Cleanup date format and fill empty dates
# 2. Fill empty NTIL (for same hill, not missing values)
# 3. Fill empty NTShB (for same hill, not missing values)

library(tidyr)
library(readr)
library(dplyr)

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
DS2016 <- rbind(as.data.frame(DS2016_A1),
                as.data.frame(DS2016_A2),
                as.data.frame(DS2016_A3),
                as.data.frame(DS2016_A4))

# write CSV to cache -----------------------------------------------------------
write_csv(DS2016, "./cache/AWD_2016_Data.csv")

# eos

