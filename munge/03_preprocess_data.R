
# Join the 2015 and 2016 Data into one Tibble ----------------------------------

RAW_data <- as_tibble(rbind(as.data.frame(DS2015),
                            as.data.frame(DS2016)))

# remove improper ratings for Tiller and Leaf Sheath Blight --------------------
# there are incidences of leaf sheath blight and tiller sheath blight where they
# are incorrectly entered as 5, the scale is 0-4 only
RAW_data$LEAF_ShB[RAW_data$LEAF_ShB == 5] <- NA
RAW_data$TIL_ShB[RAW_data$TIL_ShB == 5] <- NA

# convert columns to factor ----------------------------------------------------
RAW_data$YEAR <- factor(RAW_data$YEAR)
RAW_data$ASMT <- factor(RAW_data$ASMT)
RAW_data$REP <- factor(RAW_data$REP)
RAW_data$TRT <- factor(RAW_data$TRT)
RAW_data$WMGT <- factor(RAW_data$WMGT)
RAW_data$NRTE <- factor(RAW_data$NRTE)
RAW_data$SMPL <- factor(RAW_data$SMPL)
RAW_data$HILL <- factor(RAW_data$HILL)
RAW_data$TIL <- factor(RAW_data$TIL)
RAW_data$LEAF <- factor(RAW_data$LEAF)
RAW_data$TIL_ShB <- factor(RAW_data$TIL_ShB)
RAW_data$LEAF_ShB <- factor(RAW_data$LEAF_ShB)


# reorder the treatments by level/year - low to high, 2015-2016
RAW_data$NRTE <-
  forcats::fct_relevel(RAW_data$NRTE, "0", "100", "120", "60", "180")

RAW_data$TRT <-
  forcats::fct_relevel(
    RAW_data$TRT,
    "AWD_N0",
    "AWD_N100",
    "AWD_N120",
    "AWD_N60",
    "AWD_N180",
    "FLD_N0",
    "FLD_N100",
    "FLD_N120",
    "FLD_N60",
    "FLD_N180"
  )

DS2015 <- subset(RAW_data, YEAR == "2015")
DS2016 <- subset(RAW_data, YEAR == "2016")

# calculate tiller sheath blight incidence AUDPDS ------------------------------
AUDPS <-
  RAW_data %>%
  group_by(YEAR, DATE, REP, TRT) %>%
  summarise_each(funs(mean), PLOT_TShB_incidence = TShB_incidence) %>%
  arrange(YEAR, REP, TRT, DATE)

AUDPS <- ddply(AUDPS,
               c("YEAR", "REP", "TRT"),
               summarise,
               AUDPS = audps(PLOT_TShB_incidence, dates = DATE))
AUDPS$AUDPS <- as.numeric(AUDPS$AUDPS)
