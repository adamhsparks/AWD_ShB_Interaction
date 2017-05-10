



# Join the 2015 and 2016 Data into one Tibble ----------------------------------

RAW_data <- as_tibble(rbind(as.data.frame(DS2015),
                            as.data.frame(DS2016)))

# convert columns to factor ----------------------------------------------------
RAW_data$YEAR <- factor(RAW_data$YEAR)
RAW_data$ASMT <- factor(RAW_data$ASMT)
RAW_data$PLOT <- factor(RAW_data$PLOT)
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



# calculate AUDPS values -------------------------------------------------------

# 2015 Tiller Incidence AUDPS --------------------------------------------------
TShB_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(mean), PLOT_TShB_incidence = TShB_incidence) %>%
  arrange(PLOT)

TShB_wide <-
  dcast(TShB_15, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

AUDPS <-
  audps(evaluation = TShB_wide[, 2:6], dates = as_vector(TShB_15[1:5, 6]))

AUDPS_15 <-
  as_tibble(cbind(PLOT = 1:24,
                  AUDPS = round(AUDPS, 2)))

AUDPS_15$PLOT <- as.factor(as.character(AUDPS_15$PLOT))

TShB_15 <- left_join(TShB_15, AUDPS_15, by = "PLOT")

# 2016 Tiller Incidence AUDPS --------------------------------------------------

TShB_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(mean), PLOT_TShB_incidence = TShB_incidence) %>%
  arrange(PLOT)

TShB_wide <-
  dcast(TShB_16, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

AUDPS <-
  audps(evaluation = TShB_wide[, 2:5], dates = as_vector(TShB_16[1:4, 6]))

AUDPS_16 <-
  as_tibble(cbind(PLOT = 1:16,
                  AUDPS = round(AUDPS, 2)))

AUDPS_16$PLOT <- as.factor(as.character(AUDPS_16$PLOT))

TShB_16 <- left_join(TShB_16, AUDPS_16, by = "PLOT")

# Merge Tiller Incidence AUDPS data --------------------------------------------

AUDPS <- rbind(TShB_15, TShB_16)
