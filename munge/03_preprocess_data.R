
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

# 2015 AUDPS -------------------------------------------------------------------
TShB_inc_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE), 2)),
                 PLOT_TShB_incidence = TShB_incidence) %>%
  arrange(PLOT)

TShB_inc_wide <-
  dcast(TShB_inc_15, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

TShB_sev_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE), 2)),
                 PLOT_TShB_severity = TIL_ShB) %>%
  arrange(PLOT)

TShB_sev_wide <-
  dcast(TShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

LShB_sev_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE), 2)),
                 PLOT_LShB_severity = LEAF_ShB) %>%
  arrange(PLOT)

LShB_sev_wide <-
  dcast(LShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

TShB_inc_AUDPS <-
  audps(evaluation = TShB_inc_wide[, 2:6], dates = as_vector(TShB_inc_15[1:5, 6]))

TShB_sev_AUDPS <-
  audps(evaluation = TShB_sev_wide[, 2:6], dates = as_vector(TShB_sev_15[1:5, 6]))

LShB_sev_AUDPS <-
  audps(evaluation = LShB_sev_wide[, 2:6], dates = as_vector(LShB_sev_15[1:5, 6]))

AUDPS_15 <-
  as_tibble(
    cbind(
      PLOT = 1:24,
      TShB_inc_AUDPS,
      TShB_sev_AUDPS,
      LShB_sev_AUDPS
    )
  )

AUDPS_15$PLOT <- as.character(AUDPS_15$PLOT)
TShB_inc_15$PLOT <- as.character(TShB_inc_15$PLOT)

ShB_15 <- left_join(TShB_inc_15, AUDPS_15, by = "PLOT")

# 2016 AUDPS -------------------------------------------------------------------

TShB_inc_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE), 2)),
                 PLOT_TShB_incidence = TShB_incidence) %>%
  arrange(PLOT)

TShB_inc_wide <-
  dcast(TShB_inc_16, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

TShB_inc_AUDPS <-
  audps(evaluation = TShB_inc_wide[, 2:5], dates = as_vector(TShB_inc_16[1:4, 6]))

TShB_sev_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE), 2)),
                 PLOT_TShB_severity = TIL_ShB) %>%
  arrange(PLOT)

TShB_sev_wide <-
  dcast(TShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

TShB_sev_AUDPS <-
  audps(evaluation = TShB_sev_wide[, 2:5], dates = as_vector(TShB_sev_16[1:4, 6]))

LShB_sev_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_each(funs(round(mean(., na.rm = TRUE), 2)),
                 PLOT_LShB_severity = LEAF_ShB) %>%
  arrange(PLOT)

LShB_sev_wide <-
  dcast(LShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_sev_AUDPS <-
  audps(evaluation = LShB_sev_wide[, 2:5], dates = as_vector(LShB_sev_16[1:4, 6]))

AUDPS_16 <-
  as_tibble(
    cbind(
      PLOT = 1:16,
      TShB_inc_AUDPS,
      TShB_sev_AUDPS,
      LShB_sev_AUDPS
    )
  )

# add plot numbers to merge with the remaing treatment data
AUDPS_16$PLOT <- as.character(AUDPS_16$PLOT)
TShB_inc_16$PLOT <- as.character(TShB_inc_16$PLOT)

ShB_16 <- left_join(TShB_inc_16, AUDPS_16, by = c("PLOT" = "PLOT"))

# Merge AUDPS data for graphing ------------------------------------------------
AUDPS <- as_data_frame(as_tibble(rbind(ShB_15, ShB_16)))
