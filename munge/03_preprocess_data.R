
# Add plot and observation numbers ---------------------------------------------
DS2015$PLOT <- as.factor(rep(1:24, 4))
DS2016$PLOT <- as.factor(rep(1:16, 4))

# Join the 2015 and 2016 Data into one Tibble ----------------------------------

RAW_data <- as_tibble(rbind(as.data.frame(DS2015),
                            as.data.frame(DS2016)))

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

# 2016
DATE_1_2016 <- DATE_2_2016 <- DS2016$DATE
DATE_1_2016[which(DATE_1_2016 == min(DATE_1_2016))] = NA
DATE_2_2016[which(DATE_2_2016 == max(DATE_2_2016))] = NA

DATE_1 <- na.omit(DATE_1_2016)
DATE_2 <- na.omit(DATE_2_2016)

DAYS_2016 <- time_length(DATE_1 - DATE_2, unit = "day")
DS2016$DAYS <-
  c(rep(0, nrow(DS2016) - length(DAYS_2016)), DAYS_2016)

# calculate AUDPS values -------------------------------------------------------

TShB_15 <-
  DS2015 %>%
  group_by(DATE, REP, PLOT, TRT, WMGT, NRTE, SMPL, HILL, TIL, DAYS) %>%
  summarise_each(funs(mean), PLOT_TShB_incidence = TShB_incidence) %>%
  arrange(REP, TRT, SMPL, HILL, TIL, DATE)

AUDPS <- TShB_15[, c(1, 9:11)]
reshape2::dcast(AUDPS, DATE + DAYS ~ TIL + PLOT_TShB_incidence)

(TShB_15, DAYS, value = c(DATE, REP, TRT, WMGT, NRTE, SMPL, HILL, TIL, PLOT_TShB_incidence))

TShB_15 <-
  TShB_15 %>%
  group_by(DATE, REP, TRT, WMGT, NRTE, SMPL, HILL, TIL) %>%
  mutate(PLOT_TShB_auc = audps(evaluation = PLOT_TShB_incidence,
                               dates = DAYS))

TShB_AUDPS <-
  mutate(TShB_15, TShB_AUC_inc = audps(PLOT_TShB_incidence, dates = DAYS))
