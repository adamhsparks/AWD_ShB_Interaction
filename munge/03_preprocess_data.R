

# Join the 2015 and 2016 Data into one Tibble ----------------------------------

RAW_data <- as_tibble(rbind(as.data.frame(DS2015),
                            as.data.frame(DS2016)))

# convert columns to factor

RAW_data$YEAR <- as.factor(RAW_data$YEAR)
RAW_data$ASMT <- as.factor(RAW_data$ASMT)
RAW_data$REP <- as.factor(RAW_data$REP)
RAW_data$TRT <- as.factor(RAW_data$TRT)
RAW_data$WMGT <- as.factor(RAW_data$WMGT)
RAW_data$NRTE <- as.factor(RAW_data$NRTE)
RAW_data$SMPL <- as.factor(RAW_data$SMPL)
RAW_data$HILL <- as.factor(RAW_data$HILL)
RAW_data$TIL <- as.factor(RAW_data$TIL)
RAW_data$LEAF <- as.factor(RAW_data$LEAF)

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

