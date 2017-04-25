# Join the 2015 and 2016 Data into one Tibble ----------------------------------

RAW_data <- as_tibble(rbind(as.data.frame(DS2015),
                            as.data.frame(DS2016)))

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
