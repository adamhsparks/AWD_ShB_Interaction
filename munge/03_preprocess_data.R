# Add plot numbers just to keep track of the data for calculations -------------
DS2015$PLOT <- rep(1:24, 2160)
DS2016$PLOT <- rep(1:16, 1728)

# calculate tiller sheath blight incidence AUDPDS ------------------------------

#2015
DATE_1_2015 <- DATE_2_2015 <- DS2015$DATE

# set the first and last dates to NA
DATE_1_2015[which(DATE_1_2015 == min(DATE_1_2015))] = NA
DATE_2_2015[which(DATE_2_2015 == max(DATE_2_2015))] = NA

# remove NA values
DATE_1_2015 <- DATE_1_2015[!is.na(DATE_1_2015)]
DATE_2_2015 <- DATE_2_2015[!is.na(DATE_2_2015)]

# using lubridate::difftime calculate the time between observations
DAYS_2015 <- difftime(DATE_1_2015, DATE_2_2015)

# Add DAYS column to DS2015
DS2015$DAYS <-
  c(rep(0, nrow(DS2015) - length(DAYS_2015)), DAYS_2015)

# 2016
DATE_1_2016 <- DATE_2_2016 <- DS2016$DATE
DATE_1_2016[which(DATE_1_2016 == min(DATE_1_2016))] = NA
DATE_2_2016[which(DATE_2_2016 == max(DATE_2_2016))] = NA

# remove NA values
DATE_1_2016 <- DATE_1_2016[!is.na(DATE_1_2016)]
DATE_2_2016 <- DATE_2_2016[!is.na(DATE_2_2016)]

# using lubridate::difftime calculate the time between observations
DAYS_2016 <- difftime(DATE_1_2016, DATE_2_2016)

# Add DAYS column to DS2015
DS2016$DAYS <-
  c(rep(0, nrow(DS2016) - length(DAYS_2016)), DAYS_2016)

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

# calculate AUDPS --------------------------------------------------------------
AUDPS <-
  RAW_data %>%
  group_by(YEAR, REP, TRT, WMGT, NRTE, PLOT) %>%
  summarise_each(funs(audps(., dates = DAYS)),
                 TShB_incidence)
