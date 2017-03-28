
# Add plot numbers just to keep track of the data for calculations -------------
DS2015$PLOT <- rep(1:24, 5)
DS2016$PLOT <- rep(1:16, 4)

# Create new columns of dates to calcluate AUDPS -------------------------------
#2015
DATE_1_2015 <- DATE_2_2015 <- DS2015$DATE
DATE_1_2015[which(DATE_1_2015 == min(DATE_1_2015))] = NA
DATE_2_2015[which(DATE_2_2015 == max(DATE_2_2015))] = NA

DATE_1 <- na.omit(DATE_1_2015)
DATE_2 <- na.omit(DATE_2_2015)

DAYS_2015 <- difftime(DATE_1, DATE_2)
DS2015$DAYS <- c(rep(0, 24), DAYS_2015)

# 2016
DATE_1_2016 <- DATE_2_2016 <- DS2016$DATE
DATE_1_2016[which(DATE_1_2016 == min(DATE_1_2016))] = NA
DATE_2_2016[which(DATE_2_2016 == max(DATE_2_2016))] = NA

DATE_1 <- na.omit(DATE_1_2016)
DATE_2 <- na.omit(DATE_2_2016)

DAYS_2016 <- difftime(DATE_1, DATE_2)
DS2016$DAYS <- c(rep(0, 16), DAYS_2016)

# Join 2015 and 2016 AUDPS data to make next few steps quicker -----------------

RAW_data <- AUDPS <- as_tibble(rbind(as.data.frame(DS2015),
                                     as.data.frame(DS2016)))

# Calculate AUDPS (Simco and Piepho 2012, DOI: 10.1094/PHYTO-07-11-0216) -------

mean_sd <-
  AUDPS %>%
  group_by(YEAR, REP, TRT, WMGT, NRTE, PLOT) %>%
  summarise_each(funs(mean),
                 LShB_rating_sd,
                 TShB_rating_sd,
                 TShB_incidence_sd)

cols <- c(7:8)
mean_sd[, cols] <- apply(mean_sd[, cols], 2, function(x)
  round(x, 2))

AUDPS <-
  AUDPS %>%
  group_by(YEAR, REP, TRT, WMGT, NRTE, PLOT) %>%
  summarise_each(funs(audps(., dates = DAYS)),
                 LShB_rating_mean,
                 TShB_rating_mean,
                 TShB_incidence_mean)

AUDPS <- left_join(AUDPS, mean_sd)

AUDPS$YEAR <- as.factor(AUDPS$YEAR)
AUDPS$WMGT <- as.factor(AUDPS$WMGT)
AUDPS$NRTE <- as.factor(AUDPS$NRTE)
AUDPS$TRT <- as.factor(AUDPS$TRT)
AUDPS$REP <- as.factor(AUDPS$REP)

AUDPS <- plyr::rename(AUDPS, c("LShB_rating_mean" = "LShB_AUDPS",
                               "TShB_rating_mean" = "TShB_AUDPS"))

AUDPS$NRTE <- factor(AUDPS$NRTE, levels = c(0, 100, 120, 60, 180))
AUDPS$TRT <- factor(AUDPS$TRT, levels = c("AWD_N0", "AWD_N100", "AWD_N120",
                                          "AWD_N60", "AWD_N180", "FLD_N0",
                                          "FLD_N100", "FLD_N120", "FLD_N60",
                                          "FLD_N180"))

# Split the 2015 and 2016 AUDPS data for easier analysis -----------------------

AUDPS_2015 <- droplevels(AUDPS[AUDPS$YEAR == 2015, ])
AUDPS_2016 <- droplevels(AUDPS[AUDPS$YEAR == 2016, ])
