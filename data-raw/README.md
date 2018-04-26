Clean 2015 Raw Data
================

The raw data are located in the `data` folder of the installed
*rice.awd.pests*

# 2015 experiment

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Preprocess 2015 data for all assessment dates

files <-
  list.files("data", pattern = "^DS2015_Raw", full.names = TRUE)

reformat <- function(files) {
  x <- read_csv(
    files,
    cols(
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_double(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer()
    ),
    col_names = TRUE
  )
  x[is.na(x)] <- 0
  
  x <- dplyr::arrange(x, REP, TRT)
  
  # add plot numbers
  x$PLOT <- rep(1:24, each = 18)
  
  # Calculate tiller sheath blight incidence -----------------------------------
  x <-
    x %>%
    dplyr::mutate(TShB_incidence = NTShB / NTIL)
  
  TShB_incidence <-
    x %>% dplyr::select(REP,
                        TRT,
                        WMGT,
                        NRTE,
                        SMPL,
                        HILL,
                        HGHT,
                        NTIL,
                        NTShB,
                        TShB_incidence)
  
  # Gather tillers--------------------------------------------------------------
  TIL <- vector(mode = "list", length = 4)
  
  for (i in 1:4) {
    y <-
      x %>%
      dplyr::select(
        TRT,
        REP,
        WMGT,
        SMPL,
        NRTE,
        HILL,
        paste0("TIL", i),
        paste0("GL", i),
        paste0("DL", i),
        paste0("SHB", i),
        paste0("SLA", i),
        paste0("SLB", i),
        paste0("SLC", i),
        paste0("SLD", i),
        paste0("SLE", i),
        paste0("SLF", i)
      ) %>%
      tidyr::gather(LEAF,
                    LEAF_ShB,
                    dplyr::starts_with("SL"))
    
    names(y)[7] <- "TIL"
    names(y)[8] <- "GL"
    names(y)[9] <- "DL"
    names(y)[10] <- "TIL_ShB"
    
    y$LEAF[y$LEAF == paste0("SLA", i)] <- 1
    y$LEAF[y$LEAF == paste0("SLB", i)] <- 2
    y$LEAF[y$LEAF == paste0("SLC", i)] <- 3
    y$LEAF[y$LEAF == paste0("SLD", i)] <- 4
    y$LEAF[y$LEAF == paste0("SLE", i)] <- 5
    y$LEAF[y$LEAF == paste0("SLF", i)] <- 6
    
    TIL[[i]] <- y
  }
  
  z <- dplyr::bind_rows(TIL)
  x <- dplyr::left_join(TShB_incidence, z)
  
  # Add dates ------------------------------------------------------------------
  
  switch(
    files,
    "data/DS2015_Raw_22DAI.csv" = {
      DATE <- rep(as.Date("2015-02-12", origin = "1970-01-01"),
                  times = nrow(x))
      ASMT <- rep(1, times = nrow(x))
      visit <- data.frame(DATE, ASMT, x)
    },
    "data/DS2015_Raw_35DAI.csv" = {
      DATE <- rep(as.Date("2015-02-20", origin = "1970-01-01"),
                  times = nrow(x))
      ASMT <- rep(2, times = nrow(x))
      visit <- data.frame(DATE, ASMT, x)
    },
    "data/DS2015_Raw_49DAI.csv" = {
      DATE <- rep(as.Date("2015-03-05", origin = "1970-01-01"),
                  times = nrow(x))
      ASMT <- rep(3, times = nrow(x))
      visit <- data.frame(DATE, ASMT, x)
    },
    "data/DS2015_Raw_62DAI.csv" = {
      DATE <- rep(as.Date("2015-03-19", origin = "1970-01-01"),
                  times = nrow(x))
      ASMT <- rep(4, times = nrow(x))
      visit <- data.frame(DATE, ASMT, x)
    },
    "data/DS2015_Raw_83DAI.csv" = {
      DATE <- rep(as.Date("2015-04-01", origin = "1970-01-01"),
                  times = nrow(x))
      ASMT <- rep(5, times = nrow(x))
      visit <- data.frame(DATE, ASMT, x)
    }
  )
}

# Run reformat function for all 2015 files -------------------------------------
DS2015 <- purrr::map_df(files, reformat)
```

    ## Joining, by = c("REP", "TRT", "WMGT", "NRTE", "SMPL", "HILL")

    ## Joining, by = c("REP", "TRT", "WMGT", "NRTE", "SMPL", "HILL")
    ## Joining, by = c("REP", "TRT", "WMGT", "NRTE", "SMPL", "HILL")
    ## Joining, by = c("REP", "TRT", "WMGT", "NRTE", "SMPL", "HILL")
    ## Joining, by = c("REP", "TRT", "WMGT", "NRTE", "SMPL", "HILL")

``` r
# Add days after inoculation column
DS2015 <- 
  DS2015 %>%
  mutate(
    DAI = case_when(
      ASMT == 1 ~ 14,
      ASMT == 2 ~ 22,
      ASMT == 3 ~ 35,
      ASMT == 4 ~ 49,
      ASMT == 5 ~ 62,
    )
  )

# Replace "N*" with a number for NRTE ------------------------------------------
DS2015$NRTE[which(DS2015$NRTE == "N0")] <- 0
DS2015$NRTE[which(DS2015$NRTE == "N1")] <- 100
DS2015$NRTE[which(DS2015$NRTE == "N2")] <- 120

# Add treatment numbers --------------------------------------------------------
DS2015$TRT <- NA
DS2015$TRT[which(DS2015$WMGT == "FLD" &
                   DS2015$NRTE == 0)] = "FLD_N0"
DS2015$TRT[which(DS2015$WMGT == "FLD" &
                   DS2015$NRTE == 100)] = "FLD_N100"
DS2015$TRT[which(DS2015$WMGT == "FLD" &
                   DS2015$NRTE == 120)] = "FLD_N120"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 0)] = "AWD_N0"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 100)] = "AWD_N100"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 120)] = "AWD_N120"

DS2015$YEAR <- lubridate::year(DS2015$DATE)

DS2015$PLOT <- rep(1:24, each = 432)

# Arrange columns --------------------------------------------------------------
DS2015 <-
  DS2015 %>% dplyr::select(
    YEAR,
    DATE,
    ASMT,
    DAI,
    REP,
    PLOT,
    TRT,
    SMPL,
    HILL,
    TIL,
    LEAF,
    GL,
    DL,
    TShB_incidence,
    TIL_ShB,
    LEAF_ShB,
    WMGT,
    NRTE
  )

DS2015$LEAF_ShB <- as.numeric(DS2015$LEAF_ShB)

DS2015 <- tibble::as_tibble(dplyr::arrange(DS2015, ASMT, PLOT))

# Create new columns of days to calcluate AUDPS -------------------------------

DAYS <- as.numeric(substr(files, 17, 18))

DS2015$DAYS <- c(rep(0, 10368), rep(diff(DAYS), each = 10368))

rm(files, reformat)
```

# 2016 experiment

``` r
# Preprocess 2016 data for all assessments
#
# 1. Cleanup date format and fill empty dates
# 2. Fill empty NTIL (for same hill, not missing values)
# 3. Fill empty NTShB (for same hill, not missing values)
# 4. Add column with number of days after inoculation (DAI)

# First assessment -------------------------------------------------------------
DS2016_A1 <- readr::read_csv("data/DS2016_Raw_1.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_integer(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_integer(),
    ##   NTIL = col_integer(),
    ##   NTShB = col_integer(),
    ##   NSHShB = col_integer(),
    ##   TIL = col_integer(),
    ##   GL = col_integer(),
    ##   DL = col_integer(),
    ##   ShBL1 = col_integer(),
    ##   ShBL2 = col_integer(),
    ##   ShBL3 = col_integer(),
    ##   ShBL4 = col_integer(),
    ##   ShBL5 = col_integer(),
    ##   ShBL6 = col_integer()
    ## )

``` r
# Fill missing values in NTIL and NTShB
DS2016_A1 <-
  DS2016_A1 %>% tidyr::fill(NTIL)
DS2016_A1 <-
  DS2016_A1 %>% tidyr::fill(NTShB)

# Fill dates
DS2016_A1[, 1] <- as.Date("2016-03-15", origin = "1970-01-01")
DS2016_A1$ASMT <- rep(1, nrow(DS2016_A1))
DS2016_A1$PLOT <- rep(1:16, each = 72)

# Fill days after inoculation
DS2016_A1$DAI <- rep(14, nrow(DS2016_A1))

# Second assessement -----------------------------------------------------------
DS2016_A2 <- readr::read_csv("data/DS2016_Raw_2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_integer(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_integer(),
    ##   NTIL = col_integer(),
    ##   NTShB = col_integer(),
    ##   NSHShB = col_integer(),
    ##   TIL = col_integer(),
    ##   GL = col_integer(),
    ##   DL = col_integer(),
    ##   ShBL1 = col_integer(),
    ##   ShBL2 = col_integer(),
    ##   ShBL3 = col_integer(),
    ##   ShBL4 = col_integer(),
    ##   ShBL5 = col_integer(),
    ##   ShBL6 = col_integer()
    ## )

``` r
DS2016_A2 <-
  DS2016_A2 %>% tidyr::fill(NTIL)
DS2016_A2 <-
  DS2016_A2 %>% tidyr::fill(NTShB)

# Fill dates
DS2016_A2[, 1] <- as.Date("2016-03-29", origin = "1970-01-01")
DS2016_A2$ASMT <- rep(2, nrow(DS2016_A2))
DS2016_A2$PLOT <- rep(1:16, each = 72)

# Fill days after inoculation
DS2016_A2$DAI <- rep(28, nrow(DS2016_A2))

# Third assessement ------------------------------------------------------------
DS2016_A3 <- readr::read_csv("data/DS2016_Raw_3.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_integer(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_integer(),
    ##   NTIL = col_integer(),
    ##   NTShB = col_integer(),
    ##   NSHShB = col_integer(),
    ##   TIL = col_integer(),
    ##   GL = col_integer(),
    ##   DL = col_integer(),
    ##   ShBL1 = col_integer(),
    ##   ShBL2 = col_integer(),
    ##   ShBL3 = col_integer(),
    ##   ShBL4 = col_integer(),
    ##   ShBL5 = col_integer(),
    ##   ShBL6 = col_character()
    ## )

``` r
DS2016_A3 <-
  DS2016_A3 %>% tidyr::fill(NTIL)
DS2016_A3 <-
  DS2016_A3 %>% tidyr::fill(NTShB)

# Fill dates
DS2016_A3[, 1] <- as.Date("2016-04-12", origin = "1970-01-01")
DS2016_A3$ASMT <- rep(3, nrow(DS2016_A3))
DS2016_A3$PLOT <- rep(1:16, each = 72)

# Fill days after inoculation
DS2016_A3$DAI <- rep(43, nrow(DS2016_A3))

# Fourth assessment ------------------------------------------------------------
DS2016_A4 <- readr::read_csv("data/DS2016_Raw_4.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_integer(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_integer(),
    ##   NTIL = col_integer(),
    ##   NTShB = col_integer(),
    ##   NSHShB = col_integer(),
    ##   TIL = col_integer(),
    ##   GL = col_integer(),
    ##   DL = col_integer(),
    ##   ShBL1 = col_integer(),
    ##   ShBL2 = col_integer(),
    ##   ShBL3 = col_integer(),
    ##   ShBL4 = col_character(),
    ##   ShBL5 = col_character(),
    ##   ShBL6 = col_character()
    ## )

``` r
DS2016_A4 <-
  DS2016_A4 %>% tidyr::fill(NTIL)
DS2016_A4 <-
  DS2016_A4 %>% tidyr::fill(NTShB)

# Fill dates
DS2016_A4[, 1] <- as.Date("2016-04-26", origin = "1970-01-01")
DS2016_A4$ASMT <- rep(4, nrow(DS2016_A4))
DS2016_A4$PLOT <- rep(1:16, each = 72)

# Fill days after inoculation
DS2016_A4$DAI <- rep(59, nrow(DS2016_A4))

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

# Replace "N*" with a number for NRTE ------------------------------------------
DS2016$NRTE[which(DS2016$NRTE == "N1")] = 60
DS2016$NRTE[which(DS2016$NRTE == "N2")] = 180

# Add treatment numbers --------------------------------------------------------
DS2016$TRT <- NA
DS2016$TRT[which(DS2016$WMGT == "FLD" &
                   DS2016$NRTE == 60)] = "FLD_N60"
DS2016$TRT[which(DS2016$WMGT == "FLD" &
                   DS2016$NRTE == 180)] = "FLD_N180"
DS2016$TRT[which(DS2016$WMGT == "AWD" &
                   DS2016$NRTE == 60)] = "AWD_N60"
DS2016$TRT[which(DS2016$WMGT == "AWD" &
                   DS2016$NRTE == 180)] = "AWD_N180"

DS2016 <-
  DS2016 %>%
  tidyr::gather(
    LEAF,
    LEAF_ShB,
    dplyr::starts_with("ShBL"),
    -DATE,
    -ASMT,
    -DAI,
    -PLOT,
    -TRT,
    -REP,
    -WMGT,
    -SMPL,
    -NRTE,
    -HILL,
    -NTIL,
    -GL,
    -DL,
    -NTShB,
    -NSHShB,
    -TIL
  )

# reclassify LEAF column to leaf numbers 1...6

DS2016$LEAF[DS2016$LEAF == "ShBL1"] <- 1
DS2016$LEAF[DS2016$LEAF == "ShBL2"] <- 2
DS2016$LEAF[DS2016$LEAF == "ShBL3"] <- 3
DS2016$LEAF[DS2016$LEAF == "ShBL4"] <- 4
DS2016$LEAF[DS2016$LEAF == "ShBL5"] <- 5
DS2016$LEAF[DS2016$LEAF == "ShBL6"] <- 6

# before classifying incidence, set any values where there are more infected
# tillers in the hill than there are tillers to NA for both columns

DS2016$NTIL[DS2016$NTIL < DS2016$NTShB] <- NA
DS2016$NTShB[is.na(DS2016$NTIL)] <- NA

# calculate tiller sheath blight incidence
DS2016 <-
  DS2016 %>%
  dplyr::group_by(DATE, ASMT, DAI, REP, PLOT, TRT, WMGT, NRTE, SMPL, HILL) %>%
  dplyr::mutate(TShB_incidence = NTShB / NTIL)

DS2016 <- dplyr::rename(DS2016, TIL_ShB = NSHShB)

DS2016$YEAR <- lubridate::year(DS2016$DATE)
DS2016$LEAF_ShB <- as.numeric(DS2016$LEAF_ShB)

DS2016 <-
  DS2016 %>% dplyr::select(
    YEAR,
    DATE,
    ASMT,
    DAI,
    REP,
    PLOT,
    TRT,
    SMPL,
    HILL,
    TIL,
    LEAF,
    GL,
    DL,
    TShB_incidence,
    TIL_ShB,
    LEAF_ShB,
    WMGT,
    NRTE
  )

DS2016 <- tibble::as_tibble(dplyr::arrange(DS2016, ASMT, PLOT))

# Create new columns of days to calcluate AUDPS -------------------------------

DATE_1_2016 <- DATE_2_2016 <- DS2016$DATE
DATE_1_2016[which(DATE_1_2016 == min(DATE_1_2016))] <- NA
DATE_2_2016[which(DATE_2_2016 == max(DATE_2_2016))] <- NA

DATE_1 <- na.omit(DATE_1_2016)
DATE_2 <- na.omit(DATE_2_2016)

DAYS_2016 <- lubridate::time_length(DATE_1 - DATE_2, unit = "day")
DS2016$DAYS <-
  c(rep(0, nrow(DS2016) - length(DAYS_2016)), DAYS_2016)

# cleanup workspace ------------------------------------------------------------

rm(
  DS2016_A1,
  DS2016_A2,
  DS2016_A3,
  DS2016_A4,
  DATE_1,
  DATE_1_2016,
  DATE_2,
  DATE_2_2016,
  DAYS_2016
)

# Arrange columns --------------------------------------------------------------
DS2016 <-
  DS2016 %>% dplyr::select(
    YEAR,
    DATE,
    ASMT,
    DAI,
    REP,
    PLOT,
    TRT,
    SMPL,
    HILL,
    TIL,
    LEAF,
    GL,
    DL,
    TShB_incidence,
    TIL_ShB,
    LEAF_ShB,
    WMGT,
    NRTE,
    DAYS
  )
```

# Calculating Area Under the Disease Progress Stairs (AUDPS)

Because the data were collected on an ordinal scale, but not evenly
spaced, the data are converted to the midpoint value of the percent
range for severity and then the AUDPS is calculated from that. See lines
544 and 559 for this.

After that the AUDPS is calculated using functionality from the
*agricolae* package.

Last, the data are saved in the package for use analysis, which is
detailed in the package
[vingettes](../vignettes).

``` r
# Join the 2015 and 2016 Data into one Tibble ----------------------------------

RAW_data <- tibble::as_tibble(rbind(as.data.frame(DS2015),
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


# reorder the treatments by level/year - low to high, 2015-2016 for graphs -----
RAW_data$NRTE <-
  forcats::fct_relevel(RAW_data$NRTE,
                       "0",
                       "100",
                       "120",
                       "60",
                       "180")

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

# add column with midpoint % tiller ShB severity -------------------------------
RAW_data <-
  dplyr::mutate(
    RAW_data,
    PERC_TIL_ShB = dplyr::case_when(
      TIL_ShB == 0 ~ 0,
      TIL_ShB == 1 ~ 1,
      TIL_ShB == 2 ~ 2.5,
      TIL_ShB == 3 ~ 10,
      TIL_ShB == 4 ~ 32.5,
      TRUE ~ 75
    )
  )

# add column with midpoint % leaf ShB severity ---------------------------------
RAW_data <-
  dplyr::mutate(
    RAW_data,
    PERC_LEAF_ShB = dplyr::case_when(
      LEAF_ShB == 0 ~ 0,
      LEAF_ShB == 1 ~ 1,
      LEAF_ShB == 2 ~ 2.5,
      LEAF_ShB == 3 ~ 10,
      LEAF_ShB == 4 ~ 32.5,
      TRUE ~ 75
    )
  )



DS2015 <- subset(RAW_data, YEAR == "2015")
DS2016 <- subset(RAW_data, YEAR == "2016")

# calculate AUDPS values -------------------------------------------------------

# 2015 AUDPS -------------------------------------------------------------------

# 2015 Tiller incidence setup --------------------------------------------------
TShB_inc_15 <-
  DS2015 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_TShB_incidence = TShB_incidence)) %>%
  dplyr::arrange(PLOT)

TShB_inc_wide <-
  reshape2::dcast(TShB_inc_15, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

# 2015 Tiller Severity AUDPS ---------------------------------------------------
TShB_sev_15 <-
  DS2015 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_TShB_severity = TIL_ShB)) %>%
  dplyr::arrange(PLOT)

TShB_sev_wide <-
  reshape2::dcast(TShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

TShB_perc_15 <-
  DS2015 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_TShB_PERCENT = PERC_TIL_ShB)) %>%
  dplyr::arrange(PLOT)

TShB_perc_wide <-
  reshape2::dcast(TShB_perc_15, PLOT ~ ASMT, value.var = "PLOT_TShB_PERCENT")

# 2015 Leaf Severity setup -----------------------------------------------------
LShB_sev_15 <-
  DS2015 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_LShB_severity = LEAF_ShB)) %>%
  dplyr::arrange(PLOT)

LShB_sev_wide <-
  reshape2::dcast(LShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_perc_15 <-
  DS2015 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_LShB_PERCENT = PERC_LEAF_ShB)) %>%
  dplyr::arrange(PLOT)

LShB_perc_wide <-
  reshape2::dcast(LShB_perc_15, PLOT ~ ASMT, value.var = "PLOT_LShB_PERCENT")

# Calculate 2015 AUDPS ---------------------------------------------------------

TShB_inc_AUDPS <-
  agricolae::audps(evaluation = TShB_inc_wide[, 2:6],
                   dates = dplyr::pull(TShB_inc_15[1:5, 6]))

TShB_sev_AUDPS <-
  agricolae::audps(evaluation = TShB_sev_wide[, 2:6],
                   dates = dplyr::pull(TShB_sev_15[1:5, 6]))

LShB_sev_AUDPS <-
  agricolae::audps(evaluation = LShB_sev_wide[, 2:6],
                   dates = dplyr::pull(LShB_sev_15[1:5, 6]))

TShB_percent_AUDPS <-
  agricolae::audps(evaluation = TShB_perc_wide[, 2:6],
        dates = dplyr::pull(TShB_perc_15[1:5, 6]))

LShB_percent_AUDPS <-
  agricolae::audps(evaluation = LShB_perc_wide[, 2:6],
        dates = dplyr::pull(LShB_perc_15[1:5, 6]))

AUDPS_15 <-
  tibble::as_tibble(
    cbind(
      PLOT = 1:24,
      TShB_inc_AUDPS,
      TShB_sev_AUDPS,
      TShB_percent_AUDPS,
      LShB_sev_AUDPS,
      LShB_percent_AUDPS
    )
  )

AUDPS_15$PLOT <- as.character(AUDPS_15$PLOT)
TShB_inc_15$PLOT <- as.character(TShB_inc_15$PLOT)

ShB_15 <- dplyr::left_join(TShB_inc_15, AUDPS_15, by = "PLOT")

# 2016 AUDPS -------------------------------------------------------------------

# 2016 Tiller Incidence setup --------------------------------------------------
TShB_inc_16 <-
  DS2016 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_TShB_incidence = TShB_incidence)) %>%
  dplyr::arrange(PLOT)

TShB_inc_wide <-
  reshape2::dcast(TShB_inc_16, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

TShB_sev_16 <-
  DS2016 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_TShB_severity = TIL_ShB)) %>%
  dplyr::arrange(PLOT)

TShB_sev_wide <-
  reshape2::dcast(TShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

# 2016 Tiller Severity setup ---------------------------------------------------

TShB_perc_16 <-
  DS2016 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_TShB_PERCENT = PERC_TIL_ShB)) %>%
  dplyr::arrange(PLOT)

TShB_perc_wide <-
  reshape2::dcast(TShB_perc_16, PLOT ~ ASMT, value.var = "PLOT_TShB_PERCENT")

# 2016 Leaf Severity setup -----------------------------------------------------

LShB_sev_16 <-
  DS2016 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_LShB_severity = LEAF_ShB)) %>%
  dplyr::arrange(PLOT)

LShB_sev_wide <-
  reshape2::dcast(LShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_perc_16 <-
  DS2016 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_LShB_PERCENT = PERC_LEAF_ShB)) %>%
  dplyr::arrange(PLOT)

LShB_perc_wide <-
  reshape2::dcast(LShB_perc_16, PLOT ~ ASMT, value.var = "PLOT_LShB_PERCENT")

# 2016 AUDPS -------------------------------------------------------------------

TShB_inc_AUDPS <-
  agricolae::audps(evaluation = TShB_inc_wide[, 2:5],
                   dates = dplyr::pull(TShB_inc_16[1:4, 6]))

TShB_sev_AUDPS <-
  agricolae::audps(evaluation = TShB_sev_wide[, 2:5],
                   dates = dplyr::pull(TShB_sev_16[1:4, 6]))

LShB_sev_AUDPS <-
  agricolae::audps(evaluation = LShB_sev_wide[, 2:5],
                   dates = dplyr::pull(LShB_sev_16[1:4, 6]))

TShB_percent_AUDPS <-
  agricolae::audps(evaluation = TShB_perc_wide[, 2:5],
                   dates = dplyr::pull(TShB_perc_16[1:4, 6]))

LShB_percent_AUDPS <-
  agricolae::audps(evaluation = LShB_perc_wide[, 2:5],
                   dates = dplyr::pull(LShB_perc_16[1:4, 6]))

AUDPS_16 <-
  tibble::as_tibble(
    cbind(
      PLOT = 1:16,
      TShB_inc_AUDPS,
      TShB_sev_AUDPS,
      TShB_percent_AUDPS,
      LShB_sev_AUDPS,
      LShB_percent_AUDPS
    )
  )

# add plot numbers to merge with the remaining treatment data
AUDPS_16$PLOT <- as.character(AUDPS_16$PLOT)
TShB_inc_16$PLOT <- as.character(TShB_inc_16$PLOT)

ShB_16 <- dplyr::left_join(TShB_inc_16, AUDPS_16, by = c("PLOT" = "PLOT"))

# Merge AUDPS data for graphing ------------------------------------------------
AUDPS <- tibble::as_data_frame(tibble::as_tibble(rbind(ShB_15, ShB_16)))
AUDPS <- tidyr::separate(data = AUDPS, col = TRT,
                         sep = "_",
                         into = c("WMGT", "NRTE"))
AUDPS <- dplyr::mutate_at(.tbl = AUDPS,
                              .funs = factor,
                              .vars = c("WMGT", "NRTE"))

# Save data for inclusion in the package ---------------------------------------

if (!dir.exists("../data")) {
  dir.create("../data", recursive = TRUE)
}

devtools::use_data(RAW_data,
                   compress = "bzip2",
                   overwrite = TRUE)
```

    ## Saving RAW_data as RAW_data.rda to /Users/asparks/Development/rice_awd_pests/data

``` r
devtools::use_data(AUDPS,
                   compress = "bzip2",
                   overwrite = TRUE)
```

    ## Saving AUDPS as AUDPS.rda to /Users/asparks/Development/rice_awd_pests/data

# N rates

Create a `tibble` of N rates and time of application used in this study.

``` r
Year <- as.integer(c(2015,
                     2015,
                     2015,
                     2016,
                     2016))

`Total N (kg/ha)` <- as.integer(c(0,
                                  100,
                                  120,
                                  60,
                                  180))

`Basal N (kg/ha)` <- as.integer(c(0,
                                  60,
                                  60,
                                  30,
                                  60))

`Tillering N (kg/ha)` <- as.integer(c(0,
                                      20,
                                      30,
                                      30,
                                      60))

`Panicle Initiation N (kg/ha)` <- as.integer(c(0,
                                               20,
                                               30,
                                               0,
                                               60))

N_rates <- cbind(Year,
                 `Total N (kg/ha)`,
                 `Basal N (kg/ha)`,
                 `Tillering N (kg/ha)`,
                 `Panicle Initiation N (kg/ha)`)

row.names(N_rates) <- c("N0", "N100", "N120", "N60", "N180")

devtools::use_data(N_rates,
                   compress = "bzip2",
                   overwrite = TRUE)
```

    ## Saving N_rates as N_rates.rda to /Users/asparks/Development/rice_awd_pests/data

# Colophon

``` r
sessioninfo::session_info()
```

    ## ─ Session info ──────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 3.5.0 (2018-04-23)
    ##  os       macOS High Sierra 10.13.4   
    ##  system   x86_64, darwin17.5.0        
    ##  ui       unknown                     
    ##  language (EN)                        
    ##  collate  en_AU.UTF-8                 
    ##  tz       Australia/Brisbane          
    ##  date     2018-04-26                  
    ## 
    ## ─ Packages ──────────────────────────────────────────────────────────────
    ##  package     * version date       source         
    ##  agricolae     1.2-8   2017-09-12 cran (@1.2-8)  
    ##  AlgDesign     1.1-7.3 2014-10-15 cran (@1.1-7.3)
    ##  assertthat    0.2.0   2017-04-11 CRAN (R 3.5.0) 
    ##  backports     1.1.2   2017-12-13 CRAN (R 3.5.0) 
    ##  bindr         0.1.1   2018-03-13 CRAN (R 3.5.0) 
    ##  bindrcpp    * 0.2.2   2018-03-29 CRAN (R 3.5.0) 
    ##  boot          1.3-20  2017-08-06 CRAN (R 3.5.0) 
    ##  clisymbols    1.2.0   2017-05-21 CRAN (R 3.5.0) 
    ##  cluster       2.0.7-1 2018-04-13 CRAN (R 3.5.0) 
    ##  coda          0.19-1  2016-12-08 CRAN (R 3.5.0) 
    ##  combinat      0.0-8   2012-10-29 cran (@0.0-8)  
    ##  deldir        0.1-15  2018-04-01 CRAN (R 3.5.0) 
    ##  devtools      1.13.5  2018-02-18 CRAN (R 3.5.0) 
    ##  digest        0.6.15  2018-01-28 CRAN (R 3.5.0) 
    ##  dplyr       * 0.7.4   2017-09-28 CRAN (R 3.5.0) 
    ##  evaluate      0.10.1  2017-06-24 CRAN (R 3.5.0) 
    ##  expm          0.999-2 2017-03-29 CRAN (R 3.5.0) 
    ##  forcats       0.3.0   2018-02-19 CRAN (R 3.5.0) 
    ##  gdata         2.18.0  2017-06-06 CRAN (R 3.5.0) 
    ##  glue          1.2.0   2017-10-29 CRAN (R 3.5.0) 
    ##  gmodels       2.16.2  2015-07-22 CRAN (R 3.5.0) 
    ##  gtools        3.5.0   2015-05-29 CRAN (R 3.5.0) 
    ##  highr         0.6     2016-05-09 CRAN (R 3.5.0) 
    ##  hms           0.4.2   2018-03-10 CRAN (R 3.5.0) 
    ##  htmltools     0.3.6   2017-04-28 CRAN (R 3.5.0) 
    ##  httpuv        1.4.1   2018-04-21 CRAN (R 3.5.0) 
    ##  klaR          0.6-14  2018-03-19 cran (@0.6-14) 
    ##  knitr         1.20    2018-02-20 CRAN (R 3.5.0) 
    ##  later         0.7.1   2018-03-07 CRAN (R 3.5.0) 
    ##  lattice       0.20-35 2017-03-25 CRAN (R 3.5.0) 
    ##  LearnBayes    2.15.1  2018-03-18 CRAN (R 3.5.0) 
    ##  lubridate     1.7.4   2018-04-11 CRAN (R 3.5.0) 
    ##  magrittr      1.5     2014-11-22 CRAN (R 3.5.0) 
    ##  MASS          7.3-49  2018-02-23 CRAN (R 3.5.0) 
    ##  Matrix        1.2-14  2018-04-13 CRAN (R 3.5.0) 
    ##  memoise       1.1.0   2017-04-21 CRAN (R 3.5.0) 
    ##  mime          0.5     2016-07-07 CRAN (R 3.5.0) 
    ##  miniUI        0.1.1   2016-01-15 cran (@0.1.1)  
    ##  nlme          3.1-137 2018-04-07 CRAN (R 3.5.0) 
    ##  pillar        1.2.1   2018-02-27 CRAN (R 3.5.0) 
    ##  pkgconfig     2.0.1   2017-03-21 CRAN (R 3.5.0) 
    ##  plyr          1.8.4   2016-06-08 CRAN (R 3.5.0) 
    ##  promises      1.0.1   2018-04-13 CRAN (R 3.5.0) 
    ##  purrr         0.2.4   2017-10-18 CRAN (R 3.5.0) 
    ##  questionr     0.6.2   2017-11-01 cran (@0.6.2)  
    ##  R6            2.2.2   2017-06-17 CRAN (R 3.5.0) 
    ##  Rcpp          0.12.16 2018-03-13 CRAN (R 3.5.0) 
    ##  readr       * 1.1.1   2017-05-16 CRAN (R 3.5.0) 
    ##  reshape2      1.4.3   2017-12-11 CRAN (R 3.5.0) 
    ##  rlang         0.2.0   2018-02-20 CRAN (R 3.5.0) 
    ##  rmarkdown     1.9     2018-03-01 CRAN (R 3.5.0) 
    ##  rprojroot     1.3-2   2018-01-03 CRAN (R 3.5.0) 
    ##  rstudioapi    0.7     2017-09-07 CRAN (R 3.5.0) 
    ##  sessioninfo   1.0.0   2017-06-21 CRAN (R 3.5.0) 
    ##  shiny         1.0.5   2017-08-23 CRAN (R 3.5.0) 
    ##  sp            1.2-7   2018-01-19 CRAN (R 3.5.0) 
    ##  spData        0.2.8.3 2018-03-25 CRAN (R 3.5.0) 
    ##  spdep         0.7-7   2018-04-03 CRAN (R 3.5.0) 
    ##  stringi       1.1.7   2018-03-12 CRAN (R 3.5.0) 
    ##  stringr       1.3.0   2018-02-19 CRAN (R 3.5.0) 
    ##  tibble        1.4.2   2018-01-22 CRAN (R 3.5.0) 
    ##  tidyr         0.8.0   2018-01-29 CRAN (R 3.5.0) 
    ##  tidyselect    0.2.4   2018-02-26 CRAN (R 3.5.0) 
    ##  withr         2.1.2   2018-03-15 CRAN (R 3.5.0) 
    ##  xtable        1.8-2   2016-02-05 CRAN (R 3.5.0) 
    ##  yaml          2.1.18  2018-03-08 CRAN (R 3.5.0)
