Clean 2015 Raw Data
================

The raw data are located in the `extdata` folder of the installed *rice.awd.pests*

Dry season 2015 importing and cleaning
======================================

DS2015 data can be accessed using:

``` r
load(system.file("extdata", "DS2015_Raw_22DAI.csv", package = "rice.awd.pests"))
load(system.file("extdata", "DS2015_Raw_35DAI.csv", package = "rice.awd.pests"))
load(system.file("extdata", "DS2015_Raw_49DAI.csv", package = "rice.awd.pests"))
load(system.file("extdata", "DS2015_Raw_62DAI.csv", package = "rice.awd.pests"))
load(system.file("extdata", "DS2015_Raw_83DAI.csv", package = "rice.awd.pests"))
```

``` r
library(readr)
library(magrittr)

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

# Save data for inclusion in the package ---------------------------------------

if (!dir.exists("../data")) {
  dir.create("../data", recursive = TRUE)
}
```

Dry season 2016 importing and cleaning
======================================

The 2016 season raw data are located in the `extdata` folder of the installed *rice.awd.pests* and can be accessed using:

``` r
load(system.file("extdata", "DS2016_Raw_1.csv", package = "rice.awd.pests"))
load(system.file("extdata", "DS2016_Raw_2.csv", package = "rice.awd.pests"))
load(system.file("extdata", "DS2016_Raw_3.csv", package = "rice.awd.pests"))
load(system.file("extdata", "DS2016_Raw_4.csv", package = "rice.awd.pests"))
```

``` r
# Preprocess 2016 data for all assessments
#
# 1. Cleanup date format and fill empty dates
# 2. Fill empty NTIL (for same hill, not missing values)
# 3. Fill empty NTShB (for same hill, not missing values)

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
  dplyr::group_by(DATE, ASMT, REP, PLOT, TRT, WMGT, NRTE, SMPL, HILL) %>%
  dplyr::mutate(TShB_incidence = NTShB / NTIL)

DS2016 <- dplyr::rename(DS2016, TIL_ShB = NSHShB)

DS2016$YEAR <- lubridate::year(DS2016$DATE)
DS2016$LEAF_ShB <- as.numeric(DS2016$LEAF_ShB)

DS2016 <-
  DS2016 %>% dplyr::select(
    YEAR,
    DATE,
    ASMT,
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

Calculating Area Under the Disease Progress Stairs (AUDPS)
==========================================================

Because the data were collected on an ordinal scale, but not evenly spaced, the data are converted to the midpoint value of the percent range for severity and then the AUDPS is calculated from that. See lines 544 and 559 for this.

After that the AUDPS is calculated using functionality from the *agricolae* package.

Last, the data are saved in the package for use analysis, which is detailed in the package [vingettes](../vignettes).

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
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_TShB_PERCENT = PERC_TIL_ShB)) %>%
  dplyr::arrange(PLOT)

TShB_perc_wide <-
  reshape2::dcast(TShB_perc_15, PLOT ~ ASMT, value.var = "PLOT_TShB_PERCENT")

# 2015 Leaf Severity setup -----------------------------------------------------
LShB_sev_15 <-
  DS2015 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  dplyr::summarise_at(.funs = dplyr::funs(mean(., na.rm = TRUE)),
                      .vars = dplyr::vars(PLOT_LShB_severity = LEAF_ShB)) %>%
  dplyr::arrange(PLOT)

LShB_sev_wide <-
  reshape2::dcast(LShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_perc_15 <-
  DS2015 %>%
  dplyr::group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
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

devtools::use_data(AUDPS,
                   compress = "bzip2",
                   overwrite = TRUE)
```

    ## Saving AUDPS as AUDPS.rda to /Users/asparks/Development/rice_awd_pests/data