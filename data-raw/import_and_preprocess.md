AWD Sheath Blight Data Import and Processing
================
A H Sparks, N P Castilla and M Noel
2020-06-11

The raw data are located in the `data` folder of the installed
*rice.awd.pests*

*Note* TRT is created here as a combination of the main plot/split plot
treatment. This is used in place of plot numbers to keep the data in
order. However, it is not used in the final analysis as a treatment
effect.

# Load Libraries

``` r
library("tidyverse")
```

    ## ── Attaching packages ────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library("agricolae")
library("usethis")
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

# Load Data

## 2015 Experiment

Preprocess 2015 data for all assessment dates.

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
  
  x <- arrange(x, REP, TRT)
  
  # add plot numbers
  x$PLOT <- rep(1:24, each = 18)
  
  # Calculate tiller sheath blight incidence -----------------------------------
  x <-
    x %>%
    mutate(TShB_incidence = NTShB / NTIL)
  
  TShB_incidence <-
    x %>% select(REP,
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
      select(
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
      gather(LEAF,
             LEAF_ShB,
             starts_with("SL"))
    
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
  
  z <- bind_rows(TIL)
  x <- left_join(TShB_incidence, z)
  
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
DS2015 <- map_df(files, reformat)
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
  mutate(DAI = case_when(ASMT == 1 ~ 14,
                         ASMT == 2 ~ 22,
                         ASMT == 3 ~ 35,
                         ASMT == 4 ~ 49,
                         ASMT == 5 ~ 62))

# Replace "N*" with a number for NRTE ------------------------------------------
DS2015$NRTE[which(DS2015$NRTE == "N0")] <- 0
DS2015$NRTE[which(DS2015$NRTE == "N1")] <- 100
DS2015$NRTE[which(DS2015$NRTE == "N2")] <- 120

# Add treatment numbers --------------------------------------------------------
DS2015$TRT <- NA
DS2015$TRT[which(DS2015$WMGT == "PDL" &
                   DS2015$NRTE == 0)] = "PDL_N0"
DS2015$TRT[which(DS2015$WMGT == "PDL" &
                   DS2015$NRTE == 100)] = "PDL_N100"
DS2015$TRT[which(DS2015$WMGT == "PDL" &
                   DS2015$NRTE == 120)] = "PDL_N120"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 0)] = "AWD_N0"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 100)] = "AWD_N100"
DS2015$TRT[which(DS2015$WMGT == "AWD" &
                   DS2015$NRTE == 120)] = "AWD_N120"

DS2015$YEAR <- year(DS2015$DATE)

DS2015$PLOT <- rep(1:24, each = 432)

# Arrange columns --------------------------------------------------------------
DS2015 <-
  DS2015 %>% select(
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
  ) %>% 
  mutate(REP = as.integer(REP),
         HILL = as.integer(HILL))

DS2015$LEAF_ShB <- as.numeric(DS2015$LEAF_ShB)

DS2015 <- arrange(DS2015, ASMT, PLOT)

# Create new columns of days to calcluate AUDPS -------------------------------

DAYS <- as.numeric(substr(files, 17, 18))

DS2015$DAYS <- c(rep(0, 10368), rep(diff(DAYS), each = 10368))

rm(files, reformat)
```

## 2016 Experiment

Preprocess 2016 data for all assessments

1.  Clean up date format and fill empty dates

2.  Fill empty NTIL (for same hill, not missing values)

3.  Fill empty NTShB (for same hill, not missing values)

4.  Add column with number of days after inoculation (DAI)

<!-- end list -->

``` r
# First assessment -------------------------------------------------------------
DS2016_A1 <- read_csv("data/DS2016_Raw_1.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_double(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_double(),
    ##   NTIL = col_double(),
    ##   NTShB = col_double(),
    ##   NSHShB = col_double(),
    ##   TIL = col_double(),
    ##   GL = col_double(),
    ##   DL = col_double(),
    ##   ShBL1 = col_double(),
    ##   ShBL2 = col_double(),
    ##   ShBL3 = col_double(),
    ##   ShBL4 = col_double(),
    ##   ShBL5 = col_double(),
    ##   ShBL6 = col_double()
    ## )

``` r
# Fill missing values in NTIL and NTShB
DS2016_A1 <-
  DS2016_A1 %>% fill(NTIL)
DS2016_A1 <-
  DS2016_A1 %>% fill(NTShB)

# Fill dates
DS2016_A1[, 1] <- as.Date("2016-03-15", origin = "1970-01-01")
DS2016_A1$ASMT <- rep(1, nrow(DS2016_A1))
DS2016_A1$PLOT <- rep(1:16, each = 72)

# Fill days after inoculation
DS2016_A1$DAI <- rep(14, nrow(DS2016_A1))

# Second assessement -----------------------------------------------------------
DS2016_A2 <- read_csv("data/DS2016_Raw_2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_double(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_double(),
    ##   NTIL = col_double(),
    ##   NTShB = col_double(),
    ##   NSHShB = col_double(),
    ##   TIL = col_double(),
    ##   GL = col_double(),
    ##   DL = col_double(),
    ##   ShBL1 = col_double(),
    ##   ShBL2 = col_double(),
    ##   ShBL3 = col_double(),
    ##   ShBL4 = col_double(),
    ##   ShBL5 = col_double(),
    ##   ShBL6 = col_double()
    ## )

``` r
DS2016_A2 <-
  DS2016_A2 %>% fill(NTIL)
DS2016_A2 <-
  DS2016_A2 %>% fill(NTShB)

# Fill dates
DS2016_A2[, 1] <- as.Date("2016-03-29", origin = "1970-01-01")
DS2016_A2$ASMT <- rep(2, nrow(DS2016_A2))
DS2016_A2$PLOT <- rep(1:16, each = 72)

# Fill days after inoculation
DS2016_A2$DAI <- rep(28, nrow(DS2016_A2))

# Third assessement ------------------------------------------------------------
DS2016_A3 <- read_csv("data/DS2016_Raw_3.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_double(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_double(),
    ##   NTIL = col_double(),
    ##   NTShB = col_double(),
    ##   NSHShB = col_double(),
    ##   TIL = col_double(),
    ##   GL = col_double(),
    ##   DL = col_double(),
    ##   ShBL1 = col_double(),
    ##   ShBL2 = col_double(),
    ##   ShBL3 = col_double(),
    ##   ShBL4 = col_double(),
    ##   ShBL5 = col_double(),
    ##   ShBL6 = col_logical()
    ## )

``` r
DS2016_A3 <-
  DS2016_A3 %>% fill(NTIL)
DS2016_A3 <-
  DS2016_A3 %>% fill(NTShB)

# Fill dates
DS2016_A3[, 1] <- as.Date("2016-04-12", origin = "1970-01-01")
DS2016_A3$ASMT <- rep(3, nrow(DS2016_A3))
DS2016_A3$PLOT <- rep(1:16, each = 72)

# Fill days after inoculation
DS2016_A3$DAI <- rep(43, nrow(DS2016_A3))

# Fourth assessment ------------------------------------------------------------
DS2016_A4 <- read_csv("data/DS2016_Raw_4.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   DATE = col_character(),
    ##   REP = col_double(),
    ##   WMGT = col_character(),
    ##   NRTE = col_character(),
    ##   SMPL = col_character(),
    ##   HILL = col_double(),
    ##   NTIL = col_double(),
    ##   NTShB = col_double(),
    ##   NSHShB = col_double(),
    ##   TIL = col_double(),
    ##   GL = col_double(),
    ##   DL = col_double(),
    ##   ShBL1 = col_double(),
    ##   ShBL2 = col_double(),
    ##   ShBL3 = col_double(),
    ##   ShBL4 = col_logical(),
    ##   ShBL5 = col_logical(),
    ##   ShBL6 = col_logical()
    ## )

``` r
DS2016_A4 <-
  DS2016_A4 %>% fill(NTIL)
DS2016_A4 <-
  DS2016_A4 %>% fill(NTShB)

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

# Replace "N*" with a number for NRTE ------------------------------------------
DS2016$NRTE[which(DS2016$NRTE == "N1")] = 60
DS2016$NRTE[which(DS2016$NRTE == "N2")] = 180

# Add treatment numbers --------------------------------------------------------
DS2016$TRT <- NA
DS2016$TRT[which(DS2016$WMGT == "PDL" &
                   DS2016$NRTE == 60)] = "PDL_N60"
DS2016$TRT[which(DS2016$WMGT == "PDL" &
                   DS2016$NRTE == 180)] = "PDL_N180"
DS2016$TRT[which(DS2016$WMGT == "AWD" &
                   DS2016$NRTE == 60)] = "AWD_N60"
DS2016$TRT[which(DS2016$WMGT == "AWD" &
                   DS2016$NRTE == 180)] = "AWD_N180"

DS2016 <-
  DS2016 %>%
  gather(
    LEAF,
    LEAF_ShB,
    starts_with("ShBL"),
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
  group_by(DATE, ASMT, DAI, REP, PLOT, TRT, WMGT, NRTE, SMPL, HILL) %>%
  mutate(TShB_incidence = NTShB / NTIL)

DS2016 <- rename(DS2016, TIL_ShB = NSHShB)

DS2016$YEAR <- year(DS2016$DATE)
DS2016$LEAF_ShB <- as.numeric(DS2016$LEAF_ShB)

DS2016 <-
  DS2016 %>% select(
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

DS2016 <- arrange(DS2016, ASMT, PLOT)

# Create new columns of days to calcluate AUDPS -------------------------------

DATE_1_2016 <- DATE_2_2016 <- DS2016$DATE
DATE_1_2016[which(DATE_1_2016 == min(DATE_1_2016))] <- NA
DATE_2_2016[which(DATE_2_2016 == max(DATE_2_2016))] <- NA

DATE_1 <- na.omit(DATE_1_2016)
DATE_2 <- na.omit(DATE_2_2016)

DAYS_2016 <- time_length(DATE_1 - DATE_2, unit = "day")
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
  DS2016 %>% select(
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

# Combine 2015 and 2016 Data

First join the data from the two experiments to perform some formatting
and reordering on the full set of data. Then create new data frames with
the reformatted data.

``` r
# Join the 2015 and 2016 Data into one Tibble ----------------------------------

RAW_data <- bind_rows(DS2015, DS2016)

# convert columns to factor ----------------------------------------------------
RAW_data <-
  RAW_data %>%
  mutate(
    YEAR = as.factor(YEAR),
    ASMT = as.factor(ASMT),
    PLOT = as.factor(PLOT),
    REP = as.factor(REP),
    TRT = as.factor(TRT),
    WMGT = as.factor(WMGT),
    NRTE = as.factor(NRTE),
    SMPL = as.factor(SMPL),
    HILL = as.factor(HILL),
    TIL = as.factor(TIL),
    LEAF = as.factor(LEAF)
  )

# reorder the treatments by level/year - low to high, 2015-2016 for graphs -----
RAW_data$NRTE <-
  fct_relevel(RAW_data$NRTE,
              "0",
              "100",
              "120",
              "60",
              "180")

RAW_data$TRT <-
  fct_relevel(
    RAW_data$TRT,
    "AWD_N0",
    "AWD_N100",
    "AWD_N120",
    "AWD_N60",
    "AWD_N180",
    "PDL_N0",
    "PDL_N100",
    "PDL_N120",
    "PDL_N60",
    "PDL_N180"
  )

# add column with midpoint % tiller ShB severity -------------------------------
RAW_data <-
  mutate(
    RAW_data,
    PERC_TIL_ShB = case_when(
      TIL_ShB == 0 ~ 0,
      TIL_ShB == 1 ~ 1,
      TIL_ShB == 2 ~ 2.5,
      TIL_ShB == 3 ~ 10,
      TIL_ShB == 4 ~ 32.5,
      TRUE ~ 75
    )
  )

# add column with midpoint +1  % tiller ShB severity ---------------------------

RAW_data <-
  mutate(
    RAW_data,
    PERC_TIL_ShB_1 = case_when(
      TIL_ShB == 0 ~ 1,
      TIL_ShB == 1 ~ 2.5,
      TIL_ShB == 2 ~ 10,
      TIL_ShB == 3 ~ 32.5,
      TIL_ShB == 4 ~ 75,
      TRUE ~ 100
    )
  )

# add column with midpoint % leaf ShB severity ---------------------------------
RAW_data <-
  mutate(
    RAW_data,
    PERC_LEAF_ShB = case_when(
      LEAF_ShB == 0 ~ 0,
      LEAF_ShB == 1 ~ 1,
      LEAF_ShB == 2 ~ 2.5,
      LEAF_ShB == 3 ~ 10,
      LEAF_ShB == 4 ~ 32.5,
      TRUE ~ 75
    )
  )

# add column with midpoint +1  % leaf ShB severity -----------------------------
RAW_data <-
  mutate(
    RAW_data,
    PERC_LEAF_ShB_1 = case_when(
      LEAF_ShB == 0 ~ 1,
      LEAF_ShB == 1 ~ 2.5,
      LEAF_ShB == 2 ~ 10,
      LEAF_ShB == 3 ~ 32.5,
      LEAF_ShB == 4 ~ 75,
      TRUE ~ 100
    )
  )

DS2015 <- subset(RAW_data, YEAR == "2015")
DS2016 <- subset(RAW_data, YEAR == "2016")
```

# Calculating Area Under Disease Progress Curve (AUDPC) Area Under the Disease Progress Stairs (AUDPS)

Because the data were collected on an ordinal scale, but not evenly
spaced, the data are converted to the midpoint value of the percent
range for disease severity index and then the AUDPC and AUDPS are
calculated from that.

## 2015 Area Calculations

``` r
TShB_sev_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise(PLOT_TShB_severity = mean(TIL_ShB), na.rm = TRUE) %>%
  arrange(PLOT)

TShB_sev_wide <-
  reshape2::dcast(TShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

TShB_perc_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  summarise(PLOT_TShB_percent = mean(PERC_TIL_ShB),
            na.rm = TRUE) %>%
  arrange(PLOT)

TShB_perc_wide <-
  reshape2::dcast(TShB_perc_15, PLOT ~ ASMT, value.var = "PLOT_TShB_percent")

# 2015 Leaf Severity setup -----------------------------------------------------
LShB_sev_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  summarise(PLOT_LShB_severity = mean(LEAF_ShB), na.rm = TRUE) %>%
  arrange(PLOT)

LShB_sev_wide <-
  reshape2::dcast(LShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_perc_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  summarise(PLOT_LShB_percent = mean(PERC_LEAF_ShB),
            na.rm = TRUE) %>%
  arrange(PLOT)

LShB_perc_wide <-
  reshape2::dcast(LShB_perc_15, PLOT ~ ASMT, value.var = "PLOT_LShB_percent")

TShB_inc_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise(PLOT_TShB_incidence = mean(TShB_incidence),
            na.rm = TRUE) %>%
  arrange(PLOT) %>%
  group_by(YEAR, REP, TRT, PLOT) %>%
  summarise(
    AUDPC = audpc(PLOT_TShB_incidence, DAYS),
    AUDPS = audps(PLOT_TShB_incidence, DAYS)
  )

# AUDPS data frame for incidence

TShB_inc_AUDPS <-  pull(TShB_inc_15, AUDPS)

TShB_sev_AUDPS <-
  audps(evaluation = TShB_sev_wide[, 2:6],
        dates = pull(TShB_sev_15[1:5, 6]))

LShB_sev_AUDPS <-
  audps(evaluation = LShB_sev_wide[, 2:6],
        dates = pull(LShB_sev_15[1:5, 6]))

TShB_percent_AUDPS <-
  audps(evaluation = TShB_perc_wide[, 2:6],
        dates = pull(TShB_perc_15[1:5, 6]))

LShB_percent_AUDPS <-
  audps(evaluation = LShB_perc_wide[, 2:6],
        dates = pull(LShB_perc_15[1:5, 6]))

AUDPS_15 <-
  as_tibble(
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

# AUDPC data frame for incidence
TShB_inc_AUDPC <-  pull(TShB_inc_15, AUDPC)

TShB_sev_AUDPC <-
  audpc(evaluation = TShB_sev_wide[, 2:6],
        dates = pull(TShB_sev_15[1:5, 6]))

LShB_sev_AUDPC <-
  audpc(evaluation = LShB_sev_wide[, 2:6],
        dates = pull(LShB_sev_15[1:5, 6]))

TShB_percent_AUDPC <-
  audpc(evaluation = TShB_perc_wide[, 2:6],
        dates = pull(TShB_perc_15[1:5, 6]))

LShB_percent_AUDPC <-
  audpc(evaluation = LShB_perc_wide[, 2:6],
        dates = pull(LShB_perc_15[1:5, 6]))

AUDPC_15 <-
  as_tibble(
    cbind(
      PLOT = 1:24,
      TShB_inc_AUDPC,
      TShB_sev_AUDPC,
      TShB_percent_AUDPC,
      LShB_sev_AUDPC,
      LShB_percent_AUDPC
    )
  )

AUDPC_15$PLOT <- as.character(AUDPC_15$PLOT)
TShB_inc_15$PLOT <- as.character(TShB_inc_15$PLOT)

# create final data frame of AUDPC_S

ShB_15 <- left_join(TShB_inc_15, AUDPS_15, by = "PLOT")
ShB_15 <- left_join(ShB_15, AUDPC_15, by = "PLOT")
```

## 2016 Area Calculations

``` r
TShB_inc_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise(PLOT_TShB_incidence = mean(TShB_incidence),
            na.rm = TRUE) %>%
  arrange(PLOT)

TShB_inc_wide <-
  reshape2::dcast(TShB_inc_16, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

TShB_sev_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise(PLOT_TShB_severity = mean(TIL_ShB), na.rm = TRUE) %>%
  arrange(PLOT)

TShB_sev_wide <-
  reshape2::dcast(TShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

# 2016 Tiller Severity setup ---------------------------------------------------

TShB_perc_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise(PLOT_TShB_percent = mean(PERC_TIL_ShB),
            na.rm = TRUE) %>%
  arrange(PLOT)

TShB_perc_wide <-
  reshape2::dcast(TShB_perc_16, PLOT ~ ASMT, value.var = "PLOT_TShB_percent")

# 2016 Leaf Severity setup -----------------------------------------------------

LShB_sev_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise(PLOT_LShB_severity = mean(LEAF_ShB), na.rm = TRUE) %>%
  arrange(PLOT)

LShB_sev_wide <-
  reshape2::dcast(LShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_perc_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise(PLOT_LShB_percent = mean(PERC_LEAF_ShB),
            na.rm = TRUE) %>%
  arrange(PLOT)

LShB_perc_wide <-
  reshape2::dcast(LShB_perc_16, PLOT ~ ASMT, value.var = "PLOT_LShB_percent")

# 2016 audpS -------------------------------------------------------------------

TShB_inc_AUDPS <-
  audps(evaluation = TShB_inc_wide[, 2:5],
        dates = pull(TShB_inc_16[1:4, 6]))

TShB_sev_AUDPS <-
  audps(evaluation = TShB_sev_wide[, 2:5],
        dates = pull(TShB_sev_16[1:4, 6]))

LShB_sev_AUDPS <-
  audps(evaluation = LShB_sev_wide[, 2:5],
        dates = pull(LShB_sev_16[1:4, 6]))

TShB_percent_AUDPS <-
  audps(evaluation = TShB_perc_wide[, 2:5],
        dates = pull(TShB_perc_16[1:4, 6]))

LShB_percent_AUDPS <-
  audps(evaluation = LShB_perc_wide[, 2:5],
        dates = pull(LShB_perc_16[1:4, 6]))

AUDPS_16 <-
  as_tibble(
    cbind(
      PLOT = 1:16,
      TShB_inc_AUDPS,
      TShB_sev_AUDPS,
      TShB_percent_AUDPS,
      LShB_sev_AUDPS,
      LShB_percent_AUDPS
    )
  )

# 2016 audpC -------------------------------------------------------------------

TShB_inc_AUDPC <-
  audpc(evaluation = TShB_inc_wide[, 2:5],
        dates = pull(TShB_inc_16[1:4, 6]))

TShB_sev_AUDPC <-
  audpc(evaluation = TShB_sev_wide[, 2:5],
        dates = pull(TShB_sev_16[1:4, 6]))

LShB_sev_AUDPC <-
  audpc(evaluation = LShB_sev_wide[, 2:5],
        dates = pull(LShB_sev_16[1:4, 6]))

TShB_percent_AUDPC <-
  audpc(evaluation = TShB_perc_wide[, 2:5],
        dates = pull(TShB_perc_16[1:4, 6]))

LShB_percent_AUDPC <-
  audpc(evaluation = LShB_perc_wide[, 2:5],
        dates = pull(LShB_perc_16[1:4, 6]))

AUDPC_16 <-
  as_tibble(
    cbind(
      PLOT = 1:16,
      TShB_inc_AUDPC,
      TShB_sev_AUDPC,
      TShB_percent_AUDPC,
      LShB_sev_AUDPC,
      LShB_percent_AUDPC
    )
  )

# add plot numbers to merge with the remaining treatment data
AUDPS_16$PLOT <- as.character(AUDPS_16$PLOT)
AUDPC_16$PLOT <- as.character(AUDPC_16$PLOT)
TShB_inc_16$PLOT <- as.character(TShB_inc_16$PLOT)

ShB_16 <- left_join(TShB_inc_16, AUDPS_16, by = "PLOT")
ShB_16 <- left_join(ShB_16, AUDPC_16, by = "PLOT")
```

# Merge AUDPC and AUDPS Data

``` r
AUDPC_S <- as_data_frame(as_tibble(rbind(ShB_15, ShB_16)))
```

    ## Warning: `as_data_frame()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
AUDPC_S <- separate(
  data = AUDPC_S,
  col = TRT,
  sep = "_",
  into = c("WMGT", "NRTE")
)
AUDPC_S <- mutate_at(.tbl = AUDPC_S,
                     .funs = factor,
                     .vars = c("WMGT", "NRTE"))

# Arrange NRTE values
AUDPC_S$NRTE <-
  fct_relevel(AUDPC_S$NRTE,
              "N0",
              "N100",
              "N120",
              "N60",
              "N180")
```

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

N_rates <- cbind(
  Year,
  `Total N (kg/ha)`,
  `Basal N (kg/ha)`,
  `Tillering N (kg/ha)`,
  `Panicle Initiation N (kg/ha)`
)

row.names(N_rates) <- c("N0", "N100", "N120", "N60", "N180")
```

# Save Processed Data

Save data for inclusion in the package.

``` r
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

use_data(RAW_data,
         compress = "bzip2",
         overwrite = TRUE)
```

    ## ✓ Setting active project to '/Users/adamsparks/Sources/GitHub/Analysis/rice_awd_pests'

    ## ✓ Saving 'RAW_data' to 'data/RAW_data.rda'

    ## ● Document your data (see 'https://r-pkgs.org/data.html')

``` r
use_data(AUDPC_S,
         compress = "bzip2",
         overwrite = TRUE)
```

    ## ✓ Saving 'AUDPC_S' to 'data/AUDPC_S.rda'
    ## ● Document your data (see 'https://r-pkgs.org/data.html')

``` r
use_data(N_rates,
         compress = "bzip2",
         overwrite = TRUE
)
```

    ## ✓ Saving 'N_rates' to 'data/N_rates.rda'
    ## ● Document your data (see 'https://r-pkgs.org/data.html')
