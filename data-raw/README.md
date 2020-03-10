Clean and Process Raw Data
================

The raw data are located in the `data` folder of the installed
*rice.awd.pests*

*Note* TRT is created here as a combination of the main plot/split plot
treatment. This is used in place of plot numbers to keep the data in
order. However, it is not used in the final analysis as a treatment
effect.

# Load Libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(agricolae)
library(usethis)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

# 2015 experiment

Preprocess 2015 data for all assessment dates

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

# 2016 experiment

Preprocess 2016 data for all assessments

1.  Cleanup date format and fill empty dates
2.  Fill empty NTIL (for same hill, not missing values)
3.  Fill empty NTShB (for same hill, not missing values)
4.  Add column with number of days after inoculation (DAI)

<!-- end list -->

``` r
library(tidyverse)
library(agricolae)
library(usethis)
library(lubridate)

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

# Calculating Disease Severity Index and Area Under the Disease Progress Stairs (AUDPS)

Because the data were collected on an ordinal scale, but not evenly
spaced, the data are converted to the midpoint value of the percent
range for disease severity index and then the AUDPS is calculated from
that.

Last, the data are saved in the package for use analysis, which is
detailed in the package [vingettes](../vignettes).

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

# Calculate disease index values

See K. S. Chiang et al. 2017

DSI<sub>Modified</sub> = Mid<sub><em>Q</em></sub> + <em>R</em>/Total
Number of Observations \* (Mid<sub><em>Q+1</em></sub> -
Mid<sub><em>Q</em></sub>)

``` r
#DS2015 <- 
#  DS2015 %>% 
#  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>% 
#  mutate(TShB_DSI = (TIL_ShB + )/
```

``` r
# calculate AUDPS values -------------------------------------------------------

# 2015 AUDPS -------------------------------------------------------------------

TShB_sev_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_TShB_severity = TIL_ShB)) %>%
  arrange(PLOT)
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
TShB_sev_wide <-
  reshape2::dcast(TShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

TShB_perc_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_TShB_percent = PERC_TIL_ShB)) %>%
  arrange(PLOT)

TShB_perc_wide <-
  reshape2::dcast(TShB_perc_15, PLOT ~ ASMT, value.var = "PLOT_TShB_percent")

# 2015 Leaf Severity setup -----------------------------------------------------
LShB_sev_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_LShB_severity = LEAF_ShB)) %>%
  arrange(PLOT)

LShB_sev_wide <-
  reshape2::dcast(LShB_sev_15, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_perc_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAI, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_LShB_percent = PERC_LEAF_ShB)) %>%
  arrange(PLOT)

LShB_perc_wide <-
  reshape2::dcast(LShB_perc_15, PLOT ~ ASMT, value.var = "PLOT_LShB_percent")

TShB_inc_15 <-
  DS2015 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
               .vars = vars(PLOT_TShB_incidence = TShB_incidence)) %>%
  arrange(PLOT) %>%
  group_by(YEAR, REP, TRT, PLOT) %>%
  summarise(AUDPS = audps(PLOT_TShB_incidence, DAYS))

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

ShB_15 <- left_join(TShB_inc_15, AUDPS_15, by = "PLOT")

# 2016 AUDPS -------------------------------------------------------------------

# 2016 Tiller Incidence setup --------------------------------------------------
TShB_inc_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_TShB_incidence = TShB_incidence)) %>%
  arrange(PLOT)

TShB_inc_wide <-
  reshape2::dcast(TShB_inc_16, PLOT ~ ASMT, value.var = "PLOT_TShB_incidence")

TShB_sev_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_TShB_severity = TIL_ShB)) %>%
  arrange(PLOT)

TShB_sev_wide <-
  reshape2::dcast(TShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_TShB_severity")

# 2016 Tiller Severity setup ---------------------------------------------------

TShB_perc_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_TShB_percent = PERC_TIL_ShB)) %>%
  arrange(PLOT)

TShB_perc_wide <-
  reshape2::dcast(TShB_perc_16, PLOT ~ ASMT, value.var = "PLOT_TShB_percent")

# 2016 Leaf Severity setup -----------------------------------------------------

LShB_sev_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_LShB_severity = LEAF_ShB)) %>%
  arrange(PLOT)

LShB_sev_wide <-
  reshape2::dcast(LShB_sev_16, PLOT ~ ASMT, value.var = "PLOT_LShB_severity")

LShB_perc_16 <-
  DS2016 %>%
  group_by(YEAR, REP, TRT, PLOT, ASMT, DAYS) %>%
  summarise_at(.funs = funs(mean(., na.rm = TRUE)),
                      .vars = vars(PLOT_LShB_percent = PERC_LEAF_ShB)) %>%
  arrange(PLOT)

LShB_perc_wide <-
  reshape2::dcast(LShB_perc_16, PLOT ~ ASMT, value.var = "PLOT_LShB_percent")

# 2016 AUDPS -------------------------------------------------------------------

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

# add plot numbers to merge with the remaining treatment data
AUDPS_16$PLOT <- as.character(AUDPS_16$PLOT)
TShB_inc_16$PLOT <- as.character(TShB_inc_16$PLOT)

ShB_16 <- left_join(TShB_inc_16, AUDPS_16, by = c("PLOT" = "PLOT"))

# Merge AUDPS data for graphing ------------------------------------------------
AUDPS <- as_data_frame(as_tibble(rbind(ShB_15, ShB_16)))
```

    ## Warning: `as_data_frame()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

``` r
AUDPS <- separate(data = AUDPS, col = TRT,
                         sep = "_",
                         into = c("WMGT", "NRTE"))
AUDPS <- mutate_at(.tbl = AUDPS,
                              .funs = factor,
                              .vars = c("WMGT", "NRTE"))

# Arrange NRTE values
AUDPS$NRTE <-
  fct_relevel(AUDPS$NRTE,
                       "N0",
                       "N100",
                       "N120",
                       "N60",
                       "N180")

# Save data for inclusion in the package ---------------------------------------

if (!dir.exists("../data")) {
  dir.create("../data", recursive = TRUE)
}

use_data(RAW_data,
                  compress = "bzip2",
                  overwrite = TRUE)
```

    ## ✔ Setting active project to '/Users/adamsparks/Sources/GitHub/Analysis/rice_awd_pests'
    ## ✔ Saving 'RAW_data' to 'data/RAW_data.rda'

``` r
use_data(AUDPS,
                  compress = "bzip2",
                  overwrite = TRUE)
```

    ## ✔ Saving 'AUDPS' to 'data/AUDPS.rda'

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

use_data(N_rates,
                  compress = "bzip2",
                  overwrite = TRUE)
```

    ## ✔ Saving 'N_rates' to 'data/N_rates.rda'

# Colophon

``` r
sessioninfo::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 3.6.3 (2020-02-29)
    ##  os       macOS Catalina 10.15.3      
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_AU.UTF-8                 
    ##  ctype    en_AU.UTF-8                 
    ##  tz       Australia/Brisbane          
    ##  date     2020-03-09                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version    date       lib source                             
    ##  agricolae   * 1.3-2      2020-01-19 [1] CRAN (R 3.6.0)                     
    ##  AlgDesign     1.2.0      2019-11-29 [1] CRAN (R 3.6.0)                     
    ##  assertthat    0.2.1      2019-03-21 [1] CRAN (R 3.6.0)                     
    ##  backports     1.1.5      2019-10-02 [1] CRAN (R 3.6.0)                     
    ##  broom         0.5.5      2020-02-29 [1] CRAN (R 3.6.0)                     
    ##  cellranger    1.1.0      2016-07-27 [1] CRAN (R 3.6.0)                     
    ##  cli           2.0.2      2020-02-28 [1] CRAN (R 3.6.0)                     
    ##  clisymbols    1.2.0      2017-05-21 [1] CRAN (R 3.6.0)                     
    ##  cluster       2.1.0      2019-06-19 [1] CRAN (R 3.6.3)                     
    ##  colorspace    1.4-1      2019-03-18 [1] CRAN (R 3.6.0)                     
    ##  combinat      0.0-8      2012-10-29 [1] CRAN (R 3.6.0)                     
    ##  crayon        1.3.4      2017-09-16 [1] CRAN (R 3.6.0)                     
    ##  DBI           1.1.0      2019-12-15 [1] CRAN (R 3.6.0)                     
    ##  dbplyr        1.4.2      2019-06-17 [1] CRAN (R 3.6.0)                     
    ##  digest        0.6.25     2020-02-23 [1] CRAN (R 3.6.0)                     
    ##  dplyr       * 0.8.5      2020-03-07 [1] CRAN (R 3.6.3)                     
    ##  ellipsis      0.3.0      2019-09-20 [1] CRAN (R 3.6.0)                     
    ##  evaluate      0.14       2019-05-28 [1] CRAN (R 3.6.0)                     
    ##  fansi         0.4.1      2020-01-08 [1] CRAN (R 3.6.0)                     
    ##  fastmap       1.0.1      2019-10-08 [1] CRAN (R 3.6.0)                     
    ##  forcats     * 0.5.0      2020-03-01 [1] CRAN (R 3.6.0)                     
    ##  fs            1.3.2      2020-03-05 [1] CRAN (R 3.6.0)                     
    ##  generics      0.0.2      2018-11-29 [1] CRAN (R 3.6.0)                     
    ##  ggplot2     * 3.3.0      2020-03-05 [1] CRAN (R 3.6.0)                     
    ##  glue          1.3.2      2020-03-08 [1] Github (tidyverse/glue@5010cc6)    
    ##  gtable        0.3.0      2019-03-25 [1] CRAN (R 3.6.0)                     
    ##  haven         2.2.0      2019-11-08 [1] CRAN (R 3.6.0)                     
    ##  highr         0.8        2019-03-20 [1] CRAN (R 3.6.0)                     
    ##  hms           0.5.3      2020-01-08 [1] CRAN (R 3.6.0)                     
    ##  htmltools     0.4.0      2019-10-04 [1] CRAN (R 3.6.0)                     
    ##  httpuv        1.5.2      2019-09-11 [1] CRAN (R 3.6.0)                     
    ##  httr          1.4.1.9000 2020-03-08 [1] Github (hadley/httr@844c8c7)       
    ##  jsonlite      1.6.1      2020-02-02 [1] CRAN (R 3.6.0)                     
    ##  klaR          0.6-15     2020-02-19 [1] CRAN (R 3.6.0)                     
    ##  knitr         1.28       2020-02-06 [1] CRAN (R 3.6.0)                     
    ##  later         1.0.0      2019-10-04 [1] CRAN (R 3.6.0)                     
    ##  lattice       0.20-38    2018-11-04 [1] CRAN (R 3.6.3)                     
    ##  lifecycle     0.2.0      2020-03-06 [1] CRAN (R 3.6.0)                     
    ##  lubridate   * 1.7.4      2018-04-11 [1] CRAN (R 3.6.0)                     
    ##  magrittr      1.5        2014-11-22 [1] CRAN (R 3.6.0)                     
    ##  MASS          7.3-51.5   2019-12-20 [1] CRAN (R 3.6.3)                     
    ##  mime          0.9        2020-02-04 [1] CRAN (R 3.6.0)                     
    ##  miniUI        0.1.1.1    2018-05-18 [1] CRAN (R 3.6.0)                     
    ##  modelr        0.1.6      2020-02-22 [1] CRAN (R 3.6.0)                     
    ##  munsell       0.5.0      2018-06-12 [1] CRAN (R 3.6.0)                     
    ##  nlme          3.1-144    2020-02-06 [1] CRAN (R 3.6.3)                     
    ##  pillar        1.4.3      2019-12-20 [1] CRAN (R 3.6.0)                     
    ##  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 3.6.0)                     
    ##  plyr          1.8.6      2020-03-03 [1] CRAN (R 3.6.0)                     
    ##  promises      1.1.0      2019-10-04 [1] CRAN (R 3.6.0)                     
    ##  prompt        1.0.0      2020-03-08 [1] Github (gaborcsardi/prompt@b332c42)
    ##  purrr       * 0.3.3      2019-10-18 [1] CRAN (R 3.6.0)                     
    ##  questionr     0.7.0      2018-11-26 [1] CRAN (R 3.6.0)                     
    ##  R6            2.4.1      2019-11-12 [1] CRAN (R 3.6.0)                     
    ##  Rcpp          1.0.3      2019-11-08 [1] CRAN (R 3.6.0)                     
    ##  readr       * 1.3.1      2018-12-21 [1] CRAN (R 3.6.0)                     
    ##  readxl        1.3.1      2019-03-13 [1] CRAN (R 3.6.0)                     
    ##  reprex        0.3.0      2019-05-16 [1] CRAN (R 3.6.0)                     
    ##  reshape2      1.4.3      2017-12-11 [1] CRAN (R 3.6.0)                     
    ##  rlang         0.4.5      2020-03-01 [1] CRAN (R 3.6.0)                     
    ##  rmarkdown     2.1        2020-01-20 [1] CRAN (R 3.6.0)                     
    ##  rprojroot     1.3-2      2018-01-03 [1] CRAN (R 3.6.0)                     
    ##  rstudioapi    0.11       2020-02-07 [1] CRAN (R 3.6.0)                     
    ##  rvest         0.3.5      2019-11-08 [1] CRAN (R 3.6.0)                     
    ##  scales        1.1.0      2019-11-18 [1] CRAN (R 3.6.0)                     
    ##  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 3.6.0)                     
    ##  shiny         1.4.0      2019-10-10 [1] CRAN (R 3.6.0)                     
    ##  stringi       1.4.6      2020-02-17 [1] CRAN (R 3.6.0)                     
    ##  stringr     * 1.4.0      2019-02-10 [1] CRAN (R 3.6.0)                     
    ##  tibble      * 2.1.3      2019-06-06 [1] CRAN (R 3.6.0)                     
    ##  tidyr       * 1.0.2      2020-01-24 [1] CRAN (R 3.6.0)                     
    ##  tidyselect    1.0.0      2020-01-27 [1] CRAN (R 3.6.0)                     
    ##  tidyverse   * 1.3.0      2019-11-21 [1] CRAN (R 3.6.0)                     
    ##  usethis     * 1.5.1      2019-07-04 [1] CRAN (R 3.6.0)                     
    ##  vctrs         0.2.3      2020-02-20 [1] CRAN (R 3.6.0)                     
    ##  withr         2.1.2      2018-03-15 [1] CRAN (R 3.6.0)                     
    ##  xfun          0.12       2020-01-13 [1] CRAN (R 3.6.0)                     
    ##  xml2          1.2.2      2019-08-09 [1] CRAN (R 3.6.0)                     
    ##  xtable        1.8-4      2019-04-21 [1] CRAN (R 3.6.0)                     
    ##  yaml          2.2.1      2020-02-01 [1] CRAN (R 3.6.0)                     
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
