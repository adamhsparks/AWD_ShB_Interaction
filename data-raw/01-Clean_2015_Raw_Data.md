Clean 2015 Raw Data
================

The raw data are located in the `extdata` folder of the installed *rice.awd.pests* and can be accessed using:

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
```
