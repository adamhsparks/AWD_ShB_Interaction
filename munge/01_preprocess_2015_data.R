# Preprocess 2015 data for all assessments
#

library(readr)
library(tidyr)
library(plyr)

w <- list.files("data/", pattern = "^DS2015_Raw")

reformat <- function(files) {
  x <- read_csv("data/DS2015_Raw_22DAI.csv")

  x_TIL <- gather(x, DISCARD, TIL, TIL1, TIL2, TIL3, TIL4)
  x_SHB <- gather(x, DISCARD, SHB, SHB1, SHB2, SHB3, SHB4)
  x_GL <- gather(x, DISCARD, GL, GL1, GL2, GL3, GL4)
  x_DL <- gather(x, DISCARD, DL, DL1, DL2, DL3, DL4)
  x_SLA <- gather(x, DISCARD, SLA, SLA1, SLA2, SLA3, SLA4)
  x_SLB <- gather(x, DISCARD, SLB, SLB1, SLB2, SLB3, SLB4)
  x_SLC <- gather(x, DISCARD, SLC, SLC1, SLC2, SLC3, SLC4)
  x_SLD <- gather(x, DISCARD, SLD, SLD1, SLD2, SLD3, SLD4)
  x_SLE <- gather(x, DISCARD, SLE, SLE1, SLE2, SLE3, SLE4)
  x_SLF <- gather(x, DISCARD, SLF, SLF1, SLF2, SLF3, SLF4)

  x <- cbind(x_TIL[, c(1:9, 47)],
             x_SHB[, 47],
             x_GL[, 47],
             x_DL[, 47],
             x_SLA[, 47],
             x_SLB[, 47],
             x_SLC[, 47],
             x_SLD[, 47],
             x_SLE[, 47],
             x_SLF[, 47]
  )
}

DS2015 <- ldply(w, reformat)

write_csv(DS2015, "./cache/AWD_2015_Data.csv")
