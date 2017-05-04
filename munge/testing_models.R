library(ordinal)
Res.clmm <- clmm(TIL_ShB  ~ WMGT * NRTE + (1 | REP/TRT/SMPL/HILL/TIL/LEAF), data = DS2015)
summary(Res.clmm)
