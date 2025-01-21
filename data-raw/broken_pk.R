## code to prepare `broken_pk` dataset goes here

library(dplyr)
library(tidyr)
library(dmcognigen)



broken_pk <-
  dmcognigen::dmcognigen_pk %>%
   mutate(
    DV=ifelse(USUBJID %in% c("01-705-1310","01-710-1249") & EVID==0 & TSFD %in% c(2,6,12,24),NA_real_,DV),
    DTTM=if_else(USUBJID %in% c("01-703-1403","01-701-1341") & EVID==0 & TSFD %in% c(1,6,8,48),lubridate::NA_POSIXct_,DTTM),
    DTTM=if_else(USUBJID=="01-709-1326" & EVID==1,lubridate::NA_POSIXct_,DTTM,DTTM)
         ) %>%
  mutate(DELFN=case_when(
    TRT=="PLACEBO"  ~ 7,
    EVID==0 & is.na(DV) ~ 9,
    EVID==0 & is.na(DTTM)  ~ 10,
    USUBJID=="01-709-1326" ~ 14,
    ONUM %in% c(3489,1119,821) ~ 50,
    TRUE ~ 0),
    DELFNC=case_when(DELFN==7 ~ "Placebo Group Not Included in the Analysis",
                     DELFN==9 ~ "Missing Concentration Value",
                     DELFN==10 ~ "Missing Sample Date and/or Time",
                     DELFN==14 ~ "Missing Dose Date or Time",
                     DELFN==50 ~ "Analyst-Identified Outliers",
                     TRUE ~ "Analysis Record")
    )





broken_pk %>%
  cnt(DVID,DVIDC,
      DELFN,DELFNC)

# write -------------------------------------------------------------------

usethis::use_data(broken_pk, overwrite = TRUE)
