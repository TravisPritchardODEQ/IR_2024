library(DBI)
library(tidyverse)
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

no_wqstd_code <- tbl(con, 'InputRaw') |> 
  select(AU_ID, MLocID,StationDes,MonLocType, Lat_DD, Long_DD, ben_use_code, FishCode ) |> 
  filter(is.na(FishCode)) |> 
  distinct() |> 
  collect()
  

library(openxlsx)


write.xlsx(no_wqstd_code, file ='C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Database/mlocs_no_wqstnd.xlsx')
