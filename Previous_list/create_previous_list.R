library(tidyverse)
library(openxlsx)


#Read in AU file

prev_list_AU_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/AU_all_rollup.xlsx",
                                 sheet = 'AU_all')



prev_list_AU <- prev_list_AU_import |> 
  select(AU_ID, Pollutant, Pollu_ID, wqstd_code, period, AU_parameter_category, Rationale) |> 
  rename(prev_category = AU_parameter_category,
         prev_rationale = Rationale)

prev_list_GNIS_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/GNIS_rollup_final.xlsx") 

prev_list_GNIS <- prev_list_GNIS_import |> 
  select(AU_ID, AU_GNIS_Name, Pollutant, Pollu_ID, wqstd_code, period, GNIS_22_category, Rationale) |> 
  rename(prev_GNIS_category = GNIS_22_category,
         prev_GNIS_rationale = Rationale)


prev_list_MLoc_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/MLoc_rollup_final.xlsx")

prev_list_Mloc <- prev_list_MLoc_import |> 
  select(AU_ID, MLocID, Pollutant, Pollu_ID, wqstd_code, period, MLocID_IR_category, Rationale)


prev_list_delist <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/Error Delistings.xlsx")


save(prev_list_AU, prev_list_GNIS, prev_list_Mloc, prev_list_delist, file = 'Previous_list/previous_list.Rdata')
