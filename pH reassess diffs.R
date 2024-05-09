library(openxlsx)
library(tidyverse)


# Read in AL toxics files ---------------------------------------------------------------------------------



AL_toxics_import_old <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Code Outputs/Draft Outputs/Tox_AL.xlsx")
AL_toxics_import_new <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Code Outputs/Draft Outputs/Tox_AL-2024-04-01.xlsx")


# Join together ---------------------------------------------------------------------------------------------------

old_join <- AL_toxics_import_old |> 
  select(AU_ID, Pollu_ID, wqstd_code, period, final_AU_cat, Rationale) |> 
  rename(initial_cat = final_AU_cat,
         initial_rationale = Rationale)



AL_toxics_compare <- AL_toxics_import_new |> 
  left_join(old_join, relationship = "many-to-many") |> 
  filter(final_AU_cat != initial_cat,
         )

write.xlsx(AL_toxics_compare, file = "pH_rerun_diffs-2024-04-01.xlsx")
