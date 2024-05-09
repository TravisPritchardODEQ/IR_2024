library(tidyverse)
library(openxlsx)
library(odeqtmdl)
library(odeqmloctools)

pastee <- function(x) {paste(sort(na.omit(unique(x))), collapse = "; ")}

# read in AU_deceisions -------------------------------------------------------------------------------------------


AU_decisions_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/DraftList/Public Draft/IR_2024_Rollup-2024-04-10.xlsx")


AU_decisions_strip_TMDL <- AU_decisions_import |> 
    mutate(final_AU_cat = case_when(final_AU_cat %in% c('4A') ~ '5',
                                    TRUE ~ final_AU_cat),
           TMDLs = NA_character_,
           action_ids = NA_character_,
           TMDL_pollutants = NA_character_,
           TMDL_Periods = NA_character_) |> 
  select(-TMDLs, -action_ids, -TMDL_pollutants, -TMDL_Periods)


# Rejoin TMDLS ----------------------------------------------------------------------------------------------------



tmdl_actual_periods <- tibble::tribble(~Period, ~Actual_period,
                                       "year_round", "year_round",
                                       "spawning", "spawn",
                                       "Both", "year_round",
                                       "Both", "spawn",
                                       "Mixed (Both, year_round)", "year_round",
                                       "Mixed (Both, year_round)", "spawn",
                                       NA_character_, NA_character_)

LU_wqstd_ir <- odeqtmdl::LU_wqstd |>
  mutate(wqstd_code_IR = wqstd_code,
         wqstd_code_IR = case_when(Pollu_ID == 126 ~ 26,
                                   Pollu_ID == 173 ~ NA_integer_,
                                   Pollu_ID == 174 ~ 18,
                                   Pollu_ID == 175 ~ NA_integer_,
                                   Pollu_ID == 172 ~NA_integer_,
                                   TRUE ~ wqstd_code_IR)) |>
  rbind(tibble::tribble(~Pollu_ID, ~wqstd_code, ~wqstd_code_IR,
                        174, 11, 19)) |>
  arrange(Pollu_ID, wqstd_code_IR)


tmdl_aus <- odeqtmdl::tmdl_au |>
  filter(!AU_ID == "99") |>
  filter(TMDL_scope == "TMDL") |>
  left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
            by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) |>
  filter(TMDL_status == "Active") |>
  left_join(tmdl_wqstd, relationship = "many-to-many", by = c("action_id", "Pollu_ID")) |>
  left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name")], by = "action_id") |>
  # fixes
  left_join(tmdl_actual_periods, relationship = "many-to-many", by = "Period") |>
  left_join(LU_wqstd_ir, relationship = "many-to-many", by = c("Pollu_ID", "wqstd_code")) |>
  select(-wqstd_code) |>
  rename(TMDL_Period = Period,
         period = Actual_period,
         wqstd_code = wqstd_code_IR,
         Char_Name = TMDL_wq_limited_parameter) |>
  mutate(Char_Name = case_when(Pollu_ID == 126 ~ "Phosphorus",
                               TRUE ~ Char_Name)) |>
  group_by(AU_ID, Char_Name, period, wqstd_code, Pollu_ID) |>
  summarize(action_id = pastee(action_id),
            TMDL_name = pastee(TMDL_name),
            TMDL_pollutants = pastee(TMDL_pollutant),
            TMDL_Periods = pastee(TMDL_Period),
            TMDL_AU_Percent = max(TMDL_AU_Percent)) |> 
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))



tmdl_aus_join <- tmdl_aus |>
  ungroup() |> 
  select(-Char_Name)



# These are the AU IDs addressed by the replacement temperature TMDLs
temp_tmdl_au_ids <- tmdl_au %>%
  filter(TMDL_wq_limited_parameter == "Temperature",
         action_id %in% c("10006", "10791", "11395", "12241", "30674",
                          "30358", "32071", "33829", "35888", "35887",
                          "35890", "9294", "39782", "39294", "39753")) %>%
  pull(AU_ID) %>% unique()


AU_decisions_TMDL <- AU_decisions_strip_TMDL |> 
  left_join(tmdl_aus_join)




AU_decisions <- AU_decisions_TMDL |> 
  mutate(final_AU_cat = case_when(final_AU_cat == '5' & !is.na(action_id) ~ '4A',
                                  TRUE ~final_AU_cat)) |> 
    mutate(final_AU_cat = case_when(Char_Name == "Temperature"
                                  & final_AU_cat == "4A"
                                  & AU_ID %in% temp_tmdl_au_ids ~ "5",
                                  TRUE ~ final_AU_cat),
           action_id =  case_when(Char_Name == "Temperature"
                                  & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                                  TRUE ~ action_id),
           TMDL_name = case_when(Char_Name == "Temperature"
                                 & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                                 TRUE ~ TMDL_name),
           TMDL_pollutants = case_when(Char_Name == "Temperature"
                                       & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                                       TRUE ~ TMDL_pollutants),
           TMDL_Periods = case_when(Char_Name == "Temperature"
                                    & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                                    TRUE ~ TMDL_Periods),
           TMDL_AU_Percent = case_when(Char_Name == "Temperature"
                                       & AU_ID %in% temp_tmdl_au_ids ~ NA_integer_,
                                       TRUE ~ TMDL_AU_Percent),
           
           )





# GNIS ------------------------------------------------------------------------------------------------------------

GNIS_decisions_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/DraftList/Public Draft/IR_2024_Rollup-2024-04-10.xlsx",
                                   sheet = 'GNIS_decisions')

GNIS_decisions_strip_TMDL <- GNIS_decisions_import |> 
  mutate(final_GNIS_cat = case_when(final_GNIS_cat %in% c('4A') ~ '5',
                                  TRUE ~ final_GNIS_cat),
         TMDLs = NA_character_,
         action_ids = NA_character_,
         TMDL_pollutants = NA_character_,
         TMDL_Periods = NA_character_) |> 
  select(-TMDLs, -action_ids, -TMDL_pollutants, -TMDL_Periods)



tmdl_GNISs <- odeqtmdl::tmdl_au_gnis |>
  filter(!AU_ID == "99") |>
  filter(TMDL_scope == "TMDL") |>
  left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
            by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) |>
  filter(TMDL_status == "Active") |>
  left_join(tmdl_wqstd, relationship = "many-to-many", by = c("action_id", "Pollu_ID")) |>
  left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name")], by = "action_id") |>
  # fixes
  left_join(tmdl_actual_periods, relationship = "many-to-many", by = "Period") |>
  left_join(LU_wqstd_ir, relationship = "many-to-many", by = c("Pollu_ID", "wqstd_code")) |>
  select(-wqstd_code) |>
  rename(TMDL_Period = Period,
         period = Actual_period,
         wqstd_code = wqstd_code_IR,
         Char_Name = TMDL_wq_limited_parameter) |>
  mutate(Char_Name = case_when(Pollu_ID == 126 ~ "Phosphorus",
                               TRUE ~ Char_Name),
         AU_ID = case_when(AU_ID == "OR_SR_1707010307_02_102616" ~ "OR_SR_1707010308_02_102616",
                           TRUE ~ AU_ID)) |>
  group_by(AU_ID,AU_GNIS_Name, Char_Name, period, wqstd_code, Pollu_ID) |>
  summarize(action_id = pastee(action_id),
            TMDL_name = pastee(TMDL_name),
            TMDL_pollutants = pastee(TMDL_pollutant),
            TMDL_Periods = pastee(TMDL_Period)) |> 
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))




tmdl_GNISs_join <- tmdl_GNISs |>
  ungroup() |> 
  select(-Char_Name)



GNIS_decisions_TMDL <- GNIS_decisions_strip_TMDL |> 
  left_join(tmdl_GNISs_join)




GNIS_decisions <- GNIS_decisions_TMDL |> 
  mutate(final_GNIS_cat = case_when(final_GNIS_cat == '5' & !is.na(action_id) ~ '4A',
                                  TRUE ~final_GNIS_cat)) |> 
  mutate(final_GNIS_cat = case_when(Char_Name == "Temperature"
                                 & final_GNIS_cat == "4A"
                                 & AU_ID %in% temp_tmdl_au_ids ~ "5",
                                 TRUE ~ final_GNIS_cat),
        action_id =  case_when(Char_Name == "Temperature"
                               & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                               TRUE ~ action_id),
        TMDL_name = case_when(Char_Name == "Temperature"
                              & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                              TRUE ~ TMDL_name),
        TMDL_pollutants = case_when(Char_Name == "Temperature"
                                    & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                                    TRUE ~ TMDL_pollutants),
        TMDL_Periods = case_when(Char_Name == "Temperature"
                                 & AU_ID %in% temp_tmdl_au_ids ~ NA_character_,
                                 TRUE ~ TMDL_Periods)
        
  )
  

reprint <- list('AU_decisions' = AU_decisions,
                'GNIS_decisions' = GNIS_decisions)


write.xlsx(reprint, paste0("TMDL_update-", Sys.Date(), ".xlsx"))



test <- tmdl_aus |> 
  filter(Pollu_ID == 126)


write.xlsx(test, file = 'phos_TMDL.xlsx')
