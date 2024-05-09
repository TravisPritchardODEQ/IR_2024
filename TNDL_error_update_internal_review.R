library(tidyverse)
library(openxlsx)
library(odeqIRtools)
library(odeqtmdl)


TMDL_errors_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report Internal Review - IR 2024 Internal Review/Internal Review Form.xlsx",
                                sheet = 'TMDL updates') |> 
  mutate(Pollu_ID = as.character(Pollu_ID))



tmdl_actual_periods <- tibble::tribble(
  ~Period, ~Actual_period,
  "Both",   "year_round",
  "Both",     "spawn",
  "Mixed (Both, year_round)",   "year_round",
  "Mixed (Both, year_round)",     "spawn"
)

tmdl_names <- odeqtmdl::tmdl_actions |>
  dplyr::select(action_id, TMDL_name) |>
  dplyr::distinct()



tmdl_au0 <- odeqtmdl::tmdl_au |>
  dplyr::left_join(tmdl_actual_periods, relationship = "many-to-many") |>
  dplyr::rename(TMDL_Period = Period,
                period = Actual_period) |>
  dplyr::left_join(tmdl_names)

TMDLs <- tmdl_au0 %>%
  dplyr::filter(TMDL_scope == "TMDL") |>
  dplyr::left_join(odeqtmdl::tmdl_parameters[, c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(TMDL_status == 'Active') |>
  dplyr::select(AU_ID, TMDL_name, action_id, Pollu_ID, period, TMDL_Period, TMDL_pollutant, TMDL_status) |>
  dplyr::group_by(AU_ID, Pollu_ID, period) |>
  dplyr::summarise(TMDLs =  stringr::str_c(unique(TMDL_name), collapse = "; "),
                   action_ids =  stringr::str_c(unique(action_id), collapse = "; "),
                   TMDL_pollutants = stringr::str_c(unique(TMDL_pollutant), collapse = "; "),
                   TMDL_Periods = stringr::str_c(unique(TMDL_Period), collapse = "; "),
                   TMDL_Periods = stringr::str_c(unique(TMDL_Period), collapse = "; ")) |>
  dplyr::mutate(Pollu_ID = as.character(Pollu_ID))


update_4A <- TMDL_errors_import |>
  dplyr::filter(New.category == '4A') |> 
  dplyr::left_join(TMDLs) |>
  select(AU_ID, Pollu_ID, wqstd_code, period, New.category, TMDLs, action_ids, TMDL_pollutants, TMDL_Periods)

update_5 <- TMDL_errors_import |>
  dplyr::filter(New.category == '5') |> 
  select(AU_ID, Pollu_ID, wqstd_code, period, New.category)

TMDL_updates <- bind_rows(update_4A, update_5) |> 
  mutate(wqstd_code = as.character(wqstd_code)) |> 
  rename(TMDLs.new = TMDLs,
         action_ids.new = action_ids, 
         TMDL_pollutants.new = TMDL_pollutants,
         TMDL_Periods.new = TMDL_Periods)
  


