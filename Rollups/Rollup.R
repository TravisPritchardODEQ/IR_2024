library(tidyverse)
library(openxlsx)
library(odeqIRtools)
library(odeqtmdl)
library(duckdb)




filepath <- 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Code Outputs/Draft Outputs/'

# Filenames -------------------------------------------------------------------------------------------------------

bact_coast <- 'Done-bacteria coast contact- 2024-01-25.xlsx'
bact_fresh <- 'Done-bacteria_freshwater_contact-2024-01-24.xlsx'

chl <- 'Done-chl-a-2024-01-25.xlsx'

DO <- 'Done - DO-2024-01-31.xlsx'

pH <- 'Done pH-2024-02-06.xlsx'

temp <- 'KSE-temperature-2024-01-26.xlsx'

tox_al <- 'Done-Tox_AL.xlsx'

tox_hh <- 'Done-Tox_HH-2024-01-26.xlsx'

turb <- 'Done-turbidity-2024-01-26.xlsx'

biocriteria <- 'Done - Biocriteria-2024-02-02.xlsx'

non_R <- 'Done-non_R-2024-01-25.xlsx'


# Pull data in ----------------------------------------------------------------------------------------------------



## Bacteria --------------------------------------------------------------------------------------------------------



au_bact_coast <- read.xlsx(paste0(filepath, bact_coast),
                           sheet = 'AU_Decisions') |> 
  mutate(across(1:21, .fns = as.character))

# gnis_bact_coast <- read.xlsx(paste0(filepath, bact_coast),
#                              sheet = 'WS GNIS categorization')

au_bact_fresh <- read.xlsx(paste0(filepath, bact_fresh),
                           sheet = 'AU_Decisions') |> 
  mutate(across(1:21, .fns = as.character))

gnis_bact_fresh <- read.xlsx(paste0(filepath, bact_fresh),
                             sheet = 'WS GNIS categorization') |> 
  mutate(across(1:24, .fns = as.character))


## Chl -------------------------------------------------------------------------------------------------------------


au_chl <- read.xlsx(paste0(filepath, chl),
                    sheet = 'AU_Decisions')|> 
  mutate(across(1:21, .fns = as.character))

gnis_chl <- read.xlsx(paste0(filepath, chl),
                      sheet = 'WS GNIS categorization')|> 
  mutate(across(1:24, .fns = as.character))


## DO --------------------------------------------------------------------------------------------------------------

au_do <- read.xlsx(paste0(filepath, DO),
                   sheet = 'AU_Decisions') |> 
  mutate(across(1:21, .fns = as.character))

gnis_do <- read.xlsx(paste0(filepath, DO),
                     sheet = 'WS GNIS categorization')|> 
  mutate(across(1:24, .fns = as.character))


## pH --------------------------------------------------------------------------------------------------------------

au_pH <- read.xlsx(paste0(filepath, pH),
                   sheet = 'AU_Decisions')|> 
  mutate(across(1:21, .fns = as.character))

gnis_pH <- read.xlsx(paste0(filepath, pH),
                     sheet = 'WS GNIS categorization') |> 
  mutate(across(1:24, .fns = as.character))


## temperature -----------------------------------------------------------------------------------------------------


au_temp <- read.xlsx(paste0(filepath, temp),
                     sheet = 'AU_Decisions')|> 
  mutate(Char_Name = 'Temperature, water') |> 
  mutate(across(1:21, .fns = as.character))

gnis_temp <- read.xlsx(paste0(filepath, temp),
                       sheet = 'WS_GNIS_categorization')  |> 
  mutate(Char_Name = 'Temperature, water') |> 
  mutate(across(1:24, .fns = as.character))

## tox AL -----------------------------------------------------------------------------------------------------


au_tox_al <- read.xlsx(paste0(filepath, tox_al),
                       sheet = 'AU_Decisions') |> 
  mutate(across(1:21, .fns = as.character))

gnis_tox_al <- read.xlsx(paste0(filepath, tox_al),
                         sheet = 'GNIS_cat')  |> 
  mutate(across(1:24, .fns = as.character))

## tox HH -----------------------------------------------------------------------------------------------------


au_tox_hh <- read.xlsx(paste0(filepath, tox_hh),
                       sheet = 'AU_Decisions')|> 
  mutate(across(1:21, .fns = as.character))

gnis_tox_hh <- read.xlsx(paste0(filepath, tox_hh),
                         sheet = 'WS GNIS categorization') |> 
  mutate(across(1:24, .fns = as.character))



## turbidity -----------------------------------------------------------------------------------------------------


au_turb <- read.xlsx(paste0(filepath, turb),
                     sheet = 'AU_Decisions') |> 
  mutate(across(1:21, .fns = as.character))

gnis_turb <- read.xlsx(paste0(filepath, turb),
                       sheet = 'WS GNIS categorization') |> 
  mutate(across(1:16, .fns = as.character))




# Biocriteria -----------------------------------------------------------------------------------------------------

au_biocriteria <- read.xlsx(paste0(filepath, biocriteria),
                            sheet = 'AU_Decisions')|> 
  mutate(across(1:21, .fns = as.character))

gnis_biocriteria <- read.xlsx(paste0(filepath, biocriteria),
                              sheet = 'WS GNIS categorization') |> 
  mutate(across(1:24, .fns = as.character))


# non_R -----------------------------------------------------------------------------------------------------------
au_nonR <- read.xlsx(paste0(filepath, non_R),
                     sheet = 'AU_Decisions')|> 
  mutate(across(1:21, .fns = as.character))

gnis_nonR <- read.xlsx(paste0(filepath, non_R),
                       sheet = 'WS GNIS categorization') |> 
  mutate(across(1:24, .fns = as.character))



# Put all together ------------------------------------------------------------------------------------------------


AU_decisions <- bind_rows(au_bact_coast, au_bact_fresh, au_chl, au_do, au_pH, au_temp, au_tox_al, 
                          au_tox_hh, au_turb,au_biocriteria, au_nonR) 


# Get unassessed pollutants to move forward -----------------------------------------------------------------------
assessed_polluids <- AU_decisions$Pollu_ID 



unassessed_params <- odeqIRtools::prev_list_AU |> 
  filter(!Pollu_ID %in% assessed_polluids) |>
  filter(!is.na(Pollu_ID)) |> 
  rename(Char_Name = Pollutant) |> 
  mutate(final_AU_cat = prev_category,
         Rationale = prev_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'AU')

AU_decisions_joined <- AU_decisions |> 
  bind_rows(unassessed_params)


# Missing GNIS ----------------------------------------------------------------------------------------------------

antijoin <- odeqIRtools::prev_list_AU |> 
  anti_join(AU_decisions_joined, by = join_by(AU_ID, Pollu_ID, wqstd_code, period)) |> 
  rename(Char_Name = Pollutant) |> 
  mutate(final_AU_cat = prev_category,
         Rationale = prev_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'AU')


AU_decisions_joined <- AU_decisions_joined |> 
  bind_rows(antijoin)



# pollutant rename ------------------------------------------------------------------------------------------------
#open connection to database
con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


db_qry <- glue::glue_sql( "SELECT distinct [Pollu_ID]
      ,[Pollutant_DEQ WQS] as Char_Name
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)

# Send query to database and return with the data
Char_rename <-  DBI::dbGetQuery(con, db_qry)

Char_rename <- Char_rename |> 
  mutate(Pollu_ID = as.character(Pollu_ID))



AU_decisions <- AU_decisions_joined |> 
  select(-Char_Name) |> 
  left_join(Char_rename) |> 
  relocate(Char_Name, .after = AU_ID)|> 
  select(-AU_Name, -AU_UseCode, -HUC12) |> 
  join_AU_info() |> 
  join_hucs() |> 
  arrange(AU_ID, Char_Name)

# Get assessment labels -------------------------------------------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


wqstd_info <- tbl(con, "LU_Wqstd_Code") |> 
  mutate(wqstd_code = as.character(wqstd_code) ) |> 
  rename('Assessment' = 'wqstd') |> 
  collect()

DBI::dbDisconnect(con)

AU_decisions <- AU_decisions |> 
  left_join(wqstd_info) |> 
  relocate(Assessment, .after = Char_Name)

# GNIS ------------------------------------------------------------------------------------------------------------

GNIS_Decisions <- bind_rows(gnis_bact_fresh, gnis_biocriteria, gnis_chl, gnis_do, gnis_nonR,
                            gnis_pH, gnis_temp, gnis_tox_al, gnis_tox_hh, gnis_turb)



## Get unassessed pollutants to move forward -----------------------------------------------------------------------



assessed_polluids <- GNIS_Decisions$Pollu_ID 



unassessed_params <- odeqIRtools::prev_list_GNIS |> 
  filter(!Pollu_ID %in% assessed_polluids) |>
  rename(Char_Name = Pollutant) |> 
  mutate(final_GNIS_cat = prev_GNIS_category,
         Rationale_GNIS = prev_GNIS_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'GNIS')

GNIS_decisions <- GNIS_Decisions |> 
  bind_rows(unassessed_params)


antijoin2 <- odeqIRtools::prev_list_GNIS |> 
  anti_join(GNIS_decisions, by = join_by(AU_ID, Pollu_ID, wqstd_code, period)) |> 
  rename(Char_Name = Pollutant) |> 
  mutate(final_GNIS_cat = prev_GNIS_category,
         Rationale_GNIS = prev_GNIS_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'GNIS')


GNIS_decisions <- GNIS_Decisions |> 
  bind_rows(antijoin2)

# Get assessment labels -------------------------------------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


wqstd_info <- tbl(con, "LU_Wqstd_Code") |> 
  mutate(wqstd_code = as.character(wqstd_code) ) |> 
  rename('Assessment' = 'wqstd') |> 
  collect()

DBI::dbDisconnect(con)

GNIS_decisions <- GNIS_decisions |> 
  left_join(wqstd_info) |> 
  relocate(Assessment, .after = Char_Name)




# Clear all but needed --------------------------------------------------------------------------------------------

#rm(list=setdiff(ls(), c("GNIS_decisions", 'AU_decisions')))


# Ben use ---------------------------------------------------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

LU_BU_Assessment <- DBI::dbReadTable(con, 'LU_BU_Assessment') %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


# lookup Benuses --------------------------------------------------------------------------------------------------


LU_benuses <- DBI::dbReadTable(con, 'LU_BenUseCode')

names(LU_benuses) <- c("ben_use_code", "ben_use_id", "ben_use")

LU_benuses$ben_use_code <- as.numeric(LU_benuses$ben_use_code)

# Join_benuses ----------------------------------------------------------------------------------------------------

#get a list of AU ben use codes
AU_to_ben_use <- AU_decisions %>%
  select(AU_ID, AU_UseCode) %>%
  distinct()
  mutate(AU_UseCode = as.character(AU_UseCode))
  
  
  
  
  # This is a long form table of all the benefical uses that apply to a given AU
  all_ben_uses <- AU_to_ben_use %>%
    mutate(ben_use_code = as.numeric(AU_UseCode)) %>%
    left_join(LU_benuses) %>%
    filter(!is.na(ben_use),
           ben_use != "NULL")
  
  all_ben_uses_2 <- all_ben_uses |> 
    mutate(keep = "keep") %>%
    select(-ben_use)
  
  
  
  AU_BU <- AU_decisions %>%
    left_join(select(LU_BU_Assessment, -Assessment), by = c("Pollu_ID", 'wqstd_code'), relationship = "many-to-many" ) %>%
    right_join(all_ben_uses_2) %>%
    select(-keep) %>%
    filter(!is.na(Char_Name))
  
  
  
  
  
  BU_rollup <- AU_BU %>%
    mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                               ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                               ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                               ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                               ben_use == "Water Contact Recreation" ~ "water contact recreation",
                               ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                               ben_use == "Livestock Watering" ~ "livestock watering",
                               ben_use == "Boating" ~ "boating",
                               TRUE ~ ben_use
    )) |> 
    mutate(final_AU_cat = factor(final_AU_cat, 
                                 levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A', "5" ), ordered=TRUE)) %>%
    group_by(AU_ID, ben_use) %>%
    summarise(AU_Name = max(AU_Name, na.rm = TRUE),
              Category = max(final_AU_cat),
              parameters = stringr::str_c(unique(Char_Name), collapse = "; ")) %>%
    full_join(filter(all_ben_uses, AU_ID %in% AU_decisions$AU_ID),relationship = "many-to-many") %>%
    mutate(Category = as.character(Category),
           Category = ifelse(is.na(Category), 'Unassessed', Category )) %>%
    select(-ben_use_id) %>%
    group_by(AU_ID) %>%
    mutate(AU_Name = max(AU_Name, na.rm = TRUE)) |> 
    relocate(AU_Name, .after='AU_ID')
  
  
  
  
  BU_rollup_wide <- BU_rollup %>%
    select(-parameters) |> 
    #mutate(Category = ifelse(is.na(Category), "-", Category)) %>%
    spread(ben_use, Category, fill = "-") 
  
  
  

# Map display -----------------------------------------------------------------------------------------------------

  
  maps_display <- AU_decisions |> 
    mutate(final_AU_cat = factor(final_AU_cat, 
                                 levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A', "5" ), ordered=TRUE)) |> 
    mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Char_Name, "- ", period),
                                      wqstd_code == 15 ~  paste0(Char_Name, "- Aquatic Life Toxics"),
                                      wqstd_code == 16 ~  paste0(Char_Name, "- Human Health Toxics"),
                                      TRUE ~ Char_Name
    )) |> 
    group_by(AU_ID) %>%
    summarise(AU_status = case_when(any(str_detect(final_AU_cat, '5') | str_detect(final_AU_cat, '4'))~ 'Impaired',
                                    any(str_detect(final_AU_cat, '2')) ~ "Attaining",
                                    all(str_detect(final_AU_cat, '3')) ~ "Insufficient Data",
                                    TRUE ~ "ERROR"),
              year_last_assessed = max(year_last_assessed, na.rm = TRUE),
              Year_listed = ifelse(AU_status == 'Impaired', as.integer(min(Year_listed),  na.rm = TRUE), NA_integer_ ) ,
    Cat_5_count = length(pollutant_strd[final_AU_cat == '5']),
    Cat_4_count = length(pollutant_strd[str_detect(final_AU_cat, '4')]),
    Impaired_count = Cat_5_count + Cat_4_count,
    Impaired_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & (str_detect(final_AU_cat, '5') | str_detect(final_AU_cat, '4'))]), ", "),
    Cat_2_count = length(pollutant_strd[final_AU_cat == '2']),
    Attaining_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & final_AU_cat == '2']), ", "),
    Cat_3_count = length(pollutant_strd[final_AU_cat == '3']),
    Cat_3B_count = length(pollutant_strd[final_AU_cat == '3B']),
    Cat_3D_count = length(pollutant_strd[final_AU_cat == '3D']),
    Cat_3_count_total = sum(Cat_3_count, Cat_3B_count, Cat_3D_count),
    Insufficient_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & str_detect(final_AU_cat, '3')]), ", ")
    )
    

