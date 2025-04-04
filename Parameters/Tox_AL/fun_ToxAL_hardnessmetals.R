library(tidyverse)
library(odeqIRtools) 
library(odbc)
library(DBI)
library(glue)




Hardness_based_metals <- function(database){
  

# Testing ---------------------------------------------------------------------------------------------------------

#database <- "IR_Dev"  
  


# Database fetch --------------------------------------------------------------------------------------------------


  print("Fetch metals data from IR database")
  
  #open connection to database
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  #Build query language to get Pentachlorophenol data out. this grabs the IR 2018 db view [dbo].[VW_Pentachlorophenol]
  
  db_qry <- glue::glue_sql( "SELECT *
  FROM [IntegratedReport].[dbo].[VW_metals]
  WHERE AU_ID != '99'", .con = con)
  
  # Send query to database and return with the data
  Results_import <-  DBI::dbGetQuery(con, db_qry)
  
  Results_import_no_NAs <- Results_import %>%
    filter(!is.na(MLocID))
  
  Results_import_no_NAs <- odeqIRtools::data_aggregation(Results_import_no_NAs)
  
  print(paste("Returned", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations"))
  
  #Create a vector of monitoring locations with metals data. This list is used as a filter for the hardness query
  mlocs <- unique(Results_import_no_NAs$MLocID)
  
  # chr_uids for hardness based metals ancillary data
  # Hardness = 1097
  # Ca = 727
  # mg = 1244
  print("Fetch ancillary data from IR database")
  ancillary_qry <- glue::glue_sql("SELECT [MLocID]
                                ,[SampleStartDate]
                                ,[Char_Name]
                                ,[Char_Speciation]
                                ,[Sample_Fraction]
                                ,[IRResultNWQSunit]
                                ,[Result_Depth]
                                ,[IRWQSUnitName]
                                ,[Result_UID]
                                FROM [IntegratedReport].[dbo].[ResultsRawWater]
                                WHERE chr_uid in (1097, 727, 1244) 
                                AND (Statistical_Base IS NULL)
                                AND MLocID in ({mlocs*})
                                AND IRResultNWQSunit IS NOT NULL", .con = con)
  
  #Query to get ancillary data
  Results_ancillary <- DBI::dbGetQuery(con, ancillary_qry)
  
  exclude_data <- tbl(con, 'Unused_Results') |> 
    select(Result_UID) |> 
    collect() |> 
    pull(Result_UID)
  
  Results_ancillary <- Results_ancillary |> 
    filter(!Result_UID %in% exclude_data)
  
  
  
  print("Joining ancillary data")
  # remove suspended fraction, if any
  # Simplify Sample fraction to either Total or Dissolved
  # Set new char name to incorporate sample fraction, this makes for easier to read column headers
  # Remove columns that ae not needed in acillary data
  # group by parameters that define single sample
  # Choose only the first result for that day
  # Spread the data from long format to wide format
  spread <- Results_ancillary %>%
    filter(!Sample_Fraction %in% c("Suspended")) %>%
    mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction %in% c("Total Recoverable", "Acid Soluble") | is.na(Sample_Fraction), 'Total', Sample_Fraction )) %>%
    mutate(Char_Name = paste(Char_Name, "-", Simplfied_Sample_Fraction)) %>%
    select(-Sample_Fraction, -Char_Speciation, -IRWQSUnitName) %>%
    group_by(MLocID, SampleStartDate,Char_Name,Result_Depth  ) %>%
    summarise(result = first(IRResultNWQSunit)) %>%
    arrange(MLocID, SampleStartDate) %>%
    spread(key = Char_Name, value = result) 
  
  # Make the column names a bit easier to work with 
  names(spread) <- gsub(" ", "", names(spread))
  names(spread) <- gsub(",|-", "_", names(spread))
  
  
  
  # Get hardness value to be used in assessment -----------------------------
  
  # default to using total Hardness as CaCO3, if we have it
  # Use dissolved Hardness as CaCO3, if we have it
  # If we don't have either, use total calcium and Magnesium
  # use dissolved fractions of Ca and Mg, if we don't have total
  Hardness <- spread %>%
    mutate(Hardness = ifelse(!is.na(Hardness_Ca_Mg_Total), Hardness_Ca_Mg_Total, 
                             ifelse(!is.na(Hardness_Ca_Mg_Dissolved), Hardness_Ca_Mg_Dissolved, 
                                    ifelse(!is.na(Calcium_Total) & !is.na(Magnesium_Total), 2.497*Calcium_Total + 4.1189*Magnesium_Total, 
                                           ifelse(!is.na(Calcium_Total) & !is.na(Magnesium_Dissolved) & is.na(Magnesium_Total) , 2.497*Calcium_Total + 4.1189*Magnesium_Dissolved, 
                                                  ifelse(!is.na(Calcium_Dissolved) & is.na(Calcium_Total) & !is.na(Magnesium_Total), 2.497*Calcium_Dissolved + 4.1189*Magnesium_Total, 
                                                         ifelse(!is.na(Calcium_Dissolved) & is.na(Calcium_Total) & !is.na(Magnesium_Dissolved) & is.na(Magnesium_Total), 2.497*Calcium_Dissolved +4.1189*Magnesium_Dissolved, 
                                                                NA )))))))
  
  # Join ancillary data to metals data --------------------------------------
  
  # Join data together 
  # get default hardness values, if we don't have a measured (or calculated) hardness
  # get value of hardness to use in calculating criteria
  # if hardness is > 400, set at 400
  metals_hardness <- Results_import_no_NAs %>%
    left_join(Hardness, by = c('MLocID', 'SampleStartDate', 'Result_Depth')) %>%
    mutate(default_hardness = ifelse(AU_ID %in% c('REPLACE THIS', 'WITH', 'AU_IDs', "OF COLUMBIA MAIN STEM"), 48.7, 
                                     ifelse(EcoRegion3 == 11, 21.7,
                                            ifelse(EcoRegion3 == 4, 10.0, 
                                                   ifelse(EcoRegion3 == 1, 14.5, 
                                                          ifelse(EcoRegion3 == 10, 23.4, 
                                                                 ifelse(EcoRegion3 == 9, 19.3, 
                                                                        ifelse(EcoRegion3 == 78, 28.5,
                                                                               ifelse(EcoRegion3 == 80, 32.3,
                                                                                      ifelse(EcoRegion3 == 12, 80.9, 
                                                                                             ifelse(EcoRegion3 == 3, 25.0, NA )))))))))),
           crit_hardness = ifelse(is.na(Hardness), default_hardness, Hardness )) %>%
    mutate(crit_hardness = ifelse(crit_hardness > 400, 400, crit_hardness ))
  
  
  
  
  #Build constants table to use to join
  constants <- data.frame("Char_Name" = c('Cadmium', 'Chromium', 'Lead', 'Nickel', 'Silver', 'Zinc'),
                          "ma" = c(NA, 0.8190, 1.273, 0.8460, 1.72, 0.8473),
                          "ba" = c(NA, 3.7256, -1.460, 2.255, -6.59, 0.884),
                          "mc" = c(0.7409, .08190, 1.273, 0.8460, NA, 0.8473),
                          'bc' = c(-4.719, 0.6848, -4.705, 0.0584, NA, 0.884), stringsAsFactors = FALSE)
  
  
  # join metals data with constants table
  # Get the CF constant value
  # get criteria, for silver, if the acute criteria is lower than the chronic, use acute, else use chronic
  Hardness_analysis <- metals_hardness %>%
    left_join(constants, by = "Char_Name") %>%
    mutate(CF = ifelse(Char_Name == 'Cadmium', 1.101672 -  (log(crit_hardness) * 0.041838), 
                       ifelse(Char_Name == 'Chromium', 0.860, 
                              ifelse(Char_Name == 'Lead', 1.46203 - (log(crit_hardness) * 0.145712), 
                                     ifelse(Char_Name == 'Nickel', 0.997, 
                                            ifelse(Char_Name == 'Silver', 0.85, 
                                                   ifelse(Char_Name == 'Zinc', 0.986, "ERROR" ) )))))) %>%
    mutate(CF = as.numeric(CF)) %>%
    mutate(crit = ifelse(Char_Name == 'Silver', pmin(0.10, (exp(ma*log(crit_hardness)+ba)*CF)), 
                         exp(mc*log(crit_hardness)+bc)*CF ),
           crit = ifelse(WaterTypeCode %in% c(1,3), pmin(Chronic_SW, Acute_SW, na.rm = TRUE), crit ))
  
  
  print('Begin analysis')
  
  
  # Do the data censoring and deal with samples with both total and dissolved on same date and time
  # Group by single sample event (mloc, date, time, char, and depth)
  # Flag if the group contains a dissolved fraction sample
  # Keep only results that have a dissolved fraction and are dissolved, or keep total if
  #group does not have a dissolved (where where have dissolved, remove total fractions)
  # Use the conversion factor to transform total results to dissolved results
  Results_censored <- censor_data(Hardness_analysis ) %>%
    mutate(Result_cen = as.numeric(Result_cen)) %>%
    mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction ==  "Dissolved",  "Dissolved", "Total" )) %>%
    group_by(MLocID, SampleStartDate, Char_Name,Result_Depth) %>%
    mutate(Has_Crit_Fraction = ifelse(Crit_fraction == "Total" & max(Simplfied_Sample_Fraction) == "Total", 1, 
                                      ifelse(Crit_fraction == "Dissolved" & min(Simplfied_Sample_Fraction) == "Dissolved", 1, 0 ))) %>%
    # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
    ungroup() %>%
    filter((Has_Crit_Fraction == 1 & Simplfied_Sample_Fraction == Crit_fraction) | Has_Crit_Fraction == 0) %>%
    mutate(excursion = ifelse(Result_cen > crit , 1, 0 ))
  
  
  # We made the decision that CF values were not "site_specific translotors, and we do not convert total fraction
  # to dissolved fraction. We follow the dissolved criteria rules on page 56 of the IR methodology.
  
  # If total sammple is less than the dissolved crit, it is considered valid to determine attainment. If total recoverable
  # is greater than a dissolved crit, 3b may be assigned if there are no other dissolved samples to indicate impairment
  
  # IR_export(Results_censored, "Parameters/Tox_AL/Data_Review/", "TOX_AL_Hardness_Metals", "Data")
  
  AL_tox_harndess_assess_fun <- function(df_data = Results_censored, AU_type){
    
    
    # Testing ---------------------------------------------------------------------------------------------------------
    # df_data = Results_censored
    # AU_type = 'other'
    # 
    if(AU_type == "other"){  
      group1 <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'Char_Name')
      
    
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name',  'Pollu_ID', 'wqstd_code', 'Char_Name' )
      

      inverse <- FALSE
    }
  
  Results_tox_AL_HBM_cats <- df_data %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    group_by_at(group1) %>%
    #Summarise data
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              criteria_fraction = first(Crit_fraction),
              num_samples = n(),
              num_sample_days = n_distinct(SampleStartDate),
              percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
              num_fraction_types = n_distinct(Simplfied_Sample_Fraction),
              num_samples_total_fraction = sum(Simplfied_Sample_Fraction == "Total"),
              num_Samples_dissolved_fraction = sum(Simplfied_Sample_Fraction == "Dissolved"),
              num_excursions_all = sum(excursion),
              num_excursions_total_fraction = sum(excursion[Simplfied_Sample_Fraction == "Total"]),
              num_excursions_dissolved_fraction = sum(excursion[Simplfied_Sample_Fraction == "Dissolved"]),
              num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction, 
                                                       num_Samples_dissolved_fraction + (num_samples_total_fraction - num_excursions_total_fraction )),
              critical_excursions = binomial_excursions(num_samples_crit_excursion_calc, type = "Toxics")) %>%
    # Assign categories
    mutate( IR_category = case_when(percent_3d == 100 ~ "3D",
                                   num_samples >= 2 & criteria_fraction == "Dissolved" & num_excursions_dissolved_fraction >= critical_excursions ~ "5",
                                   num_samples >= 2 & criteria_fraction == "Total" & num_excursions_all >= critical_excursions ~ "5",
                                   num_samples < 10 & num_excursions_all >= 1 ~ "3B",
                                   criteria_fraction == "Dissolved" & num_excursions_total_fraction > 0 & num_Samples_dissolved_fraction == 0 ~ "3B",
                                   (num_samples_crit_excursion_calc < 10 | num_samples < 10 | num_sample_days < 10)  ~ "3",
                                  
                                   num_samples >= 10 & num_excursions_dissolved_fraction < critical_excursions ~ "2"
                                   ),
           Rationale = case_when(percent_3d == 100 ~ paste0("Insufficient data: All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                 
                                 num_samples >= 2 & criteria_fraction == "Dissolved" & num_excursions_dissolved_fraction >= critical_excursions ~  paste0("Impaired: ", num_excursions_dissolved_fraction,
                                                                                                                                      " excursion of dissolved fraction results. ",
                                                                                                                                      num_samples, " total samples"),
                                 num_samples >= 2 & criteria_fraction == "Total" & num_excursions_all >= critical_excursions ~  paste0(num_excursions_all, " excursions is less than ",
                                                                                                                   critical_excursions, " needed to list- ",
                                                                                                                   num_samples, " total samples"),
                                 num_samples < 10 & num_excursions_all >= 1 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                      " excursion of criteria with ",
                                                                                      num_samples, " total samples"),
                                 criteria_fraction == "Dissolved" & num_excursions_total_fraction > 0 & num_Samples_dissolved_fraction == 0 ~ paste0("Insufficient data: ", "Only total fraction results available, criteria is 'Dissolved' ",
                                                                                                                                                     num_excursions_all, " total excursions of ", 
                                                                                                                                                     num_samples, " total samples"),
                                 (num_samples_crit_excursion_calc < 10 | num_samples < 10 | num_sample_days < 10)  ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                                         " excursion of criteria with ",
                                                                                                         num_samples, " total samples. ", num_sample_days,
                                                                                                         " total sample days"),
                                 
                                 num_samples >= 10 & num_excursions_dissolved_fraction < critical_excursions ~ paste0("Attaining: ", num_excursions_all, " excursions is less than ",
                                                                                                 critical_excursions, " needed to list- ",
                                                                                                 num_samples, " total samples")
           )) %>%
    mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE)) |> 
    mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code)) |> 
    mutate(period = NA_character_) |> 
    mutate(Delist_eligability = case_when(num_samples >= 18 & num_excursions_dissolved_fraction <= binomial_delisting(num_samples, 'Toxics')  ~ 1,
                                          TRUE ~ 0)) 
  
  
  #Deal with Chromium
  
  Results_tox_AL_HBM_cats_chrom <- Results_tox_AL_HBM_cats |> 
    mutate(Rationale = case_when(Char_Name == 'Chromium' ~ paste0("Total Chromium assessed as Chromium(III). ", Rationale),
                                 TRUE ~ Rationale),
           Char_Name = case_when(Char_Name == 'Chromium' ~ "Chromium(III)",
                                 TRUE ~Char_Name ),
           Pollu_ID = case_when(Pollu_ID == 188 ~ 42,
                                TRUE ~ Pollu_ID),
    )
             
    
  return(Results_tox_AL_HBM_cats_chrom)
  
  }
 

# char rename -----------------------------------------------------------------------------------------------------


    con <- DBI::dbConnect(odbc::odbc(), database)

  db_qry <- glue::glue_sql( "SELECT distinct [Pollu_ID]
      ,[Pollutant_DEQ WQS] as Char_Name
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)
  
  # Send query to database and return with the data
  Char_rename <-  DBI::dbGetQuery(con, db_qry) |> 
    mutate(Pollu_ID = as.character(Pollu_ID))
  

# Watershed Assessment --------------------------------------------------------------------------------------------

  
  
  AL_Tox_Hard_WS <- AL_tox_harndess_assess_fun(df_data = Results_censored, AU_type = "WS")
  ## GNIS rollup -----------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup <- AL_Tox_Hard_WS %>%
    ungroup() %>%
    group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
    summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
              IR_category_GNIS_24 = max(IR_category),
              Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
              Delist_eligability = max(Delist_eligability)) %>% 
    mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
                                          TRUE ~ 0)) |> 
    mutate(IR_category_GNIS_24 = factor(IR_category_GNIS_24, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
    mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  
  
  WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS") |> 
    select(-Char_Name) |> 
    left_join(Char_rename) |> 
    relocate(Char_Name, .after = AU_GNIS_Name)
  ### Delist process --------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')
  
  ## AU Rollup -------------------------------------------------------------------------------------------------------
  
  
  WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup, char_name_field = Char_Name) 
  WS_AU_rollup <- WS_AU_prev_list(WS_AU_rollup) 
  

# Other assessment ------------------------------------------------------------------------------------------------

  
  AL_Tox_Hard_other <- AL_tox_harndess_assess_fun(df_data = Results_censored, AU_type = "other")
  
  
  other_category <- join_prev_assessments(AL_Tox_Hard_other, AU_type = 'Other')|> 
    select(-Char_Name) |> 
    left_join(Char_rename) |> 
    relocate(Char_Name, .after = AU_ID)
  
  other_category_delist <-  assess_delist(other_category, type = "Other")  
  
  
  # prep data for export --------------------------------------------------------------------------------------------
  
  AU_display_other <- other_category_delist |> 
    select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)
  
  AU_display_ws <- WS_AU_rollup |> 
    rename(prev_category = prev_AU_category,
           prev_rationale = prev_AU_rationale,
           final_AU_cat = IR_category_AU_24,
           Rationale = Rationale_AU)
  
  AU_display <- bind_rows(AU_display_other, AU_display_ws)|> 
    mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                                 .default = Rationale))|> 
    join_TMDL(type = 'AU')|> 
    join_AU_info() |> 
    relocate(prev_category, .after = year_last_assessed) |> 
    relocate(prev_rationale, .after = prev_category) |> 
    mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2024",
                                          TRUE ~ year_last_assessed)) |> 
    mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2024',
                                   TRUE ~  Year_listed)) 
  
  
  WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
    join_TMDL(type = 'GNIS') |> 
    join_AU_info()|> 
    relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
    relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
    relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  
  
  

# Export ----------------------------------------------------------------------------------------------------------

  Results_tox_hard_AL <- list(data = Results_censored,
                              AU_Decisions = AU_display,
                              Other_AU_categorization = other_category_delist,
                              WS_Station_cat = AL_Tox_Hard_WS,
                              WS_GNIS_cat = WS_GNIS_rollup_delist)
  
  

  return(Results_tox_hard_AL)
  
}
