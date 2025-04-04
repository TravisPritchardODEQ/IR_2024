

TOX_AL_analysis <- function(df, database = "IR_Dev"){
 

# Testing ---------------------------------------------------------------------------------------------------------
# 
# df <- Tox_AL_Censored_data 
# database <- "IR_Dev"
#    


# pollutant rename ------------------------------------------------------------------------------------------------
  #open connection to database
  con <- DBI::dbConnect(odbc::odbc(), database)

  
  db_qry <- glue::glue_sql( "SELECT distinct [Pollu_ID]
      ,[Pollutant_DEQ WQS] as Char_Name
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)
  
  # Send query to database and return with the data
  Char_rename <-  DBI::dbGetQuery(con, db_qry)
  
 # Pull data out for summing -----------------------------------------------
  
  # DDT ---------------------------------------------------------------------
  
  DDT_data <- df %>%
    filter(Pollu_ID %in% c(48,49,50)) %>%
    mutate(summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen),
           is_3d = ifelse(Result_Operator == "<" & IRResultNWQSunit > ifelse(WaterTypeCode == 2, 
                                                                             pmin(Acute_FW, Chronic_FW, na.rm = TRUE), 
                                                                             pmin(Acute_SW, Chronic_SW, na.rm = TRUE)), 1, 0)) %>%
    group_by(OrganizationID, MLocID, SampleStartDate, SampleStartTime, Analytical_method, act_depth_height) %>%
    mutate(IR_note = "Sum of DDT and metabolites",
           Summed_values = sum(summed_censored_value),
           summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
           summed_percent_3d = round(sum(is_3d == 1)/n()*100)) %>%
    # Keep only the first row. This preserves all the metadata
    filter(row_number() == 1) %>%
    # Change the Char_Name to DDT and the Result_cen column to the summed value
    mutate(Char_Name = "DDT",
           Result_cen = Summed_values) %>%
    # get rid of extra columns that were created
    select(-Summed_values, -summed_censored_value, -is_3d)
  
  
  # Endosulfan data ---------------------------------------------------------
  
  
  endosulfan_data <- df %>%
    # Filter for endosulfan Pollu_IDs
    filter(Pollu_ID %in% c(77,78,79 )) %>%
    # Set a flag for if the value is total_endosulfan
    mutate(is_total_endosulfan = ifelse(Pollu_ID == 77, 1, 0 ),
           summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen),
           is_3d = ifelse(Result_Operator == "<" & IRResultNWQSunit > ifelse(WaterTypeCode == 2, 
                                                                             pmin(Acute_FW, Chronic_FW, na.rm = TRUE), 
                                                                             pmin(Acute_SW, Chronic_SW, na.rm = TRUE)), 1, 0)) %>%
    # Set a group that identifies a single sample
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # Flag if the group has a total endosulfan result 
    mutate(Has_total_endosulfan = ifelse(max(is_total_endosulfan) == 1, 1, 0)) %>%
    # undo the grouping so the filter works properly
    #I'm not sure this is needed or not, just to be safe
    ungroup() %>%
    # remove alpha and beta componants if the group has a total
    filter((Has_total_endosulfan == 1 & is_total_endosulfan == 1) | Has_total_endosulfan == 0) %>%
    # regroup
    group_by(OrganizationID, MLocID, SampleStartDate,SampleStartTime, Analytical_method, act_depth_height) %>%
    # Count the number of endosulfan types in the group
    # this is needed to be sure that when we are summing the inidividual componants
    # we have both of them
    mutate(num_types = n_distinct(Char_Name),
           summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
           summed_percent_3d = round(sum(is_3d == 1)/n()*100)) %>%
    #Filter out the results where the group has only alpha or beta, but not both
    filter(Pollu_ID == 77 | num_types == 2 ) %>%
    # Create a comment field so we know where we did the summing
    # Also add the values in the group to get the summed value
    # All values in the group willget this value
    mutate(IR_note = ifelse(Pollu_ID %in% c(78,79), "Sum of alpha and beta endosulfan", "" ),
           Summed_values = sum(summed_censored_value)) %>%
    # Keep only the first row. This preserves all the metadata
    filter(row_number() == 1) %>%
    # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
    mutate(Char_Name = "Endosulfan",
           Result_cen = Summed_values,
           Pollu_ID = 77) %>%
    # get rid of extra columns that were created
    select(-Summed_values,  -num_types,  -Has_total_endosulfan, -is_total_endosulfan, -summed_censored_value, -is_3d)
  
  
  
  # Chlordane ---------------------------------------------------------------
  
  # Get only chlordate data
  # mark if result is  total chlordane  
  Chlordane <- df %>%
    filter(Pollu_ID %in% c(27)) %>%
    mutate(is_total = ifelse(chr_uid %in% c(767), 1, 0 ),
           summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen),
           is_3d = ifelse(Result_Operator == "<" & IRResultNWQSunit > ifelse(WaterTypeCode == 2, 
                                                                             pmin(Acute_FW, Chronic_FW, na.rm = TRUE), 
                                                                             pmin(Acute_SW, Chronic_SW, na.rm = TRUE)), 1, 0)) %>%
    # Set a group that identifies a single sample
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # Flag if the group has a total endosulfan result 
    mutate(has_total_chlordane = ifelse(max(is_total) == 1, 1, 0)) %>%
    # remove isomers, metabolites, etc if the group has a total
    filter((has_total_chlordane == 1 & is_total == 1) | has_total_chlordane == 0) %>%
    mutate( summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
    mutate(IR_note = ifelse(chr_uid != 767, "Sum of isomers, metabolites, and other constituents", "" ),
           Summed_values = ifelse(is_total == 1, IRResultNWQSunit, sum(summed_censored_value) ),
           summed_percent_3d = round(sum(is_3d == 1)/n()*100)) %>%
    # Keep only the first row. This preserves all the metadata
    filter(row_number() == 1) %>%
    # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
    mutate(Char_Name = "Chlordane",
           Result_cen = Summed_values) %>%
    select(-Summed_values, -is_total, -summed_censored_value,-has_total_chlordane, -is_3d )
  
  # PCB data ----------------------------------------------------------------
  
  PCB_data <- df  %>%
    filter(Pollu_ID == '153') %>%
    #Identufy the aroclors
    mutate(is_aroclor = ifelse(chr_uid %in% c('575','578','580','582','583','586','587'), 1, 0 )) %>% #These are the uid for the Arochlors
    # Group by org, mloc, date, and depth to identify sampling event
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # Flag of the grouping has an arochlor sample
    mutate(Has_aroclor = ifelse(max(is_aroclor) == 1, 1, 0)) %>%
    # Undo the grouping
    ungroup() %>%
    # keep the type (aroclor or congener) that has the least amount of non-detects by percentage
    # Group by the same as above, but add in the is_arochlor flag
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height, is_aroclor) %>%
    # Calculate the percent nondetect of each group
    mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
    #undo the grouping
    ungroup() %>%
    # redo the original single sample grouping
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # remove individual congeners if the group has arochlor data & the aroclors have a lower percentage of nondetects
    filter((Has_aroclor == 1 & is_aroclor == 1 & summed_percent_nondetect == min(summed_percent_nondetect)) | Has_aroclor == 0) %>%
    # Recalculate the percent censored values
    mutate(summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen ),
           is_3d = ifelse(Result_Operator == "<" & IRResultNWQSunit > ifelse(WaterTypeCode == 2,
                                                                             pmin(Acute_FW, Chronic_FW, na.rm = TRUE),
                                                                             pmin(Acute_SW, Chronic_SW, na.rm = TRUE)), 1, 0)) %>%
    mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
           # Do the summing
           Summed_values = sum(summed_censored_value),
           summed_percent_3d = round(sum(is_3d == 1)/n()*100),
           # Create note on what the summing is based on
           IR_note = ifelse(Has_aroclor ==  1, "PCB - Sum of Aroclors",
                            ifelse(Has_aroclor ==  0, "PCB - Sum of congeners", "ERROR" )),
           Result_Operator = max(Result_Operator)
    ) %>%
    # Keep only the first row. This preserves all the metadata
    filter(row_number() == 1) %>%
    # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
    mutate(Char_Name = "PCBs",
           Result_cen = Summed_values) %>%
    # get rid of extra columns that were created
    select(-Summed_values,  -Has_aroclor,  -is_aroclor, -summed_censored_value, -is_3d)
  

# Chromium --------------------------------------------------------------------------------------------------------

Chromium_data <- df  %>%
    filter(Pollu_ID %in%  c('43', '188')) |> 
    mutate(is_chrom_IV = case_when(Char_Name == 'Chromium(VI)' ~ 1,
                                    TRUE ~ 0)) |> 
    group_by(AU_ID) |> 
    mutate(has_chrom_IV = ifelse(max(is_chrom_IV) == 1, 1, 0)) %>%
    filter((has_chrom_IV == 1 & is_chrom_IV == 1) | has_chrom_IV == 0) %>%
    mutate(IR_note = ifelse(Pollu_ID %in% c(188), "Total Chromium assessed as Chromium VI due to lack of chromium IV data in AU", "" )) |> 
    select(-is_chrom_IV, -has_chrom_IV)
             
  
  # Put data back together --------------------------------------------------
  
  
  results_analysis <- df %>%
    filter(!Pollu_ID %in% c(77,78,79 )) %>%
    filter(Pollu_ID != 153) %>%
    filter(!Pollu_ID %in% c(48,49,50)) %>%
    filter(Pollu_ID != 27) %>%
    filter(!Pollu_ID %in% c(43,188)) %>%
    bind_rows(endosulfan_data) %>%
    bind_rows(PCB_data) %>%
    bind_rows(DDT_data) %>%
    bind_rows(Chlordane) |> 
    bind_rows(Chromium_data)
  
  
  Results_tox_AL_analysis <- results_analysis %>%
    arrange(OrganizationID, MLocID, Char_Name, SampleStartDate,SampleStartTime) %>%
    # Create column for simplfied version of sample fraction
    # THese distinctions came from Sara Krepps
    mutate(Simplified_sample_fraction = ifelse(Sample_Fraction %in% c("Total", "Extractable",
                                                                      "Total Recoverable","Total Residual", 
                                                                      "None", "volatile", "Semivolatile")  |
                                                 is.na(Sample_Fraction), 'Total', 
                                               ifelse(Sample_Fraction == "Dissolved"  |
                                                        Sample_Fraction == "Filtered, field"  |
                                                        Sample_Fraction == "Filtered, lab"  , "Dissolved", "Error"))) %>%
    group_by(OrganizationID, MLocID, Char_Name, SampleStartDate,SampleStartTime, act_depth_height) %>%
    # If group has criteria fraction match, mark 1,itherwise mark with 0
    mutate(Has_Crit_Fraction = ifelse(Fraction == "Total" & max(Simplified_sample_fraction) == "Total", 1, 
                                      ifelse(Fraction == "Dissolved" & min(Simplified_sample_fraction) == "Dissolved", 1, 0 ))) %>%
    # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
    ungroup() %>%
    filter((Has_Crit_Fraction == 1 & Simplified_sample_fraction == Fraction) | Has_Crit_Fraction == 0) %>%
    # evaluate against the "most stringent of the acute or chronic criterion" - methodology
    # set evaluation criteria to be the lowest of the acute or chronic, depending on water type code
    mutate(evaluation_crit = ifelse(WaterTypeCode == 2, pmin(Acute_FW, Chronic_FW, na.rm = TRUE), pmin(Acute_SW, Chronic_SW, na.rm = TRUE) )) %>%
    # Remove results with null evaluation_criteria (indicating a mismatch between water type and criteria (ex freshwater phosporus samples ))
    filter(!is.na(evaluation_crit)) %>%
    # For arsenic, there is a conversion factor to convert total recoverable arsenic to inorganic. This does that conversion
    mutate(evaluation_result = ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode == 2, Result_cen*0.8, 
                                      ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode != 2, Result_cen*0.59, Result_cen )),
           #`Label as excursion if the evaluation criteria is above (or below for alkalinity) the criteria
           excursion = ifelse(Char_Name %in% c("Alkalinity, total", "Alkalinity, bicarbonate")  & evaluation_result < evaluation_crit, 1, 
                              ifelse(!(Char_Name %in% c("Alkalinity, total", "Alkalinity, bicarbonate")) & evaluation_result > evaluation_crit, 1, 0 )),
           is_3d = case_when(summed_percent_3d == 100 ~ 1,
                             Result_Operator == "<" & IRResultNWQSunit > evaluation_crit & is.na(summed_percent_3d) ~ 1,
                             TRUE ~ 0) 
    )
  
  

# Results_tox_AL_analysis is the data table -----------------------------------------------------------------------

  AL_tox_assess_fun <- function(df_data = Results_tox_AL_analysis, AU_type){
    
    
    # Testing ---------------------------------------------------------------------------------------------------------
    # df_data = Results_tox_AL_analysis
    # AU_type = 'other'
    # 
    if(AU_type == "other"){  
      group1 <- c('AU_ID',  'Pollu_ID', 'wqstd_code')
      
      group2 <- c('AU_ID', 'Char_Name')
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code')
      
      group2 <- c('AU_ID', 'MLocID', 'Char_Name')
      inverse <- FALSE
    }
  
  Results_tox_AL_categories <- Results_tox_AL_analysis %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    group_by_at(group1) %>%
     #Summarise data
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              criteria_fraction = first(Fraction),
              num_samples = n(),
              num_3d = sum(is_3d),
              num_not_3d = num_samples - num_3d,
              percent_3d = sum(is_3d)/num_samples * 100,
              summed_percent_nondetect = sum(summed_percent_nondetect)/n(),
              num_sample_days = n_distinct(SampleStartDate),
              num_fraction_types = n_distinct(Simplified_sample_fraction),
              num_samples_total_fraction = sum(Simplified_sample_fraction == "Total"),
              num_Samples_dissolved_fraction = sum(Simplified_sample_fraction == "Dissolved"),
              num_excursions_all = sum(excursion),
              num_excursions_total_fraction = sum(excursion[Simplified_sample_fraction == "Total"]),
              num_excursions_dissolved_fraction = sum(excursion[Simplified_sample_fraction == "Dissolved"]),
              num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction,
                                                       ifelse(Char_Name == "Arsenic", num_samples, 
                                                              num_Samples_dissolved_fraction + (num_samples_total_fraction - num_excursions_total_fraction ) )), 
              critical_excursions = binomial_excursions(num_samples_crit_excursion_calc, type = "Toxics")) %>%
    left_join(Char_rename) %>%
    # Assign categories
    mutate(IR_category = case_when(percent_3d == 100 &  num_samples_crit_excursion_calc >= 1 ~ "3D",
                                   num_excursions_all >= critical_excursions ~ "5",
                                   (num_samples_crit_excursion_calc < 10 | num_samples < 10 | num_sample_days < 10) & num_excursions_all == 1 ~ "3B",
                                   (Pollu_ID == 5 ) & num_excursions_all > 0 ~ "3B",
                                   (num_samples_crit_excursion_calc < 10 | num_samples < 10 | num_sample_days < 10) & num_excursions_all == 0 ~ "3",
                                   num_samples_crit_excursion_calc == 0 & criteria_fraction == "Total" & num_excursions_all < critical_excursions ~ "3",
                                   num_samples_crit_excursion_calc == 0 & criteria_fraction == "Dissolved" & num_excursions_all >= critical_excursions ~ "3B", 
                                   num_excursions_all < critical_excursions & num_samples >= 10 ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale = case_when(percent_3d == 100 &  num_samples_crit_excursion_calc >= 1 ~ paste0("Insuffcient data: All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                 num_excursions_all >= critical_excursions ~ paste0("Impaired: ", num_excursions_all,
                                                                                    " excursion of criteria with ",
                                                                                    num_samples, " total samples"),
                                 (num_samples_crit_excursion_calc < 10 | num_samples < 10 | num_sample_days < 10) & num_excursions_all == 1 ~ paste0("Insuffcient data: ", num_excursions_all,
                                                                                                                                                      " excursion of criteria with ",
                                                                                                                                                      num_samples, " total samples. ", 
                                                                                                                                                     num_sample_days, " total sample days."),
                                 (Pollu_ID == 5 ) & num_excursions_all > 0 ~ paste0("Insuffcient data: ", "Analytical data indicates alkalinity is less than the criterion"),
                                 (num_samples_crit_excursion_calc < 10 | num_samples == 1 | num_sample_days < 10) & 
                                   num_excursions_all == 0 ~ paste0("Insuffcient data: ", num_excursions_all,
                                                                    " excursion of criteria with ",
                                                                    num_samples, " total samples. ", 
                                                                    num_sample_days, " total sample days."),
                                 num_samples_crit_excursion_calc == 0 & criteria_fraction == "Total" & num_excursions_all < critical_excursions ~ paste0("Insuffcient data: ", "Only dissolved fraction results available, criteria is 'Total' ",
                                                                                                                                                         num_excursions_all, " total excursions of ", 
                                                                                                                                                         num_samples, " total samples"),
                                 num_samples_crit_excursion_calc == 0 & criteria_fraction == "Dissolved" & num_excursions_all >= critical_excursions ~ paste0("Insuffcient data: ", "Only total fraction results available, criteria is 'Dissolved' ",
                                                                                                                                                              num_excursions_all, " total excursions of ", 
                                                                                                                                                              num_samples, " total samples"), 
                             
                                 num_excursions_all < critical_excursions & num_samples >= 10 ~ paste0("Attaining: ", num_excursions_all, " excursions is less than ",
                                                                                   critical_excursions, " needed to list- ",
                                                                                   num_samples, " total samples"),
                                 TRUE ~ "ERROR")) %>%
    mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE)) |> 
    mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code)) |> 
    mutate(period = NA_character_) |> 
    mutate(Delist_eligability = case_when(num_samples >= 18 & num_excursions_all <= binomial_delisting(num_samples, 'Toxics')  ~ 1,
                                          TRUE ~ 0)) 
  
  #Deal with Chromium
  
  Results_tox_AL_categories_chrom <- Results_tox_AL_categories |> 
    mutate(IR_category = case_when((Char_Name == 'Chromium' & IR_category == '5') ~ "3B",
                                    TRUE ~ IR_category),
           Rationale = case_when(Char_Name == 'Chromium' ~ paste0("Total Chromium assessed as Chromium(VI). ", Rationale),
                                 TRUE ~ Rationale),
           Char_Name = case_when(Char_Name == 'Chromium' ~ "Chromium(VI)",
                                 TRUE ~Char_Name ),
           Pollu_ID = case_when(Pollu_ID == 188 ~ 43,
                                TRUE ~ Pollu_ID),
           )
  

  
  return(Results_tox_AL_categories_chrom)
  }
  
Char_rename2 <- Char_rename |> 
  mutate(Pollu_ID = as.character(Pollu_ID))
  

# Watershed assessment --------------------------------------------------------------------------------------------------------------
  AL_Tox_WS <- AL_tox_assess_fun(df_data = Results_tox_AL_analysis, AU_type = "WS")
  
  ## GNIS rollup -----------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup <- AL_Tox_WS %>%
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
    left_join(Char_rename2) |> 
    relocate(Char_Name, .after = AU_GNIS_Name)
  
  
  ### Delist process --------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')
  
  
  ## AU Rollup -------------------------------------------------------------------------------------------------------

  
  WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup, char_name_field = Char_Name) 
  WS_AU_rollup <- WS_AU_prev_list(WS_AU_rollup) 

# Other assessment-----------------------------------------------------------------------------------------------

  
  AL_Tox_other <- AL_tox_assess_fun(df_data = Results_tox_AL_analysis, AU_type = "other")
  

  other_category <- join_prev_assessments(AL_Tox_other, AU_type = 'Other')|> 
    select(-Char_Name) |> 
    left_join(Char_rename2) |> 
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
  
  
  AU_display <- bind_rows(AU_display_other, AU_display_ws) |> 
    mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                                 TRUE ~  Rationale))|> 
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
  
  
  
  Results_tox_AL <- list(data =Results_tox_AL_analysis,
                         AU_Decisions = AU_display,
                         Other_AU_categorization = other_category_delist,
                         WS_Station_cat = AL_Tox_WS,
                         WS_GNIS_cat = WS_GNIS_rollup_delist)
  
  
  return(Results_tox_AL)
  
}                             