library(zoo)

fun_DO_spawn <- function(df, write_excel = TRUE ){
  
  source("Parameters/DO/DO_Delist_checker.R")
 
# Testing and setup -----------------------------------------------------------------------------------------------


# # 
# df <- Results_censored_DO
# write_excel <- TRUE
a <- Sys.time()

# Variable setup --------------------------------------------------------------------------------------------------

# Number of 30-d samples needed in a year to use continuous metrics
required_crit_7d_periods <- 15

#Number of critical period samples needed to use instantaneous metrics
req_inst_crit_samples <- 8




# Data cleanup ----------------------------------------------------------------------------------------------------


Results_spawndates <- df %>%
  mutate(SampleStartDate = ymd(SampleStartDate),
         SampleStartTime = stringr::str_sub(SampleStartTime, start = 1, end=5),
         SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd=mdy(SpawnEnd),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         SpawnEnd = if_else(SpawnEnd < SpawnStart & SampleStartDate >= SpawnEnd, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                            SpawnEnd),
         SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                              SpawnStart),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         critstart = mdy(paste0("6/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ),
         crit_spawn = 11.0,
         crit_Min = 9.0) %>%
  filter(DO_code %in% c(2,3,4)) %>%
  filter(in_spawn == 1) %>%
  select(-crit_30D, -crit_7Mi,  -crit_Instant, -critstart, -critend, -is.crit )

spawn_delist_eligability <- spawn_DO_delist_checker(Results_spawndates)

# Summarize available data to get a list of AU's to be analyzed using cont. data
results_cont_summary <- Results_spawndates %>%
  #filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  filter(Statistical_Base == "7DADMean") %>%
  group_by(AU_ID, year(SampleStartDate)) %>%
  summarise(tot_7d_metrics = n()) %>%
  filter(tot_7d_metrics >= required_crit_7d_periods,
         !is.na(AU_ID)) %>%
  pull(AU_ID) 

# results_cont_summary_WS <- Results_spawndates %>%
#   filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
#   filter(Statistical_Base == "7DADMean") %>%
#   group_by(MLocID, year(SampleStartDate)) %>%
#   summarise(tot_7d_metrics = n()) %>%
#   filter(tot_7d_metrics >= required_crit_7d_periods) %>%
#   pull(MLocID) 
# 
# 
# continuous_AUs <- unique(Results_spawndates$AU_ID)

# Spawning Continuous ---------------------------------------------------------------------------------------------



spawn_cont_function <- function(df = Results_spawndates, continuous_list = results_cont_summary, AU_type){

  

## Testing ---------------------------------------------------------------------------------------------------------
  # df = Results_spawndates
  # continuous_list = results_cont_summary
  # AU_type = 'other'
  # 
  
#Setting AU_type to 'other' will group the analysis by AU_ID and set the filter to discard WS units (inverse = TRUE)
#Setting AU_type to 'WS' will group the analysis by AU_ID and MlocID, and set the filter to only keep WS units (inverse = FALSE)
if(AU_type == "other"){  
  group1 <- c('AU_ID')
  group2 <- c('AU_ID', 'Pollu_ID', 'wqstd_code')
  group3 <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'period')
  inverse <- TRUE
  query_type = 'AU_ID'
  
  
  
} else if (AU_type == "WS"){
  group1 <- c('AU_ID', 'AU_GNIS_Name', 'MLocID' )
  group2 <- c('AU_ID', 'AU_GNIS_Name', 'MLocID', 'GNIS_Name', 'Pollu_ID', 'wqstd_code') 
  roup3 <- c('AU_ID', 'MLocID','AU_GNIS_Name', 'GNIS_Name', 'Pollu_ID', 'wqstd_code', 'period') 
  inverse <- FALSE
  query_type = 'MLocID'
}




# Initial Continuous criteria analysis --------------------------------------------

# This initial analysis is used to see where we need to calculate DO Sat 
# Calculating the 7-D DO SAt is computationally expensive
# so we only calculate it at locations where it woudl influnce the
# IR category

# filter down to AUs that are to be evaluated with cont metrics
# Filter down to only 7-D, and daily minimums
# Flag various violations


  
  continuous_data_analysis <- Results_spawndates %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    filter(AU_ID %in% continuous_list) %>%
    filter(Statistical_Base %in% c("7DADMean", "Minimum")) %>%
    mutate(Violation = case_when(Statistical_Base == "7DADMean" & IRResultNWQSunit < crit_spawn ~ 1,
                                 Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min ~ 1,
                                 TRUE ~ 0 )) 


  Do_sat_sites <-continuous_data_analysis |> 
    pull(AU_ID)
# Get DO saturation -----------------------------------------------------------------------------------------------





# Remove from here ------------------------------------------------------------------------------------------------


# Run through initial categorization
# This all gets redone in the end
# Where percent saturation would make a difference, set category as "Check percent Sat"
# continuous_data_categories <- continuous_data_analysis %>%
#   filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
#   group_by_at(group1) %>%
#   summarise(Total_violations = sum(Violation),
#             Sum_7D_violations = sum(Violation [Statistical_Base == "7DADMean"]),
#             Sum_abs_min_violations = sum(Violation [Statistical_Base == "Minimum"])) %>%
#   mutate(IR_category = case_when(DO_Class != "Cold Water" & Sum_7D_violations >= 2 ~ "5",
#                                  DO_Class != "Cold Water" & Sum_abs_min_violations >= 2 ~ "5",
#                                  DO_Class == "Cold Water" & Sum_abs_min_violations >= 2 ~ "5",
#                                  DO_Class == "Cold Water" & Sum_7D_violations >= 2 &
#                                     Sum_abs_min_violations < 2 ~ "Check percent Sat",
#                                  Sum_7D_violations < 2 & Sum_abs_min_violations < 2 ~ "2",
#                                  TRUE ~ "Error" ))
# 
# # Datatable of results that need percent saturation
# cont_perc_sat_check <- continuous_data_categories %>%
#   filter(AU_ID %in% unique(subset(continuous_data_categories, IR_category == "Check percent Sat" )$AU_ID) )


# Query Database --------------------------------------------------------------------------------------------------

#if(nrow(cont_perc_sat_check) > 0){
  
# 
#   # List of monitoring locations that need DO sat 
#   # This list is used for the sql query that follows
#   continuous_mon_locs <- unique(cont_perc_sat_check$AU_ID)
#   
  
  # Get data from database --------------------------------------------------
  
  # Get DO and temp data from IR_database to calculate percent sat --------
  
  
  con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  
  
  
  #Query DOSat from AWQMS
  DOsat_AWQMS <- "SELECT [OrganizationID],[MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [ResultsRawWater]
WHERE   Char_Name = 'Dissolved oxygen saturation' AND 
        AU_ID in ({Do_sat_sites*}) AND 
        Statistical_Base = 'Mean'"
  
  
  DoSatqry <- glue::glue_sql(DOsat_AWQMS, .con = con)
  
  perc_sat_AWQMS_DOSat <- DBI::dbGetQuery(con, DoSatqry)
  
  
  
  # Query out the mean DO values from the identified monitoring locations
  Doqry <- "SELECT * 
FROM            VW_DO
WHERE        (Statistical_Base = 'Mean') AND AU_ID in ({Do_sat_sites*})"
  
  
  Doqry <- glue::glue_sql(Doqry, .con = con)
  
  perc_sat_DO <- DBI::dbGetQuery(con, Doqry)
  
  
  # Query out the mean temp values from the identified monitoring locations
  tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        (Statistical_Base = 'Mean') AND AU_ID in ({Do_sat_sites*})"
  
  tempqry <- glue::glue_sql(tempqry, .con = con)
  
  perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)
  
  DBI::dbDisconnect(con)
  
  
  # Modfy DOSat table from AWQMS and push to perc_sat_DO --------------------------------------------------
  
  # This ensures that if AWQMS has DOSat values, we use those, and only calculate DOsat values
  # Where we don't already have them
  
  
  
  perc_sat_DO <- perc_sat_DO %>%
    left_join(perc_sat_AWQMS_DOSat, by =c('OrganizationID','MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))
  
  
  
  # Join Do and temp and calculate DOSat ------------------------------------
  
  # Pare down the temperature table to be used to join
  perc_sat_temp_join <- perc_sat_temp %>%
    select(OrganizationID,MLocID, IRResultNWQSunit, SampleStartDate, SampleStartTime) %>%
    rename(Temp_res = IRResultNWQSunit)
  
  
  # prep the imported DO table to be joined
  # Rename the DO result to DO_res
  # Join the DO table to the pared down temperature table
  # Calculate DOSat
  DO_sat <- perc_sat_DO %>%
    rename(DO_res =  IRResultNWQSunit) %>%
    left_join(perc_sat_temp_join, by = c('OrganizationID','MLocID', 'SampleStartDate', 'SampleStartTime')) %>%
    mutate(DO_sat = ifelse(is.na(DO_sat), DOSat_calc(DO_res, Temp_res, ELEV_Ft ), DO_sat ),
           DO_sat = ifelse(DO_sat > 100, 100, DO_sat ),
           SampleStartTime = stringr::str_sub(SampleStartTime, start = 1, end=5),
           DO_sat = round(DO_sat,digits = 1)) 
  
  
  # Calculate moving 7 day average
  # create flag for which results have 7 days worth of data
  # calculate 7 day moving average of DO_Sat off of daily mean DO_Sat
  DO_sat_7dma <- DO_sat %>%
    mutate(Date = as.Date(SampleStartDate)) %>%
    arrange(MLocID, SampleStartDate) %>%
    group_by(MLocID) %>%
    mutate(startdate7 = lag(Date, 6, order_by = Date),
           # flag out which result gets a moving average calculated
           calc7ma = ifelse(startdate7 == (Date - 6), 1, 0 ),
           dosat_mean7= ifelse(calc7ma == 1, round(rollmean(x = DO_sat, 7, align = "right", fill = NA),1) , NA )) 
  
  
  # Join DOsat to 7_D metrics -----------------------------------------------
  
  # Pare down the table of DO_Sats so it can be joined
  DO_sat_join <- DO_sat_7dma %>%
    select(MLocID, dosat_mean7, Date) 
  

  
  # Join DO_Sat values to the table that will be used for evaluation
  spawn_DO_data <- Results_spawndates %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    filter(AU_ID %in% continuous_list) %>%
    filter(Statistical_Base %in% c("7DADMean", "Minimum")) %>%
    left_join(DO_sat_join, by = c('MLocID', 'SampleStartDate'  = 'Date')) %>%
    mutate(dosat_mean7 = case_when(Statistical_Base == 'Minimum' ~ NA_real_,
                                   TRUE ~ dosat_mean7)) |> 
    mutate(Violation = case_when(!is.na(dosat_mean7) & Statistical_Base == "7DADMean" & IRResultNWQSunit < crit_spawn &
                                   dosat_mean7 < 95 ~ 1,
                                 is.na(dosat_mean7) & Statistical_Base == "7DADMean" & IRResultNWQSunit < crit_spawn ~ 1,
                                 Statistical_Base == 'Minimum' & IRResultNWQSunit < crit_Min ~ 1,
                                 TRUE ~ 0 
                                 ))
  

  
  spawn_cont_categories <- spawn_DO_data %>%
    left_join(spawn_delist_eligability, relationship = "many-to-many") |> 
    group_by_at(group2) %>%
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              Total_dates = n_distinct(SampleStartDate),
              Total_excursions = sum(Violation),
              Sum_7D_excursions = sum(Violation [Statistical_Base == "7DADMean"]),
              Sum_abs_min_excursions = sum(Violation [Statistical_Base == "Minimum"]),
              Delist_eligability = max(Delist_eligability)) %>%
    mutate(period = "spawn",
           IR_category = case_when(Sum_7D_excursions >= 2 ~ "5",
                                   Sum_abs_min_excursions >= 2 ~ "5",
                                   Sum_7D_excursions < 2 &
                                     Sum_abs_min_excursions < 2  ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale = case_when(Sum_7D_excursions >= 2 ~ paste0("Impaired- ", Sum_7D_excursions, " valid excursions of 7-D metric"),
                                 Sum_abs_min_excursions >= 2 ~ paste0("Impaired- ",Sum_abs_min_excursions, " valid excursions of abs minimum criteria. ", 
                                                                      Sum_7D_excursions, " valid excursions of 7-D metric"),
                                 Sum_7D_excursions < 2 &
                                   Sum_abs_min_excursions < 2  ~ paste0("Attaining: ",
                                                                        Sum_7D_excursions, " valid excursions of 7-D metric. ",
                                                                        Sum_abs_min_excursions, " excursions of alternate minimum criteria. "
                                   ),
                                 TRUE ~ "ERROR")) %>%
    mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))
  
  

    
  spawn_cont_list <- list(data = as.data.frame(spawn_DO_data),
                          AU_categories = spawn_cont_categories)
            
  return(spawn_cont_list)
  
}





# spawn instant function ------------------------------------------------------------------------------------------




spawn_inst_function <- function(df = Results_spawndates, continuous_list = results_cont_summary, AU_type){
  
  

# testing ---------------------------------------------------------------------------------------------------------

  # df = Results_spawndates
  # continuous_list = results_cont_summary
  # AU_type <- 'other'
  
  #Setting AU_type to 'other' will group the analysis by AU_ID and set the filter to discard WS units (inverse = TRUE)
  #Setting AU_type to 'WS' will group the analysis by AU_ID and MlocID, and set the filter to only keep WS units (inverse = FALSE)
  if(AU_type == "other"){  
    group1 <- c('AU_ID')
    group2 <- c('AU_ID',  'Pollu_ID', 'wqstd_code')
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID','AU_GNIS_Name',  'MLocID', 'DO_Class')
    group2 <- c('AU_ID','AU_GNIS_Name',  'MLocID', 'GNIS_Name', 'Pollu_ID', 'wqstd_code',  'OWRD_Basin', 'DO_Class') 
    inverse <- FALSE
  }
  
  
  
    
    instant_data_analysis <- df %>%
      filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
      filter(!AU_ID %in% continuous_list) %>%
      filter(Statistical_Base %in% c("Minimum", NA)) %>%
      mutate(Violation_crit = ifelse(IRResultNWQSunit < crit_spawn, 1, 0 ))
    
 


  
  # data query from IR database
  instant_mon_locs <- unique(instant_data_analysis$MLocID)
  
  
  
  # Get DO and temp data from IR_database to calculate percent sat --------
  
  #Query Dsat
  
  con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  #Query DOSat from AWQMS
  
  DOsat_AWQMS <- "SELECT [OrganizationID], [MLocID], [act_depth_height], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [IntegratedReport].[dbo].[ResultsRawWater]
WHERE ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*}) AND Char_Name = 'Dissolved oxygen saturation') OR 
      ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}) AND Char_Name = 'Dissolved oxygen saturation')"
  
  
  Dosatqry <- glue::glue_sql(DOsat_AWQMS, .con = con)
  
  instant_perc_sat_DO_AWQMS <- DBI::dbGetQuery(con, Dosatqry) |> 
    mutate(act_depth_height = as.numeric(act_depth_height))
  
#   # Query DO data
#   
#   Doqry <- "SELECT * 
# FROM            VW_DO
# WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"
#   
#   
#   
#   Doqry <- glue::glue_sql(Doqry, .con = con)
#   
#   instant_perc_sat_DO <- DBI::dbGetQuery(con, Doqry)
#   
  
  # Query temp data
  
  tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"
  
  tempqry <- glue::glue_sql(tempqry, .con = con)
  
  instant_perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)
  
  DBI::dbDisconnect(con)
  
  # # Remove duplicate DO sat Values that are mistakenly in AWQMS
  # instant_perc_sat_DO_AWQMS <- instant_perc_sat_DO_AWQMS %>%
  #   mutate(SampleStartTime = substr(SampleStartTime, 1, 5)) %>%
  #   distinct(MLocID, SampleStartDate,SampleStartTime,Statistical_Base, .keep_all = TRUE) %>%
  #   mutate(act_depth_height = as.numeric(act_depth_height))
  
  # 
  # instant_perc_sat_DO <- instant_perc_sat_DO %>%
  #   left_join(instant_perc_sat_DO_AWQMS, by =c('MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))
  
  
  
  # Pare down temp table to be used for joining
  instant_perc_sat_temp_join <- instant_perc_sat_temp %>%
    select(OrganizationID, MLocID, Statistical_Base, IRResultNWQSunit, SampleStartDate, SampleStartTime, act_depth_height) %>%
    mutate(SampleStartTime = substr(SampleStartTime, 1, 5)) %>%
    rename(Temp_res = IRResultNWQSunit) %>%
    mutate(SampleStartDate = ymd(SampleStartDate)) %>%
    mutate(act_depth_height = as.numeric(act_depth_height)) 
  
  
  # Calculate DOSat
  DO_sat_data <- instant_data_analysis %>%
    left_join(spawn_delist_eligability) |> 
    mutate(SampleStartTime = substr(SampleStartTime, 1, 5)) %>%
    left_join(instant_perc_sat_DO_AWQMS, c("OrganizationID", "MLocID", "Statistical_Base", 
                                           "SampleStartDate", "SampleStartTime", "act_depth_height")) %>%
    left_join(instant_perc_sat_temp_join,  by = c("OrganizationID", "MLocID", "Statistical_Base", 
                                                  "SampleStartDate", "SampleStartTime", "act_depth_height")) %>%
    mutate(DO_concentration = IRResultNWQSunit,
           DO_sat = ifelse(is.na(DO_sat), DOSat_calc(DO_concentration, Temp_res, ELEV_Ft ), DO_sat ),
           DO_sat = ifelse(DO_sat > 100, 100, DO_sat ),
           DO_sat = round(DO_sat,digits = 1)) %>%
    mutate(Violation = case_when(is.na(DO_sat) & DO_concentration < crit_spawn ~ 1,
                                 !is.na(DO_sat) &  DO_concentration < crit_spawn & DO_sat < 95 ~ 1,
                                 TRUE ~ 0))
  
  
  spawn_inst_categories <- DO_sat_data %>%
    group_by_at(group2) %>%
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              total_samples = n(),
              critical_excursions = binomial_excursions(total_samples, "Conventionals"),
              Total_dates = n_distinct(SampleStartDate),
              Total_excursions = data.table::uniqueN(SampleStartDate[Violation == 1]),
              Delist_eligability = max(Delist_eligability)) %>%
    mutate(period = "spawn",
           IR_category = case_when(Total_dates >= req_inst_crit_samples & Total_excursions > critical_excursions ~ "5",
                                   Total_dates < req_inst_crit_samples & Total_excursions > 0 ~ "3B",
                                   Total_dates < req_inst_crit_samples & Total_excursions == 0 ~ "3",
                                   Total_dates >= req_inst_crit_samples & Total_excursions <= critical_excursions ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale = case_when(Total_dates >= req_inst_crit_samples & Total_excursions > critical_excursions ~ paste0("Impaired: ", Total_excursions, " total excurions is > ",
                                                                                                                        critical_excursions, " needed to list.- ",
                                                                                                                        Total_dates, " total sample dates"),
                                 Total_dates < req_inst_crit_samples & Total_excursions > 0 ~ paste0('Insufficient data: ',
                                                                                                     Total_dates, " total dates is < 8 needed. ",
                                                                                                     Total_excursions, " total excursions"),
                                 Total_dates < req_inst_crit_samples & Total_excursions == 0 ~ paste0('Insufficient data: ',
                                                                                                      Total_dates, " total dates is < 8 needed. ",
                                                                                                      Total_excursions, " total excursions"),
                                 Total_dates >= req_inst_crit_samples & Total_excursions <= critical_excursions ~ paste0("Attaining: ",
                                                                                                                         Total_excursions, " total excurions is <= ",
                                                                                                                         critical_excursions, " needed to list.- ",
                                                                                                                         Total_dates, " total sample dates"),
                                 TRUE ~ "ERROR")) %>%
    mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))
  

  
  spawn_inst_list <- list(data = as.data.frame(DO_sat_data),
                          AU_categories = spawn_inst_categories)
  
  return(spawn_inst_list)
  
  
}







# Run continuous function -----------------------------------------------------------------------------------------


## Watershed -------------------------------------------------------------------------------------------------------


spawn_cont_WS <- spawn_cont_function(continuous_list = results_cont_summary, AU_type = "WS" )


spawn_cont_WS_data <- spawn_cont_WS[['data']]
spawn_cont_WS_cats <- spawn_cont_WS[['AU_categories']]



## Other -----------------------------------------------------------------------------------------------------------


spawn_cont_other <- spawn_cont_function(continuous_list = results_cont_summary, AU_type = "other" )


spawn_cont_other_data <- spawn_cont_other[['data']]
spawn_cont_other_cats <- spawn_cont_other[['AU_categories']]



# run instant function --------------------------------------------------------------------------------------------

## Watershed -------------------------------------------------------------------------------------------------------


spawn_inst_WS <- spawn_inst_function(df = Results_spawndates, continuous_list = results_cont_summary, AU_type = "WS")

spawn_inst_WS_data <- spawn_inst_WS[['data']]
spawn_inst_WS_cats <- spawn_inst_WS[['AU_categories']]



## Other -----------------------------------------------------------------------------------------------------------


spawn_inst_other <- spawn_inst_function(df = Results_spawndates, continuous_list = results_cont_summary, AU_type = "other")

spawn_inst_other_data <- spawn_inst_other[['data']]
spawn_inst_other_cats <- spawn_inst_other[['AU_categories']]






# Data combine ----------------------------------------------------------------------------------------------------


## Data files ------------------------------------------------------------------------------------------------------


cont_data_combined <- bind_rows(spawn_cont_other_data, spawn_cont_WS_data)

inst_data_combined <- bind_rows(spawn_inst_other_data, spawn_inst_WS_data) 




# GNIS rollup -----------------------------------------------------------------------------------------------------


WS_GNIS_rollup_inst <- spawn_inst_WS_cats %>%
  mutate(Char_Name = 'Dissolved oxygen (DO)') |> 
  mutate(Rationale = paste0(MLocID, ": ", Rationale)) |> 
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name,Char_Name, Pollu_ID, wqstd_code, period, stations) %>%
  summarise(IR_category_GNIS_24 = max(IR_category),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_24 = factor(IR_category_GNIS_24, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  


WS_GNIS_rollup_cont <- spawn_cont_WS_cats %>%
  mutate(Char_Name = 'Dissolved oxygen (DO)') |> 
  mutate(Rationale = paste0(MLocID, ": ", Rationale)) |> 
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name,Char_Name, Pollu_ID, wqstd_code, period, stations) %>%
  summarise(IR_category_GNIS_24 = max(IR_category),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_24 = factor(IR_category_GNIS_24, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  

## Combine mloc ----------------------------------------------------------------------------------------------------

WS_categories <- bind_rows(spawn_cont_WS_cats, spawn_inst_WS_cats)

## Combine GNIS ----------------------------------------------------------------------------------------------------

WS_GNIS_rollup <- bind_rows(WS_GNIS_rollup_cont, WS_GNIS_rollup_inst)
WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")

## Delist process --------------------------------------------------------------------------------------------------


WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS') |> 
  mutate(Char_Name = 'Dissolved oxygen (DO)')


## AU Rollup -------------------------------------------------------------------------------------------------------


WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup_delist, char_name_field = Char_Name)
WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) 

# Combine AU decisions --------------------------------------------------------------------------------------------
Other_categories <- bind_rows(spawn_inst_other_cats, spawn_cont_other_cats)
other_category <- join_prev_assessments(Other_categories, AU_type = 'Other')

other_category_delist <-  assess_delist(other_category, type = "Other") |> 
  mutate(Char_Name = 'Dissolved oxygen (DO)')|> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  

# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  select(AU_ID, Char_Name,  Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)

AU_display_ws <- WS_AU_rollup_joined |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_24,
         Rationale = Rationale_AU)

AU_display <- bind_rows(AU_display_other, AU_display_ws) |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               .default = Rationale))|> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category)|> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2024",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2024',
                                 TRUE ~  Year_listed))  |> 
  mutate(Char_Name = 'Dissolved oxygen (DO)')



WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  |> 
  mutate(Char_Name = 'Dissolved oxygen (DO)')


spawn_cont_data <- bind_rows(spawn_cont_other_data, spawn_cont_WS_data)
spawn_inst_data <- bind_rows(spawn_inst_other_data, spawn_inst_WS_data)

Sys.time() - a

if(write_excel){
  

  
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "DO spawn Data Inst",     tabColour = 'paleturquoise2')
  addWorksheet(wb, sheetName = "DO spawn Data Cont",     tabColour = 'paleturquoise2')
  
  
  
  
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Other_AU_categorization", x = other_category_delist, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = WS_categories, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
  
  
  writeData(wb = wb, sheet = "DO spawn Data Inst",   x = spawn_inst_data, headerStyle = header_st )
  writeData(wb = wb, sheet = "DO spawn Data Cont",   x = spawn_cont_data, headerStyle = header_st )
  
  
  print("Writing excel doc")
  saveWorkbook(wb, paste0("Parameters/Outputs/DO Spawn-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
  
  
  
  
  
  
}

# list out --------------------------------------------------------------------------------------------------------


DO_spawn_list <- list(AU_display = AU_display,
                      Other_AU_categorization = other_category_delist,
                      WS_GNIS_categorization = as.data.frame(WS_GNIS_rollup_delist),
                      WS_station_categorization = WS_categories,
                      cont_data = spawn_cont_data,
                      inst_data = spawn_inst_data)
     
     
    return(DO_spawn_list)  
  
}
