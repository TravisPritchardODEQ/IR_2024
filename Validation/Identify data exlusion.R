#Work in progress for duplicate checker and data aggregation




library(DBI)
library(odbc)
library(glue)
library(tidyverse)
library(openxlsx)



options(scipen = 999999)

# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR_Dev")

# Function to import custom input raw script
# This script queries input raw, but includes all the paramter view conditions. 
# This allows us to check duplciates only on data used in the parameter assessments

getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""
  
  while (TRUE){
    line <- readLines(con, n = 1)
    
    if ( length(line) == 0 ){
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}



IR_Res_qry <- getSQL("Validation/InputRaw limited to data views.sql")


IR_res_db <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql))


# Read in identified exclusions -----------------------------------------------------------------------------------

exclusions <- dbReadTable(IR.sql, 'Unused_Results')

IR_res <- IR_res_db %>%
  filter(!Result_UID %in% exclusions$Result_UID)
  
# Straight Duplicates ---------------------------------------------------------------------------------------------

#These are the values that are suspected duplicates

straight_duplicates <- IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           IRResultNWQSunit,
           act_depth_height,
           Result_Depth,
           Analytical_method,
           Result_Unit,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(num = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID)) %>%
  mutate(group_num =cur_group_id()) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name) %>%
  filter(num_resUID > 1 & num_distinct_results == 1) %>%
  arrange(group_num)


#different result UIDs. These are likely duplicates in AWQMS. They can be removed through adding resUID to unused data  
# dup_diff_resuid <- straight_duplicates %>%
#   filter(num_resUID > 1 & num_distinct_results == 1) %>%
#   arrange(group_num)

# write.xlsx(dup_diff_resuid, file = "Validation/Dupdata/same_date_time_method_result.xlsx")


#Data to exclude:
# 
# straight_duplicates_exclude <- straight_duplicates %>%
#   ungroup() %>%
#   group_by(group_num) %>%
#   filter(row_number() > 1) %>%
#   ungroup() %>%
#   select(Result_UID, Char_Name) %>%
#   mutate(Data_Review_Code = NA_character_,
#          Data_Review_Comment = "Duplicate result at same date/time in database" )
# 
# write.xlsx(straight_duplicates_exclude, file = "Validation/Dupdata/same_date_time_method_result_exclude_list.xlsx")

# same day/time/method different result ----------------------------------------------------------------------------------

day_time_dups <- IR_res %>%

filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           act_depth_height,
           Result_Depth,
           Analytical_method,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation,
           Time_Basis) %>%
  mutate(num = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_activity_ID = n_distinct(act_id)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num > 1,
         num_distinct_results > 1) %>%
  ungroup() %>%
  arrange(group_num, MLocID, SampleStartDate, Char_Name)


write.xlsx(day_time_dups, file = "Validation/Dupdata/same_date_time_method_diff_result.xlsx")


# Different Methods -----------------------------------------------------------------------------------------------
#Keep one with lowest MRL? HIghest DQL?


diff_methods <-  IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(OrganizationID, 
           MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           act_depth_height,
           Result_Depth,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_analytical_methods = n_distinct(Analytical_method),
         num_orgs = n_distinct(OrganizationID)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_analytical_methods > 1) %>%
  ungroup() %>%
  arrange(group_num, MLocID, SampleStartDate, Char_Name)

write.xlsx(diff_methods, file = "Validation/Dupdata/diff_method.xlsx")

diff_methods_exclude <- diff_methods %>%
  ungroup() %>%
  mutate(QL = pmin(MDLValue,MRLValue, na.rm = TRUE)) %>%
  group_by(group_num) %>%
  mutate(keep = case_when(QL == min(QL, na.rm = TRUE) ~ 1,
                          all(is.na(QL)) ~ 1,
                             TRUE ~ 0)) %>%
  mutate(num_keep = sum(keep)) %>%
  mutate(keep2 = case_when(any(str_detect(Analytical_method, 'Computation')) & !str_detect(Analytical_method, 'Computation')  ~ 1,
                          any(str_detect(Analytical_method, 'Computation')) & str_detect(Analytical_method, 'Computation') ~ 0,
                          TRUE ~ keep),
         Data_Review_Comment =  case_when(any(str_detect(Analytical_method, 'Computation')) & str_detect(Analytical_method, 'Computation') ~ "Result is from a computational method, and an analytical method exists for the same date/time",
                                           keep == 0 ~ "Another result at same date/time has a lower quantification limit",
                                           TRUE ~ NA_character_) ) %>%
  filter(keep2 == 0) %>%
  ungroup() %>%
  #mutate(Data_Review_Code = NA_character_) %>%
  select(Result_UID, Char_Name,  Data_Review_Comment) |> 
  distinct()

dbAppendTable(IR.sql, 'Unused_Results', diff_methods_exclude,row.names = NULL)

# 
# test <-  diff_methods %>%
#   ungroup() %>%
#   mutate(QL = pmin(MDLValue,MRLValue, na.rm = TRUE)) %>%
#   group_by(group_num) %>%
#   mutate(keep = case_when(QL == min(QL) ~ 1,
#                           TRUE ~ 0)) %>%
#   mutate(num_keep = sum(keep)) %>%
#   mutate(keep2 = case_when(any(str_detect(Analytical_method, 'Computation')) & !str_detect(Analytical_method, 'Computation') ~ 1,
#                            any(str_detect(Analytical_method, 'Computation')) & str_detect(Analytical_method, 'Computation') ~ 0,
#                            TRUE ~ keep),
#          Data_Review_Comment =  case_when(any(str_detect(Analytical_method, 'Computation')) & str_detect(Analytical_method, 'Computation') ~ "Result is from a computational method, and an analytical method exists for the same date/time",
#                                            keep == 0 ~ "Another result at same date/time has a lower quantification limit",
#                                            TRUE ~ NA_character_) ) %>%
#   filter(sum(keep2) !=  1 )
# Different Depths ------------------------------------------------------------------------------------------------

#Keep most shallow?

diff_depths <-  IR_res %>%
  mutate(depth = pmax(Result_Depth,act_depth_height, na.rm = TRUE )) %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(OrganizationID, 
           MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           #SampleStartTime,
           Statistical_Base,
           wqstd_code,
           Sample_Fraction,
           Analytical_method,
           Char_Speciation)%>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         #num_depths = n_distinct(Result_Depth),
         num_act_depth = n_distinct(act_depth_height),
         num_depths = n_distinct(depth)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_depths > 1  ) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)


#write.xlsx(diff_depths, file = "Validation/Dupdata/diff_depths.xlsx")

diff_depths_exclude <- diff_depths %>%
  mutate(depth = pmax(Result_Depth,act_depth_height, na.rm = TRUE )) %>%
  group_by(group_num) %>%
  mutate(keep = case_when(depth == min(depth) ~ 1,
                          all(is.na(depth)) ~ 1,
                          TRUE ~ 0)) %>%
  filter(keep == 0) %>%
  ungroup() %>%
  select(Result_UID, Char_Name) %>%
  mutate(Data_Review_Comment = "Data exists on the same date at shallower depth" )


dbAppendTable(IR.sql, 'Unused_Results', diff_depths_exclude,row.names = NULL)

# data_to_exclude <- bind_rows(straight_duplicates_exclude, diff_methods_exclude, diff_depths_exclude)
# 
# 
# write.xlsx(data_to_exclude, file = "Validation/Dupdata/res_UIDs_to_exclude.xlsx")


# pH- Millivolts --------------------------------------------------------------------------------------------------

pH_mV <- IR_res %>%
  filter(Result_Unit == "mV",
         Char_Name == "pH") %>%
  select(Result_UID, Char_Name) %>%
  mutate(Data_Review_Comment = "pH data reported in millivolts" )

dbAppendTable(IR.sql, 'Unused_Results', pH_mV,row.names = NULL)
# write.xlsx(pH_mV, file = "Validation/Dupdata/pH_mV.xlsx")

#

  


# Results > 1 m ---------------------------------------------------------------------------------------------------
# I havn't run this for 2024 yet

deep_results <- IR_res %>%
  mutate(depth = as.numeric(pmax(Result_Depth,act_depth_height, na.rm = TRUE )),
         depth_unit = pmax(Result_Depth_Unit,ActDepthUnit, na.rm = TRUE )) %>%
  filter(!is.na(depth)) %>%
  mutate(deep = case_when(depth_unit == 'm' & depth > 1 ~ 1,
                          depth_unit == 'ft' & depth/3.281 > 1 ~ 1,
                          depth_unit == 'cm' & depth/100 > 1 ~ 1,
                          depth_unit == 'in' & depth/39.37 > 1 ~ 1,
                          TRUE ~ 0
                          )) %>%
    filter(deep == 1) %>%
  select(Result_UID, Char_Name) %>%
  mutate(Data_Review_Comment = "activity depth > 1 m" )





# different activity types ----------------------------------------------------------------------------------------

diff_act_types  <- IR_res %>%
  filter(AU_ID != '99') %>%
 group_by(MLocID,
           Char_Name,
           SampleStartDate,
           Statistical_Base,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(group_num =cur_group_id()) %>%
  mutate(num_act_types = n_distinct(Activity_Type)) %>%
  filter(num_act_types > 1) %>%
  arrange(group_num)


act_type_exclude <- diff_act_types %>%
  group_by(group_num) %>%
  mutate(has_field = case_when(any(grepl("Field", Activity_Type)) ~ 1,
                               TRUE ~0),
         is_field = case_when(grepl("Field", Activity_Type) ~ 1,
                              TRUE ~0)) %>%
  filter(has_field == 1 & is_field == 0) %>%
  ungroup() %>%
  select(Result_UID, Char_Name) %>%
  mutate(Data_Review_Comment = "Field data exists on the same date" )

dbAppendTable(IR.sql, 'Unused_Results', act_type_exclude,row.names = NULL)



# diff qualifiers -------------------------------------------------------------------------------------------------


qualifiers <- IR_res %>%
  filter(AU_ID != '99',
         Result_Unit != "mV") |> 
  filter(!(Result_UID %in% exclusions$Result_UID)) %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           act_depth_height,
           Result_Depth,
           Analytical_method,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation,
           Time_Basis) |> 
  mutate(num = n(),
         num_qualifiers = n_distinct(Result_Operator),
         num_resUID = n_distinct(Result_UID)) |> 
  mutate(group_num =cur_group_id()) 

qualifiers_exclude <- qualifiers |> 
  ungroup() |> 
  filter(num_qualifiers > 1) |> 
  group_by(group_num) |> 
  mutate(is_equal = ifelse(Result_Operator == "=", 1, 0),
         is_nondetect =ifelse(Result_Operator == "<", 1, 0),
         equal_group = ifelse(any(Result_Operator == "="), 1, 0),
         keep = case_when(equal_group == 1 & is_equal == 1 ~ 1,
                          equal_group == 1 & is_nondetect == 1 ~ 0)) |> 
  filter(keep == 0) |> 
  ungroup() |> 
  select(Result_UID, Char_Name) %>%
  mutate(Data_Review_Comment = "Multiple samples on same day/time. This sample is a non-detect, detects exist with other samples" )

dbAppendTable(IR.sql, 'Unused_Results', qualifiers_exclude,row.names = NULL)

# cont pH dups ----------------------------------------------------------------------------------------------------



# Pull selected data from InputRaw.
IR_cont_Res_qry <-
  "SELECT * 
  FROM [IntegratedReport].[dbo].[InputRaw_ContpH]" 

IR_cont_res <- DBI::dbGetQuery(IR.sql, glue_sql(IR_cont_Res_qry, .con = IR.sql))


IR_cont_res <- IR_cont_res |> 
  mutate(Result_UID = as.character(Result_UID))


cont_diff_depths <- IR_cont_res %>%
  filter(AU_ID != '99') %>%
  mutate(Depth = ifelse(Depth == 'NA', NA, Depth )) %>%
  group_by(MLocID, Char_Name,Result_Date,  Result_Time) %>%
  mutate(num_in_group = n(),
         num_depths = n_distinct(Depth)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_depths > 1)


cont_diff_depths_exclude <- cont_diff_depths %>%
  group_by(group_num) %>%
  mutate(keep = case_when(Depth == min(Depth) ~ 1,
                          all(is.na(Depth)) ~ 1,
                          TRUE ~ 0)) %>%
  filter(keep == 0) %>%
  ungroup() %>%
  select(Result_UID, Char_Name) %>%
  mutate(Data_Review_Comment = "Continuous Data exists on the same date/time at shallower depth" )

dbAppendTable(IR.sql, 'Unused_Results', cont_diff_depths_exclude,row.names = NULL)

# seafet vs YSI ---------------------------------------------------------------------------------------------------

#Keep seafet and seaphox

equipment_cont_res <- IR_cont_res %>%
  filter(AU_ID != '99') %>%
  #filter(!ContResUID %in% cont_diff_depths_exclude$ContResUID) %>%
  mutate(Depth = ifelse(Depth == 'NA', NA, Depth )) %>%
  group_by(MLocID, Char_Name,Result_Date, Result_Time) %>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(Result_Numeric),
         num_depths = n_distinct(Depth)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_in_group > 1) %>%
  arrange(group_num) %>%
  ungroup() %>%
  group_by(group_num) %>%
  mutate(has_marine_equip = case_when(any(Equipment_ID %in% c("SeaFET", 'SeapHOx'))  ~ 1,
                                TRUE ~ 0),
         is_marine_equip = case_when(Equipment_ID %in% c("SeaFET", "SeapHOx") ~ 1,
                               TRUE ~ 0))

equipment_cont_res_exclude <- equipment_cont_res %>%
  filter(has_marine_equip == 1 & is_marine_equip == 0) %>%
  ungroup() %>%
  select(Result_UID, Char_Name) %>%
  mutate(Data_Review_Comment = "Multiple equipment exists at same date/time. More precise instrumentation is available" )


dbAppendTable(IR.sql, 'Unused_Results', equipment_cont_res_exclude,row.names = NULL)

exclusions <- dbReadTable(IR.sql, 'Unused_Results')

cont_dupes <- IR_cont_res %>%
  filter(AU_ID != '99') %>%
  filter(!Result_UID %in% exclusions$Result_UID) %>%
  # filter(!ContResUID %in% equipment_cont_res_exclude$ContResUID) %>%
  mutate(Depth = ifelse(Depth == 'NA', NA, Depth )) %>%
  group_by(MLocID, Char_Name,Result_Date, Result_Time, Depth) %>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(Result_Numeric),
         num_depths = n_distinct(Depth)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_in_group > 1) %>%
  arrange(group_num)

dbAppendTable(IR.sql, 'Unused_Results', cont_dupes,row.names = NULL)


# 
# continuous_exlude <- cont_diff_depths_exclude %>%
#   bind_rows(equipment_cont_res_exclude)
# 
# save(continuous_exlude, file = paste0("Validation/continuous_exlude-", Sys.Date(),".RData"))
# 
# 
# # By view ---------------------------------------------------------------------------------------------------------
# 
# 
# db_tables <- DBI::dbListTables(IR.sql)
# 
# 
# db_param_tables <- c(
#   'VW_Aluminum',   
#   'VW_Ammonia_AL',                                
#   'VW_Bacteria',
#   'VW_Chl',          
#   'VW_Copper',                                    
#   'VW_DO',           
#   'VW_FishTissue_Hg',                             
#   'VW_metals',       
#   'VW_Pentachlorophenol',                         
#   'VW_pH',           
#   'VW_Temperature',                  
#   'VW_ToxAL',        
#   'VW_ToxHH',                                     
#   'VW_Turbidity')    
# 
# groupings <- c(
#   'MLocID',
#   'Char_Name',
#   'Activity_Type', 
#   'SampleStartDate',
#   'SampleStartTime',
#   'Statistical_Base',
#   'act_depth_height',
#   'Result_Depth',
#   'Analytical_method',
#   'wqstd_code',
#   'Sample_Fraction',
#   'Char_Speciation',
#   'Time_Basis'
# )
# 
# for (i in 1:length(db_param_tables)){
#   
#   
#   tbl <- db_param_tables[i]
#   
#   print(paste("starting", tbl))
#   
#   IR_Res_qry <- paste0(
#     "Select *
#      FROM [IntegratedReport].[dbo].", tbl )
#   
#   IR_res <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql))
#   
#   group <- intersect(names(IR_res), groupings)
#   
#   
#   day_time_dups <- IR_res %>%
#     filter(AU_ID != '99') %>%
#     mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
#     group_by_at(group) %>%
#     mutate(num = n(),
#            num_distinct_results = n_distinct(IRResultNWQSunit),
#            num_resUID = n_distinct(Result_UID)) %>%
#     mutate(group_num =cur_group_id()) %>%
#     filter(num > 1,
#            num_distinct_results > 1) %>%
#     ungroup() %>%
#     arrange(group_num, MLocID, SampleStartDate, Char_Name)
#   
#   
#   write.xlsx(day_time_dups, file = paste0("Validation/Dupdata/", tbl, "_same_date_time_method_diff_result.xlsx"))
#   
#   
# }


