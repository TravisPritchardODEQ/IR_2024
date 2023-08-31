#script to investigate questionable data to be investigated. 


library(DBI)
library(odbc)
library(glue)
library(tidyverse)
library(openxlsx)



options(scipen = 999999)



# Get data from the IR database -----------------------------------------------------------------------------------


# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR_Dev")

# # Function to import custom input raw script
# # This script queries input raw, but includes all the paramter view conditions. 
# # This allows us to check duplciates only on data used in the parameter assessments
# 
# getSQL <- function(filepath){
#   con = file(filepath, "r")
#   sql.string <- ""
#   
#   while (TRUE){
#     line <- readLines(con, n = 1)
#     
#     if ( length(line) == 0 ){
#       break
#     }
#     
#     line <- gsub("\\t", " ", line)
#     
#     if(grepl("--",line) == TRUE){
#       line <- paste(sub("--","/*",line),"*/")
#     }
#     
#     sql.string <- paste(sql.string, line)
#   }
#   
#   close(con)
#   return(sql.string)
# }
# 
# 
# 
# IR_Res_qry <- getSQL("Validation/InputRaw limited to data views.sql")
# 
# 
# IR_res_db <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql))
IR_res_db <- dbReadTable(IR.sql, 'ResultsRawWater')

# Get data exclusion table

exclusions <- dbReadTable(IR.sql, 'Unused_Results')


IR_res <- IR_res_db %>%
  filter(!Result_UID %in% exclusions$Result_UID)



# Straight Duplicates ---------------------------------------------------------------------------------------------

# These are the values that are suspected duplicates
# Need to be investigated in AWQMS

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
           #wqstd_code,
           Sample_Fraction,
           Char_Speciation,
           Time_Basis) %>%
  mutate(num = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID)) %>%
  mutate(group_num =cur_group_id()) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name) %>%
  filter(num_resUID > 1 & num_distinct_results == 1) %>%
  arrange(group_num) |> 
  mutate(dup_type = "Duplicate")


# same day/time/method different result -----------------------------------------------------------------------------
 #need to be investigated in AWQMS

day_time_dups <- IR_res %>%
  filter(AU_ID != '99',
         Result_Unit != "mV") |> 
  filter(!(Result_UID %in% straight_duplicates$Result_UID)) %>%
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
           #wqstd_code,
           Sample_Fraction,
           Char_Speciation,
           Time_Basis) %>%
  mutate(num = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_activity_ID = n_distinct(act_id)) %>%
  mutate(group_num =cur_group_id() + 10000000000) %>%
  filter(num > 1,
         num_distinct_results > 1) %>%
  ungroup() %>%
  arrange(group_num, MLocID, SampleStartDate, Char_Name) |> 
  mutate(dup_type = "Same Day/Time/Method; dif result")





# Write to excel --------------------------------------------------------------------------------------------------
# 
# l <- list("straight_duplicates" = straight_duplicates, "day_time_dups" = day_time_dups)
# write.xlsx(l, file = "Validation/AWQMS_duplicates.xlsx")


all_together <- bind_rows(straight_duplicates, day_time_dups)
write.xlsx(all_together,  file = "Validation/AWQMS_duplicates.xlsx")
