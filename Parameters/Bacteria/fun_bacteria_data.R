

Bacteria_data <- function(database) {
  
  
  require(tidyverse)
  require(RODBC)
  require(odeqIRtools)

  print("Fetch bacteria data from IR database")
  #connect to IR database view as a general user
  # import Temperature data
  IR.sql <-  DBI::dbConnect(odbc::odbc(), database)
  

  Results_import <- DBI::dbReadTable(IR.sql, "VW_Bacteria") 


  DBI::dbDisconnect(IR.sql)
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_tibble -> Results_import 
  
  Results_import <- Results_import |> 
    mutate(Result_UID = as.character(Result_UID))
  
  Results_import <- odeqIRtools::data_aggregation(Results_import)
  

# Data censoring --------------------------------------------------------------------------------------------------
  
  print("Data censor process")
  # Get all the standards to be used when dealing with the censored data
  Results_crit <- Results_import %>%
    # Get lowest criteria value to set censored results
    mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  
Results_censored <- censor_data(Results_crit, crit = lowest_crit)
  
  
  return(Results_censored)
  
}
