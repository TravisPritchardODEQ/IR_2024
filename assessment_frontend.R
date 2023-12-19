library(tidyverse)
#devtools::install_github('TravisPritchardODEQ/odeqIRtools')
library(odeqIRtools)



# Temperature Assessment ------------------------------------------------------------------------------------------


source('Parameters/Temperature/fun_temp_data.R')
source('Parameters/Temperature/fun_temp_assess.R')




temp_results <- temp_data("IR_Dev")
temp_assessments <- fun_temp_analysis(temp_results, write_excel = TRUE)

save(temp_assessments, file = 'Parameters/Outputs/rdata files/temperature.Rdata')



# chl assessment --------------------------------------------------------------------------------------------------

source("Parameters/Chl/fun_chl_data.R")
source("Parameters/Chl/fun_chl_analysis.R")


Results_censored_chla <- chla_data("IR_Dev")



chla_summary <- chl_assessment(Results_censored_chla)

save(chla_summary, file = 'Parameters/Outputs/rdata files/chla.Rdata')