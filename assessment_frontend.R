library(tidyverse)
#devtools::install_github('TravisPritchardODEQ/odeqIRtools', ref = 'prev_assessment_join')
library(odeqIRtools)

source('Parameters/Temperature/fun_temp_data.R')




Results <- temp_data("IR_Dev")
df <- Results
