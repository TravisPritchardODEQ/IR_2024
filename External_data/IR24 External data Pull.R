# External data pull
library(tidyverse)
library(httr2)
library(sf)
library(tibble)
library(rlang)
library(runner)
library(odeqIRextdata)



#NWIS data pull. Ran 6/16/2023
NWIS_cont_data_pull(start.date = "2017-12-26", 
                    end.date = "2022-12-31",
                    project = "Integrated Report - Call for Data",
                    save_location = "C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/DataAssembly/NWIS/")



#BES data pull. Ran 7/12/2023
  # Get password
source('External_data/pdx_BES_pass.R')

BES <- PDX_BES_data(userid, pass,
                     save_location = "C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/DataAssembly/BES/",
                     startdate = '2016/01/01',
                     enddate = '2020/12/31')


  
#NERRS
NERRS_sum_stats(path = 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/DataAssembly/NERRS/Orginal Files/')
  