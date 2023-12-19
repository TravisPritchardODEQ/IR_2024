#  

#Travis - can make a function with the rest of the code 
#fw_bio <- function(df, write_excel = TRUE){
  
  library(runner)
  library(openxlsx)

stn <- Results_import %>%
       select(MLocID,AU_GNIS_Name,GNIS_Name) %>%
       distinct()

#### Watershed units - Mloc assessment ####  
# single Sample MWCF 
  MWCF_SS_WS <- Results_import %>%
    filter(EcoRegion2 == "MARINE WEST COAST FOREST" & str_detect(AU_ID, "WS", negate = FALSE))%>%
    group_by(AU_ID,MLocID) %>% 
    mutate(num_Samples = n()) %>%
    filter(num_Samples == 1) %>%
    summarise(num_Samples = n(),
              n_over_5 = sum(as.numeric(Score >= 20)),
              n_btwn_3B = sum(as.numeric(Score) >= 15 & as.numeric(Score) <= 20),
              n_btwn_3C = sum(as.numeric(Score) >= 9 & as.numeric(Score) <= 14),
              n_less_3C = sum(as.numeric(Score) <= 8)) %>%
    mutate(IR_Cat = case_when(n_over_5 >=1 ~"5",
                              n_btwn_3B >=1 ~"3B",
                              n_btwn_3C >= 1 ~ "3C",
                              n_less_3C  >= 1 ~ "2",
                              TRUE ~ "ERROR"),
           mean_all_samples = NA,
           model = "MWCF") %>% 
    select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
           "n_btwn_3C","n_less_3C",model,"IR_Cat")
### multiple Sample at the same station or GNIS MWCF 
  MWCF_AU_MS_WS = Results_import %>%
    filter(EcoRegion2 == "MARINE WEST COAST FOREST"& str_detect(AU_ID, "WS", negate = FALSE)) %>%
    group_by(AU_ID,MLocID) %>% 
    mutate(num_Samples = n()) %>%
    filter(num_Samples >= 2) %>%
    summarise(num_Samples = n(),
              n_over_5 = sum(as.numeric(Score) >= 15),
              n_btwn_3C = sum(as.numeric(Score) >= 9 & as.numeric(Score) <= 14),
              n_less_3C = sum(as.numeric(Score) <= 8),
              mean_all_samples = mean(as.numeric(Score))) %>%
    mutate(IR_Cat = case_when(mean_all_samples >= 15 ~"5",
                                  mean_all_samples >= 9 & mean_all_samples <= 14 ~ "3C",
                                  mean_all_samples <= 8 ~ "2",
                                  TRUE ~ "ERROR"),
           n_btwn_3B = NA,
           model = "MWCF") %>% 
    select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
           "n_btwn_3C","n_less_3C",model,"IR_Cat")

#### single Sample WCCP
WCCP_AU_SS_WS = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>% 
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(Score >= 27),
            n_btwn_3B = sum(Score >= 22 & Score <= 26),
            n_btwn_3C = sum(Score >= 8 & Score <= 21),
            n_less_3C = sum(Score <= 7)) %>%
  mutate(IR_Cat = case_when( n_over_5 >=1 ~"5",
                             n_btwn_3B >=1 ~"3B",
                             n_btwn_3C >= 1 ~ "3C",
                             n_less_3C >= 1 ~ "2",
                            TRUE ~ "ERROR"),
  mean_all_samples = NA,
  model = "WCCP") %>% 
  select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat")


WCCP_AU_MS_WS = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(Score >= 22),
            n_btwn_3C = sum(Score >= 8 & Score <= 21),
            n_less_3C = sum(Score <= 7),
            mean_all_samples = mean(as.numeric(Score))) %>%
  mutate(IR_Cat = case_when(mean_all_samples >= 22 ~"5",
                                mean_all_samples >= 8 & mean_all_samples <= 21 ~ "3C",
                                mean_all_samples <= 7 ~ "2",
                                TRUE ~ "ERROR"),
         n_btwn_3B = NA,
         model = "WCCP") %>% 
  select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat")

WS_Station_Cat <- rbind(MWCF_SS_WS,MWCF_AU_MS_WS,WCCP_AU_SS_WS,WCCP_AU_MS_WS) %>% 
                  left_join(stn, by = "MLocID") %>% 
                  mutate(Rationale = case_when(IR_Cat == "2" ~ paste("Attaining - samples from",
                                                                       MLocID,"below applicable taxa loss benchmark for",model,"model region"),
                                               IR_Cat == "5" ~ paste("Impaired - samples from",
                                                                     MLocID,"above applicable taxa loss benchmark for",model,"model region"),
                                               IR_Cat == "3B" ~ paste("Insufficient Data; Potential Concern - samples from",
                                                                    MLocID,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                                               IR_Cat == "3C" ~ paste("Insufficient Data; Non-Reference - samples from",
                                                                     MLocID,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                                               TRUE ~ "ERROR"),
                           
                         Pollu_ID = 156,
                         wqstd_code = 5,
                         Char_Name = "BioCriteria",
                          period = NA) %>% 
                    select(AU_ID, MLocID, AU_GNIS_Name, GNIS_Name,Char_Name,Pollu_ID,wqstd_code,
                           period,IR_Cat,Rationale)%>% 
                    rename(IR_category = IR_Cat)

#### WS_GNIS_Cat ##### 

WS_GNIS_Cat <- WS_Station_Cat %>% 
               group_by(AU_ID,AU_GNIS_Name) %>% 
               summarise(n_mlocids = n(),
                         n_cat5 = sum(IR_category == "5"),
                         n_cat2 = sum(IR_category == "2"),
                         n_cat3C = sum(IR_category == "3C"),
                         n_cat3B = sum(IR_category == "3B"),
                         Rationale_GNIS = paste(Rationale, collapse = " ; ")) %>% 
              mutate(IR_category_GNIS_24 = case_when(n_cat5 >= 1 ~ "5",
                                             n_cat2 >= 1 & n_cat5 == 0  ~ "2",
                                             n_cat3B >= 1& n_cat5 == 0  & n_cat2 == 0   ~"3B", 
                                             n_cat3C >= 1& n_cat5 == 0  & n_cat2 == 0  ~"3C"),
                     Pollu_ID = 156,
                     wqstd_code = 5,
                     Char_Name = "BioCriteria",
                     period = NA) %>% 
                select(AU_ID, AU_GNIS_Name,Char_Name,Pollu_ID,wqstd_code,period,
                       IR_category_GNIS_24,Rationale_GNIS)
                     
                                            
### River Stream Units ####
MWCF_SS_SR <- Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST" & str_detect(AU_ID, "SR", negate = FALSE))%>%
  group_by(AU_ID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(as.numeric(Score >= 20)),
            n_btwn_3B = sum(as.numeric(Score) >= 15 & as.numeric(Score) <= 20),
            n_btwn_3C = sum(as.numeric(Score) >= 9 & as.numeric(Score) <= 14),
            n_less_3C = sum(as.numeric(Score) <= 8),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when(n_over_5 >=1 ~"5",
                            n_btwn_3B >=1 ~"3B",
                            n_btwn_3C >= 1 ~ "3C",
                            n_less_3C  >= 1 ~ "2",
                            TRUE ~ "ERROR"),
         mean_all_samples = NA,
         model = "MWCF") %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)

### multiple Sample at the same station or GNIS MWCF 
MWCF_MS_SR = Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST"& str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(as.numeric(Score) >= 15),
            n_btwn_3C = sum(as.numeric(Score) >= 9 & as.numeric(Score) <= 14),
            n_less_3C = sum(as.numeric(Score) <= 8),
            mean_all_samples = mean(as.numeric(Score)),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when(mean_all_samples >= 15 ~"5",
                            mean_all_samples >= 9 & mean_all_samples <= 14 ~ "3C",
                            mean_all_samples <= 8 ~ "2",
                            TRUE ~ "ERROR"),
         n_btwn_3B = NA,
         model = "MWCF") %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)

#### single Sample WCCP
WCCP_SS_SR = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>% 
  filter(str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(Score >= 27),
            n_btwn_3B = sum(Score >= 22 & Score <= 26),
            n_btwn_3C = sum(Score >= 8 & Score <= 21),
            n_less_3C = sum(Score <= 7),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when( n_over_5 >=1 ~"5",
                             n_btwn_3B >=1 ~"3B",
                             n_btwn_3C >= 1 ~ "3C",
                             n_less_3C >= 1 ~ "2",
                             TRUE ~ "ERROR"),
         mean_all_samples = NA,
         model = "WCCP",) %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)


WCCP_MS_SR = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  filter(str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(Score >= 22),
            n_btwn_3C = sum(Score >= 8 & Score <= 21),
            n_less_3C = sum(Score <= 7),
            mean_all_samples = mean(as.numeric(Score)),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when(mean_all_samples >= 22 ~"5",
                            mean_all_samples >= 8 & mean_all_samples <= 21 ~ "3C",
                            mean_all_samples <= 7 ~ "2",
                            TRUE ~ "ERROR"),
         n_btwn_3B = NA,
         model = "WCCP") %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)


Other_AU_Cat <- rbind(MWCF_SS_SR,MWCF_MS_SR,WCCP_SS_SR,WCCP_MS_SR) %>% 
  mutate(Rationale = case_when(IR_Cat == "2" ~ paste("Attaining - samples from",
                                                     MLocIDs,"below applicable taxa loss benchmark for",model,"model region"),
                               IR_Cat == "5" ~ paste("Impaired - samples from",
                                                     MLocIDs,"above applicable taxa loss benchmark for",model,"model region"),
                               IR_Cat == "3B" ~ paste("Insufficient Data; Potential Concern - samples from",
                                                      MLocIDs,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                               IR_Cat == "3C" ~ paste("Insufficient Data; Non-Reference - samples from",
                                                      MLocIDs,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                               TRUE ~ "ERROR"),
         
         Pollu_ID = 156,
         wqstd_code = 5,
         Char_Name = "BioCriteria",
         period = NA) %>% 
  select(AU_ID,Pollu_ID,wqstd_code,
         period,IR_Cat,Rationale,MLocIDs)%>% 
  rename(IR_category = IR_Cat)

Data <- Results_import %>% 
        mutate(Char_Name = "BioCriteria",
               Pollu_ID = 156,
               wqstd_code = 5,
               period = NA) %>% 
        select(AU_ID,MLocID,AU_GNIS_Name,GNIS_Name,Char_Name,Pollu_ID,wqstd_code,period, 
               EcoRegion2,Index_Name,Score)