library(tidyverse)
library(openxlsx)




# Bring in assessments --------------------------------------------------------------------------------------------

AU_decision_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/DraftList/Public Draft/IR_2024_Rollup-2024-04-16_w_TMDL_Priority_Rankings.xlsx")



options(scipen = 999) 
# New Assessments -------------------------------------------------------------------------------------------------

new_Assessments <- AU_decision_import |> 
  filter(status_change == 'New Assessment')



# Delistings ------------------------------------------------------------------------------------------------------


delist <- AU_decisions |> 
  filter(status_change == 'Delist')



delist_param <- delist |> 
  group_by(Char_Name) |> 
  summarise(count = n()) |> 
  arrange(count)

ggplot(delist_param) +
  geom_col(aes(x = fct_reorder(Char_Name, count, .desc = TRUE), y = count))+
  labs(y = "Num. Delistings",
       title = "2024 IR Delistings")+
  theme_bw() +
  theme(axis.title.x=element_blank(),
        text = element_text(size=18))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45))
 



# Data Sources ----------------------------------------------------------------------------------------------------


library(DBI)
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

inputRaw <- tbl(con, "InputRaw") |> 
  group_by(OrganizationID) |> 
  summarise(count = n()) |> 
  collect()


inputRaw_cont <- tbl(con, "InputRaw_ContpH") |> 
  group_by(OrganizationID) |> 
  summarise(count = n()) |> 
  collect()


inputRaw_1 <- inputRaw |> 
  mutate(type = "Grab (including cont. metrics)")

inputRaw_cont_1 <- inputRaw_cont |> 
  mutate(type = "Continuous (pH)")


IR_data_org <- bind_rows(inputRaw_1, inputRaw_cont_1)



org_class <- data.frame(
  stringsAsFactors = FALSE,
                 OrganizationID = c("1119USBR_WQX","11NPSWRD_WQX","21ORBCH",
                                    "ADVENTURESCIENTISTS","ARAUCO(NOSTORETID)",
                                    "ARCLIN(NOSTORETID)",
                                    "ARKEMA_GROUNDWATER(NOSTORETID)","BENTON_SWCD","BLM(NOSTORETID)",
                                    "BURNSPAIUTE","CASCADE_PAC_PULP(NOSTORETID)",
                                    "CHERRY_RIVERSIDE(NOSTORETID)",
                                    "CHERRYGROWERS(NOSTORETID)","CITY_ALBANY(NOSTORETID)",
                                    "CITY_ASHLAND(NOSTORETID)",
                                    "CITY_ASTORIA(NOSTORETID)","CITY_BEND(NOSTORETID)",
                                    "CITY_BROOKINGS(NOSTORETID)",
                                    "CITY_CANBY(NOSTORETID)","CITY_COTTAGEGROVE(NOSTORETID)",
                                    "CITY_DALLAS(NOSTORETID)",
                                    "CITY_EUGENE(NOSTORETID)","CITY_GRESHAM(NOSTORETID)",
                                    "CITY_HERMISTON(NOSTORETID)",
                                    "CITY_JEFFERSON(NOSTORETID)","CITY_KERNVILLE-GLENEDEN(NOSTORETID)",
                                    "CITY_KLAMATHFALLS(NOSTORETID)",
                                    "CITY_LAKEOSWEGO(NOSTORETID)",
                                    "CITY_LEBANON(NOSTORETID)","CITY_LINCOLN(NOSTORETID)",
                                    "CITY_MCMINNVILLE(NOSTORETID)",
                                    "CITY_MILWAUKIE(NOSTORETID)","CITY_MOLALLA(NOSTORETID)",
                                    "CITY_MYRTLECREEK(NOSTORETID)",
                                    "CITY_MYRTLEPOINT(NOSTORETID)","CITY_NEWBERG(NOSTORETID)",
                                    "CITY_NEWPORT(NOSTORETID)",
                                    "CITY_ONTARIO(NOSTORETID)","CITY_OREGONCITY(NOSTORETID)",
                                    "CITY_PHILOMATH(NOSTORETID)",
                                    "CITY_PRINEVILLE(NOSTORETID)","CITY_SALEM(NOSTORETID)",
                                    "CITY_SANDY(NOSTORETID)","CITY_STAYTON(NOSTORETID)",
                                    "CITY_STHELENS/BC(NOSTORETID)",
                                    "CITY_SWEETHOME(NOSTORETID)",
                                    "CITY_THEDALLES(NOSTORETID)","CITY_TILLAMOOK(NOSTORETID)",
                                    "CITY_TROUTDALE(NOSTORETID)",
                                    "CITY_WESTLINN(NOSTORETID)","CITY_WILLAMINA(NOSTORETID)",
                                    "CITY_WILSONVILLE(NOSTORETID)",
                                    "CITY_WOODBURN(NOSTORETID)","CLACKAMAS_SWCD","CLPWC",
                                    "COLUMBIA_SWCD","COOSWA","COQUILLE_WA",
                                    "COQUILLETRIBE_WQX","CRITFC(NOSTORETID)","CRK_WQX",
                                    "CTCLUSI_WQX","CTGR","CTUIR_WQX",
                                    "CURRY_WP","CWS(NOSTORETID)","DRA(NOSTORETID)",
                                    "DRYCRK_LF(NOSTORETID)","DVWD_(NOSTORETID)",
                                    "DYNONOBEL(NOSTORETID)",
                                    "EMSWCD(NOSTORETID)","EVRAZ(NOSTORETID)",
                                    "FRANKLUMBER(NOSTORETID)","GILLIAM_SWCD","GP-WM(NOSTORETID)",
                                    "GP_HALSEY(NOSTORETID)",
                                    "GRAPHIC_PKG(NOSTORETID)","HRWG_AW(NOSTORETID)","HW",
                                    "INTERNATIONALPAPER(NOSTORETID)","IPC(NOSTORETID)",
                                    "JCWC_AW(NOSTORETID)",
                                    "KLAMATH_SWCD(NOSTORETID)","KLAMATHTRIBES_WQX",
                                    "KRAFTHEINZ(NOSTORETID)","LONGTOM_WC","LSWCD_WQX",
                                    "LWC_(NOSTORETID)","MCHI_SHERIDAN(NOSTORETID)",
                                    "MEDFORD_WC(NOSTORETID)",
                                    "MIDDLEDESCHUTES_WC","NALMS","NARS_WQX","NCWA_WQX",
                                    "NERRS_SS(NOSTORETID)","NFJOHNDAYWC_(NOSTORETID)",
                                    "NNWC_(NOSTORETID)",
                                    "NOAANEWPORTLINE_(NOSTORETID)","OAKLODGE(NOSTORETID)",
                                    "ODA(NOSTORETID)","ODFW_(NOSTORETID)",
                                    "OOI_(NOSTORETID)","OREGONDEQ","PBWC_WQX",
                                    "PDX_BES(NOSTORETID)","PDX_WB(NOSTORETID)",
                                    "PRE_RD_PDXHARBOR(NOSTORETID)","PUR_WQX",
                                    "ROSEBURG_LF(NOSTORETID)","RRK_WQX","RRWC_WQX","RVCG",
                                    "RVSS(NOSTORETID)",
                                    "SHERMAN_WC(NOSTORETID)","SILTRONIC(NOSTORETID)","SIUSLAW",
                                    "SLLI(NOSTORETID)","SOUTHSANTIAM_WC",
                                    "SSSD(NOSTORETID)","SSWCD",
                                    "SUNSTONE_CIRCUTS(NOSTORETID)","TILLAMOOK_EP","TUALATIN_SWCD",
                                    "UDESCHUTES_WC(NOSTORETID)",
                                    "UMPQUA_SWCD(NOSTORETID)","UNEHALEM_WC(NOSTORETID)",
                                    "UNIVAR(NOSTORETID)","UPPERWILLAMETTE_SWCD",
                                    "USEPA_(NOSTORETID)","USFS(NOSTORETID)","USGS-ID",
                                    "USGS-OR","USGS-OR(INTERNAL)",
                                    "VM_SDCWC_AW","WALLAWALLA_WC",
                                    "WASCO_SWCD(NOSTORETID)","WES(NOSTORETID)",
                                    "WES_KELLOGGCREEK(NOSTORETID)","WES_TRICITY(NOSTORETID)",
                                    "WEYERHAUSER(NOSTORETID)",
                                    "WILL_FALLS_PAPER(NOSTORETID)"),
                          Class = c("Federal","Federal","DEQ","Other","Permittee",
                                    "Permittee","Permittee","Volunteer",
                                    "Federal","Tribal","Permittee","Permittee",
                                    "Permittee","Permittee","Permittee",
                                    "Permittee","Call For Data","Permittee",
                                    "Permittee","Permittee","Permittee","Permittee",
                                    "Permittee","Permittee","Call For Data",
                                    "Call For Data","Permittee","Permittee",
                                    "Permittee","Permittee","Permittee",
                                    "Permittee","Permittee","Permittee",
                                    "Permittee","Permittee","Permittee","Permittee",
                                    "Permittee","Call For Data","Permittee",
                                    "Permittee","Permittee","Permittee",
                                    "Permittee","Permittee","Permittee","Permittee",
                                    "Permittee","Permittee","Call For Data",
                                    "Permittee","Permittee","Volunteer",
                                    "Volunteer","Volunteer","Volunteer","Volunteer",
                                    "Tribal","Tribal","Volunteer","Tribal",
                                    "Tribal","Tribal","Volunteer",
                                    "Permittee","Call For Data","Permittee",
                                    "Call For Data","Permittee","Call For Data",
                                    "Permittee","Permittee","Volunteer","Permittee",
                                    "Permittee","Permittee","Volunteer",
                                    "Volunteer","Permittee","Call For Data",
                                    "Volunteer","Volunteer","Tribal","Permittee",
                                    "Volunteer","Volunteer","Volunteer",
                                    "Permittee","Volunteer","Volunteer","Other",
                                    "Federal","Volunteer","Call For Data",
                                    "Volunteer","Call For Data","Call For Data",
                                    "Permittee","Volunteer","Call For Data",
                                    "Call For Data","DEQ","Volunteer",
                                    "Permittee","Call For Data","Call For Data",
                                    "Volunteer","Permittee","Volunteer","Volunteer",
                                    "Volunteer","Permittee","Volunteer",
                                    "Permittee","Volunteer","Permittee",
                                    "Volunteer","Permittee","Volunteer","Permittee",
                                    "Volunteer","Volunteer","Volunteer",
                                    "Volunteer","Volunteer","Permittee","Volunteer",
                                    "Federal","Federal","Federal","Federal",
                                    "Federal","Volunteer","Volunteer",
                                    "Volunteer","Permittee","Permittee",
                                    "Permittee","Call For Data","Permittee")
             )

IR_data_org_class <- IR_data_org |> 
  left_join(org_class)

class_counts <- IR_data_org_class |> 
  group_by(Class,type ) |> 
  summarise(count = sum(count))

library(ghibli)


ggplot(IR_data_org_class)+
   geom_col(aes(x = fct_relevel(Class, c("Other", 'DEQ', 'Tribal', 'Call For Data', 'Volunteer', "Permittee", 
                                         "Federal")), y = count, fill = type), width = 0.7)+
  coord_flip()+
  theme_minimal()+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  labs( title = "2024 IR Data Sources",
        y = "Count of Results")+
  theme(axis.title.y=element_blank(),
        text = element_text(size=18),
        legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma)




# gen stats -------------------------------------------------------------------------------------------------------

inputRaw <- tbl(con, "InputRaw") |> 
  select(MLocID) |> 
  distinct() |> 
  collect()




# status by attains group -----------------------------------------------------------------------------------------

library(DBI)
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


LU_Pollutant_import <- tbl(con, 'LU_Pollutant') |> 
  select(Pollu_ID, Attains_Group) |> 
  distinct() |> 
  collect()


LU_Pollutant <- LU_Pollutant_import |> 
  mutate(Attains_Group = str_to_title(Attains_Group),
         Attains_Group2 = case_when(str_detect(Attains_Group, "Toxic") ~ "Toxics",
                                    str_detect(Attains_Group, "Radiation") ~ "Toxics",
                                    str_detect(Attains_Group, "Pesticides") ~ "Toxics",
                                    str_detect(Attains_Group, "Ammonia") ~ "Toxics",
                                    str_detect(Attains_Group, "Metals") ~ "Toxics",
                                    str_detect(Attains_Group, "Dioxins") ~ "Toxics",
                                    str_detect(Attains_Group, "Mercury") ~ "Toxics",
                                    str_detect(Attains_Group, "Polychlorinated") ~ "Toxics",
                                    str_detect(Attains_Group, "Chlorine") ~ "Toxics",
                                    str_detect(Attains_Group, "Consumption Advisory") ~ "Toxics",
                                    str_detect(Attains_Group, "Pathogens") ~ "Bacteria",
                                    str_detect(Attains_Group, "Cause Unknown - Impaired Biota") ~ "Biocriteria",
                                    TRUE ~ Attains_Group
                                    ),
         Attains_group3 = case_when(Attains_Group2 == "Toxics" ~ "Toxics",
                                    Pollu_ID == 187 ~"Toxics",
                                    Pollu_ID == 28 ~"Toxics",
                                    Attains_Group2 == 'Algal' ~ 'Algal\nGrowth',
                                    Pollu_ID == 174 ~'Algal\nGrowth',
                                    Pollu_ID == 173 ~'Algal\nGrowth',
                                    Attains_Group2 == 'Bacteria' ~ 'Bacteria',
                                    Attains_Group2 == 'Ph' ~ 'pH',
                                    Attains_Group2 == 'Temperature' ~ 'Temperature',
                                    Attains_Group2 == 'Dissolved Oxygen' ~ 'Dissolved\nOxygen',
                                    Pollu_ID == 189 ~'Dissolved\nOxygen',
                                    Attains_Group2 == 'Biocriteria' ~ 'Biocriteria',
                                    Attains_Group2 == 'Biotoxins' ~ 'Algal\nGrowth',
                                    TRUE ~ "Other"
                                    
                                    
           
           
         )) |> 
  select(Pollu_ID, Attains_group3)
  
  

AU_decisions <- AU_decision_import |> 
  left_join(LU_Pollutant) |> 
  mutate(status = case_when(grepl('5', final_AU_cat) | grepl('4', final_AU_cat) ~ "Impaired",
                            grepl('3', final_AU_cat) ~ "Insufficient",
                            grepl('2',final_AU_cat) ~ 'Attain',
                            TRUE ~ 'Unassessed')) |> 
  filter(status != 'Unassessed')



# By parameter assessment -----------------------------------------------------------------------------------------



graph_data <- AU_decisions |> 
  select(Attains_group3, status) |> 
  filter(!is.na(Attains_group3)) |> 
  mutate(status = factor(status, ordered = TRUE, 
                         levels = c("Insufficient", "Impaired", "Attain")))


IR_colors <-  c("#01856F", "#D5B502", "#9A00C4")
names(IR_colors) <- c('Attain', "Insufficient", "Impaired")

ggplot(graph_data, aes(Attains_group3, fill = status))+
  coord_flip()+
  geom_bar(position=position_dodge())+
  scale_fill_manual(values = IR_colors)+
  theme_minimal()+
  labs(y = "Number of assessments",
       x = element_blank())+
  theme(legend.title=element_blank(),
        text = element_text(size = 20))


# By AU -----------------------------------------------------------------------------------------------------------


# 
# graph_data <- AU_decisions |> 
#   mutate(status = factor(status, ordered = TRUE, 
#                         levels = c("Impaired", "Attain", "Insufficient"))) |> 
#   group_by(AU_ID, Attains_group3) |>
#   summarise(overall_status = max(status))
# 
# 
# ggplot(graph_data, aes(Attains_group3, fill = overall_status))+
#   coord_flip()+
#   geom_bar(position=position_dodge())+
#   scale_fill_manual(values = IR_colors)
#     

