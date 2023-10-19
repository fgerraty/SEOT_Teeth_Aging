###############################################################################
# Sea Otter Teeth Aging Project ###############################################
# Code Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
###############################################################################
# Script 01: Data Clean + Manipulation ########################################
#------------------------------------------------------------------------------

# Part 1: Load raw dataset ----------------------------------------------------

SEOT_teeth_raw <- read_excel("data/raw/SEOT_teeth_raw.xlsx")

# Part 2: Clean and summarize -------------------------------------------------

# Categorize tooth types (premolars, canines, incisors, molars, or unknown)
P <- c("P2a","P2b","P2","Pa","Pb","P","UP","LLP2","URP3","URP2","ULP3","ULP2","URP2 or ULP2","P2, additional tooth sent in 2023","LRP2","URP4")
C <- c("C","URC","LRC","ULC","ULC (BROKEN)")
I <- c("URI3","URI1","I3","ULI3","LLI2","LLI1","LRI2","UI1?","I2","UI2","LRI1")
M <- c("M","URM1","LRM1","ULM1","M2")
UNK <- c("UNK", "UNKa", "UNKb")


#Clean raw dataset
SEOT_teeth <- SEOT_teeth_raw %>% 
  clean_names()%>% 
#Remove individuals with no teeth collected and/or no ages calculated due to tooth damage
  filter(tooth_collected == "Y",
         otter_num != "SOD-2021-200", #individual with tooth damage (couldnt be aged)
         otter_num != "KEFJ-SOD-2014-01" #individual with certainty code missing
         ) %>% 
#Categorize teeth to broad categories
  mutate(tooth_category = 
           #categorize all unknown teeth as "UNK"           
           case_when(tooth %in% UNK ~ "UNK",
                     tooth %in% C ~ "C",
                     tooth %in% P ~ "P",
                     tooth %in% I ~ "I",
                     tooth %in% M ~ "M")) %>% 
#keep only relevant columns
  select(year, collection_date, 
         otter_id = otter_num,
         tooth_id = tooth_num, 
         area, age, age_class, certainty_code, matson_min, matson_max, matson_notes, sex, tooth_category, latitude, longitude)








#Uncertainty by tooth type
tooth_type <- SEOT_teeth %>% 
  group_by(tooth_category, certainty_code) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  ungroup()



