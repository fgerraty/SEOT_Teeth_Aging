###############################################################################
# Sea Otter Teeth Aging Project ###############################################
# Code Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
###############################################################################
# Script 01: Data Clean + Manipulation ########################################
#------------------------------------------------------------------------------

###############################################
# Part 1: Load raw dataset ####################
###############################################

SEOT_teeth_raw <- read_excel("data/raw/SEOT_teeth_raw.xlsx")

###############################################
# Part 2: Clean and summarize datasets ########
###############################################

# Part 2A: Categorize tooth types (premolars, canines, incisors, molars, or unknown) ----
P <- c("P2a","P2b","P2","Pa","Pb","P","UP","LLP2","URP3","URP2","ULP3","ULP2","URP2 or ULP2","P2, additional tooth sent in 2023","LRP2","URP4")
C <- c("C","URC","LRC","ULC","ULC (BROKEN)")
I <- c("URI3","URI1","I3","ULI3","LLI2","LLI1","LRI2","UI1?","I2","UI2","LRI1")
M <- c("M","URM1","LRM1","ULM1","M2")
UNK <- c("UNK", "UNKa", "UNKb")


# Part 2B: Generate clean, single-tooth dataset ----------------------------------------
SEOT_teeth <- SEOT_teeth_raw %>% 
  clean_names()%>% 
#Remove individuals with no teeth collected and/or no ages calculated due to tooth damage
  filter(tooth_collected == "Y",
         otter_num != "SOD-2021-200", #individual with tooth damage (couldnt be aged)
         otter_num != "KEFJ-SOD-2014-01",#individual with certainty code missing
         otter_num != "SOD-2006-17/ capture otter SO-05-04",
         otter_num != "SOD-2007-02/capture otter SO-01-01", #teeth pulled different years
         otter_num != "SOD-2007-01/ capture otter SO-04-24", #teeth pulled different years
         otter_num != "WPWS-SOD-2014-02/ capture otter SO-2010-08" #teeth pulled different years
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

# Part 2C: Generate clean, 2-tooth dataset ----------------------------------------------

tooth_age_comparison <- SEOT_teeth %>% 
  select(otter_id, age, age_class, certainty_code, tooth_category) %>% 
  #Group by otter ID and then create a new line for each combination of 2 teeth within the same otter
  group_by(otter_id) %>% 
  reframe(age = t(combn(age, 2)),
          age_class = t(combn(age_class, 2)),
          certainty_code = t(combn(certainty_code, 2)),
          tooth_category = t(combn(tooth_category, 2))) %>% 
  #Extract values from list objects
  mutate(age1 = age[,1],
         age2 = age[,2],
         age_diff = abs(age1-age2),
         age_class1 = age_class[,1],
         age_class2 = age_class[,2],
         certainty_code1 = certainty_code[,1],
         certainty_code2 = certainty_code[,2],
         tooth_category1 = tooth_category[,1],
         tooth_category2 = tooth_category[,2]) %>% 
  #Remove listed objects (redundant)
  select(-age,-age_class,-certainty_code,-tooth_category) %>% 
  #Calculate binary (1/0) response of same age and age class values
  mutate(age_agreement = if_else(age1 == age2, 1,0),
         age_class_agreement =if_else(age_class1 == age_class2, 1,0)) %>% 
  #Generate certainty_code combinations 
  mutate(certainty_code_combo = case_when(
    certainty_code1 == "A" & certainty_code2 == "A" ~ "AA",
    certainty_code1 == "A" & certainty_code2 == "B" ~ "AB",
    certainty_code1 == "B" & certainty_code2 == "A" ~ "AB",
    certainty_code1 == "A" & certainty_code2 == "C" ~ "AC",
    certainty_code1 == "C" & certainty_code2 == "A" ~ "AC",
    certainty_code1 == "B" & certainty_code2 == "B" ~ "BB",
    certainty_code1 == "B" & certainty_code2 == "C" ~ "BC",
    certainty_code1 == "C" & certainty_code2 == "B" ~ "BC",
    certainty_code1 == "C" & certainty_code2 == "C" ~ "CC",
    TRUE ~ NA_character_)) %>%
  #Generate tooth_category combinations
  mutate(tooth_category_combo = case_when(
    tooth_category1 == "M" & tooth_category2 == "M" ~ "MM",
    tooth_category1 == "M" & tooth_category2 == "P" ~ "MP",
    tooth_category1 == "M" & tooth_category2 == "C" ~ "MC",
    tooth_category1 == "M" & tooth_category2 == "I" ~ "MI",
    tooth_category1 == "P" & tooth_category2 == "M" ~ "MP",
    tooth_category1 == "P" & tooth_category2 == "P" ~ "PP",
    tooth_category1 == "P" & tooth_category2 == "C" ~ "PC",
    tooth_category1 == "P" & tooth_category2 == "I" ~ "PI",
    tooth_category1 == "C" & tooth_category2 == "M" ~ "MC",
    tooth_category1 == "C" & tooth_category2 == "P" ~ "PC",
    tooth_category1 == "C" & tooth_category2 == "C" ~ "CC",
    tooth_category1 == "C" & tooth_category2 == "I" ~ "CI",
    tooth_category1 == "I" & tooth_category2 == "M" ~ "MI",
    tooth_category1 == "I" & tooth_category2 == "P" ~ "PI",
    tooth_category1 == "I" & tooth_category2 == "C" ~ "CI",
    tooth_category1 == "I" & tooth_category2 == "I" ~ "II",
    tooth_category1 == "UNK" | tooth_category2 == "UNK" ~ "UNK",
    TRUE ~ NA_character_))%>% 
  mutate(certainty_category = if_else(certainty_code_combo == "AA", "High", "Low")) %>% 
  #Bring predictors (year, region) into dataframe
  left_join(., unique(SEOT_teeth[,c(1,3,5)]), by="otter_id")


####################################################
# Part 3: Export clean datasets ####################
####################################################

# Export clean datasets as .csvs in "processed data" folder
write_csv(SEOT_teeth, "data/processed/SEOT_teeth.csv")
write_csv(tooth_age_comparison, "data/processed/tooth_age_comparison.csv")
