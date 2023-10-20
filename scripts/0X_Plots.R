###############################################################################
# Sea Otter Teeth Aging Project ###############################################
# Code Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
###############################################################################
# Script 0X: Plots #############################################################
#------------------------------------------------------------------------------

###############################################
# Part 1: Load datasets #######################
###############################################

SEOT_teeth <- read_csv("data/processed/SEOT_teeth.csv")
tooth_age_comparison <- read_csv("data/processed/tooth_age_comparison.csv")


###############################################################################
# Part 2: Uncertainty by Different Variables ##################################
###############################################################################

# Uncertainty by tooth type plot and table ------------------------------------
tooth_type <- SEOT_teeth %>% 
  group_by(tooth_category, certainty_code) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n), #calculate frequency of each uncertainty level
         tooth_category = factor(tooth_category, levels = c("M", "P", "C", "I"))) %>% 
  ungroup() %>% 
  filter(tooth_category != "UNK") #remove unknown teeth


ggplot(tooth_type, aes(x=tooth_category, y=freq, fill=tooth_category))+
  geom_bar(stat='identity')+
  facet_grid(cols = vars(certainty_code), scales = "free_y")+
  theme_classic()+
  scale_fill_viridis_d(labels = c("Molar", "Premolar", "Canine", "Incisor"))+
  labs(y="Frequency of Uncertainty Level", x="", fill = "Tooth Type")


#Uncertainty by area
area <- SEOT_teeth %>% 
  group_by(area, certainty_code) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n)) %>%  #calculate frequency of each uncertainty level) %>% 
  ungroup() 


ggplot(area, aes(x=area, y=freq, fill=area))+
  geom_bar(stat='identity')+
  facet_grid(cols = vars(certainty_code), scales = "free_y")+
  theme_classic()+
  scale_fill_viridis_d()+
  labs(y="Frequency of Uncertainty Level", x="", fill = "Area")


#Uncertainty by year
year <- SEOT_teeth %>% 
  group_by(year, certainty_code) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n),
         year = as.character(year)) %>%  #calculate frequency of each uncertainty level) %>% 
  ungroup() 


ggplot(year, aes(x=year, y=freq, fill=year))+
  geom_bar(stat='identity')+
  facet_grid(cols = vars(certainty_code), scales = "free_y")+
  theme_classic()+
  scale_fill_viridis_d()+
  labs(y="Frequency of Uncertainty Level", x="", fill = "Year")


# Explore outputs of the 2-tooth dataset ---------------------------------------

#What proportion of the 2 tooth combinations are the same age?
sum(tooth_age_comparison$age_agreement)/length(tooth_age_comparison$age_agreement)

#What proportion of the 2 tooth combinations are the same age class?
sum(tooth_age_comparison$age_class_agreement)/length(tooth_age_comparison$age_class_agreement)


tooth_comparison_certainty_summary <- tooth_age_comparison %>% 
  group_by(certainty_code_combo) %>% 
  summarise(n = n(),
            n_age_agreement = sum(age_agreement),
            n_age_class_agreement = sum(age_class_agreement),
            freq_age_agreement = sum(age_agreement)/n(),
            freq_age_class_agreement = sum(age_class_agreement)/n())

ggplot(tooth_comparison_certainty_summary, aes(certainty_code_combo, freq_age_agreement, fill = certainty_code_combo, label = paste(n_age_agreement,n,sep = "/")))+
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_viridis_d()+
  geom_text(vjust = -0.5)
  
ggplot(tooth_comparison_certainty_summary, aes(certainty_code_combo, freq_age_class_agreement, fill = certainty_code_combo, label = paste(n_age_class_agreement,n,sep = "/")))+
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_viridis_d()+
  geom_text(vjust = -0.5)
