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

pal = c("A" = "#0072b2", "B" = "#009e73", "C" = "#d55e00")

# Uncertainty by tooth type plot and table ------------------------------------
tooth_type <- SEOT_teeth %>% 
  group_by(tooth_category, certainty_code) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n), #calculate frequency of each uncertainty level
         tooth_category = factor(tooth_category, levels = c("M", "P", "C", "I")),
         certainty_code = factor(certainty_code, levels = c("C", "B", "A")),
         n_category = sum(n)) %>% 
  ungroup() %>% 
  filter(tooth_category != "UNK") #remove unknown teeth

ggplot(tooth_type, aes(x=tooth_category, y=freq))+
  geom_bar(aes(fill=certainty_code), position = "fill", stat='identity')+
  scale_fill_manual(values = pal, breaks = c("A", "B", "C"), labels = c("A (low)", "B (medium)", "C (high)"))+
  theme_classic()+
  labs(y="Frequency of Uncertainty Level", x="Tooth Type", fill = "Uncertainty Level")+
  geom_text(aes(label = paste(n), group = certainty_code), position = position_stack(vjust = 0.5), color = "white")+
  scale_x_discrete(labels = c("Molar", "Premolar", "Canine", "Incisor"))

#Uncertainty by area ----------------------------------------------------------
area <- SEOT_teeth %>% 
  group_by(area, certainty_code) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n),
         n_area = sum(n),
         certainty_code = factor(certainty_code, levels = c("C", "B", "A"))) %>%  
  ungroup() 


ggplot(area, aes(x=area, y=freq))+
  geom_bar(aes(fill=certainty_code), position = "fill", stat='identity')+
  scale_fill_manual(values = pal, breaks = c("A", "B", "C"), labels = c("A (low)", "B (medium)", "C (high)"))+
  theme_classic()+
  geom_text(aes(label = paste(n), group = certainty_code), position = position_stack(vjust = 0.5), color = "white")+
  labs(y="Frequency of Uncertainty Level", x="Region", fill = "Uncertainty Level")
  
  

#Uncertainty by year ----------------------------------------------------------
year <- SEOT_teeth %>% 
  group_by(year, certainty_code) %>% 
  summarise(n=n()) %>% 
  mutate(freq = n/sum(n),
         year = as.character(year),
         certainty_code = factor(certainty_code, levels = c("C", "B", "A"))) %>%
  ungroup() 


ggplot(year, aes(x=year, y=freq))+
  geom_bar(aes(fill=certainty_code), position = "fill", stat='identity')+
  scale_fill_manual(values = pal, breaks = c("A", "B", "C"), labels = c("A (low)", "B (medium)", "C (high)"))+
  theme_classic()+
  geom_text(aes(label = paste(n), group = certainty_code), position = position_stack(vjust = 0.5), color = "white")+
  labs(y="Frequency of Uncertainty Level", x="Year", fill = "Uncertainty Level")


# Explore outputs of the 2-tooth dataset ---------------------------------------

#What proportion of the 2 tooth combinations are the same age?
sum(tooth_age_comparison$age_agreement)/length(tooth_age_comparison$age_agreement)

#What proportion of the 2 tooth combinations are the same age class?
sum(tooth_age_comparison$age_class_agreement)/length(tooth_age_comparison$age_class_agreement)

tooth_comparison_certainty_summary <- tooth_age_comparison %>% 
  group_by(certainty_code_combo) %>% 
  summarise(n = n(),
            n_age_agreement = sum(age_agreement),
            n_age_class_agreement = sum(age_class_agreement)) %>% 
  adorn_totals("row") %>% 
  mutate(freq_age_agreement = n_age_agreement/n,
         freq_age_class_agreement = n_age_class_agreement/n) %>% 
  ungroup() %>% pivot_longer(cols = c(freq_age_agreement, freq_age_class_agreement))


#NOTE: N values are wrong!!!
ggplot(tooth_comparison_certainty_summary, aes(certainty_code_combo, value, fill = certainty_code_combo
                                #               ,label = value
                                               ))+
  geom_bar(stat='identity')+
  facet_grid(rows = "name", scales = "free")+
  theme_few()+
  scale_fill_viridis_d()#+
#  geom_text(vjust = -0.5)
