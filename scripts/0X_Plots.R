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


###############################################################################
# Part 2: Precision by Different Variables ####################################
###############################################################################

# Tooth Aging Certainty Code Combination --------------------------------------

tooth_comparison_certainty_summary <- tooth_age_comparison %>% 
  group_by(certainty_code_combo) %>% 
  summarise(n = n(),
            n_age_agreement = sum(age_agreement),
            n_age_class_agreement = sum(age_class_agreement)) %>% 
  adorn_totals("row") %>% 
  ungroup() %>% 
  pivot_longer(cols = c(n_age_agreement, n_age_class_agreement))%>% 
  mutate(freq= value/n)
 
fct_labels <- c("n_age_agreement" = "Frequency Age Agreement", "n_age_class_agreement" = "Frequency Age Class Agreement")


ggplot(tooth_comparison_certainty_summary, aes(certainty_code_combo, freq, fill = certainty_code_combo, label = paste(value, "/", n)))+
  geom_bar(stat='identity')+
  facet_grid(rows = "name", scales = "free", switch="y", labeller = as_labeller(fct_labels))+
  theme_few()+
  scale_fill_viridis_d()+
  geom_text(vjust = -0.5)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  labs(y = "", x = "", fill = "Certainty Code \nCombination")+
  theme(strip.placement.y = "outside",
        strip.text.y = element_text(face = "bold"))

  
# Tooth Type Combination ------------------------------------------------------

tooth_combo_certainty <- tooth_age_comparison %>% 
  group_by(tooth_category_combo) %>% 
  summarise(n = n(),
            n_age_agreement = sum(age_agreement),
            n_age_class_agreement = sum(age_class_agreement)) %>% 
  mutate(tooth_category_combo = factor(tooth_category_combo, 
                                             levels = c("MP","MC","MI","PP","PC","PI","CI","UNK"))) %>%  
  filter(tooth_category_combo != "UNK") %>% 
  adorn_totals("row") %>% 
  ungroup() %>% 
  pivot_longer(cols = c(n_age_agreement, n_age_class_agreement))%>% 
  mutate(freq= value/n)



ggplot(tooth_combo_certainty, aes(tooth_category_combo, freq, fill = tooth_category_combo, label = paste(value, "/", n)))+
  geom_bar(stat='identity')+
  facet_grid(rows = "name", scales = "free", switch="y", labeller = as_labeller(fct_labels))+
  theme_few()+
  scale_fill_viridis_d()+
  geom_text(vjust = -0.5)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  labs(y = "", x = "", fill = "Tooth Type \nCombination")+
  theme(strip.placement.y = "outside",
        strip.text.y = element_text(face = "bold"))


# Area ------------------------------------------------------


area_certainty <- tooth_age_comparison %>% 
  group_by(area) %>% 
  summarise(n = n(),
            n_age_agreement = sum(age_agreement),
            n_age_class_agreement = sum(age_class_agreement)) %>% 
  adorn_totals("row") %>% 
  mutate(area = factor(area, levels = c("KATM", "WPWS", "Total"))) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(n_age_agreement, n_age_class_agreement))%>% 
  mutate(freq= value/n)



ggplot(area_certainty, aes(area, freq, fill = area, label = paste(value, "/", n)))+
  geom_bar(stat='identity')+
  facet_grid(rows = "name", scales = "free", switch="y", labeller = as_labeller(fct_labels))+
  theme_few()+
  scale_fill_viridis_d()+
  geom_text(vjust = -0.5)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  labs(y = "", x = "", fill = "Area")+
  theme(strip.placement.y = "outside",
        strip.text.y = element_text(face = "bold"))



# Year ------------------------------------------------------

year_certainty <- tooth_age_comparison %>% 
  group_by(year) %>% 
  summarise(n = n(),
            n_age_agreement = sum(age_agreement),
            n_age_class_agreement = sum(age_class_agreement)) %>% 
  adorn_totals("row") %>% 
  ungroup() %>% 
  pivot_longer(cols = c(n_age_agreement, n_age_class_agreement))%>% 
  mutate(freq= value/n)


ggplot(year_certainty, aes(year, freq, fill = year, label = paste(value, "/", n)))+
  geom_bar(stat='identity')+
  facet_grid(rows = "name", scales = "free", switch="y", labeller = as_labeller(fct_labels))+
  theme_few()+
  scale_fill_viridis_d()+
  geom_text(vjust = -0.5)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  labs(y = "", x = "", fill = "Year")+
  theme(strip.placement.y = "outside",
        strip.text.y = element_text(face = "bold"))


###############################################################################
# Part 3: Age Difference ######################################################
###############################################################################

age_diff_breakdown <- tooth_age_comparison %>%
  group_by(age_diff) %>% 
  mutate()
  

ggplot(tooth_age_comparison, aes(x=age_diff, fill = certainty_code_combo))+
  geom_histogram()+
  scale_fill_viridis_d()
  
ggplot(tooth_age_comparison, aes(x=age_diff, fill = factor(age1)))+
  geom_histogram()+
  scale_fill_viridis_d()


frequency_disagreement <- tooth_age_comparison %>% 
  mutate(mean_age = (age1+age2)/2) %>% 
  group_by(mean_age,certainty_code_combo) %>% 
  summarise(freq_disagreement = 1-(sum(age_class_agreement)/n()))


ggplot(frequency_disagreement, aes(x=mean_age, y=freq_disagreement, fill=certainty_code_combo))+
  geom_bar(stat="identity")+
  theme_few()+
  scale_fill_viridis_d()
  
