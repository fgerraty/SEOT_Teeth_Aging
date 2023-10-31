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
  scale_fill_manual(values = pal, breaks = c("A", "B", "C"), labels = c("A (high)", "B (medium)", "C (low)"))+
  theme_classic()+
  labs(y="Frequency of Uncertainty Level", x="Tooth Type", fill = "Certainty Level")+
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
  scale_fill_manual(values = pal, breaks = c("A", "B", "C"), labels = c("A (high)", "B (medium)", "C (low)"))+
  theme_classic()+
  geom_text(aes(label = paste(n), group = certainty_code), position = position_stack(vjust = 0.5), color = "white")+
  labs(y="Frequency of Uncertainty Level", x="Region", fill = "Certainty Level")
  
  

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
  scale_fill_manual(values = pal, breaks = c("A", "B", "C"), labels = c("A (high)", "B (medium)", "C (low)"))+
  theme_classic()+
  geom_text(aes(label = paste(n), group = certainty_code), position = position_stack(vjust = 0.5), color = "white")+
  labs(y="Frequency of Uncertainty Level", x="Year", fill = "Certainty Level")


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

age_diff <- tooth_age_comparison %>% 
  group_by(certainty_code_combo) %>% 
  summarise(mean_diff = mean(age_diff), 
         se_diff = sd(age_diff)/sqrt(length(age_diff)))

ggplot(age_diff, aes(x=certainty_code_combo, y=mean_diff, fill = certainty_code_combo))+
  geom_bar(stat = "identity")+
  theme_few()+
  scale_fill_viridis_d()+
  geom_errorbar(aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff, width = .2))+
  labs(y = "Age Difference", x = "",fill = "Certainty Code \nCombination")


age_diff_by_age <- tooth_age_comparison %>% 
  group_by(age1) %>% 
  summarise(mean_diff = mean(age_diff), 
            se_diff = sd(age_diff)/sqrt(length(age_diff)))

ggplot(age_diff_by_age, aes(x=age1, y=mean_diff))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff, width = .2))


probability_age_class_disagreement <- tooth_age_comparison %>% 
  group_by(age1) %>% 
  summarise(freq_disagreement = (n()-sum(age_class_agreement))/n())

ggplot(probability_age_class_disagreement, aes(x=age1, y=freq_disagreement, fill = certainty_code_combo))+
  geom_bar(stat = "identity")+
  theme_few()+
  scale_fill_viridis_d()+
  labs(y="Frequency of Age Class Disagreement", x="Age (randomly selected otter)")



#Could tooth type be underpinning differences? 

tooth_type_diff <- tooth_age_comparison %>% 
  select(tooth_category1, tooth_category2, age1, age2) %>% 
  filter(tooth_category1 != "UNK" & tooth_category2 != "UNK") %>% 
  unite(tooth_cat_combo, c("tooth_category1","tooth_category2")) %>% 
  mutate(tooth_cat_combo2 = if_else(tooth_cat_combo == "M_M"|
                                    tooth_cat_combo == "M_P"|
                                    tooth_cat_combo == "M_C"|
                                    tooth_cat_combo == "M_I"|
                                    tooth_cat_combo == "P_P"|
                                    tooth_cat_combo == "P_C"|
                                    tooth_cat_combo == "P_I"|
                                    tooth_cat_combo == "C_C"|
                                    tooth_cat_combo == "C_I"|
                                    tooth_cat_combo == "I_I", 
                                    tooth_cat_combo, 
                                    if_else(tooth_cat_combo == "P_M", "M_P",
                                    if_else(tooth_cat_combo == "C_M", "M_C",
                                    if_else(tooth_cat_combo == "C_P", "P_C",
                                    if_else(tooth_cat_combo == "I_P", "P_I",
                                    if_else(tooth_cat_combo == "I_M", "M_I", 
                                    if_else(tooth_cat_combo == "I_C", "C_I", NA)))))))) %>% 
  mutate(age3 = if_else(tooth_cat_combo==tooth_cat_combo2, age1, age2),
         age4 = if_else(tooth_cat_combo==tooth_cat_combo2, age2, age1)) %>% 
  select(tooth_cat_combo = tooth_cat_combo2,
         age1 = age3, 
         age2 = age4) %>% 
  filter(tooth_cat_combo != "P_P") %>% 
  mutate(age_diff = age1-age2) %>% 
  group_by(tooth_cat_combo) %>% 
  summarise(mean_age_diff = mean(age_diff), 
            sd_age_diff = sd(age_diff),
         se_age_diff = sd(age_diff)/sqrt(n())) %>% 
  mutate(tooth_cat_combo = 
           factor(tooth_cat_combo, 
                  levels = c("M_P", "M_C", "M_I", "P_C", "P_I", "C_I")))
  

ggplot(tooth_type_diff, aes(x=tooth_cat_combo, y=mean_age_diff))+
  geom_bar(stat="identity")+
  theme_few()+
  geom_errorbar(aes(ymin = mean_age_diff-se_age_diff, ymax = mean_age_diff+se_age_diff), width = .2)+
  labs(y= "Age Difference (mean ± SE)", x="Tooth Type Comparison (i.e. M_P = Molar - Premolar)")
