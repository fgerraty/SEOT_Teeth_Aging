###############################################################################
# Sea Otter Teeth Aging Project ###############################################
# Code Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
###############################################################################
# Script 0X: Plots #############################################################
#------------------------------------------------------------------------------




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
