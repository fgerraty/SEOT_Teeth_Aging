###############################################################################
# Sea Otter Teeth Aging Project ###############################################
# Code Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
###############################################################################
# Script 02: Modeling #########################################################
#------------------------------------------------------------------------------

###############################################
# Part 1: Load datasets #######################
###############################################

SEOT_teeth <- read_csv("data/processed/SEOT_teeth.csv")
tooth_age_comparison <- read_csv("data/processed/tooth_age_comparison.csv")

###############################################################################
# Part 2: Predictors of age and age_class agreement ##########################
###############################################################################

#Removing categories with low counts helps to remove convergence errors ------
table(tooth_age_comparison$certainty_code_combo)
table(tooth_age_comparison$tooth_category_combo)
table(tooth_age_comparison$year)
table(tooth_age_comparison$area)
table(tooth_age_comparison$certainty_category)

temp <- tooth_age_comparison %>% 
#  filter(certainty_code_combo == "AA"|
#           certainty_code_combo == "AB"|
#           certainty_code_combo == "BB") %>% 
  filter(tooth_category_combo != "UNK") %>% 
  filter(year == "2019" |
           year == "2021" |
           year == "2022") %>% 
  mutate(year = as.character(year))

# Test for correlations among predictors ---------------------------------------
chisq.test(temp$certainty_category, temp$tooth_category_combo, simulate.p.value = TRUE)
chisq.test(temp$certainty_category, temp$area, simulate.p.value = TRUE)
chisq.test(temp$certainty_category, temp$year, simulate.p.value = TRUE)
chisq.test(temp$tooth_category_combo, temp$area, simulate.p.value = TRUE)
chisq.test(temp$tooth_category_combo, temp$year, simulate.p.value = TRUE)
chisq.test(temp$area, temp$year, simulate.p.value = TRUE)
#As we can tell, "area" and "year" are significantly correlated, and "tooth_category_combo" and "year are significantly correlated

#Generate models with non-colinear predictors ----------------------------------
f1 <- glmer(age_agreement ~ certainty_category + (1 | otter_id), data = temp);summary(f1)
fx <- glmer(age_agreement ~ certainty_level_binary + (1 | otter_id), data = temp);summary(fx)
f2 <- glmer(age_agreement ~ tooth_category_combo + (1 | otter_id), data = temp); summary(f2)
f3 <- glmer(age_agreement ~ area + (1 | otter_id), data = temp); summary(f3)
f4 <- glmer(age_agreement ~ year + (1 | otter_id), data = temp); summary(f4)
f5 <- glmer(age_agreement ~ certainty_category + area + (1 | otter_id), data = temp); summary(f5)
f6 <- glmer(age_agreement ~ certainty_category + year + (1 | otter_id), data = temp); summary(f6)
null <- glmer(age_agreement ~ 1 + (1 | otter_id), data = temp); summary(null)

#These models threw errors! 
#f5 <- glmer(age_agreement ~ certainty_category + tooth_category_combo + (1 | otter_id), data = temp); summary(f5)
#f8 <- glmer(age_agreement ~ tooth_category_combo + area + (1 | otter_id), data = temp); summary(f8)
#f11 <- glmer(age_agreement ~ certainty_category + tooth_category_combo + area + (1 | otter_id), data = temp); summary(f11)


cand.mod.names <- c("f1", "fx", "f2", "f3", "f4", "f5", "f6", "null") 
cand.mods <- list( ) 

# This function fills the list by model names
for(i in 1:length(cand.mod.names)) {
  cand.mods[[i]] <- get(cand.mod.names[i]) }

# Function aictab does the AICc-based model comparison
print(aictab(cand.set = cand.mods, 
             modnames = cand.mod.names))














#Generate models with non-colinear predictors

g1 <- glmer(age_class_agreement ~ certainty_category + (1 | otter_id), data = temp);summary(g1)
g2 <- glmer(age_class_agreement ~ tooth_category_combo + (1 | otter_id), data = temp); summary(g2)
g3 <- glmer(age_class_agreement ~ area + (1 | otter_id), data = temp); summary(g3)
g4 <- glmer(age_class_agreement ~ year + (1 | otter_id), data = temp); summary(g4)
g5 <- glmer(age_class_agreement ~ certainty_category + area + (1 | otter_id), data = temp); summary(g5)
g6 <- glmer(age_class_agreement ~ certainty_category + year + (1 | otter_id), data = temp); summary(g6)
null <- glmer(age_class_agreement ~ 1 + (1 | otter_id), data = temp); summary(null)

#These models threw errors! 
#f5 <- glmer(age_class_agreement ~ certainty_category + tooth_category_combo + (1 | otter_id), data = temp); summary(f5)
#f8 <- glmer(age_class_agreement ~ tooth_category_combo + area + (1 | otter_id), data = temp); summary(f8)
#f11 <- glmer(age_class_agreement ~ certainty_category + tooth_category_combo + area + (1 | otter_id), data = temp); summary(f11)


cand.mod.names <- c("g1", "g2", "g3", "g4", "g5", "g6", "null") 
cand.mods <- list( ) 

# This function fills the list by model names
for(i in 1:length(cand.mod.names)) {
  cand.mods[[i]] <- get(cand.mod.names[i]) }

# Function aictab does the AICc-based model comparison
print(aictab(cand.set = cand.mods, 
             modnames = cand.mod.names))


#Diff Age

h1 <- glm.nb(age_diff ~ certainty_category + (1 | otter_id)
               , 
               data = temp);summary(h1)
h2 <- glmer.nb(age_diff ~ tooth_category_combo + (1 | otter_id), data = temp); summary(h2)
h3 <- glmer.nb(age_diff ~ area + (1 | otter_id), data = temp); summary(h3)
h4 <- glmer.nb(age_diff ~ year + (1 | otter_id), data = temp); summary(h4)
h5 <- glmer.nb(age_diff ~ certainty_category + area + (1 | otter_id), data = temp); summary(h5)
h6 <- glmer.nb(age_diff ~ certainty_category + year + (1 | otter_id), data = temp); summary(h6)
null <- glmer.nb(age_diff ~ 1 + (1 | otter_id), data = temp); summary(null)

#These models threw errors! 
#f5 <- glmer.nb(age_diff ~ certainty_category + tooth_category_combo + (1 | otter_id), data = temp); summary(f5)
#f8 <- glmer.nb(age_diff ~ tooth_category_combo + area + (1 | otter_id), data = temp); summary(f8)
#f11 <- glmer.nb(age_diff ~ certainty_category + tooth_category_combo + area + (1 | otter_id), data = temp); summary(f11)


cand.mod.names <- c("h1", "h2", "h3", "h4", "h5", "h6", "null") 
cand.mods <- list( ) 

# This function fills the list by model names
for(i in 1:length(cand.mod.names)) {
  cand.mods[[i]] <- get(cand.mod.names[i]) }

# Function aictab does the AICc-based model comparison
print(aictab(cand.set = cand.mods, 
             modnames = cand.mod.names))

