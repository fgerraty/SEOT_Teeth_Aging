###############################################################################
# Sea Otter Teeth Aging Project ###############################################
# Code Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
###############################################################################
# Script 00: Load packages ####################################################
#------------------------------------------------------------------------------

# Part 1: Load Packages --------------------------------------------------

# Load packages
packages<- c("tidyverse", "readxl", "janitor", "lme4", "AICcmodavg", "viridis", "ggthemes")

pacman::p_load(packages, character.only = TRUE)

