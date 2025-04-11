#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Registers - Count Simulated kin
# U:/SOCSIM/SOCSIM_Registers/2_Count_Simulated_Kin.R

## Get reference table and cohort size from SOCSIM output 
# that matches the structure of kin counts from the Swedish Kinship Universe

# Created on 21-11-2023
# Last modified on 11-04-2025
#------------------------------------------------------------------------------------------------------
## General settings and functions -----
# Prevent scientific notation
options(scipen=999999)

## Load packages 
library(data.table)
library(tidyverse)
library(reshape2)

# Load the functions getRefTable and AddBirthsDeaths
# These are a slightly modified version of the R-code written 
# for "The Swedish Kinship Universe" (Kolk et al., 2023) by Emma Pettersson
source("Functions/Functions_Kin_Count.R")

#------------------------------------------------------------------------------------------------------
## Create and save reference table, N_Cohort and N_Cohort_sex from SOCSIM output ----

## 0. Load opop from simulation with no heterogeneous fertility but parity-specific rates (0_par)
load("opop.RData")

## 1. Format the data
  
# Function to convert SOCSIM months to calendar years
asYr <- function(month, last_month, final_sim_year) {
  return(final_sim_year - trunc((last_month - month)/12))}

# Define parameters to convert SOCSIM months to calendar years
last_month <- max(opop$dob)
final_sim_year <- 2017

# Add variables and reformat to Swedish register format
opop2 <- opop %>% 
  mutate(birthyear = asYr(dob, last_month, final_sim_year),
         deathyear = ifelse(dod == 0, NA, asYr(dod, last_month, final_sim_year)), 
         Kon = fem + 1, # Sex
         FodelselandGrp = 1) %>% # Country of birth
  select(LopNr = pid, 
         FoddAr = birthyear, 
         Kon,
         FodelselandGrp,
         LopNrFar = pop,
         LopNrMor = mom, 
         deathyear) 

# Filter population
year_min = 1900

popdat <- opop2 %>%
  filter(between(FoddAr, year_min -100, final_sim_year)) # (-100 years to avoid missing parents or grandparents)

## 2. Create kinship objects 

# Get Reference Table using the getRefTable() function
refTableList <- getRefTable(df = popdat, ref_TypeI = "all")

# Add years of birth and death 
SimRefTableList <- lapply(refTableList, AddBirthsDeaths, df = opop2)

reference_table_SweBorn <- 
  bind_rows(SimRefTableList, .id = "refGroup") %>% 
  mutate(refGroup = gsub("_df", "", refGroup)) %>%
  inner_join(select(filter(popdat, FodelselandGrp == 1), LopNr, Kon, FodelselandGrp, FoddAr), 
             by = c("ID" = "LopNr")) %>% 
  filter(between(IDbirthYear, year_min, final_sim_year)) %>% 
  setDT()
 

## 3. Get other quantities

# Total Population 
TotalPopulation <- popdat %>% setDT()

# Tables with N per cohort
N_Cohort <- list(TotalPopulation[, .(N_ever = .N), keyby = FoddAr], 
                 reference_table_SweBorn[, .(N_17 = uniqueN(ID)), keyby = FoddAr]) %>%
  reduce(left_join, by = "FoddAr") %>%
  mutate(IDbirthYear = FoddAr)

# Table with N_cohort by sex
N_Cohort_sex <- reference_table_SweBorn[!is.na(FoddAr), .(N_17 = uniqueN(ID)), keyby = .(FoddAr, Kon)] %>% 
  rename(IDbirthYear = FoddAr)


## 4. Export 

# Indicate simulation parameters
sim_param = ""

# Create a sub-folder to save the output files 
output_folder <- paste0("Output_", sim_param, "/")

if (!dir.exists(output_folder)) { dir.create(output_folder)}
  
save(reference_table_SweBorn, file = paste0(output_folder, "reference_table_SweBorn.RData"))
save(N_Cohort, file = paste0(output_folder, "N_Cohort.RData"))
save(N_Cohort_sex, file = paste0(output_folder, "N_Cohort_sex.RData"))