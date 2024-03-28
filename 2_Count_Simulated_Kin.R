#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Registers - Data Format
# U:/SOCSIM/SOCSIM_Registers/1_Data_Format.R

## Get reference table and cohort size from socsim output 
# that matches the structure of kin counts from the Swedish Kinship Universe

# Created on 21-11-2023
# Last modified on 26-03-2024

# This code is an adapted version of the 2_socsim_kin_structures.R created by Diego Alburez-Gutierrez
#------------------------------------------------------------------------------------------------------
## General settings and functions -----
# Prevent scientific notation
options(scipen=999999)

## Load packages 
library(data.table)
library(tidyverse)
library(reshape2)

# Load the functions getRefTable and AddBirthsDeaths (from kin_count_sweden)
source("Functions/Functions_Kin_Count.R")

#------------------------------------------------------------------------------------------------------
## Load SOCSIM output, convert time and add relevant variables

# Load opop and omar from simulation with no heterogeneous fertility (hetfert_0)
load("opop_hetfert0.RData")
load("omar_hetfert0.RData")

# Load opop and omar from simulation with heterogeneous fertility, a0b1
# with alpha 0 (no mother inheritance), beta 1 (exactly initial random fmult)
load("z_Simulations/opop_a0b1.RData")
load("z_Simulations/omar_a0b1.RData")

# Load opop and omar from simulation with heterogeneous fertility, a0b05
# with alpha 0 (no mother inheritance), beta 0.7 (higher fmult than initial)
load("z_Simulations/opop_a0b07.RData")
load("z_Simulations/omar_a0b07.RData")

# Load opop and omar from simulation with heterogeneous fertility, a0b2
# with alpha 0 (no mother inheritance), beta 2 (lower fmult than initial)
load("z_Simulations/opop_a0b2.RData")
load("z_Simulations/omar_a0b2.RData")

#------------------------------------------------------------------------------------------------------
## Function to create and save reference table, temp and N_Cohort from different opop ----


reference_table_opop <- function(opop, sim_param, final_sim_year, year_min, year_max) {
  
## 1. Format the data
  
# Function to convert SOCSIM months to calendar years
  asYr <- function(month, last_month, final_sim_year) {
    return(final_sim_year - trunc((last_month - month)/12))
  }

# Define parameters to convert SOCSIM months to calendar years
last_month <- max(opop$dob)

# Add year of birth and year of death to the opop file
opop <- opop %>% 
  mutate(birth_year = asYr(dob, last_month, final_sim_year),
         death_year = ifelse(dod == 0, 9999, asYr(dod, last_month, final_sim_year)))

opop2 <- opop %>%
  mutate(Kon = fem + 1, 
         FodelselandGrp = 1) %>% 
  # Reformat to Swedish register format
  select(LopNr = pid, 
         FoddAr = birth_year, 
         Kon,
         FodelselandGrp,
         LopNrFar = pop,
         LopNrMor = mom,
         deathyear = death_year) %>%
  # Add NA for the parents of the founder generation
  mutate(LopNrFar = ifelse(LopNrFar == 0, NA, LopNrFar), 
         LopNrMor = ifelse(LopNrMor == 0, NA, LopNrMor))


# Filter population (-100 years to avoid missing parents or grandparents)
popdatdeaths <- opop2 %>%  
  filter(between(FoddAr, year_min -100, year_max))  


## 2. Create kinship objects 

popdat <- popdatdeaths %>% select(-deathyear)

# get Ref Table (from Martin's functions)
refTableList <- getRefTable(df = popdat, ref_TypeI = "all")

# Add years of birth and death (from Martin's functions)
# According to Diego, this is not really needed, but still did it
SimRefTableList <- lapply(refTableList, AddBirthsDeaths, df = opop2)

reference_table_SweBorn <- 
  bind_rows(SimRefTableList, .id = "refGroup") %>% 
  mutate(refGroup = gsub("_df", "", refGroup)) %>%
  inner_join(select(filter(popdat, FodelselandGrp == 1), LopNr, Kon, FodelselandGrp, FoddAr), 
             by = c("ID" = "LopNr")) %>% 
  filter(between(IDbirthYear, year_min, year_max)) %>% 
  setDT() ## Add to allow the code below run
 

## 3. Get other quantities

# Get table with number of Swedish born parents per person
temp <- reference_table_SweBorn %>% 
  filter(refGroup == "parent" & !is.na(refID)) %>%
  group_by(ID, IDbirthYear, FoddAr) %>%
  summarise(n_swe_parents = n(), .groups = "drop") %>%
  setDT()

TotalPopulation <- popdatdeaths %>% setDT()

# Get tables with N per cohort
N_Cohort <- list(TotalPopulation[, .(N_ever = .N), keyby = FoddAr], 
                 reference_table_SweBorn[, .(N_17 = uniqueN(ID)), keyby = FoddAr], 
                 temp[n_swe_parents == 2, .N, keyby = FoddAr]) %>%
  reduce(left_join, by = "FoddAr") %>%
  rename(N_sweparent = N) %>% 
  mutate(IDbirthYear = FoddAr)

# Table with N_cohort by sex
temp <- reference_table_SweBorn[!is.na(FoddAr), .(N_17 = uniqueN(ID)), keyby = .(FoddAr, Kon)]

temp <- temp %>% 
  rename(IDbirthYear = FoddAr)

# 4. Export 

# Create a sub-folder called "Output_" and the simulation parameters to save the files
output_folder <- paste0("Output_", sim_param, "/")

if (!dir.exists(output_folder)) { dir.create(output_folder)}
  
  save(reference_table_SweBorn, file = paste0(output_folder, "reference_table_SweBorn.RData"))
  save(temp, file = paste0(output_folder, "temp.RData"))
  save(N_Cohort, file = paste0(output_folder, "N_Cohort.RData"))
  
}

#------------------------------------------------------------------------------------------------------
## Get the reference table and additional tables for each opop file ----

reference_table_opop(opop = opop_hetfert0, 
                     sim_param = "hetfert0", 
                     final_sim_year = 2022, 
                     year_min = 1900, 
                     year_max = 2022)

reference_table_opop(opop = opop_a0b1, 
                     sim_param = "a0b1", 
                     final_sim_year = 2022, 
                     year_min = 1900, 
                     year_max = 2022)

reference_table_opop(opop = opop_a0b07, 
                     sim_param = "a0b07", 
                     final_sim_year = 2022, 
                     year_min = 1900, 
                     year_max = 2022)

reference_table_opop(opop = opop_a0b2, 
                     sim_param = "a0b2", 
                     final_sim_year = 2022, 
                     year_min = 1900, 
                     year_max = 2022)