# Load packages ----------------------------------------------------------------
library(dplyr)
library(janitor)
# For column to row function
library(tibble)
#
library(tidyr)

# Load raw data ----------------------------------------------------------------
raw_agb_data <- 
    readxl::read_xlsx("./raw_data/abundancia_aboveground_biomass_data/data_AB_sp_parcela.xlsx",
                      sheet = 2)

# Clean data -------------------------------------------------------------------
 
#data_agb_long <- 
    raw_agb_data %>% 
        clean_names() %>% 
        #column_to_rownames("parcela") %>% 
        pivot_longer(!parcela, names_to = "spcode", values_to = "agb")

# Total should be 190
# In this list Brosimum panamense is treat as a different species and is 
# not. Removed
filter(!name_submitted == "Brosimum panamense",
       !name_submitted == "Hirtella media")