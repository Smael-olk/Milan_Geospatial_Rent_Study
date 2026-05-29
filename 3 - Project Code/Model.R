library(sf)
library(tidyverse)
library(here)
library(mapview)
library(spdep)
library(ggplot2)
library(treemapify)
library(dplyr)
library(ggplot2)
library(gstat)
library(stars)
library(viridis)
library(writexl)
library(ggrepel) # Required for clear text labels
library(gstat)
# 1. Load data
# raw_path <- here("1 - Data Extraction", "final_dataset.csv")
clean_df <- read_csv("./clean_data_onehot_encoded.csv")




