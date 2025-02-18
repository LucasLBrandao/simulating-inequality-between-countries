library(data.table)
library(tidyverse)

# Read in the data

countries  <- fread("entradas/wid_all_data/WID_countries.csv")
countries %>% glimpse()

