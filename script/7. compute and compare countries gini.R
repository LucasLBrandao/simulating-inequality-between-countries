library(tidyverse)
library(data.table)
library(survey)

brazil_income_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1.rds")

# Calculate the Gini index
library(convey)

brazil_income_data <- convey_prep(brazil_income_data)
gini_index <- svygini(~VD5008_DEF, design = brazil_income_data, na.rm = TRUE)

# compute the brasil Gini index calculated with pnadc data
brasil_gini_index  <- gini_index

# compute the uruguay Gini index obtained from the World Bank
uruguai_gini_index <- 40.6