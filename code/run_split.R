#!/usr/bin/env Rscript

#dry version

source("code/genus_process.R")
library(mikropml)
library(tidyverse)

# add command line input for output_file and feature_script
args <- commandArgs(trailingOnly = TRUE)
output_file <- args[1]
seed <- as.numeric(str_replace(output_file, ".*_(\\d*)\\.Rds", "\\1"))
feature_script <- args[2]

source(feature_script)

# select columns moved to external feature script.
srn_data <- composite %>%
  feature_select() %>% 
  pivot_wider(names_from=taxonomy, values_from = rel_abund) %>%
  select(-group) %>%
  mutate(srn = if_else(srn, "srn", "healthy")) %>%
  select(srn, everything())

srn_preprocess <- preprocess_data(srn_data,
                                        outcome_colname = "srn")$dat_transformed

# note hyperparameter and approach come from source.
model <- run_ml(srn_preprocess,
       method=approach,
       outcome_colname = "srn",
       kfold = 5,
       cv_times = 20,
       training_frac = 0.8,
       hyperparameters = hyperparameter,
       seed = seed)

saveRDS(model, file = output_file)
