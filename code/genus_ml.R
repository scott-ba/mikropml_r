source("code/genus_process.R")
library(mikropml)

# switch logical srn TRUE/FALSE to char srn/healthy for the ml package.
# drop the group column which is not needed for the ml package.
# move srn to the left most column.
srn_genus_data <- composite %>% 
  select(group, taxonomy, rel_abund, srn) %>% 
  pivot_wider(names_from=taxonomy, values_from=rel_abund) %>% 
  select(-group) %>% 
  mutate(srn = if_else(srn, "srn", "healthy")) %>%
  select(srn, everything())

# Required inputs are a data frame
# (must contain an outcome variable and all other columns as features)
# and the ML method. Feature importance was skipped for now
# kfold is the number of folds. cv_times is the number of partitions.
# training_frac is the fraction that is the training set.

srn_genus_results <- run_ml(srn_genus_data,
       method="glmnet",
       outcome_column = "srn",
       kfold = 2, #5,
       cv_times = 10, #100,
       training_frac = 0.5, #0.8,
       seed = 19760620)
           



