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

# Cleanup before sending to preprocess.
# check for duplicates after reducing columns, and remove them.
# set missing values recorded as zero to na.

# Preprocess to impute na values, scale, and incode catagoric to integer.
# See carot for how the scaling and centering it done.
# note we must extract the transformed dat from the list before running run_ml
srn_genus_preprocess <- preprocess_data(srn_genus_data, outcome_colname = "srn")$dat_transformed


# Required inputs are a data frame
# (must contain an outcome variable and all other columns as features)
# and the ML method. Feature importance was skipped for now
# kfold is the number of folds. cv_times is the number of partitions.
# training_frac is the fraction that is the training set.

test_hp <- list(alpha = 0,
                 lambda = c(0.1, 1, 2, 3, 4, 5, 10))

get_srn_genus_results <- function(seed){
  
  run_ml(srn_genus_preprocess,
       method="glmnet",
       outcome_column = "srn",
       kfold = 5, #5,
       cv_times = 100, #100,
       training_frac = 0.8, #0.8,
       hyperparameters = test_hp,
       seed = seed)
}

# Using three seeds
iterative_run_ml_results <- map(c(1,2,3), get_srn_genus_results)

# Extract results and combine the three runs
performance <- iterative_run_ml_results %>%
  map(pluck,"trained_model") %>%
  combine_hp_performance()

# plot
plot_hp_performance(performance$dat, lambda, AUC)

performance$dat %>% 
  group_by(alpha,lambda) %>% 
  summarize(mean_AUC = mean(AUC), .groups="drop") %>% 
#  top_n(n=1, mean_AUC)
ggplot(aes(x=lambda, y=mean_AUC, color=as.character(alpha))) + geom_line()

# get_hyperparams_list(srn_genus_preprocess, "glmnet")






