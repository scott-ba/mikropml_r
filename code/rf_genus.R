feature_select <- function(x) {

  x %>% 
    select(group, taxonomy, rel_abund, srn)
  
}

# mtry if the random forest hyper

approach <- "rf"
hyperparameter <- list(mtry=c(8, 17, 34, 100))