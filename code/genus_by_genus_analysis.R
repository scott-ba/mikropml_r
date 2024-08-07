
source("code/genus_process.R")

sig_genera <- composite %>%
  nest(data = -taxonomy) %>%
  mutate(test = map(.x=data, ~wilcox.test(rel_abund~srn, data=.x) %>% tidy)) %>%
  unnest(test) %>%
  mutate(p.adjust = p.adjust(p.value, method="BH")) %>%
  filter(p.adjust < 0.05) %>%
  select(taxonomy, p.adjust)

# show 50% confidence intervals on jitter plot, using shape 21 with stat summary
# Using median with scale_x_log10, and mean with coord_trans(x="log10")
# and switch to percentage to avoid fractions, adding the lower bound
# for the zeros.
# note scale_x_log10 applies the log before the stat_summary.

composite %>%
  inner_join(sig_genera, by="taxonomy") %>%
  mutate(rel_abund = 100 * (rel_abund + 1/20000),
         taxonomy = str_replace(taxonomy, "(.*)", "*\\1*"),
         taxonomy = str_replace(taxonomy, "\\*(.*)_unclassified\\*",
                                "Unclassified<br>*\\1*"),
         srn = factor(srn, levels = c(T, F))) %>%
  ggplot(aes(x=rel_abund, y=taxonomy, color=srn, fill=srn)) +
  geom_vline(xintercept = 100/10530, size=0.5, color="gray") +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8,
                                              jitter.width = 0.5),
              shape=21) +
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int=0.5),
               geom="pointrange",
               position = position_dodge(width=0.8),
               color="black", show.legend = FALSE) +
  scale_x_log10() +
  scale_color_manual(NULL,
                     breaks = c(F, T),
                     values = c("gray", "dodgerblue"),
                     labels = c("Healthy", "SRN")) +
  scale_fill_manual(NULL,
                    breaks = c(F, T),
                    values = c("gray", "dodgerblue"),
                    labels = c("Healthy", "SRN")) +
  labs(x= "Relative abundance (%)", y=NULL) +
  theme_classic() +
  theme(
    axis.text.y = element_markdown()
  )



ggsave("figures/significant_genera.tiff", width=6, height=4)

# function to use for mapping sens/spec calc over the scores.
# If score>threshold then TRUE compared to actual TRUE.
get_sens_spec <- function(threshold, score, actual, direction){
  
  # threshold <- 100
  # score <- test$score
  # actual <- test$srn
  # direction <- "greaterthan"
  
  predicted <- if(direction == "greaterthan") {
    score > threshold
  } else {
    score < threshold
  }
  
  tp <- sum(predicted & actual)
  tn <- sum(!predicted & !actual)
  fp <- sum(predicted & !actual)
  fn <- sum(!predicted & actual)
  
  specificity <- tn / (tn + fp)
  sensitivity <- tp / (tp + fn)
  
  tibble("specificity" = specificity, "sensitivity" = sensitivity)
}

# generate the ROC data by mapping function onto the sorted scores.
# row bind to extend the plot to the corner.
get_roc_data <- function(x, direction){
  
  thresholds <- unique(x$score) %>% sort()
  
  map_dfr(.x=thresholds, ~get_sens_spec(.x, x$score, x$srn, direction)) %>%
    rbind(c(specificity = 0, sensitivity = 1))
  
}
  

  # Map the ROC calc onto a nested data column containing group, srn, and score.
  
roc_data <- composite %>%
  inner_join(sig_genera, by="taxonomy") %>%
  select(group, taxonomy, rel_abund, fit_result, srn) %>%
  pivot_wider(names_from=taxonomy, values_from=rel_abund) %>%
  pivot_longer(cols=-c(group, srn), names_to="metric", values_to="score") %>%
  # filter(metric == "fit_result") %>%
  nest(data = -metric) %>%
  mutate(direction = if_else(metric == "Lachnospiraceae_unclassified",
                             "lessthan","greaterthan")) %>%
  mutate(roc_data = map2(.x = data, .y=direction, ~get_roc_data(.x, .y))) %>%
  unnest(roc_data) %>%
  select(metric, specificity, sensitivity)


# plot ROC by taxonomy name (metric)
roc_data %>%
  ggplot(aes(x=1-specificity, y=sensitivity, color=metric)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, color="gray") +
  theme_classic()

ggsave("figures/roc_figure.tiff", width=6, height=4)

