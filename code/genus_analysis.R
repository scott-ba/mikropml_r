library(tidyverse)
library(broom)
library(ggtext)

set.seed(19760620)

# adjust group column to char class and all others to dbl.

shared <- read_tsv("raw_data/baxter.subsample.shared",
                   col_types = cols(Group = col_character(),
                                    .default = col_double())) %>%
  rename_all(tolower) %>%
  select(group, starts_with("otu")) %>%
  pivot_longer(-group, names_to="otu", values_to="count")
  
#load baxter con taxonomy
#select otu and taxonomy
#get rid of the number in parentheses and issues with unclassified.

taxonomy <- read_tsv("raw_data/baxter.cons.taxonomy") %>%
  rename_all(tolower) %>%
  select(otu, taxonomy) %>%
  mutate(otu = tolower(otu),
         taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";unclassified", "_unclassified"),
         taxonomy = str_replace_all(taxonomy, ";unclassified", ""),
         taxonomy = str_replace_all(taxonomy, ";$", ""),
         taxonomy = str_replace_all(taxonomy, ".*;", "")
  )

# join the multiple data frames.
# add a relative abundance fraction column for each row.

metadata <- read_tsv("raw_data/baxter.metadata.tsv",
                     col_types=cols(sample = col_character())) %>%
  rename_all(tolower) %>%
  rename(group = sample) %>%
  mutate(srn = dx_bin == "Adv Adenoma" | dx_bin == "Cancer",
         lesion = dx_bin == "Adv Adenoma" | dx_bin == "Cancer" | dx_bin == "Adenoma")

composite <- inner_join(shared, taxonomy, by="otu") %>%
  group_by(group, taxonomy) %>%
  summarize(count = sum(count), .groups="drop") %>%
  group_by(group) %>%
  mutate(rel_abund = count / sum(count)) %>%
  ungroup() %>%
  select(-count) %>%
  inner_join(., metadata, by="group")

# look at each genus and find out which genera are sig diff between groups -
# based on relative abundance.
# Compare two groups using Wilcox.

# nest list of all columns for every unique taxonomy, to be able to run -
# a wilcox test by srn for each taxonomy.  Create a new column for the test results.
# Test for rel_abund differences explained by srn, with the results data
# fed back into the source data frame. Pipe to broom::tidy before un-nesting.
# Use false discovery rate to correct the p values.
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

