# Analysis for FRA scenario training data
# Assumptions:
# This script is in the same directory as a folder called 'Data', which has the required results files
# The data files have at least one sheet named 'Data' which has a data frame of the evaluation results

# Setup  ----

library(tidyverse) # install.packages('tidyverse')
library(readxl)    # install.packages('readxl')

data_files <- dir('Data')

cat('Working with data file:', data_files[1])

d <- read_excel(file.path('Data', data_files[1]), sheet = 'Data')

# View(d)

names(d) <- make.names(names(d))

# Data prep ----

# Individual percent correct. Pre-test is form 1, post-test is form 4.
d_ind <- d %>%
  filter(Form %in% c(1, 4)) %>%
  group_by(Participant.Number, Form) %>%
  summarize(percent_correct = sum(Score) / n()) 

# Improvement. Positive values are the increase in scores from form 1 to form 4. 
d_imp <- d_ind %>%  
  ungroup() %>%
  group_by(Participant.Number) %>%
  summarize(improvement = diff(percent_correct),
            percent_improvement = improvement / percent_correct[1])

# Join back in other individual level data (craft, RR)
d_imp <- d_imp %>%
  left_join(d %>% select(Participant.Number, Craft, Railroad) %>% filter(!duplicated(Participant.Number)),
            by = 'Participant.Number')

# Analysis ----

m1 <- lm(improvement ~ Craft, data = d_imp)
summary.aov(m1)
summary(m1) # no significant effect of Craft

m2 <- lm(percent_improvement ~ Craft, data = d_imp)
summary.aov(m2)
summary(m2) # no significant effect of Craft still

d_imp %>% 
  group_by(Craft) %>%
  summarize(count = n(),
            median_improve = median(improvement),
            mean_improve = mean(improvement),
            median_pct_improve = median(percent_improvement),
            mean_pct_improve = mean(percent_improvement))


# Same, ungrouped

d_imp %>%
  ungroup() %>%
  summarize(count = n(),
            median_improve = median(improvement),
            mean_improve = mean(improvement),
            median_pct_improve = median(percent_improvement),
            mean_pct_improve = mean(percent_improvement))
