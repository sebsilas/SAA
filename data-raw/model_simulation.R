
library(Berkowitz)
library(musicassessr)
library(psychTestRCATME)
library(tidyverse)

set.seed(2)


smp <- musicassessr::item_sampler(item_bank = Berkowitz::combined_item_bank,
                           no_items = 10) %>%
  select(
    N, step.cont.loc.var, tonalness, log_freq
  ) %>%
  mutate(opti3 = .50,
         tmp_scores = .50) # Give the same tmp score for all items

# item_sampler gives you items increasing in length

score_by_trial <- smp %>%
  rowwise() %>%
  mutate(ability_score = psychTestRCATME::predict_based_on_mixed_effects_arrhythmic_model(
  model = musicassessr::lm2.2,
  new_data = tibble(N = N,
                    step.cont.loc.var = step.cont.loc.var,
                    tonalness = tonalness,
                    log_freq = log_freq,
                    opti3 = opti3,
                    tmp_scores = tmp_scores)
  )) %>%
  ungroup()


score_by_trial %>%
  ggplot(aes(x = N, y = ability_score)) +
    geom_point() +
    geom_line()
