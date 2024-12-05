

library(musicassessr)
library(tidyverse)
library(lme4)

# Percentile functions

get_arrhythmic_score_percentile <- predict(musicassessr::lm2.2) %>%
  unname() %>%
  ecdf()

get_rhythmic_score_percentile <- predict(musicassessr::lm3.2) %>%
  unname() %>%
  ecdf()


long_note_score <- musicassessr::long_note_agg %>%
  select(contains('pca_long_note')) %>%
  mutate(Long_Note_Score = .33 * pca_long_note_randomness + .33 * pca_long_note_scoop + .33 * pca_long_note_accuracy)


get_long_note_score_percentile <- long_note_score %>%
  pull(Long_Note_Score) %>%
  ecdf()



# Is there some overlap between the data collections so we can (roughly) describe the final score distribution
# a combo of all 3 scores?


d1 <- ranef(musicassessr::lm2.2)$p_id %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "p_id") %>%
  rename(arrhythmic_score = `(Intercept)`)

d2 <- ranef(musicassessr::lm3.2)$p_id %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "p_id") %>%
  rename(rhythmic_score = `(Intercept)`)



long_note_tmp <- musicassessr::long_note_agg %>%
  select(p_id, contains('pca'))

d_joined <- d1 %>%
  left_join(d2, by = "p_id") %>%
  left_join(long_note_tmp, by = 'p_id') %>%
  na.omit


percentile_dist <- d_joined %>%
  transmute(
    arrhythmic_percentile = get_arrhythmic_score_percentile(arrhythmic_score),
    rhythmic_percentile = get_rhythmic_score_percentile(rhythmic_score),
    long_note_percentile = get_arrhythmic_score_percentile(.33*pca_long_note_randomness + .33*pca_long_note_scoop + .33*pca_long_note_accuracy),
    Final_SAA_Score = .33*arrhythmic_percentile + .33*rhythmic_percentile + .33*long_note_percentile
  )


hist(percentile_dist$arrhythmic_percentile)
hist(percentile_dist$rhythmic_percentile)
hist(percentile_dist$long_note_percentile)

hist(percentile_dist$Final_SAA_Score)

# But these distributions are not normal. We should be careful. This can be a temporary solution until I think of a better way of doing it.

# But the original distrs are:
# hist(d1$arrhythmic_score)
# hist(d2$rhythmic_score)
# hist(d3$pca_long_note_accuracy)
# hist(d3$pca_long_note_randomness)
# hist(d3$pca_long_note_scoop)
# Except pca_long_note_accuracy

Final_SAA_Score_m <- mean(percentile_dist$Final_SAA_Score, na.rm = TRUE)
Final_SAA_Score_sd <- sd(percentile_dist$Final_SAA_Score, na.rm = TRUE)


Berkowitz_item_bank_subset <- itembankr::subset_item_bank(Berkowitz::ngram_item_bank, item_length = c(4,20), return_as_item_bank_class = TRUE) %>%
  itembankr::set_item_bank_class()


usethis::use_data(get_arrhythmic_score_percentile,
                  get_rhythmic_score_percentile,
                  get_long_note_score_percentile,
                  Final_SAA_Score_m, Final_SAA_Score_sd,
                  overwrite = TRUE, internal = TRUE)



usethis::use_data(Berkowitz_item_bank_subset, overwrite = TRUE)


document()

credentials::set_github_pat()

install()

