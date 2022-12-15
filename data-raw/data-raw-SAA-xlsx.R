

library(tidyverse)

SAA_dict_df <- readxl::read_excel("data-raw/SAA.xlsx")
SAA_dict_df$de <- "a"

SAA_dict <- musicassessr::dict(additional_dict = SAA_dict_df)

da <- SAA_dict$as.data.frame()


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


usethis::use_data(SAA_dict_df, SAA_dict,
                  get_arrhythmic_score_percentile, get_rhythmic_score_percentile, get_long_note_score_percentile,
                  overwrite = TRUE, internal = TRUE)

