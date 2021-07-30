## code to prepare `data-raw/MST.xlsx` dataset goes here

MST_dict_df <- readxl::read_excel("data-raw/MST.xlsx")

MST_dict <- musicassessr::dict(additional_dict = MST_dict_df)


usethis::use_data(MST_dict_df, MST_dict, overwrite = TRUE, internal = TRUE)
